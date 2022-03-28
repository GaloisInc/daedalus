#include "state.hpp"
#include "debug.hpp"

#include <algorithm>
#include <cstring>
#include <vector>


template<class> inline constexpr bool always_false_v = false;

ReferenceEntry::ReferenceEntry(oref value, generation_type gen)
: value(value), gen(gen) {}

TopThunk::TopThunk(uint64_t offset) : offset(offset) {}

// Owns input
// Writes result on success
// Returns true on success
bool
TopThunk::getDecl(DDL::Input input, User::TopDecl *result)
{
    if (offset > input.length().value) {
        input.free();
        return false;
    }

    input.iDropMut(offset);

    DDL::ParseError error;
    std::vector<User::TopDecl> results;

    input.copy();
    parseTopDecl(error, results, input);

    if (results.size() != 1) {
        for (auto &&x : results) { x.free(); }
        std::cerr << "ERROR: PARSE ERROR " << input.borrowNameBytes()
                                  << " at " << error.offset << std::endl;
        input.free();
        return false;
    }

    input.free();

    User::TopDecl topDecl = results[0];
    results.clear();

    *result = topDecl;

    return true;
}

StreamThunk::StreamThunk(uint64_t container, uint64_t index)
: container(container), index(index) {}

bool
StreamThunk::getDecl(uint64_t refid, User::TopDecl *result)
{
    DDL::Maybe<User::TopDecl> streamResult;
    if (!references.resolve_reference(container, 0, &streamResult)) {
        return false;
    }

    if (streamResult.isNothing()) {
        return false;
    }

    if (DDL::Tag::TopDeclDef::stream != streamResult.borrowValue().borrow_obj().getTag()) {
        streamResult.free();
        return false;
    }

    auto stream = streamResult.borrowValue().borrow_obj().get_stream();
    streamResult.free();

    DDL::ParseError error;
    std::vector<User::ObjStream> results;
    parseObjStream(error, results, DDL::Input("empty", ""), stream);

    if (results.size() != 1) {
        for (auto && x : results) { x.free(); }
        return false;
    }

    auto objstream = Owned(results[0]);

    if (index >= objstream->borrow_index().size().value) {
        return false;
    }

    auto byteOffset = objstream->borrow_index().borrowElement(index).borrow_off().asSize();
    auto objInput = objstream->get_bytes().iDrop(byteOffset);
    
    std::vector<User::Value> vresults;
    parseValue(error, vresults, objInput);

    if (1 != vresults.size()) {
        for (auto && x : results) { x.free(); }
        return false;
    }

    User::TopDeclDef topDef;
    topDef.init_value(vresults[0]);
    result->init(refid, 0, topDef);

    return true;
}

// Owns topDecl
void
ReferenceTable::register_topdecl(uint64_t refid, generation_type gen, User::TopDecl topDecl)
{
    dbg << "NULL reference " << refid << std::endl;
    table.insert_or_assign(refid, ReferenceEntry{Owned(topDecl), gen});
}

void
ReferenceTable::register_compressed_reference(
    uint64_t refid,
    uint64_t container,
    uint64_t index
)
{
    dbg << "Compressed reference " << refid << " at " << container << " " << index << std::endl;
    table.insert_or_assign(refid, ReferenceEntry{StreamThunk{container, index}, 0});
}

void
ReferenceTable::register_uncompressed_reference(
    uint64_t refid,
    generation_type gen,
    uint64_t offset
)
{
    dbg << "Uncompressed reference " << refid << " gen " << gen <<  " at " << offset << std::endl;
    table.insert_or_assign(refid, ReferenceEntry{TopThunk(offset), gen});
}

void
ReferenceTable::unregister(uint64_t refid)
{
    table.erase(refid);
}

class ReferenceContext {
    ReferenceTable &tab;
    uint64_t oldObjId;
    generation_type oldGen;

public:
    ReferenceContext(ReferenceTable &tab, uint64_t objId, generation_type gen)
    : tab(tab), oldObjId(tab.currentObjId), oldGen(tab.currentGen) {
        tab.currentObjId = objId;
        tab.currentGen = gen;
    }

    ~ReferenceContext() {
        tab.currentObjId = oldObjId;
        tab.currentGen = oldGen;
    }
};

bool
ReferenceTable::resolve_reference(
    uint64_t refid, generation_type gen, DDL::Maybe<User::TopDecl> *result
) {
    auto cursor = table.find(refid);

    if (cursor == std::end(table) || cursor->second.gen != gen) {
        *result = DDL::Maybe<User::TopDecl>();
        return true;
    }

    // Copy the entry by value so that we can replace it with a blackhole if needed
    auto entry = cursor->second.value;

    dbg << "Resolving reference " << refid << " with thunk type " << entry.index() << std::endl;

    return std::visit([refid, gen, this, cursor, result](auto&& arg) -> bool {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Owned<User::TopDecl>>) {
            *result = arg.get();
            return true;
        } else if constexpr (std::is_same_v<T, Blackhole>) {
            return false;
        } else if constexpr (std::is_same_v<T, TopThunk>) {
            ReferenceContext refCon{*this, refid, gen};
            cursor->second.value = Blackhole();
            User::TopDecl decl;
            bool success = arg.getDecl(topinput->get(), &decl);
            if (success) {
                cursor->second.value = borrowed(decl);
                *result = decl;
            }
            return success;
        } else if constexpr (std::is_same_v<T, StreamThunk>) {
            ReferenceContext refCon{*this, refid, gen};
            cursor->second.value = Blackhole();
            User::TopDecl decl;
            bool success = arg.getDecl(refid, &decl);
            if (success) {
                cursor->second.value = borrowed(decl);
                *result = decl;
            }
            return success;
        } else {
            static_assert(always_false_v<T>, "non-exhaustive visitor!");
        }
    }, entry);
}

namespace {

}

void ReferenceTable::process_trailer(std::unordered_set<size_t> *visited, DDL::Input input, User::TrailerDict trailer)
{
    if (trailer.borrow_prev().isJust()) {
        auto offset = Owned(DDL::integer_to_uint_maybe<8 * sizeof(size_t)>(trailer.borrow_prev().borrowValue()));
        if (offset->isNothing()) {
            throw XrefException("Trailer has invalid Prev value");
        }
        dbg << "Has PREV at " << offset->borrowValue().asSize().value << std::endl;
        process_xref(visited, input, offset->borrowValue().asSize(), false);
        dbg << "prev complete" << std::endl;
    }

    // PDF specifies that entries in XRefStm take precedence over entries in Prev, so we add them after adding
    // the prev entries so that the xrefstm entries can overwrite them.
    if (trailer.borrow_xrefstm().isJust()) {
        auto offset = Owned(DDL::integer_to_uint_maybe<8 * sizeof(size_t)>(trailer.borrow_xrefstm().borrowValue()));
        if (offset->isNothing()) {
            throw XrefException("Trailer has invalid XRefStm value");
        }
        process_xref(visited, input, offset->borrowValue().asSize(), false);
    }
}

void ReferenceTable::process_newXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::XRefObjTable table, bool top)
{
    auto xrefs = table.borrow_xref();
    process_trailer(visited, input, table.borrow_trailer());

    auto subsections = table.borrow_xref();
    for (DDL::Size i = 0; i < subsections.size(); i.increment()) {
        auto subsection = subsections.borrowElement(i);

        // XXX: bounds check
        uint64_t refid = subsection.borrow_firstId().asSize().value;
        dbg << "XRef Section starting at " << refid << std::endl;

        auto entries = subsection.borrow_entries();
        for (DDL::Size j = 0; j < entries.size(); j.increment()) {
            auto entry = entries.borrowElement(j);

            switch (entry.getTag()) {
                case DDL::Tag::XRefObjEntry::compressed: {
                    auto compressed = entry.borrow_compressed();

                    uint64_t container = compressed.borrow_container_obj().asSize().value;
                    uint64_t obj_index = compressed.borrow_obj_index().asSize().value;

                    register_compressed_reference(refid, container, obj_index);
                    break;
                }
                case DDL::Tag::XRefObjEntry::free: {
                    dbg << "FREE entry " << refid << std::endl;
                    unregister(entry.borrow_free().borrow_obj().asSize().value);
                    break;
                }

                case DDL::Tag::XRefObjEntry::inUse: {
                    auto inUse = entry.borrow_inUse();
                    
                    // 10 digit natural fits in 34 bits
                    uint64_t offset = inUse.borrow_offset().asSize().value;

                    // maximum generation number is 65,535
                    generation_type gen = inUse.borrow_gen().asSize().value;

                    register_uncompressed_reference(refid, gen, offset);
                    break;
                }

                case DDL::Tag::XRefObjEntry::null: {
                    User::TopDecl topDecl;
                    User::TopDeclDef obj;
                    User::Value value;

                    value.init_null();
                    obj.init_value(value);
                    topDecl.init(refid, 0, obj);

                    register_topdecl(refid, 0, topDecl);
                    break;
                }
            }

            refid++;
        }
    }

    if (top) {
        process_trailer_post(table.borrow_trailer());
    }
}

void ReferenceTable::process_trailer_post(User::TrailerDict trailer)
{
    if (trailer.borrow_encrypt().isJust()) {
        DDL::Input emptyInput("empty", "", DDL::Size(0));
        DDL::ParseError error;
        std::vector<User::EncryptionDict> results;

        parseEncryptionDict(error, results, emptyInput, trailer.borrow_encrypt().getValue());
        if (1 != results.size()) {
            for (auto && x : results) { x.free(); }
            throw XrefException("Bad encryption dictionary");
        }

        auto edict = Owned(results[0]);
        encCtx = makeEncryptionContext(edict.borrow());
        std::cerr << "INFO: Using encryption\n";
    }

    if (trailer.borrow_root().isJust()) {
      root = Owned(trailer.borrow_root().getValue());
    }
}

// Borrows input and old
void ReferenceTable::process_oldXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::CrossRefAndTrailer old, bool top)
{
    process_trailer(visited, input, old.borrow_trailer());

    auto subsections = old.borrow_xref();

    DDL::Size in = subsections.size().value;
    for (DDL::Size i = 0; i < in; i.increment()) {
        auto sub = subsections.borrowElement(i);

        // XXX: Bounds check
        uint64_t refid = sub.borrow_firstId().asSize().value;

        auto entries = sub.borrow_entries();
        for (DDL::Size j = 0; j < entries.size(); j.increment()) {
            auto entry = entries.borrowElement(j);

            switch (entry.getTag()) {
                case DDL::Tag::CrossRefEntry::inUse: {
                    auto isUse = entry.borrow_inUse();

                    // 10 digit natural fits in 34 bits
                    uint64_t offset = isUse.borrow_offset().asSize().value;

                    // maximum generation number is 65,535
                    generation_type gen = isUse.borrow_gen().asSize().value;

                    references.register_uncompressed_reference(refid, gen, offset);
                    break;
                }

                case DDL::Tag::CrossRefEntry::free: {
                    references.unregister(entry.borrow_free().borrow_obj().asSize().value);
                    break;
                }
            }

            refid++;
        }
    }
    
    if (top) {
        process_trailer_post(old.borrow_trailer());
    }
}

std::optional<EncryptionContext> const&
ReferenceTable::getEncryptionContext() const
{
    return encCtx;
}

std::optional<Owned<User::Ref>> const&
ReferenceTable::getRoot() const { return root; }


// Borrows input
void ReferenceTable::process_xref(std::unordered_set<size_t> *visited, DDL::Input input, DDL::Size offset, bool top)
{
    // Detect if cross-reference sections form a cycle
    if (!visited->insert(offset.value).second) {
        throw XrefException("XRef tables form loop");
    }

    DDL::ParseError error;
    std::vector<User::CrossRef> crossRefs;

    input.copy();
    parseCrossRef(error, crossRefs, input.iDrop(offset));

    if (crossRefs.size() != 1) {
        for (auto &&x : crossRefs) {
            x.free();
        }
        throw XrefException("Unable to parse xrefs");
    }

    auto crossRef = Owned(crossRefs[0]);
    crossRefs.clear();

    switch (crossRef->getTag()) {
        case DDL::Tag::CrossRef::oldXref:
        process_oldXRef(visited, input, crossRef->borrow_oldXref(), top);
        break;
        case DDL::Tag::CrossRef::newXref:
        process_newXRef(visited, input, crossRef->borrow_newXref(), top);
        break;
    }
}

const size_t not_found = (size_t) (-1);

size_t findPdfEnd(size_t len, const char *bytes) {
  const char *tgt = "startxref";
  const size_t tgt_last = 8;

  if (len <= tgt_last) return not_found;

  // Look for startxref
  size_t ix = len - 1;
  size_t tgt_ix = tgt_last;
  while (true) {
    if (bytes[ix] == tgt[tgt_ix]) {
      if (tgt_ix == 0) return ix;
      if (ix == 0) return not_found;
      --tgt_ix;
      --ix;
    } else {
      if (ix == 0) return not_found;
      if (tgt_ix == tgt_last) --ix; else tgt_ix = tgt_last;
    }
  }
}


size_t findPdfStart(size_t len, char const* bytes) {
    char const* tgt = "%PDF-";

    char const* found = (char const*)memmem(bytes, len, tgt, 5);

    if (NULL == found) {
        throw XrefException("Start of PDF not found");
    }

    return found - bytes;
}

// Owns input
void ReferenceTable::process_pdf(DDL::Input input)
{
    auto start = findPdfStart(input.length().value, input.borrowBytes());
    input.iDropMut(start);

    references.topinput = Owned(input);

    auto end = findPdfEnd(input.length().rep(), input.borrowBytes());
    if (end == not_found) {
        throw XrefException("End of pdf not found");
    }

    DDL::ParseError error;
    std::vector<DDL::UInt<64>> results;

    input.copy();
    parsePdfEnd(error, results, input.iDrop(DDL::Size(end)));

    if (1 != results.size()) {
        for (auto && x : results) { x.free(); }
        throw XrefException("Failed parsing startxref");
    }

    auto result = Owned(results[0]);
    auto offset = result->asSize();
    std::unordered_set<size_t> visited;
    process_xref(&visited, input, offset, true);
}

ReferenceTable references;
