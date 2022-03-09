#include "state.hpp"

ReferenceEntry::ReferenceEntry(oref value, generation_type gen)
: value(value), gen(gen) {}

TopThunk::TopThunk(uint64_t offset) : offset(offset) {}

bool
TopThunk::getDecl(DDL::Input input, User::TopDecl *result)
{
    input.copy();
    input.iDropMut(offset);
    DDL::ParseError error;
    std::vector<User::TopDecl> results;

    parseTopDecl(input, error, results);

    if (results.size() != 1) {
        for (auto &&x : results) { x.free(); }
        std::cerr << "PARSE ERROR " << input << " @ " << error.offset << std::endl;
        return false;
    }

    User::TopDecl topDecl = results[0];
    results.clear();

    *result = topDecl;

    return true;
}

StreamThunk::StreamThunk(uint64_t container, uint64_t index)
: container(container), index(index) {}

bool
StreamThunk::getDecl(DDL::Input input, uint64_t refid, User::TopDecl *result)
{
    DDL::Maybe<User::TopDecl> streamResult;
    if (!references.resolve_reference(input, container, 0, &streamResult)) {
        return false;
    }

    if (streamResult.isNothing()) {
        return false;
    }

    if (DDL::Tag::TopDeclDef::objstream != streamResult.borrowValue().borrow_obj().getTag()) {
        streamResult.free();
        return false;
    }

    auto stream = owned(streamResult.borrowValue().borrow_obj().get_objstream());
    streamResult.free();

    if (index >= stream->borrow_index().size().value) {
        return false;
    }

    auto byteOffset = stream->borrow_index().borrowElement(index).borrow_off().asSize();
    auto objInput = stream->get_bytes().iDrop(byteOffset);
    
    std::vector<User::Value> results;
    DDL::ParseError error;
    parseValue(objInput, error, results);

    if (1 != results.size()) {
        for (auto && x : results) { x.free(); }
        return false;
    }

    User::TopDeclDef topDef;
    topDef.init_value(results[0]);
    result->init(refid, 0, topDef);

    return true;
}

// Owns topDecl
void
ReferenceTable::register_topdecl(uint64_t refid, generation_type gen, User::TopDecl topDecl)
{
    table.insert_or_assign(refid, ReferenceEntry{owned(topDecl), gen});
}

void
ReferenceTable::register_compressed_reference(
    uint64_t refid,
    uint64_t container,
    uint64_t index
)
{
    table.insert_or_assign(refid, ReferenceEntry{StreamThunk{container, index}, 0});
}

void
ReferenceTable::register_uncompressed_reference(
    uint64_t refid,
    generation_type gen,
    uint64_t offset
)
{
    table.insert_or_assign(refid, ReferenceEntry{TopThunk(offset), 0});
}

void
ReferenceTable::unregister(uint64_t refid)
{
    table.erase(refid);
}

bool
ReferenceTable::resolve_reference(
    DDL::Input input, uint64_t refid, generation_type gen, DDL::Maybe<User::TopDecl> *result
) {
    auto cursor = table.find(refid);

    if (cursor == std::end(table) || cursor->second.gen != gen) {
        *result = DDL::Maybe<User::TopDecl>();
        return true;
    }

    // Copy the entry by value so that we can replace it with a blackhole if needed
    auto entry = cursor->second.value;

    std::cerr << "Resolving reference " << refid << " with thunk type " << entry.index() << std::endl;

    if (auto *top = std::get_if<Owned<User::TopDecl>>(&entry)) {
        *result = top->get();
        return true;
    }

    if (std::holds_alternative<Blackhole>(entry)) {
        return false;
    }

    if (auto *thunk = std::get_if<TopThunk>(&entry)) {
        cursor->second.value = Blackhole();
        User::TopDecl decl;
        bool success = thunk->getDecl(input, &decl);
        if (success) {
            cursor->second.value = borrowed(decl);
            *result = decl;
        }
        return success;
    }

    if (auto *thunk = std::get_if<StreamThunk>(&entry)) {
        cursor->second.value = Blackhole();
        User::TopDecl decl;
        bool success = thunk->getDecl(input, refid, &decl);
        if (success) {
            cursor->second.value = borrowed(decl);
            *result = decl;
        }
        return success;
    }

    return true;
}

void process_trailer(std::unordered_set<size_t> *visited, DDL::Input input, User::TrailerDict trailer)
{
    if (trailer.borrow_prev().isJust()) {
        auto offset = owned(DDL::integer_to_uint_maybe<8 * sizeof(size_t)>(trailer.borrow_prev().borrowValue()));
        if (offset->isNothing()) {
            throw XrefException("Trailer has invalid Prev value");
        }
        process_xref(visited, input, offset->borrowValue().asSize());
    }

    // PDF specifies that entries in XRefStm take precedence over entries in Prev, so we add them after adding
    // the prev entries so that the xrefstm entries can overwrite them.
    if (trailer.borrow_xrefstm().isJust()) {
        auto offset = owned(DDL::integer_to_uint_maybe<8 * sizeof(size_t)>(trailer.borrow_xrefstm().borrowValue()));
        if (offset->isNothing()) {
            throw XrefException("Trailer has invalid XRefStm value");
        }
        process_xref(visited, input, offset->borrowValue().asSize());
    }
}

void process_newXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::XRefObjTable table)
{
    auto xrefs = table.borrow_xref();
    process_trailer(visited, input, table.borrow_trailer());

    auto subsections = table.borrow_xref();
    for (DDL::Size i = 0; i < subsections.size(); i.increment()) {
        auto subsection = owned(subsections[i]);

        // XXX: bounds check
        uint64_t refid = subsection->borrow_firstId().asSize().value;

        auto entries = subsection->borrow_entries();
        for (DDL::Size j = 0; j < entries.size(); j.increment()) {
            auto entry = owned(entries[j]);

            switch (entry->getTag()) {
                case DDL::Tag::XRefObjEntry::compressed: {
                    auto compressed = entry->borrow_compressed();

                    uint64_t container = compressed.borrow_container_obj().asSize().value;
                    uint64_t obj_index = compressed.borrow_obj_index().asSize().value;

                    references.register_compressed_reference(refid, container, obj_index);
                    break;
                }
                case DDL::Tag::XRefObjEntry::free: {
                    references.unregister(entry->borrow_free().borrow_obj().asSize().value);
                    break;
                }

                case DDL::Tag::XRefObjEntry::inUse: {
                    auto inUse = entry->borrow_inUse();
                    
                    // 10 digit natural fits in 34 bits
                    uint64_t offset = inUse.borrow_offset().asSize().value;

                    // maximum generation number is 65,535
                    generation_type gen = inUse.borrow_gen().asSize().value;

                    std::cerr << "Adding uncompressed xref " << refid << " " << gen << " at " << offset << std::endl;

                    references.register_uncompressed_reference(refid, gen, offset);
                    break;
                }

                case DDL::Tag::XRefObjEntry::null: {
                    User::TopDecl topDecl;
                    User::TopDeclDef obj;
                    User::Value value;

                    value.init_null();
                    obj.init_value(value);
                    topDecl.init(refid, 0, obj);

                    references.register_topdecl(refid, 0, topDecl);
                    break;
                }
            }

            refid++;
        }
    }
}

// Borrows input and old
void process_oldXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::CrossRefAndTrailer old)
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
}

// Borrows input
void process_xref(std::unordered_set<size_t> *visited, DDL::Input input, DDL::Size offset)
{
    // Detect if cross-reference sections form a cycle
    if (!visited->insert(offset.value).second) {
        throw XrefException("XRef tables form loop");
    }

    DDL::ParseError error;
    std::vector<User::CrossRef> crossRefs;

    input.copy();
    parseCrossRef(input.iDrop(offset), error, crossRefs);

    if (crossRefs.size() != 1) {
        for (auto &&x : crossRefs) {
            x.free();
        }
        throw XrefException("Unable to parse xrefs");
    }

    auto crossRef = owned(crossRefs[0]);
    crossRefs.clear();

    switch (crossRef->getTag()) {
        case DDL::Tag::CrossRef::oldXref:
        process_oldXRef(visited, input, crossRef->borrow_oldXref());
        break;
        case DDL::Tag::CrossRef::newXref:
        process_newXRef(visited, input, crossRef->borrow_newXref());
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

void process_pdf(DDL::Input input)
{
    auto end = findPdfEnd(input.length().rep(), input.borrowBytes());
    if (end == not_found) {
        throw XrefException("End of pdf not found");
    }

    DDL::ParseError error;
    std::vector<DDL::UInt<64>> results;

    input.copy();
    DDL::Input x = input.iDrop(DDL::Size(end));
    parsePdfEnd(x, error, results);

    if (1 != results.size()) {
        for (auto && x : results) { x.free(); }
        throw XrefException("Failed parsing startxref");
    }

    auto result = owned(results[0]);
    auto offset = result->asSize();
    std::unordered_set<size_t> visited;
    process_xref(&visited, input, offset);
}

ReferenceTable references;
