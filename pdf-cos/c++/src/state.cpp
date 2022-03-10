#include "state.hpp"

#include <algorithm>
#include <cstring>
#include <vector>
#include <openssl/evp.h>

ReferenceEntry::ReferenceEntry(oref value, generation_type gen)
: value(value), gen(gen) {}

TopThunk::TopThunk(uint64_t offset) : offset(offset) {}

bool
TopThunk::getDecl(DDL::Input input, User::TopDecl *result)
{
    if (offset > input.length().value) {
        return false;
    }

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
StreamThunk::getDecl(uint64_t refid, User::TopDecl *result)
{
    DDL::Maybe<User::TopDecl> streamResult;
    if (!references.resolve_reference(container, 0, &streamResult)) {
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

    std::cerr << "Resolving reference " << refid << " with thunk type " << entry.index() << std::endl;

    if (auto *top = std::get_if<Owned<User::TopDecl>>(&entry)) {
        *result = top->get();
        return true;
    }

    if (std::holds_alternative<Blackhole>(entry)) {
        return false;
    }

    if (auto *thunk = std::get_if<TopThunk>(&entry)) {
        ReferenceContext refCon{*this, refid, gen};

        cursor->second.value = Blackhole();
        User::TopDecl decl;
        bool success = thunk->getDecl(topinput, &decl);
        if (success) {
            cursor->second.value = borrowed(decl);
            *result = decl;
        }
        return success;
    }

    if (auto *thunk = std::get_if<StreamThunk>(&entry)) {
        ReferenceContext refCon{*this, refid, gen};

        cursor->second.value = Blackhole();
        User::TopDecl decl;
        bool success = thunk->getDecl(refid, &decl);
        if (success) {
            cursor->second.value = borrowed(decl);
            *result = decl;
        }
        return success;
    }

    return true;
}

namespace {

    std::string makeFileKeyAlg2(
        DDL::Array<DDL::UInt<8>> encO,
        DDL::Integer encP,
        DDL::Array<DDL::UInt<8>> id0,
        std::string password
    ){
        const uint8_t padding[] {
          0x28, 0xBF, 0x4E, 0x5E, 0x4E, 0x75, 0x8A, 0x41
        , 0x64, 0x00, 0x4E, 0x56, 0xFF, 0xFA, 0x01, 0x08
        , 0x2E, 0x2E, 0x00, 0xB6, 0xD0, 0x68, 0x3E, 0x80
        , 0x2F, 0x0C, 0xA9, 0xFE, 0x64, 0x53, 0x69, 0x7A};

        EVP_MD const* md5 = EVP_get_digestbyname("MD5"); 
        EVP_MD_CTX *ctx = EVP_MD_CTX_new();
        
        EVP_DigestInit(ctx, md5);
        
        EVP_DigestUpdate(ctx, password.data(), std::min(password.size(), size_t(32)));
        
        if (password.size() < 32) {
            EVP_DigestUpdate(ctx, padding, 32 - password.size());
        }

        std::vector<uint8_t> hashInput;
        hashInput.reserve(encO.size().value);
        for (DDL::Size i = 0; i < encO.size(); i.increment()) {
            hashInput.push_back(encO.borrowElement(i).rep());
        }
        EVP_DigestUpdate(ctx, hashInput.data(), hashInput.size());

        uint32_t pval = encP.asSize().value;
        uint8_t pbytes[4] = { uint8_t(pval >> (0*8)), uint8_t(pval >> (1*8)), uint8_t(pval >> (2*8)), uint8_t(pval >> (3*8)) };
        EVP_DigestUpdate(ctx, pbytes, std::size(pbytes));

        hashInput.clear();
        hashInput.reserve(id0.size().value);
        for (DDL::Size i = 0; i < encO.size(); i.increment()) {
            hashInput.push_back(id0.borrowElement(i).rep());
        }
        EVP_DigestUpdate(ctx, hashInput.data(), hashInput.size());

        // XXX: If document metadata is not being encrypted, pass 4 bytes with value 0xFFFFFFFF to the MD5

        unsigned char md[EVP_MAX_MD_SIZE];
        unsigned int mdsz = std::size(md);
        EVP_DigestFinal(ctx, md, &mdsz);
        EVP_MD_CTX_free(ctx);

        // XXX: Revision 3 or greater only
        for (int i = 0; i < 50; i++) {
            EVP_Digest(md, mdsz, md, &mdsz, md5, NULL);
        }     

        return std::string(reinterpret_cast<char const*>(md), size_t(mdsz));
    }


    EncryptionContext makeEncryptionContext(User::EncryptionDict dict) {
        auto encO = dict.borrow_encO();
        auto encP = dict.borrow_encP();
        auto id0 = dict.borrow_id0();
        auto pwd = "";

        std::string key = makeFileKeyAlg2(encO, encP, id0, pwd);

        return EncryptionContext(key, dict.borrow_ciph());
    }
}

void ReferenceTable::process_trailer(std::unordered_set<size_t> *visited, DDL::Input input, User::TrailerDict trailer)
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

void ReferenceTable::process_newXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::XRefObjTable table)
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

                    register_compressed_reference(refid, container, obj_index);
                    break;
                }
                case DDL::Tag::XRefObjEntry::free: {
                    unregister(entry->borrow_free().borrow_obj().asSize().value);
                    break;
                }

                case DDL::Tag::XRefObjEntry::inUse: {
                    auto inUse = entry->borrow_inUse();
                    
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

    if (table.borrow_trailer().borrow_encrypt().isJust()) {
        //XXX encCtx = makeEncryptionContext(trailer.borrow_encrypt().borrowValue());
    }
}

// Borrows input and old
void ReferenceTable::process_oldXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::CrossRefAndTrailer old)
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
    
    if (old.borrow_trailer().borrow_encrypt().isJust()) {
        //XXX encCtx = makeEncryptionContext(trailer.borrow_encrypt().borrowValue());
    }
}

std::optional<EncryptionContext> const&
ReferenceTable::getEncryptionContext() const
{
    return encCtx;
}


// Borrows input
void ReferenceTable::process_xref(std::unordered_set<size_t> *visited, DDL::Input input, DDL::Size offset)
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


size_t findPdfStart(size_t len, char const* bytes) {
    char const* tgt = "%PDF-";

    char const* found = (char const*)memmem(bytes, len, tgt, 5);

    if (NULL == found) {
        throw XrefException("Start of PDF not found");
    }

    return found - bytes;
}

void ReferenceTable::process_pdf(DDL::Input input)
{
    auto start = findPdfStart(input.length().value, input.borrowBytes());
    input.iDropMut(start);

    input.copy();
    references.topinput = input;

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
