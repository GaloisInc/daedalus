#pragma once

#include <map>
#include <tuple>
#include <variant>
#include <unordered_set>

#include "main_parser.h"
#include <ddl/input.h>
#include "Owned.hpp"

struct Blackhole {};

struct StreamThunk {
    uint64_t container;
    uint64_t index;
    explicit StreamThunk(uint64_t container, uint64_t index);

    bool getDecl(uint64_t refid, User::TopDecl *result);
};

struct TopThunk {
    uint64_t offset;

public:
    explicit TopThunk(uint64_t offset);

    // Borrows: this, input
    // Populates owned result
    // Returns true on success
    bool getDecl(DDL::Input input, User::TopDecl *result);
};

using oref = std::variant<
    Blackhole,
    TopThunk,
    StreamThunk,
    Owned<User::TopDecl>
>;

using generation_type = uint16_t;

struct ReferenceEntry {
    ReferenceEntry(oref value, generation_type gen);
    oref value;
    generation_type gen;
};

struct EncryptionContext {
    std::string key;
    EncryptionContext(std::string key)
    : key(key) {}
};

class ReferenceTable {

private:
    DDL::Input topinput;
    std::optional<EncryptionContext> encCtx;

    void process_xref(std::unordered_set<size_t>*, DDL::Input, DDL::Size);
    void process_oldXRef(std::unordered_set<size_t>*, DDL::Input, User::CrossRefAndTrailer);
    void process_newXRef(std::unordered_set<size_t>*, DDL::Input, User::XRefObjTable);
    void process_trailer(std::unordered_set<size_t>*, DDL::Input, User::TrailerDict);
    void register_uncompressed_reference(uint64_t refid, generation_type gen, uint64_t offset);
    void register_compressed_reference(uint64_t refid, uint64_t container, uint64_t index);
    void register_topdecl(uint64_t refid, generation_type gen, User::TopDecl topDecl);
    void unregister(uint64_t refid);

public: // temporarily public member
    std::map<uint64_t, ReferenceEntry> table;

    uint64_t currentObjId;
    generation_type currentGen;

public:
    std::optional<EncryptionContext> const& getEncryptionContext() const;

    bool resolve_reference(uint64_t refid, generation_type gen, DDL::Maybe<User::TopDecl> *result);
    void process_pdf(DDL::Input);
};

extern ReferenceTable references;

struct XrefException : public std::exception {
    const char * msg;
    XrefException(const char *msg) : msg(msg) {}
    const char * what () const throw () override
    {
        return msg;
    }
};
