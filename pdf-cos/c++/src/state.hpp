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
    uint64_t refid;
    uint64_t container;
    uint64_t index;
    explicit StreamThunk(uint64_t refid, uint64_t container, uint64_t index);

    bool getDecl(DDL::Input input, User::TopDecl *result);
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

class BlackholeException : public std::exception
{
public:
    const char * what () const throw () override;
};

class ReferenceTable {
public: // temporary
    std::map<std::pair<uint64_t, uint16_t>, oref> table;

public:
    void register_uncompressed_reference(uint64_t refid, uint16_t gen, uint64_t offset);
    void register_compressed_reference(uint64_t refid, uint64_t container, uint64_t index);
    void register_topdecl(uint64_t refid, uint16_t gen, User::TopDecl topDecl);
    bool resolve_reference(DDL::Input input, uint64_t refid, uint16_t gen, DDL::Maybe<User::TopDecl> *result);
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

void process_xref(std::unordered_set<size_t>*, DDL::Input, DDL::Size);
void process_oldXRef(std::unordered_set<size_t>*, DDL::Input, User::CrossRefAndTrailer);
void process_newXRef(std::unordered_set<size_t>*, DDL::Input, User::XRefObjTable);
void process_trailer(std::unordered_set<size_t>*, DDL::Input, User::TrailerDict);
void process_pdf(DDL::Input);
