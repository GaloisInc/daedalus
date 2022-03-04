#pragma once

#include <map>
#include <tuple>
#include <variant>

#include "main_parser.h"
#include "Owned.hpp"

/*

*/

struct Blackhole {};
struct StreamThunk {};
struct NullObject {};

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
    NullObject,
    User::TopDecl
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
    bool resolve_reference(DDL::Input input, uint64_t refid, uint16_t gen, DDL::Maybe<User::TopDecl> *result);
};

extern ReferenceTable references;
