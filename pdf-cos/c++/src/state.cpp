#include "state.hpp"

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

const char *
BlackholeException::what() const throw ()
{
    return "Entered a blackhole resolving references";
}

void
ReferenceTable::register_uncompressed_reference(
    uint64_t refid,
    uint16_t gen,
    uint64_t offset
)
{
    // XXX: Handle overwrite: need to .free(), etc.
    table.insert(
        std::make_pair(std::make_pair(refid, gen), TopThunk(offset))
    );
}

bool
ReferenceTable::resolve_reference(
    DDL::Input input, uint64_t refid, uint16_t gen, DDL::Maybe<User::TopDecl> *result
) {
    auto key = std::make_pair(refid, gen);

    auto cursor = table.find(key);

    if (cursor == std::end(table)) {
        *result = DDL::Maybe<User::TopDecl>();
        return true;
    }

    auto &entry = cursor->second;

    if (auto *top = std::get_if<User::TopDecl>(&entry)) {
        User::TopDecl decl = *top;
        decl.copy();
        *result = decl;
        return true;
    }

    if (std::holds_alternative<Blackhole>(entry)) {
        return false;
    }

    if (std::holds_alternative<NullObject>(entry)) {
        *result = DDL::Maybe<User::TopDecl>();
        return true;
    }

    if (auto *thunk = std::get_if<TopThunk>(&entry)) {
        User::TopDecl decl;
        bool success = thunk->getDecl(input, &decl);
        if (success) {
            decl.copy();
            table.insert(
                std::make_pair(key, decl)
            );
            *result = decl;
        }
        return success;
    }

    return true;
}


ReferenceTable references;
