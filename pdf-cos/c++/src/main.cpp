#include <iostream>
#include <chrono>
#include <unordered_set>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/mman.h>
#include <unistd.h>

#include <ddl/input.h>
#include <ddl/number.h>
#include <main_parser.h>

#include "state.hpp"
#include "Owned.hpp"

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

DDL::Input inputFromFile(const char *file) {

  DDL::Input result{};
  char *bytes;
  size_t size;

  int fd = open(file,O_RDONLY);
  if (fd == -1) return result;

  struct stat info;
  if (fstat(fd, &info) != 0) goto done;

  size = info.st_size;
  bytes = (char*)mmap(NULL, size, PROT_READ, MAP_PRIVATE, fd, 0);
  if (bytes == MAP_FAILED) goto done;

  result = DDL::Input{file,bytes,size};

  munmap (bytes, size); // wecopied them

done:
  close(fd);
  return result;
}

namespace {

  class XrefException : public std::exception {
    const char * what () const throw () override
    {
      return "Invalid Cross Reference Table";
    }
  };

  void process_xref(std::unordered_set<size_t>*, DDL::Input, DDL::Size);
  void process_oldXRef(std::unordered_set<size_t>*, DDL::Input, User::CrossRefAndTrailer);
  void process_newXRef(std::unordered_set<size_t>*, DDL::Input, User::XRefObjTable);
  void process_trailer(std::unordered_set<size_t>*, DDL::Input, User::TrailerDict);
  
  void process_trailer(std::unordered_set<size_t> *visited, DDL::Input input, User::TrailerDict trailer)
  {
    if (trailer.borrow_prev().isJust()) {
      auto offset = Owned(DDL::integer_to_uint_maybe<8 * sizeof(size_t)>(trailer.borrow_prev().getValue()));
      if (offset->isNothing()) {
        throw XrefException();
      }
      process_xref(visited, input, offset->getValue().asSize());
    }
  }

  void process_newXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::XRefObjTable table)
  {
    auto xrefs = table.borrow_xref();
    process_trailer(visited, input, table.borrow_trailer());

    auto subsections = table.borrow_xref();
    for (DDL::Size i = 0; i < subsections.size(); i.increment()) {
      auto subsection = Owned(subsections[i]);

      // XXX: bounds check
      uint64_t refid = subsection->borrow_firstId().getValue().get_ui();

      auto entries = subsection->borrow_entries();
      for (DDL::Size j = 0; j < entries.size(); j.increment()) {
        auto entry = Owned(entries[j]);

        switch (entry->getTag()) {
          case DDL::Tag::XRefObjEntry::compressed:
            throw XrefException(); // XXX: not implemented
            break;
          case DDL::Tag::XRefObjEntry::free:
            throw XrefException(); // XXX: not implemented
            break;

          case DDL::Tag::XRefObjEntry::inUse: {
            auto inUse = entry->borrow_inUse();
            
            // 10 digit natural fits in 34 bits
            uint64_t offset = inUse.borrow_offset().asSize().value;

            // maximum generation number is 65,535
            uint16_t gen = inUse.borrow_gen().getValue().get_ui();

            std::cerr << "Adding xref " << refid << " " << gen << " at " << offset << std::endl;

            references.register_uncompressed_reference(refid, gen, offset);
            break;
          }

          case DDL::Tag::XRefObjEntry::null: {
            throw XrefException(); // XXX: not implemented
            break;
          }
        }
      }


    }

    throw XrefException(); // XXX: not implemented
  }

  void process_oldXRef(std::unordered_set<size_t> *visited, DDL::Input input, User::CrossRefAndTrailer old)
  {
    process_trailer(visited, input, old.borrow_trailer());

    auto subsections = old.borrow_xref();

    DDL::Size in = subsections.size().value;
    for (DDL::Size i = 0; i < in; i.increment()) {
      auto sub = Owned(subsections[i]);

      // XXX: Bounds check
      uint64_t refid = sub->borrow_firstId().getValue().get_ui();

      auto entries = sub->borrow_entries();
      for (DDL::Size j = 0; j < entries.size(); j.increment()) {
        auto entry = Owned(entries[j]);

        switch (entry->getTag()) {
          case DDL::Tag::CrossRefEntry::inUse: {

            auto isUse = entry->borrow_inUse();

            // 10 digit natural fits in 34 bits
            uint64_t offset = isUse.borrow_offset().asSize().value;

            // maximum generation number is 65,535
            uint16_t gen = isUse.borrow_gen().getValue().get_ui();

            std::cerr << "Adding xref " << refid << " " << gen << " at " << offset << std::endl;

            references.register_uncompressed_reference(refid, gen, offset);
            break;
          }
          case DDL::Tag::CrossRefEntry::free: {
            // XXX: implement
            break;
          }
        }

        refid++;
      }
    }
  }

  void process_xref(std::unordered_set<size_t> *visited, DDL::Input input, DDL::Size offset) {

    // Detect if cross-reference sections form a cycle
    if (!visited->insert(offset.value).second) {
      throw XrefException();
    }

    DDL::ParseError error;
    std::vector<User::CrossRef> crossRefs;

    input.copy();
    parseCrossRef(input.iDrop(offset), error, crossRefs);

    if (crossRefs.size() != 1) {
      for (auto &&x : crossRefs) { x.free(); }
      throw XrefException();
    }

    auto crossRef = Owned(crossRefs[0]);
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
}


int main(int argc, char* argv[]) {
  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " FILE\n";
    return 1;
  }

  auto input = inputFromFile(argv[1]);
  auto end   = findPdfEnd(input.length().rep(), input.borrowBytes());
  if (end == not_found) {
    std::cerr << "`startxref` not found\n";
    input.free();
    return 1;
  }

  DDL::ParseError error;
  std::vector<DDL::UInt<64>> results;

  input.copy();
  DDL::Input x = input.iDrop(DDL::Size(end));
  x.dumpInput();
  parsePdfEnd(x, error, results);

  // XXX: It looks like the parser is not consuming `x` as expected.

  switch (results.size()) {
    case 0:
      std::cerr << "Parse error at offset " << error.offset << std::endl;
      std::cerr << "Failed to parse xref offset\n";
      input.free();
      return 2;

    case 1:
      break;

    default:
      std::cerr << "[bug] Ambiguous xref offset\n";
      input.free();
      return 3;
  }

  auto offset = results[0].asSize();
  std::unordered_set<size_t> visited;
  process_xref(&visited, input, offset);

  for (auto && [key, val] : references.table) {
    auto && [refid, gen] = key;
    std::cerr << "Getting " << refid << " " << gen << std::endl;
    DDL::Maybe<User::TopDecl> decl;

    if (references.resolve_reference(input, refid, gen, &decl)) {
      DDL::toJS(std::cerr, decl);
      std::cerr << std::endl;
      decl.free();
    } else {
      std::cerr << "Failed\n";
    }
  }

  input.free();
  return 0;
}
