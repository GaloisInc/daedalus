#ifndef main_parser_H
#define main_parser_H
 
#include <ddl/parser.h>
#include <ddl/size.h>
#include <ddl/input.h>
#include <ddl/unit.h>
#include <ddl/bool.h>
#include <ddl/number.h>
#include <ddl/float.h>
#include <ddl/bitdata.h>
#include <ddl/integer.h>
#include <ddl/cast.h>
#include <ddl/maybe.h>
#include <ddl/array.h>
#include <ddl/map.h>
#include <ddl/owned.h>
#include <ddl/utils.h>
#include <optional>
 
 
// --- Parsing Functions ---
namespace DDL {
  namespace ResultOf {
    using parseMain = DDL::Unit;
  }
}
void parseMain ( DDL::ParseError &error
               , std::vector<DDL::Unit> &results
               , DDL::Input a1
               );

#endif