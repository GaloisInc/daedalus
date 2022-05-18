#include "main_parser.h"
 
static bool parser_Main(DDL::ParserState&, DDL::Unit*, DDL::Input*,
                        DDL::Input);
void parseMain ( DDL::ParseError &error
               , std::vector<DDL::Unit> &results
               , DDL::Input a1
               ) {
  DDL::ParserState p;
  DDL::Unit out_result;
  DDL::Input out_input;
  if (parser_Main(p, &out_result, &out_input, a1)) {
    results.push_back(out_result);
    out_input.free();
  } else {
    error = p.getParseError();
  }
}
bool parser_Main(DDL::ParserState& p, DDL::Unit* _result,
                 DDL::Input* _result_input, DDL::Input a1) {
  
// Parameters for L_0_Main
  DDL::Input Main_0_ra0o;
  
// Parameters for L_1_Main
  DDL::Input Main_1_ra0o;
  
// Parameters for L_3_Main
  DDL::Array<DDL::UInt<8>> Main_3_ra0o;
  DDL::Input Main_3_ra1o;
  
// Parameters for L_4_Main
  DDL::Input Main_4_ra0o;
  
// Parameters for L_5_Main
  DDL::Input Main_5_ra0o;
  Main_5_ra0o = a1;
  goto Main_5;
  // NormalBlock
  Main_0: {
    p.popDebug();
    *_result = DDL::Unit();
    *_result_input = Main_0_ra0o;
    return true;
  }
  // NormalBlock
  Main_1: {
    DDL::Array<DDL::UInt<8>> r0 = DDL::Array<DDL::UInt<8>>{DDL::UInt<8>(76UL),
                                                           DDL::UInt<8>(101UL), DDL::UInt<8>(102UL),
                                                           DDL::UInt<8>(116UL), DDL::UInt<8>(111UL),
                                                           DDL::UInt<8>(118UL), DDL::UInt<8>(101UL),
                                                           DDL::UInt<8>(114UL), DDL::UInt<8>(32UL),
                                                           DDL::UInt<8>(105UL), DDL::UInt<8>(110UL),
                                                           DDL::UInt<8>(112UL), DDL::UInt<8>(117UL),
                                                           DDL::UInt<8>(116UL)};
    p.noteFail(true, "./test.ddl:3:33--3:35", Main_1_ra0o, r0);
    Main_1_ra0o.free();
    r0.free();
    p.popDebug();
    return false;
  }
  // NormalBlock
  Main_3: {
    DDL::UInt<64> r0 = DDL::UInt<64>{Main_3_ra0o.size().rep()};
    Main_3_ra0o.free();
    DDL::Input r1 = Main_3_ra1o.iDrop(DDL::Size::from(r0.rep()));
    DDL::Bool r2 = r1.length() == 0;
    if (r2.getValue()) {
      Main_0_ra0o = r1;
      goto Main_0;
    } else {
      Main_1_ra0o = r1;
      goto Main_1;
    }
  }
  // NormalBlock
  Main_4: {
    DDL::Array<DDL::UInt<8>> r0 = DDL::Array<DDL::UInt<8>>{DDL::UInt<8>(69UL),
                                                           DDL::UInt<8>(120UL), DDL::UInt<8>(112UL),
                                                           DDL::UInt<8>(101UL), DDL::UInt<8>(99UL),
                                                           DDL::UInt<8>(116UL), DDL::UInt<8>(101UL),
                                                           DDL::UInt<8>(100UL), DDL::UInt<8>(32UL)};
    DDL::Array<DDL::UInt<8>> r1 = DDL::Array<DDL::UInt<8>>{DDL::UInt<8>(97UL),
                                                           DDL::UInt<8>(10UL), DDL::UInt<8>(99UL)};
    DDL::Array<DDL::Array<DDL::UInt<8>>> r2 = DDL::Array<DDL::Array<DDL::UInt<8>>>{r0,
                                                                                   r1};
    DDL::Array<DDL::UInt<8>> r3 = DDL::Array<DDL::UInt<8>>(r2);
    r2.free();
    p.noteFail(true, "./test.ddl:3:18--3:30", Main_4_ra0o, r3);
    Main_4_ra0o.free();
    r3.free();
    p.popDebug();
    return false;
  }
  // NormalBlock
  Main_5: {
    DDL::Array<DDL::UInt<8>> r0 = DDL::Array<DDL::UInt<8>>{DDL::UInt<8>(97UL),
                                                           DDL::UInt<8>(10UL), DDL::UInt<8>(99UL)};
    DDL::Bool r1 = Main_5_ra0o.hasPrefix(r0);
    if (r1.getValue()) {
      Main_3_ra0o = r0;
      Main_3_ra1o = Main_5_ra0o;
      goto Main_3;
    } else {
      r0.free();
      Main_4_ra0o = Main_5_ra0o;
      goto Main_4;
    }
  }
}