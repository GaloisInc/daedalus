#include <gtest/gtest.h>

#include <string>
#include <string_view>
#include <vector>

#include <ddl/parse_error.h>

TEST(ParseError, TailCallContext) {
  DDL::ParserContextFrame frame("outer");
  frame.tailCall("inner");
  frame.tailCall("inner");
  frame.tailCall("outer");

  auto entries = frame.entries();
  ASSERT_EQ(entries.size(),3);
  EXPECT_EQ(
    std::string_view(std::get<DDL::ParserContextCall>(entries[0]).label),
    "outer");
  EXPECT_EQ(std::get<DDL::ParserContextCall>(entries[1]).count,2);
  EXPECT_EQ(
    std::string_view(std::get<DDL::ParserContextCall>(entries[2]).label),
    "outer");

  std::vector<std::string> labels;
  labels.reserve(66);
  for (size_t i = 0; i < 66; ++i) {
    labels.push_back("tail-" + std::to_string(i));
  }

  DDL::ParserContextFrame bounded("entry");
  for (auto const &label : labels) {
    bounded.tailCall(label.c_str());
  }

  entries = bounded.entries();
  ASSERT_EQ(entries.size(),66);
  EXPECT_EQ(
    std::string_view(std::get<DDL::ParserContextCall>(entries[0]).label),
    "entry");
  EXPECT_EQ(std::get<DDL::ParserContextOmitted>(entries[1]).count,2);
  EXPECT_EQ(
    std::string_view(std::get<DDL::ParserContextCall>(entries[2]).label),
    "tail-2");
  EXPECT_EQ(
    std::string_view(std::get<DDL::ParserContextCall>(entries[65]).label),
    "tail-65");
}
