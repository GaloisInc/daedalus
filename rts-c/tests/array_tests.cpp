#include <gtest/gtest.h>
#include <ddl/array.h>

#include <string>

#include "comparisons.hpp"

TEST(Arrays, EmptyArray) {
    DDL::Array<DDL::UInt<32>> a {DDL::Size(0)};
    EXPECT_EQ(a.size(), 0);
    a.free();
}

TEST(Arrays, Comparisons) {
    DDL::Array<DDL::Bool> cases[] {
        {DDL::Size(0)},
        {DDL::Size(1), false},
        {DDL::Size(2), false, false},
        {DDL::Size(2), false, true},
        {DDL::Size(1), true},
        {DDL::Size(2), true, false},
        {DDL::Size(2), true, true},
    };

    ComparisonsFromOrderedArray(cases);
    for (auto& x : cases) { x.free(); }
}
