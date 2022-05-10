#include <gtest/gtest.h>

#include <ddl/bool.h>
#include <ddl/maybe.h>

#include "comparisons.hpp"

TEST(Maybe, Comparisons) {
    ComparisonsFromOrderedArray((DDL::Maybe<DDL::Bool>[]) {{},{false},{true}});
}

TEST(Maybe, Selectors) {
    EXPECT_EQ(DDL::Maybe<DDL::Bool>().isNothing(), true);
    EXPECT_EQ(DDL::Maybe<DDL::Bool>().isJust(), false);
    EXPECT_EQ(DDL::Maybe<DDL::Bool>(true).isNothing(), false);
    EXPECT_EQ(DDL::Maybe<DDL::Bool>(false).isJust(), true);
}
