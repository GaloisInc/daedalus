#include <gtest/gtest.h>
#include <ddl/size.h>

#include "comparisons.hpp"

#include <limits>
#include <cstddef>

TEST(Size, Comparisons) {
    DDL::Size cases[] = {
        0, 1, 2, 3, std::numeric_limits<std::size_t>::max()
    };
    ComparisonsFromOrderedArray(cases);
}

TEST(Size, Crement) {
    DDL::Size x {10};
    x.increment();
    EXPECT_EQ(x, 11);
    x.decrement();
    EXPECT_EQ(x, 10);
    x.incrementBy(25);
    EXPECT_EQ(x, 35);
    EXPECT_EQ(x.decremented(), 34);
    EXPECT_EQ(x.incremented(), 36);
    EXPECT_EQ(x.incrementedBy(7), 42);
    EXPECT_EQ(x, 35);
}
