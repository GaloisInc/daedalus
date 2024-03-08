#include "comparisons.hpp"
#include <ddl/integer.h>
#include <gtest/gtest.h>
#include <limits>

TEST(Integer, Comparisons) {
    DDL::Integer cases[] {
        -100,
        0,
        100,
        DDL::Integer{std::numeric_limits<uint64_t>::max()} * std::numeric_limits<uint64_t>::max()
    };
    ComparisonsFromOrderedArray(cases);
    for (auto&& x : cases) { x.free(); }
}
