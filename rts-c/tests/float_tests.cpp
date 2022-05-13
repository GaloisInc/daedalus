#include "comparisons.hpp"
#include <ddl/float.h>
#include <gtest/gtest.h>
#include <limits>

TEST(Float, FloatComparisons) {
    ComparisonsFromOrderedArray((DDL::Float[]) {
        -std::numeric_limits<float>::max(),
        -10,
        -std::numeric_limits<float>::min(),
        0,
        std::numeric_limits<float>::min(),
        10,
        std::numeric_limits<float>::max(),
    });
}

TEST(Float, DoubleComparisons) {
    ComparisonsFromOrderedArray((DDL::Double[]) {
        -std::numeric_limits<double>::max(),
        -10,
        -std::numeric_limits<double>::min(),
        0,
        std::numeric_limits<double>::min(),
        10,
        std::numeric_limits<double>::max(),
    });
}
