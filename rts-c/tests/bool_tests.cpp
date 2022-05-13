#include <gtest/gtest.h>

#include <ddl/bool.h>

#include "comparisons.hpp"

TEST(Bool, Comparisons) {
    ComparisonsFromOrderedArray((DDL::Bool[]) {false,true});
}
