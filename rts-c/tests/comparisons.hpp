#pragma once

#include <cstddef>
#include <iterator>

template <typename T, std::size_t N>
void ComparisonsFromOrderedArray(const T (&cases)[N]) {
    for (auto& x : cases) {
        for (auto& y : cases) {
            EXPECT_EQ(x < y, &x < &y);
            EXPECT_EQ(x > y, &x > &y);
            EXPECT_EQ(x == y, &x == &y);
            EXPECT_EQ(x != y, &x != &y);
            EXPECT_EQ(x <= y, &x <= &y);
            EXPECT_EQ(x >= y, &x >= &y);
        }
    } 
}
