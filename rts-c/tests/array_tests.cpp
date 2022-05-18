#include <gtest/gtest.h>
#include <ddl/array.h>

#include <string>

#include "comparisons.hpp"

TEST(Arrays, EmptyArray) {
    DDL::Array<DDL::UInt<32>> a {};
    EXPECT_EQ(a.size(), 0);
    a.free();
}

TEST(Arrays, NonEmptyArray) {
    DDL::Array<DDL::Bool> a {false, true};
    EXPECT_EQ(a.size(), 2);
    EXPECT_EQ(a[0], false);
    EXPECT_EQ(a[1], true);
    a.free();
}

TEST(Arrays, Comparisons) {
    DDL::Array<DDL::Bool> cases[] {
        {},
        {false},
        {false, false},
        {false, true},
        {true},
        {true, false},
        {true, true},
    };

    ComparisonsFromOrderedArray(cases);
    for (auto& x : cases) { x.free(); }
}

TEST(Arrays, EmptyBuilder) {
    DDL::Builder<DDL::UInt<8>> b;
    DDL::Array<DDL::UInt<8>> a(b);
    EXPECT_EQ(a.size(), 0);
    a.free();
}

TEST(Arrays, Builders) {
    DDL::Builder<DDL::UInt<8>> b;

    for (int i = 0; i < 10; i++) {
        b = {b, i};
    }

    DDL::Array<DDL::UInt<8>> a(b);

    for (int i = 0; i < 10; i++) {
        EXPECT_EQ(a[i], i);
    }

    a.free();
}

TEST(Arrays, SharedBuilders) {
    DDL::Builder<DDL::UInt<8>> b;

    for (int i = 0; i < 10; i++) { b = {b, i}; }

    b.copy();
    auto b1 = b;
    for (int i = 10; i < 20; i++) { b1 = {b1, i}; }

    DDL::Array<DDL::UInt<8>> a(b), a1(b1);

    for (int i = 0; i < 10; i++) {
        EXPECT_EQ(a[i], i);
        EXPECT_EQ(a1[i], i);
    }
    for (int i = 10; i < 20; i++) {
        EXPECT_EQ(a1[i], i);
    }

    a.free();
    a1.free();
}


TEST(Arrays, BorrowBytes) {
  char const *str ="abcd";
  auto len        = strlen(str);
  DDL::Array<DDL::UInt<8>> a{97,98,99,100};
  EXPECT_EQ(a.borrowBytes(),    std::string_view(str,len));
  EXPECT_EQ(a.borrowBytes(2),   std::string_view(str+2,len-2));
  EXPECT_EQ(a.borrowBytes(2,1), std::string_view(str+2,1));
  a.free();
}


