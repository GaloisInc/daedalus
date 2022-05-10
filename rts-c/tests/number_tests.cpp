#include "comparisons.hpp"
#include <ddl/integer.h>
#include <gtest/gtest.h>
#include <limits>

TEST(UInt, Comparisons8) {
    DDL::UInt<8> cases[] {
        0,
        100,
    };
    ComparisonsFromOrderedArray(cases);
}


TEST(UInt, Comparisons10) {
    DDL::UInt<10> cases[] {
        0,
        100,
        1023,
    };
    ComparisonsFromOrderedArray(cases);
}


TEST(SInt, Comparisons8) {
    DDL::SInt<8> cases[] {
        -100,
        0,
        100,
    };
    ComparisonsFromOrderedArray(cases);
}


TEST(SInt, Comparisons10) {
    DDL::SInt<10> cases[] {
        -512,
        0,
        100,
        511,
    };
    ComparisonsFromOrderedArray(cases);
}

TEST(UInt, MaxReps) {
    EXPECT_EQ(DDL::UInt<0>::maxValRep(), UINT64_C(0x0));
    EXPECT_EQ(DDL::UInt<1>::maxValRep(), UINT64_C(0x1));
    EXPECT_EQ(DDL::UInt<8>::maxValRep(), UINT64_C(0xff));
    EXPECT_EQ(DDL::UInt<16>::maxValRep(), UINT64_C(0xffff));
    EXPECT_EQ(DDL::UInt<31>::maxValRep(), UINT64_C(0x7fffffff));
    EXPECT_EQ(DDL::UInt<32>::maxValRep(), UINT64_C(0xffffffff));
    EXPECT_EQ(DDL::UInt<63>::maxValRep(), UINT64_C(0x7fffffffffffffff));
    EXPECT_EQ(DDL::UInt<64>::maxValRep(), UINT64_C(0xffffffffffffffff));
}

TEST(SInt, MaxReps) {
    EXPECT_EQ(DDL::SInt<1>::maxValRep(), INT64_C(0x0));
    EXPECT_EQ(DDL::SInt<8>::maxValRep(), INT8_MAX);
    EXPECT_EQ(DDL::SInt<16>::maxValRep(), INT16_MAX);
    EXPECT_EQ(DDL::SInt<31>::maxValRep(), INT64_C(0x3fff'ffff));
    EXPECT_EQ(DDL::SInt<32>::maxValRep(), INT32_MAX);
    EXPECT_EQ(DDL::SInt<63>::maxValRep(), INT64_C(0x3fff'ffff'ffff'ffff));
    EXPECT_EQ(DDL::SInt<64>::maxValRep(), INT64_MAX);
}

TEST(SInt, MinReps) {
    EXPECT_EQ(DDL::SInt<1>::minValRep(), -INT64_C(0x1));
    EXPECT_EQ(DDL::SInt<8>::minValRep(), INT8_MIN);
    EXPECT_EQ(DDL::SInt<16>::minValRep(), INT16_MIN);
    EXPECT_EQ(DDL::SInt<31>::minValRep(), -INT64_C(0x4000'0000));
    EXPECT_EQ(DDL::SInt<32>::minValRep(), INT32_MIN);
    EXPECT_EQ(DDL::SInt<63>::minValRep(), -INT64_C(0x4000'0000'0000'0000));
    EXPECT_EQ(DDL::SInt<64>::minValRep(), INT64_MIN);
}

TEST(UInt, Concat) {
    DDL::UInt<12> x { DDL::UInt<4>(0xf), DDL::UInt<8>(0xf) };
    EXPECT_EQ(x, 0xf0f);

    DDL::UInt<12> y { 0xf0 };
    EXPECT_EQ(DDL::lcat(y, DDL::UInt<4>{0xf}), DDL::UInt<12>{0xf0f});

    DDL::UInt<12> z { 0xff0 };
    EXPECT_EQ(DDL::lcat(z, DDL::UInt<4>{0xf}), DDL::UInt<12>{0xf0f});

    DDL::UInt<4> w { 0xf };
    EXPECT_EQ(DDL::lcat(w, DDL::UInt<12>{0xabc}), DDL::UInt<4>{0xc});
}

