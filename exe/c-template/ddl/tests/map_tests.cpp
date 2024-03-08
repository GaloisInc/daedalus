#include <gtest/gtest.h>

#include <ddl/map.h>
#include <ddl/bool.h>
#include <ddl/number.h>

#include "comparisons.hpp"

#include <algorithm>

TEST(Map, Comparisons) {
    DDL::Map<DDL::Bool, DDL::Bool> e {};
    DDL::Map<DDL::Bool, DDL::Bool> cases[] {
        e,
        e.insert(false,false),
        e.insert(false,false).insert(true,false),
        e.insert(false,false).insert(true,true),
        e.insert(false,true),
        e.insert(false,true).insert(true,false),
        e.insert(false,true).insert(true,true),
        e.insert(true,false),
        e.insert(true,true),
    };
    
    ComparisonsFromOrderedArray(cases);

    for (auto&& x : cases) { x.free(); }
}

TEST(Map, Replace) {
    DDL::Map<DDL::Bool, DDL::Bool> e {};
    auto x = e.insert(true,true);
    auto y = e.insert(true,false).insert(true,true);
    EXPECT_EQ(x,y);
    x.free();
    y.free();
}

TEST(Map, SimpleSharing) {
    DDL::Map<DDL::Bool, DDL::Bool> m1 {};
    m1 = m1.insert(true,true);
    
    m1.copy();
    auto m2 = m1;
    m2 = m2.insert(false,false);

    EXPECT_EQ(m1.lookup(true), DDL::Maybe<DDL::Bool>(true));
    EXPECT_FALSE(m1.contains(false));

    EXPECT_EQ(m2.lookup(true), DDL::Maybe<DDL::Bool>(true));
    EXPECT_EQ(m2.lookup(false), DDL::Maybe<DDL::Bool>(false));

    m1.free();
    m2.free();
}

TEST(Map, SimpleOverwrite) {
    DDL::Map<DDL::Bool, DDL::Bool> m1 {};
    m1 = m1.insert(true,true);
    
    m1.copy();
    auto m2 = m1;
    m2 = m2.insert(true,false);

    EXPECT_EQ(m1.lookup(true), DDL::Maybe<DDL::Bool>(true));
    EXPECT_EQ(m2.lookup(true), DDL::Maybe<DDL::Bool>(false));

    m1.free();
    m2.free();
}

TEST(Map, Insertion) {
    int elements[] = {1,2,3,4,5,6};

    do {
        DDL::Map<DDL::UInt<8>, DDL::UInt<16>> m {};

        for (int i : elements) {
            m = m.insert(i, 10*i);
            EXPECT_TRUE(m.valid());
        }

        // Ensure that updating a copy doesn't corrupt the original
        m.copy();
        auto m1 = m;
        for (int i : elements) {
            m1 = m1.insert(i, 100*i);
            EXPECT_TRUE(m1.valid());
        }

        for (int i : elements) {
            EXPECT_TRUE(m.contains(i));
            EXPECT_EQ(m.lookup(i), DDL::Maybe<DDL::UInt<16>>(10*i));

            EXPECT_TRUE(m1.contains(i));
            EXPECT_EQ(m1.lookup(i), DDL::Maybe<DDL::UInt<16>>(100*i));
        }

        EXPECT_FALSE(m.contains(0));
        EXPECT_FALSE(m.lookup(0).isJust());

        EXPECT_FALSE(m1.contains(0));
        EXPECT_FALSE(m1.lookup(0).isJust());

        m.free();
        m1.free();
    } while (std::next_permutation(std::begin(elements), std::end(elements)));
}
