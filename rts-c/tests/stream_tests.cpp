#include <gtest/gtest.h>
#include <cstring>
#include <ddl/stream.h>

class StreamTestDelete {
  size_t *free_count;
public:
  StreamTestDelete() : free_count(nullptr) {}
  StreamTestDelete(size_t &c) : free_count(&c) {}

  // Just count how many deallocations happen
  // We don't actually deallocate anything as we use top
  // level string constants in the tests
  void operator() (const char* p) { ++(*free_count); }
};

DDL::Array<DDL::UInt<8>> str(char const * x) {
  return DDL::Array<DDL::UInt<8>>
              ( reinterpret_cast<DDL::UInt<8> const *>(x)
              , DDL::Size{strlen(x)}
              );
}

using Stream = DDL::Stream<StreamTestDelete>;

template <typename Fn>
void doTest
  ( size_t sample_no
  , const char* samples[]
  , Fn &&f
  ) {

  size_t free_count  = 0;
  size_t alloc_count = 0;

  DDL::ParserThread<StreamTestDelete> pt
     { "Test"
     , [&free_count, &alloc_count,f = std::forward<Fn>(f)]
       (Stream s) {
         f(alloc_count, free_count,s);
       }
     };
  pt.resume();

  StreamTestDelete del(free_count);

  while (!pt.isDone()) {
    if (alloc_count < sample_no) {
       const char* x = samples[alloc_count];
       size_t n = strlen(x);
       pt.append(n,x,del);
       ++alloc_count;
     } else {
       pt.finish();
     }
     pt.resume();
   }
}



TEST(Streams, SingleChunk) {
  const char* chunks[] = { "One" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    EXPECT_FALSE(s.isEmpty());
    EXPECT_EQ(s.iHead(), 'O');

    s = s.iDrop(1);
    EXPECT_EQ(free,0);
    EXPECT_FALSE(s.isEmpty());
    EXPECT_EQ(s.iHead(),'n');

    s = s.iDrop(2);
    EXPECT_EQ(free,1);
    EXPECT_TRUE(s.isEmpty());

    s.free();
  });
}

TEST(Streams, SingleChunkShared) {
  const char* chunks[] = { "One" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s.copy();
    Stream t = s;
    s = s.iDrop(3);
    EXPECT_EQ(free,0);
    EXPECT_TRUE(s.isEmpty());
    EXPECT_FALSE(t.isEmpty());
    EXPECT_EQ(t.iHead(),'O');
    s.free();
    EXPECT_EQ(free,0);
    t.free();
    EXPECT_EQ(free,1);

  });
}

TEST(Streams, TwoChunks1) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s = s.iDrop(6);
    EXPECT_EQ(free,2);
    EXPECT_TRUE(s.isEmpty());
    s.free();
  });
}


TEST(Streams, TwoChunks2) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s = s.iDrop(4);
    EXPECT_EQ(free,1);
    EXPECT_FALSE(s.isEmpty());
    EXPECT_EQ(s.iHead(),'w');
    s.free();
  });
}


TEST(Streams, TwoChunksDropAndDrop1) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s.copy();
    Stream t = s;
    while (!s.isEmpty()) {
      EXPECT_FALSE(t.isEmpty());
      EXPECT_EQ(s.iHead(),t.iHead());
      s = s.iDrop(1);
      t.iDropMut1();
      EXPECT_EQ(s.getOffset(),t.getOffset());
      EXPECT_EQ(free, s.getOffset().rep()/3);
    }

    s.free();
    t.free();
  });
}

TEST(Streams, TwoChunksFree1) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s.free();
    EXPECT_EQ(alloc,free);
  });
}


TEST(Streams, TwoChunksFree2) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s.copy();
    Stream t = s;
    s = s.iDrop(4);
    EXPECT_EQ(alloc,2);
    EXPECT_EQ(free,0);
    t.free();
    EXPECT_EQ(free,1);
    s.free();
    EXPECT_EQ(free,2);
  });
}




TEST(Streams, TwoChunksTake) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s = s.iTake(3);
    s = s.iDrop(3);
    EXPECT_EQ(alloc,1);
    EXPECT_EQ(free,1);
    s.free();
  });
}


TEST(Streams, TwoChunksTake2) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
    s = s.iTake(4);
    s = s.iDrop(3);   // never requests 2nd chunk
    EXPECT_EQ(alloc,1);
    EXPECT_EQ(free,1);
    EXPECT_FALSE(s.isEmpty());
    EXPECT_EQ(alloc,2);
    s.free();
    EXPECT_EQ(free,2);
  });
}


TEST(Streams, Prefix) {
  const char* chunks[] = { "One", "Two" };

  doTest(std::size(chunks), chunks, [](size_t &alloc, size_t &free, Stream s) {
      auto pref1 = str("On");
      auto pref2 = str("OneT");
      auto pref3 = str("OneTwoT");
      EXPECT_TRUE(s.hasPrefix(pref1));
      EXPECT_EQ(free,0);
      EXPECT_TRUE(s.hasPrefix(pref2));
      EXPECT_EQ(free,0);
      EXPECT_FALSE(s.hasPrefix(pref3));
      EXPECT_EQ(free,0);
      pref1.free();
      pref2.free();
      pref3.free();
  });
}






