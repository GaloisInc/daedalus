#include <gtest/gtest.h>
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

using Str = DDL::Stream<StreamTestDelete>;

template <typename Fn>
void doTest
  ( size_t sample_no
  , const char* samples[]
  , Fn &&f
  ) {

  size_t count = 0;

  DDL::ParserThread<StreamTestDelete> pt;
  pt.start([&count,f](Str s) { f(count,s); });

  StreamTestDelete del(count);

  size_t chunk_no = 0;
  while (!pt.isDone()) {
    if (chunk_no < sample_no) {
       const char* x = samples[chunk_no];
       size_t n = strlen(x);
       pt.append(n,x,del);
       ++chunk_no;
     } else {
       pt.finish();
     }
     pt.resume();
   }
}



TEST(Streams, SingleChunk) {
  const char* chunks[] = { "One" };

  doTest(std::size(chunks), chunks, [](size_t &count, Str s) {
    EXPECT_EQ(s.isEmpty(), false);
  });
}




