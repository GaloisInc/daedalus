#pragma once

#include <cassert>
#include <memory>

#include <ddl/boxed.h>
#include <ddl/size.h>
#include <ddl/number.h>

namespace DDL {

// The data source for a stream
// Invariant: the stream does not contain empty chunks, except of the
// special terminator chunk, which indicates that the stream can't be
// extended further.
template <typename Del = std::default_delete<UInt<8>[]>>
class StreamData : HasRefs {

  class Chunk: Del, HasRefs {
    Size       size;
    RefCount   ref_count;
    UInt<8>    *buffer;
    Chunk *next;

    // Deallocate data associated with the current node
    // Does not mess with `next`.
    void freeThis() {
      if (buffer != nullptr && buffer != emptyStreamBuffer) {
        (*static_cast<Del*>(this))(buffer);
      }
      delete this;
    }

    inline static UInt<8> emptyStreamBuffer[1];

  public:

    // Make a "thunk"
    Chunk()
      : size(0), ref_count(1), buffer(nullptr), next(nullptr) {}

    // Is this a special last entry in a the data stream
    bool isTerminal () const { return next == nullptr; }

    // Is this a thunk (i.e., may be extended)
    bool isThunk ()    const { return buffer == nullptr; }

    // Assumes offset < size
    UInt<8> elementAt(Size offset) const {
      assert(offset < size);
      return buffer[offset.rep()];
    }

    // The size of *this* chunk of stream.
    Size getSize() const { return size; }

    // Add an extra reference.
    void copy() { ++ref_count; }

    // Remove a feference to this node.
    // Returns an *owned* reference to the next node.
    Chunk* nextChunk() {
      assert(!isTerminal());

      auto res = next;
      if (ref_count > 1) {
        --ref_count;
        ++(res->ref_count);
      } else
        freeThis();
      return res;
    }

    void free() {
      for (auto p = this; p != nullptr;) {
        if (p->ref_count > 1) { --ref_count; return; }
        auto next = p->next;
        p->freeThis();
        p = next;
      }
    }

    // Append a new chunk of data to the stream
    // If the buffer is nullptr, then mark the stream as finished.
    // `append` should not be called on finished streams.
    template <typename D>
    void append(Size csize, UInt<8> *cbuffer, D&& del) {

      Chunk *endOfStream;
      for ( endOfStream = this;
            !endOfStream->isTerminal();
            endOfStream = endOfStream->next);

      assert(endOfStream->isThunk());

      if (cbuffer == nullptr) {
        endOfStream->buffer = emptyStreamBuffer;
        return;
      }

      if (csize == 0) return;

      endOfStream->size   = csize;
      endOfStream->buffer = cbuffer;
      *static_cast<Del*>(endOfStream) = std::forward<D>(del);
      endOfStream->next   = new Chunk();
    }
  };

  // ------------------------------------------ //

  Chunk *front;
  StreamData(Chunk *f) : front(f) {}

public:

  StreamData() : front(new Chunk()) {}

  bool isTerminal ()             const { return front->isTerminal(); }
  bool isThunk ()                const { return front->isThunk(); }
  Size getChunkSize()            const { return front->getSize(); }
  UInt<8> elementAt(Size offset) const { return front->elementAt(offset); }

  template <typename D = Del>
  void append(Size csize, UInt<8> *cbuffer, D&& del = Del()) {
    front->append<D>(csize, cbuffer, std::forward<D>(del));
  }

  // Owns this
  StreamData nextChunk() { return StreamData(front->nextChunk()); }

  void copy() { front->copy(); }
  void free() { front->free(); }
};



template <typename Del = std::default_delete<UInt<8>[]>>
class Stream : HasRefs {

  StreamData<Del> front;          // Front of the stream (not null).
  Size            chunk_offset;
  // Offset of this chunk relative to
  // the start of the stream.

  Size            offset;
  // Offset relative to the current chunk
  // If the current chunk is *not* terminal, than offset should
  // always be withing the chunk (i.e., we have at least 1 entry)


public:

  // Owns data
  Stream (StreamData<Del> data) : front(data), chunk_offset(0), offset(0) {}

  void copy() { front.copy(); }
  void free() { front.free(); }

  Size getOffset() const { return chunk_offset.incrementedBy(offset); }

  // Assumes: !isEmpty() && !isThunk()
  UInt<8> head() const { return front.elementAt(offset); }

  // This stream has no data and is not going to get any more.
  bool isEmpty() const { return front.isTerminal() && !front->isThunk(); }

  // This stream has no data but might get more.
  bool isThunk() const { return front.isThunk(); }

  // Try to advance the current offset by this much.
  // May advance by less if we hit the end of the underlying data stream.
  // Returns how many bytes still need to be dropped.
  Size dropMut(Size t) {
    while (t > 0 && !front.isTerminal()) {
      auto size = front.getSize().decrementedBy(offset);
      if (t < size) {
        offset.incrementBy(t);
        return 0;
      }
      t.decrementBy(size);
      offset = 0;
      front = front.nextChunk();
      chunk_offset.incrementBy(size);
    }
    return t;
  }

  // Special case of `dromMut` that only drops one entry.
  // Returns `true` if successful.
  bool dropMut1() {
    if (front.isTerminal()) return false;
    offset.increment();
    if (offset >= front.getSize()) {
      front  = front.nextChunk();
      offset = 0;
    }
    return true;
  }
};


}

