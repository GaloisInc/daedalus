#pragma once

#include <cassert>
#include <memory>
#include <algorithm>

#include <ddl/boxed.h>
#include <ddl/size.h>
#include <ddl/number.h>

namespace DDL {

using DefaultDeleter = std::default_delete<const char[]>;

template <typename Del>
class Stream;

// The data source for a stream
template <typename Del = DefaultDeleter>
class StreamData : HasRefs {

  class Chunk: Del, HasRefs {
    Size       size;
    RefCount   ref_count;
    const char *buffer;
    Chunk      *next;

    // Deallocate data associated with the current node
    // Does not mess with `next`.
    void freeThis() {
      if (buffer != nullptr && buffer != emptyStreamBuffer) {
        (*static_cast<Del*>(this))(buffer);
      }
      delete this;
    }

    // Terminator marker
    inline static char emptyStreamBuffer[1];


  public:


    // Make an empty or a thunk chunk
    Chunk(bool empty)
      : size(0)
      , ref_count(1)
      , buffer(empty ? emptyStreamBuffer : nullptr)
      , next(nullptr) {}

    // (borrow this) Is this a special last entry in a the data stream
    bool isTerminal () const { return next == nullptr; }

    // (borrow this) Is this a thunk (i.e., may be extended)
    bool isThunk ()    const { return buffer == nullptr; }

    // (borrow this) The size of *this* chunk of stream.
    Size getSize() const { return size; }

    // (borrow this)
    // Assumes offset < size
    UInt<8> elementAt(Size offset) const {
      assert(offset < size);
      return DDL::UInt<8>(buffer[offset.rep()]);
    }


    // Add an extra reference.
    void copy() { ++ref_count; }

    void free() {
      for (auto p = this; p != nullptr;) {
        if (p->ref_count > 1) { --(p->ref_count); return; }
        auto next = p->next;
        p->freeThis();
        p = next;
      }
    }

    // (owns this)
    // Assumes: not on a terminal node
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

    void dump() {
      for (auto p = this; p != nullptr; p = p->next) {
        std::cout
          << "[" << p->ref_count
          << "|" << (void*) p
          << "|";
        if (p->buffer == nullptr) std::cout << "thunk"; else
        if (p->buffer == emptyStreamBuffer) std::cout << "empty"; else
          std::cout << (void*)p->buffer;
        std::cout << "]\n";
      }
    }


    // (owns this)
    // Append a new chunk of data to the stream
    // Assumes: we are at the last chunk, which is a thunk.
    // If the buffer is empty, then mark the stream as finished.
    // Returns an owned pointer to the new end of the stream.
    template <typename D>
    Chunk* append(Size csize, const char *cbuffer, D&& del) {

      assert(isThunk());

      if (csize == 0) {
        buffer = emptyStreamBuffer;
        return this;
      }

      size   = csize;
      buffer = cbuffer;
      *static_cast<Del*>(this) = std::forward<D>(del);
      next   = new Chunk(false);

      return nextChunk();
    }
  };

  // ------------------------------------------ //

  Chunk *front;
  StreamData(Chunk *f) : front(f) {}

public:

  StreamData(bool empty = false) : front(new Chunk(empty)) {}

  bool isTerminal ()             const { return front->isTerminal(); }
  bool isThunk ()                const { return front->isThunk(); }
  Size getChunkSize()            const { return front->getSize(); }
  UInt<8> elementAt(Size offset) const { return front->elementAt(offset); }

  // Owns this
  // Append new buffer or terminal stream, if csize = 0
  // The data stream is updated to point to the new end of stream.
  template <typename D = Del>
  void appendMut(Size csize, const char *cbuffer, D&& del = Del()) {
    front = front->append<D>(csize, cbuffer, std::forward<D>(del));
  }

  // Owns this
  // Assume: !isTerminal()
  // Advance to the next chunk
  void nextChunkMut() { front = front->nextChunk(); }

  void copy() { front->copy(); }
  void free() { front->free(); }
  void dump() { front->dump(); }
};







template <typename Del = DefaultDeleter>
class Stream : HasRefs {

  StreamData<Del> front;  // Front of the stream (not null).

  Size chunk_offset;
  // Offset of the beginning of this chunk relative to the start of the stream.

  Size offset;
  // Offset relative to the current chunk

  Size chunk_size;
  // How much of the chunk is available to us.
  // This is used to restrict the last chunk of a take.

  Size max_size;

  void makeEmpty() {
    chunk_offset = getOffset();
    front.free();
    front = StreamData(true);
    offset = 0;
    chunk_size = 0;
    max_size = 0;
  }


  void skipDropped() {

    if (chunk_size == 0 && !front.isTerminal()) {
      chunk_size = front.getChunkSize();
      if (chunk_size > max_size) chunk_size = max_size;
    }

    while (!front.isTerminal() && offset >= chunk_size) {
      offset.decrementBy(chunk_size);
      chunk_offset.incrementBy(chunk_size);
      front.nextChunkMut();
      chunk_size = front.getChunkSize();
    }

    if (front.isTerminal()) {
      if (!front.isThunk()) max_size = 0;
      return;
    }

    if (chunk_size > max_size) chunk_size = max_size;
  }

public:

  // (owns data)
  // Make a new stream using the given data source, starting at offset 0.
  Stream (StreamData<Del> data)
    : front(data)
    , chunk_offset(0)
    , offset(0)
    , chunk_size(data.getChunkSize())
    , max_size(Size::maxValue())
  {}

  void copy() { front.copy(); }
  void free() { front.free(); }
  void dump() { front.dump(); }

  // Return the current offset, relative to the beginning of the stream.
  // (borrow this)
  Size getOffset() const { return chunk_offset.incrementedBy(offset); }

  // Is this a known empty stream.
  // Note that !isEmpty() *DOES NOT* imply *hasDasta()* as we may be at a thunk
  bool isEmpty() const { return max_size == 0; }

  // Does the stream contain any data that can be consumed
  bool hasData() {
    if (offset < chunk_size) return true;  // common case
    if (isEmpty()) return false;

    skipDropped();
    return offset < chunk_size;
  }

  // Return the front element of the stream
  // assume: hasData()
  UInt<8> head() { return front.elementAt(offset); }

  // Advance the current offset by this much.
  void dropMut(Size n) {
    if (n >= max_size) { makeEmpty(); return; }
    offset.incrementBy(n);
    max_size.decrementBy(n);
    skipDropped();
  }

  void takeMut(Size n) {
    if (n >= max_size) return;
    max_size = n;
    if (max_size == 0) { makeEmpty(); return; }
  }


};


}

