#pragma once

#include <cassert>
#include <memory>
#include <algorithm>
#include <boost/context/fiber.hpp>

#include <ddl/number.h>

#include <ddl/boxed.h>
#include <ddl/size.h>
#include <ddl/number.h>

namespace DDL {

namespace ctx=boost::context;

/// Deallocate buffers using delete[]
using DeleteNewAlloc = std::default_delete<const char[]>;


/// The data source for a stream
template <typename Del = DeleteNewAlloc>
class StreamData : HasRefs {

  class Chunk: Del, HasRefs {
    Size        size;         // Amount of data in this chunk
    RefCount    ref_count;    // Number of references to this chunk
    const char* buffer;       // Data for the chunk  (nullable)
    Chunk*      next;         // Next chunk, if any  (nullable)
    ctx::fiber* thunk;        // Where to get more data.
                              // Only meaningful on a terminal thunk

    // A marker for a terminal empty buffer.
    // If `buffer` points to this then this stream is finished and cannot
    // get any more data.
    inline static const char *emptyBuffer = "";

    /// Deallocate the current node.
    /// @return A pointer to the next node, if any.
    Chunk* freeThis() {
      Chunk *res = next;
      if (buffer != nullptr && buffer != emptyBuffer)
        (*static_cast<Del*>(this))(buffer);
      delete this;
      return res;
    }

  public:

    /// Make an empty, non-extensible buffer.
    Chunk()
      : size(0)
      , ref_count(1)
      , buffer(emptyBuffer)
      , next(nullptr)
      , thunk(nullptr)
      {}


    /// Make an extensible buffer with no data.
    /// @param getData The context that will provide more data.
    Chunk(ctx::fiber &getData)
      : size(0)
      , ref_count(1)
      , buffer(nullptr)
      , next(nullptr)
      , thunk(&getData)
      {}

    /// @return `true` if there are no more chunks after this.
    bool isTerminal () const { return next == nullptr; }

    /// @return `true` if we are a finished chunk with no more data.
    bool isEmpty()     const { return buffer == emptyBuffer; }

    /// @return The size of *this* chunk of stream.
    Size getChunkSize() const { return size; }

    /// @param offset  Offset of the element we want.
    /// @return        Element at the given offset of the *current* chunk.
    /// Assumes: offset < size
    UInt<8> elementAt(Size offset) const {
      assert(offset < size);
      return DDL::UInt<8>(buffer[offset.rep()]);
    }

    /// Add an extra reference.
    void copy() { ++ref_count; }

    /// Remove a reference.
    /// Owns this.
    void free() {
      for (auto p = this; p != nullptr;) {
        if (p->ref_count > 1) { --(p->ref_count); return; }
        p = p->freeThis();
      }
    }

    /// Assume: !isTerminal()
    /// Owns this.
    /// @return An ownded reference to the next chunk.
    /// Assert: return != nullptr
    Chunk* nextChunk() {
      assert(!isTerminal());

      if (ref_count > 1) {
        --ref_count;
        ++(next->ref_count);
        return next;
      } else
        return freeThis();
    }

    /// May suspend execution.
    /// Assumes: isTerminal()
    /// Owns this.
    /// @return returns `true` if we got new data.
    bool tryGetData() {

      if (!isTerminal()) return true;

      while (isTerminal()) {
        if (isEmpty()) return false;
        *thunk = std::move(*thunk).resume();
      }

      return true;
    }

    /// Debug view of the chunks.
    void dump() const {
      for (auto p = this; p != nullptr; p = p->next) {
        std::cout
          << "[" << p->ref_count
          << "|" << (void*) p
          << "|";
        if (p->buffer == nullptr)     std::cout << "thunk"; else
        if (p->buffer == emptyBuffer) std::cout << "empty"; else
          std::cout << (void*)p->buffer;
        std::cout << "]\n";
      }
    }

    /// Append a new chunk of data to the stream.
    /// If the size is 0 terminates the stream.
    /// Owns this.
    /// Assume: isTerminal() && !isEmpty()
    /// @return An owned reference to the new end of the stream.
    /// Assert: return != nullptr
    template <typename D>
    Chunk* append(Size csize, const char *cbuffer, D&& del) {
      assert (next == nullptr);
      assert (thunk != nullptr);
      assert (!*thunk);

      if (csize == 0) {
        buffer = emptyBuffer;
        return this;
      }

      size    = csize;
      buffer  = cbuffer;
      *static_cast<Del*>(this) = std::forward<D>(del);
      next    = new Chunk(*thunk);

      return nextChunk();
    }
  };


  // ------------------------------------------ //

  Chunk* front;       /// The front of the data.
                      /// Invariant: front != nullptr

public:

  /// Make a new non-extensible stream empty stream.
  StreamData() : front(new Chunk())  {}

  /// Make a new extensible empty stream.
  StreamData(ctx::fiber &getData) : front(new Chunk(getData))  {}

  /// @return `true` if there is currently no data in the stream.
  bool isTerminal ()  const { return front->isTerminal(); }

  /// @return The size of the first chunk in the data.
  Size getChunkSize() const { return front->getChunkSize(); }

  /// Assume: offset < getChunkSize()
  /// @param  offset  The offset of the element we are interested in.
  /// @return         The element at the given offset in the *current* chunk.
  UInt<8> elementAt(Size offset) const {
    assert(offset < getChunkSize());
    return front->elementAt(offset);
  }

  /// Owns this.
  /// Append new buffer.
  /// The data stream is updated to point to the new end of stream.
  /// If the size is 0, then marks the stream as terminated.
  /// @param csize    Size of the new data.  If 0, then terminate the stream.
  /// @param cbuffer  Data associate with the buffer.
  /// @param del      How to deallocate the data when we are done with it.
  template <typename D = Del>
  void appendMut(Size csize, const char *cbuffer, D&& del = Del()) {
    front = front->append<D>(csize, cbuffer, std::forward<D>(del));
  }

  /// Try to fill a terminal node with data.
  /// Owns this.
  /// May suspend.
  /// @return `true` if more data was available, or `false` if no more data.
  bool tryGetData() {
    assert(isTerminal());
    front->tryGetData();
    return !front->isEmpty();
  }

  /// Advance to the next chunk.
  /// Owns this.
  /// Assumes: !isTerminal()
  void nextChunkMut() { front = front->nextChunk(); }

  /// Add an extrea reference.
  void copy() { front->copy(); }

  /// Remove a reference.
  /// Owns this.
  void free() { front->free(); }

  /// Debug dump of the available data.
  void dump() const { front->dump(); }
};


// -----------------------------------------------------------------------------


template <typename Del = DeleteNewAlloc>
class Stream : HasRefs {

  StreamData<Del> data;
  /// The data backing this stream.

  Size chunk_offset;
  /// Offset of the *beginning of the data, relative to the start of the stream.

  Size offset;
  /// Offset relative to the current chunk.
  /// Invariant: chunk_size == 0 || offset < chunk_size

  Size chunk_size;
  /// How much of the chunk is available to us.
  /// This may be less then the size of the chunk, if we have restricted
  /// the stream.

  Size last_offset;
  /// The offset at the end of the stream.
  /// We are only allowed to read at smaller offsets.


  /// Do this when this stream is known to be empty to
  /// let go of the underlying stream
  void makeEmpty() {
    data.free();
    data         = StreamData<Del>();
    chunk_offset = getOffset();
    offset       = 0;
    chunk_size   = 0;
  }


  /// Adjust metadata after going to the next chunk.
  /// @param: global The global offset.
  /// Assumes: !data.isTerminal()
  void advance(Size global) {
    chunk_offset = global;
    offset       = 0;
    chunk_size   = data.getChunkSize();
    Size have    = last_offset.decrementedBy(global);
    if (chunk_size > have) chunk_size = have;
  }


public:

  /// Make a new stream using the given data source, starting at offset 0.
  /// @param data  The data to back the stream.  Owned.
  Stream (StreamData<Del> data)
    : data(data)
    , chunk_offset(0)
    , offset(0)
    , chunk_size(data.getChunkSize())
    , last_offset(Size::maxValue())
  {}

  /// Add an extre reference.
  void copy()       { data.copy(); }

  /// Remove a reference.
  /// Owns this.
  void free()       { data.free(); }

  /// Debug dump of the data in stream.
  /// XXX: Currently this dumps *all* data, ignoring `last_offset`.
  void dump() const { data.dump(); }

  /// @return the current offset, relative to the beginning of the stream.
  Size getOffset() const {
    return chunk_offset.incrementedBy(offset);
  }

  /// Check to see if the stream is empty.
  /// May suspend.
  /// @return `true` if the stream contains no data,
  ///                and won't be getting any more.
  bool isEmpty() {
    if (offset < chunk_size) return false;  // common case

    Size global = getOffset();
    if (global >= last_offset) return true;

    if (!data.isTerminal()) data.nextChunkMut();

    if (!data.tryGetData()) return true;

    advance(global);
    return false;
  }

  /// Assume: !isEmpty()
  /// @return The front element of the stream.
  UInt<8> iHead() const {
    return data.elementAt(offset);
  }

  /// Advance the current offset by this much.
  /// Mutable own this.
  /// @param nHow many bytes to advance by.
  /// @return How many bytes were NOT dropped. If all requested
  ///         bytes were dropped reutrns 0, but if the data run out before then
  ///         it will return how many bytes still needed to be dropped.
  Size iDropMut(Size n) {

    while (n > 0 && !isEmpty()) {

      Size have = chunk_size.decrementedBy(offset);
      if (n < have) {
        offset.incrementBy(n);
        return 0;
      } else {
        if (n == have) {
          Size global = getOffset();
          if (global == last_offset)
            makeEmpty();
          else {
            data.nextChunkMut();
            advance(global);
          }
          return 0;
        } else {
          offset.incrementBy(have);
          n.decrementedBy(have);
        }
      }
    }
    assert(false);
  }

  /// Restrict the stream to at most the given number of bytes.
  /// Note that this does *not* ensure that the given number of bytes
  /// is present in the stream, only that we won't consume more than this much.
  /// Mutable owns this.
  /// @param n Limit the stream to this many bytes.
  void iTakeMut(Size n) {
    Size global = getOffset();
    Size have   = last_offset.decrementedBy(global);
    if (n >= have) return;

    last_offset = global.incrementedBy(n);
    if (offset == last_offset) {
        makeEmpty();
        return;
    }
    have = last_offset.decrementedBy(chunk_offset);
    if (chunk_size > have) chunk_size = have;
  }

  /// Create a new stream that is the same as the current one,
  /// except it won't consume more than the given number of bytes.
  /// Owns this.
  /// @param n  How many bytes is the limit.
  /// @return   A new stream with the requested restriction.
  Stream iTake(Size n) const {
    Stream res(*this);
    res.iTakeMut(n);
    return res;
  }

  /// Create a new stream by skipping some bytes from the current stream.
  /// If the underlying data backing the stream has less data than we drop,
  /// then we only drop to the end of the data.
  /// Owns this.
  /// May suspend.
  /// @param n    How many bytes to skip.
  /// @return     A new stream advanced by the requested amount, or less
  ///             if the data runs out.
  Stream iDrop(Size n) const {
    Stream res(*this);
    res.iDropMut(n);
    return res;
  }

};


}
