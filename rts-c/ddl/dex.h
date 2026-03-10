#pragma once

#include <cstdint>
#include <ddl/number.h>

namespace CPP {

static inline
uint8_t as_uint8(DDL::UInt<8> x) { return x.rep(); }

static inline
uint16_t as_uint16(DDL::UInt<16> x) { return x.rep(); }

static inline
uint32_t as_uint32(DDL::UInt<32> x) { return x.rep(); }

static inline
uint64_t as_uint64(DDL::UInt<64> x) { return x.rep(); }

static inline
int8_t as_int8(DDL::SInt<8> x) { return x.rep(); }

static inline
int16_t as_int16(DDL::SInt<16> x) { return x.rep(); }

static inline
int32_t as_int32(DDL::SInt<32> x) { return x.rep(); }

static inline
int64_t as_int64(DDL::SInt<64> x) { return x.rep(); }

template<typename T>
void discard(T x) { if constexpr (DDL::hasRefs<T>()) x.free(); }

}