{-|
  Name: elf
  Description: Embedding and Linking Format; ELF executable files
  Maintainer: David A. Holland <dholland@galois.com>
  Stability: still under development

  The historical ELF specification may be found in various places on
  the internet, e.g. https://refspecs.linuxfoundation.org/elf/elf.pdf.
  This specification is from 1995 and lacks most modern features
  (symbol versioning, thread-local storage, etc.) as well as any
  discussion whatsoever of 64-bit types.

  This specification got pasted into the System V "gABI" (generic
  ABI) document. Several versions of the relevant chapters are
  available from https://www.sco.com/developers/gabi/, the most
  recent from 2013.

  There is also a full copy in the Solaris Linkers and Libraries
  Guide, which Sun maintained until being acquired by Oracle. Several
  versions of this manual can be found for ostensibly free download on
  oracle.com, the most recent from at least 2018, but all contain a
  vague and ambiguous license notice and it is not entirely clear
  whether they can be used or not. Ergo, everything in this file is
  sourced elsewhere.

  Sources potentially cited below are:

  [HIST] the historical ELF specification cited above
  [gABI4.1] the System V generic ABI version 4.1 from 1997
  [gABI12] the 31 December 2012 draft version of the gABI
  [gABI13] the 10 June 2013 draft version of the gABI

  plus in a few places:

  [NetBSD] NetBSD's sys/exec_elf.h
  [elfutils] The elf.h and elf-knowledge.h from the elfutils package

  The format specification here closely follows the layout of the
  various copies of the ELF specification (which all derive from the
  same ancestral text and are in the same order), even where that
  makes the parsing somewhat awkward, in order to maintain a close
  correspondence between the human-readable and machine-readable
  versions.

-}


-- Current shortcomings:
--
-- 00. Doesn't work yet. Immediate problem is that apparently-valid
-- segment types are being rejected.
--
-- 0. There is way too much duplication between the 32-bit and 64-bit
-- logic. Unfortunately, we want the Daedalus spec to match the
-- upstream ELF spec and that's how it's done. (Everything is repeated
-- twice whether or not it's different; I've condensed some that
-- aren't, but enough things _are_ different that most of the types
-- aren't actually the same.) We don't have typeclasses to allow
-- writing code that's polymorphic over functionally different types;
-- probably the right approach is to parse the on-disk material with
-- the types that match the spec, then lift into 32 vs. 64 sums so at
-- least the higher-level logic can be shared. However, sorting out
-- the best way to do this needs to wait until the a first version of
-- the whole thing is finished so it can be scoped out adequately, and
-- so this hasn't been done yet.
--
-- 1. The workarounds for more than 65536 sections are in place for
-- the ELF header (file header) but not the symbol table. If you have
-- too many sections there's an extra section that appears, of type
-- sht_symtab_shndx, that is a parallel array matching the symbol
-- table that contains one word per entry that's the real section
-- index. However, because the reserved section indexes (shn_ABS,
-- shn_COMMON, plus maybe machine-dependent stuff) have numbers below
-- 65536, it isn't actually sufficient to just fetch the wider number
-- from the alternate location; apparently files with huge numbers of
-- sections are allowed to reuse those numbers and so the reserved
-- section numbers need to be identified first and disambiguated in a
-- larger namespace. We can do that easily enough with another
-- sum/union type, but the infrastructure for that hasn't been set up.
-- (Note that while the section numbers in the file header can't be
-- any of the reserved ones, since one's just the count and not
-- actually a section index and the other one is a string table, the
-- numbers will still need to be injected properly into the larger
-- namespace.)
--
-- 2. Some bits haven't been handled yet, mostly the more obscure
-- dynamic linking stuff (e.g. .DYNAMIC) but also section groups and
-- notes. Notes are fairly important these days... they're also
-- simple, just didn't get to it.
--
-- 3. There are some high-level invariants on sections that we don't
-- have a good way to check yet.
--
-- 4. Checking that a few things are zero in reserved places is
-- disabled because it triggers github issue #357.
--
-- 5. There's a table of things to check on special sections which
-- doesn't work because it's a predefined table/map and there doesn't
-- seem to be a good way to do that.
--
-- 6. There are some checks that aren't in place yet. These (as well
-- as the other shortcomings noted) are tagged with XXX below.
--
-- 7. Some sections are allowed to be compressed with zlib. We can do
-- that! But it hasn't been implemented.


-- Note on file positions and seeking:
--
-- Daedalus does not (AFAIK) have an absolute seek operator on
-- streams, only take/drop operations. Positions in ELF files are,
-- however, absolute, so we need to keep a reference to the original
-- input stream still at position 0 so we can copy it and seek. I'm
-- calling this object "basestream" where it occurs.
--
-- (Note that in general some of the seeks will be backward; e.g. the
-- section headers are allowed to come after the sections they
-- describe, in which case one first reads the ELF header at the
-- beginning of the file, seeks to the end to get the section headers,
-- then seeks backward into the file to read the section data...)

import Daedalus

------------------------------------------------------------
-- Support code
------------------------------------------------------------

-- check that p is true on an entire array, passing the index
-- to it as well as the value.
def Foralli P arr =
   block
      let result =
         for (x = just (0: uint 64); a in arr)
            case x of
               nothing -> nothing
               just i -> if P i a then (just (i + 1)) else nothing
      case result of
         nothing -> false
         just _ -> true

------------------------------------------------------------
-- Integer types
------------------------------------------------------------

-- 32 bit types [HIST]
def Elf32Addr = UInt32
def Elf32Off = UInt32
def Elf32Half = UInt16
def Elf32Word = UInt32
def Elf32Sword = SInt32

-- 64 bit types [gABI13]
def Elf64Addr = UInt64
def Elf64Off = UInt64
def Elf64Half = UInt16
def Elf64Word = UInt32
def Elf64Sword = SInt32
def Elf64Xword = UInt64
def Elf64Xsword = SInt64

------------------------------------------------------------
-- ELF header (file header) found at offset 0
------------------------------------------------------------

-- 32-bit header [HIST]
def Elf32Header basestream =
   block
      SetStream basestream
      ident = Elf32Ident
      let ?bigEndian =
         case ident.data of
            elfdata_2lsb -> false
            elfdata_2msb -> true
      type = Elf32Half as? ElfType
      machine = Elf32Half as? ElfMachine
      version = Elf32Word as? uint 8 as? ElfVersion
      entry = Elf32Addr
      phoff = Elf32Off
      shoff = Elf32Off
      flags = Elf32Word
      ehsize = Elf32Half
      phentsize = Elf32Half
      phnum = Elf32Half
      shentsize = Elf32Half
      -- The values in these fields might need to be bigger than 16 bits,
      -- in which case they need to be read from elsewhere. This is done
      -- below.
      let base_shnum = Elf32Half as uint 32
      let base_shstrndx = Elf32Half as uint 32
      type == {| elftype_none |} is false
      machine == {| elfmach_none |} is false
      version == {| elfversion_none |} is false
      let tmp = if base_shnum >= 0xff00 || base_shstrndx >= 0xff00
         then
            -- the number of sections and/or the section number overflowed
            -- and the real value has to be gotten out of the first section
            -- header.
            block
               SetStream (Drop (shoff as uint 64) basestream)
               let hdr = Elf32Shdr
               shnum = if base_shnum >= 0xff00 then hdr.size else base_shnum
               shstrndx = if base_shstrndx >= 0xff00 then hdr.link else base_shstrndx
         else
            block
               shnum = base_shnum
               shstrndx = base_shstrndx
      shnum = tmp.shnum
      shstrndx = tmp.shstrndx

-- 64-bit header [gABI13]
def Elf64Header basestream =
   block
      SetStream basestream
      ident = Elf64Ident
      let ?bigEndian =
         case ident.data of
            elfdata_2lsb -> false
            elfdata_2msb -> true
      type = Elf64Half as? ElfType
      machine = Elf64Half as? ElfMachine
      version = Elf64Word as? uint 8 as? ElfVersion
      entry = Elf64Addr
      phoff = Elf64Off
      shoff = Elf64Off
      flags = Elf64Word
      ehsize = Elf64Half
      phentsize = Elf64Half
      phnum = Elf64Half
      shentsize = Elf64Half
      -- The values in these fields might need to be bigger than 16 bits,
      -- in which case they need to be read from elsewhere. This is done
      -- below.
      let base_shnum = Elf64Half as uint 32
      let base_shstrndx = Elf64Half as uint 32
      type == {| elftype_none |} is false
      machine == {| elfmach_none |} is false
      version == {| elfversion_none |} is false
      let tmp = if base_shnum >= 0xff00 || base_shstrndx >= 0xff00
         then
            -- the number of sections and/or the section number overflowed
            -- and the real value has to be gotten out of the first section
            -- header.
            block
               SetStream (Drop shoff basestream)
               let hdr = Elf64Shdr
               shnum = if base_shnum >= 0xff00 then hdr.size as? uint 32 else base_shnum
               shstrndx = if base_shstrndx >= 0xff00 then hdr.link else base_shstrndx
         else
            block
               shnum = base_shnum
               shstrndx = base_shstrndx
      shnum = tmp.shnum
      shstrndx = tmp.shstrndx

-- types for the type field [HIST]
-- the os-specific range is 0xfe00 through 0xfeff [gABI13]
-- the processor-specific range is 0xff00 through 0xffff
bitdata ElfType where
   elftype_none =    { 0: uint 16 }
   elftype_rel =     { 1: uint 16 }
   elftype_exec =    { 2: uint 16 }
   elftype_dyn =     { 3: uint 16 }
   elftype_core =    { 4: uint 16 }

-- machines for the machine field, [gABI13] except where noted
bitdata ElfMachine where
   elfmach_none =               { 0: uint 16 } -- no machine [HIST]
   elfmach_M32 =                { 1 }   -- AT&T WE 32100 [HIST]
   elfmach_SPARC =              { 2 }   -- SPARC [HIST]
   elfmach_386 =                { 3 }   -- 32-bit Intel 80x86 [HIST]
   elfmach_68k =                { 4 }   -- Motorola 68000 [HIST]
   elfmach_88k =                { 5 }   -- Motorola 88000 [HIST]
   elfmach_iamcu =              { 6 }   -- Intel MCU [gABI13]
   elfmach_860 =                { 7 }   -- Intel i860 [HIST]
   elfmach_mips =               { 8 }   -- MIPS (big-endian) [HIST]
   elfmach_s370 =               { 9 }   -- IBM System/370 [gABI13]
   elfmach_mips_rs3_le =        { 10 }  -- MIPS little-endian
   elfmach_parisc =             { 15 }  -- HP PA-RISC
   elfmach_vpp500 =             { 17 }  -- Fujitsu VPP500
   elfmach_sparc32plus =        { 18 }  -- enhanced instr. set SPARC
   elfmach_960 =                { 19 }  -- Intel i960
   elfmach_ppc =                { 20 }  -- PowerPC
   elfmach_ppc64 =              { 21 }  -- 64-bit PowerPC
   elfmach_s390 =               { 22 }  -- IBM System/390
   elfmach_spu =                { 23 }  -- IBM SPU/SPC
   elfmach_v800 =               { 36 }  -- NEC V800
   elfmach_fr20 =               { 37 }  -- Fujitsu FR20
   elfmach_rh32 =               { 38 }  -- TRW RH-32
   elfmach_rce =                { 39 }  -- Motorola RCE
   elfmach_arm =                { 40 }  -- ARM (AARCH32)
   elfmach_alpha =              { 41 }  -- Digital Alpha
   elfmach_sh =                 { 42 }  -- Hitachi SH
   elfmach_sparcv9 =            { 43 }  -- SPARC version 9
   elfmach_tricore =            { 44 }  -- Siemens TriCore
   elfmach_arc =                { 45 }  -- Argonaut RISC Core
   elfmach_h8_300 =             { 46 }  -- Hitachi H8/300
   elfmach_h8_300h =            { 47 }  -- Hitachi H8/300H
   elfmach_h8s =                { 48 }  -- Hitachi H8S
   elfmach_h8_500 =             { 49 }  -- Hitachi H8/500
   elfmach_ia_64 =              { 50 }  -- Intel IA-64
   elfmach_mips_x =             { 51 }  -- Stanford MIPS-X
   elfmach_coldfire =           { 52 }  -- Motorola ColdFire
   elfmach_68hc12 =             { 53 }  -- Motorola M68HC12
   elfmach_mma =                { 54 }  -- Fujitsu MMA
   elfmach_pcp =                { 55 }  -- Siemens PCP
   elfmach_ncpu =               { 56 }  -- Sony nCPU
   elfmach_ndr1 =               { 57 }  -- Denso NDR1
   elfmach_starcore =           { 58 }  -- Motorola Star*Core
   elfmach_me16 =               { 59 }  -- Toyota ME16
   elfmach_st100 =              { 60 }  -- STMicro ST100
   elfmach_tinyj =              { 61 }  -- Advanced Logic TinyJ
   elfmach_x86_64 =             { 62 }  -- AMD x86_64
   elfmach_pdsp =               { 63 }  -- Sony DSP
   elfmach_pdp10 =              { 64 }  -- Digital PDP-10
   elfmach_pdp11 =              { 65 }  -- Digital PDP-11
   elfmach_fx66 =               { 66 }  -- Siemens FX-66
   elfmach_st9plus =            { 67 }  -- STMicro ST9+
   elfmach_st7 =                { 68 }  -- STMicro ST7
   elfmach_68hc16 =             { 69 }  -- Motorola M68HC16
   elfmach_68hc11 =             { 70 }  -- Motorola M68HC11
   elfmach_68hc08 =             { 71 }  -- Motorola M68HC08
   elfmach_68hc05 =             { 72 }  -- Motorola M68HC05
   elfmach_svx =                { 73 }  -- Silicon Graphics SVx
   elfmach_st19 =               { 74 }  -- STMicro ST19
   elfmach_vax =                { 75 }  -- Digital VAX
   elfmach_cris =               { 76 }  -- Axis Communications
   elfmach_javelin =            { 77 }  -- Infineon Technologies
   elfmach_firepath =           { 78 }  -- Element 14
   elfmach_zsp =                { 79 }  -- LSI Logic
   elfmach_mmix =               { 80 }  -- Knuth's MMIX
   elfmach_huany =              { 81 }  -- Harvard MI object
   elfmach_prism =              { 82 }  -- SiTera Prism
   elfmach_avr =                { 83 }  -- Atmel AVR
   elfmach_fr30 =               { 84 }  -- Fujitsu FR30
   elfmach_d10v =               { 85 }  -- Mitsubishi D10V
   elfmach_d30v =               { 86 }  -- Mitsubishi D30V
   elfmach_v850 =               { 87 }  -- NEC V850
   elfmach_m32r =               { 88 }  -- Mitsubishi M32R
   elfmach_mn10300 =            { 89 }  -- Matsushita MN10300
   elfmach_mn10200 =            { 90 }  -- Matsushita MN10200
   elfmach_pj =                 { 91 }  -- picoJava
   elfmach_openrisc =           { 92 }  -- OpenRISC
   elfmach_arc_compact =        { 93 }  -- ARC Intl ARCompact
   elfmach_xtensa =             { 94 }  -- Tensilica Xtensa
   elfmach_videocore =          { 95 }  -- Alphamosaic VideoCore
   elfmach_tmm_gpp =            { 96 }  -- Thompson Multimedia
   elfmach_ns32k =              { 97 }  -- Natl Semi 32000
   elfmach_tpc =                { 98 }  -- Tenor Network TPC
   elfmach_snp1k =              { 99 }  -- Trebia SNP 1000
   elfmach_st200 =              { 100 } -- STMicro ST200
   elfmach_ip2k =               { 101 } -- Ubicom IP2xxx
   elfmach_max =                { 102 } -- MAX
   elfmach_cr =                 { 103 } -- Natl Semi CompactRISC
   elfmach_f2mc16 =             { 104 } -- Fujitsu F2MC16
   elfmach_msp430 =             { 105 } -- Texas Instruments msp430
   elfmach_blackfin =           { 106 } -- Analog Devices Blackfin
   elfmach_se_c33 =             { 107 } -- Seiko Epson S1C33
   elfmach_sep =                { 108 } -- Sharp
   elfmach_arca =               { 109 } -- Arca RISC
   elfmach_unicore =            { 110 } -- PKU-Unity
   elfmach_excess =             { 111 } -- eXcess
   elfmach_dxp =                { 112 } -- Icera Deep Execution
   elfmach_altera_nios2 =       { 113 } -- Altera Nios II
   elfmach_crx =                { 114 } -- Natl Semi CompactRISC CRX
   elfmach_xgate =              { 115 } -- Motorola XGATE
   elfmach_c166 =               { 116 } -- Infineon C16x/XC16x
   elfmach_m16c =               { 117 } -- Renesas M16C
   elfmach_dspic30f =           { 118 } -- Microchip Technology
   elfmach_ce =                 { 119 } -- Freescale Comm Engine
   elfmach_ms32c =              { 120 } -- Renesas M32C
   elfmach_tsk3000 =            { 131 } -- Altium TSK3000
   elfmach_rs08 =               { 132 } -- Freescale RS08
   elfmach_sharc =              { 133 } -- Analog Devices SHARC
   elfmach_ecog2 =              { 134 } -- Cyan Technology eCOG2
   elfmach_score7 =             { 135 } -- Sunplus S+core7
   elfmach_dsp24 =              { 136 } -- NJR DSP
   elfmach_videocore3 =         { 137 } -- Broadcom VideoCore III
   elfmach_latticemico32 =      { 138 } -- RISC for Lattice FPGA
   elfmach_se_c17 =             { 139 } -- Seiko Epson C17
   elfmach_ti_c6000 =           { 140 } -- TI TMS320C6000
   elfmach_ti_c2000 =           { 141 } -- TI TMS320C2000
   elfmach_ti_c5500 =           { 142 } -- TI TMS320C5500
   elfmach_ti_arp32 =           { 143 } -- TI Application Specific RISC
   elfmach_ti_pru =             { 144 } -- TI Programmable Realtime Unit
   elfmach_mmdsp_plus =         { 160 } -- STMicro 64-bit DSP
   elfmach_cypress_m8c =        { 161 } -- Cypress M8C
   elfmach_r32c =               { 162 } -- Renesas R32C
   elfmach_trimedia =           { 163 } -- NXP TriMedia
   elfmach_qdsp6 =              { 164 } -- QUALCOMM DSP6
   elfmach_8051 =               { 165 } -- Intel 8051
   elfmach_stxp7x =             { 166 } -- STMicro STxP7x
   elfmach_nds32 =              { 167 } -- Andes Technology
   elfmach_ecog1x =             { 168 } -- Cyan Technology eCOG1X
   elfmach_maxq30 =             { 169 } -- Dallas Semi MAXQ30
   elfmach_ximo16 =             { 170 } -- NJR 16-bit DSP
   elfmach_manik =              { 171 } -- M2000 Reconfigurable RISC
   elfmach_craynv2 =            { 172 } -- Cray Inc. NV2
   elfmach_rx =                 { 173 } -- Renesas RX
   elfmach_metag =              { 174 } -- Imagination Technologies META
   elfmach_mcst_elbrus =        { 175 } -- MCST Elbrus
   elfmach_ecog16 =             { 176 } -- Cyan Technology eCOG16
   elfmach_c16 =                { 177 } -- Nat Semi CompactRISC CR16
   elfmach_etpu =               { 178 } -- Freescale Ext Time Proc. Unit
   elfmach_sle9x =              { 179 } -- Infineon Technologies SLE9X
   elfmach_l10m =               { 180 } -- Intel L10M
   elfmach_k10m =               { 181 } -- Intel K10M
   elfmach_aarch64 =            { 183 } -- ARM64 (AARCH64)
   elfmach_avr32 =              { 185 } -- Atmel AVR32
   elfmach_stm8 =               { 186 } -- STMicro STM8
   elfmach_tile64 =             { 187 } -- Tilera Tile64
   elfmach_tilepro =            { 188 } -- Tilera TilePro
   elfmach_microblaze =         { 189 } -- Xilinx MicroBlaze
   elfmach_cuda =               { 190 } -- NVIDIA CUDA
   elfmach_tilegx =             { 191 } -- Tilera TileGx
   elfmach_cloudshield =        { 192 } -- CloudShield
   elfmach_corea_1st =          { 193 } -- KIPO-KAIST Core-A 1st gen
   elfmach_corea_2nd =          { 194 } -- KIPO-KAIST Core-A 2nd gen
   elfmach_arc_compact2 =       { 195 } -- Synopsys ARCompact V2
   elfmach_open8 =              { 196 } -- Open8
   elfmach_rl78 =               { 197 } -- Renesas RL78
   elfmach_videocore5 =         { 198 } -- Broadcom VideoCore V
   elfmach_78kor =              { 199 } -- Renesas 78KOR
   elfmach_56800ex =            { 200 } -- Freescale 56800EX
   elfmach_ba1 =                { 201 } -- Beyond BA1
   elfmach_ba2 =                { 202 } -- Beyond BA2
   elfmach_xcore =              { 203 } -- XMOS xCORE
   elfmach_mchp_pic =           { 204 } -- Microchip 8-bit
   elfmach_intel205 =           { 205 } -- (reserved by Intel)
   elfmach_intel206 =           { 206 } -- (reserved by Intel)
   elfmach_intel207 =           { 207 } -- (reserved by Intel)
   elfmach_intel208 =           { 208 } -- (reserved by Intel)
   elfmach_intel209 =           { 209 } -- (reserved by Intel)
   elfmach_km32 =               { 210 } -- KM211 KM32
   elfmach_kmx32 =              { 211 } -- KM211 KMX32
   elfmach_kmx16 =              { 212 } -- KM211 KMX16
   elfmach_kmx8 =               { 213 } -- KM211 KMX8
   elfmach_kvarc =              { 214 } -- KM211 KVARC
   elfmach_cdp =                { 215 } -- Paneve CDP
   elfmach_coge =               { 216 } -- Cognitive Smart Memory
   elfmach_cool =               { 217 } -- Bluechip Systems CoolEngine
   elfmach_norc =               { 218 } -- Nanoradio Optimized RISC
   elfmach_csr_kalimba =        { 219 } -- CSR Kalimba
   elfmach_z80 =                { 220 } -- Zilog Z80
   elfmach_visium =             { 221 } -- VISIUMcore
   elfmach_ft32 =               { 222 } -- FTDI FT32
   elfmach_moxie =              { 223 } -- Moxie
   elfmach_amdgpu =             { 224 } -- AMD GPU
   elfmach_riscv =              { 243 } -- RISC-V

-- versions for the version field [HIST]
-- note that this appears as both a 16-bit and 8-bit field, so
-- we define it as the smaller size and downcast the 16-bit version.
bitdata ElfVersion where
   elfversion_none   = { 0: uint 8 }
   elfversion_current = { 1: uint 8 }

------------------------------------------------------------
-- Ident block (16 octets at beginning of header)
------------------------------------------------------------

-- There is actually only one ident block. The "class" field tells us
-- if we're looking at a 32-bit or 64-bit file, so we have two choices
-- about how to read the header:
--    1. Read the ident block first, then use the "class" field to
--       decide whether to read a 32-bit or 64-bit
--       rest-of-file-header.
--    2. Make two versions of the ident block, one that only accepts
--       ELFCLASS32 and one that only accepts ELFCLASS64, and put each
--       inside the corresponding file header, then at the top level
--       try parsing the input as either 32-bit or 64-bit knowing that
--       only one will match.
--
-- Option (1) is the "right" way to do it, by traditional programming
-- standards, but option (2) allows defining the structures the same
-- way they appear in the ELF spec (with the ident block inside the
-- file header), so we're going to go with that.

-- The ELF spec gives symbolic names for the nine non-padding octets
-- inside the ident block. I am not going to bother with these.

-- [HIST]
def ElfIdent =
   block
      -- get exactly 16 octets
      let curstream = GetStream
      let identstream = Take 16 curstream
      let nextstream = Drop 16 curstream
      SetStream identstream
      $$ = block
         -- first four bytes are the magic number: \177 E L F
         $[0x7f]
         Match "ELF"
         -- five bytes of basic info
         class =   UInt8 as? ElfClass
         data =    UInt8 as? ElfData
         version = UInt8 as? ElfVersion
         osabi =   UInt8 as? ElfOsabi	-- [gABI13]
         osabiversion = UInt8		-- [gABI13]
         -- the remaining bytes should be 0 but are supposed to be
         -- ignored on read (in case a future version of the format
         -- puts stuff there); so, since we explicitly peeled off 16
         -- bytes and would have failed above if the file was shorter
         -- than that, we don't need to explicitly read and discard
         -- them.
         --Match [UInt8, UInt8, UInt8, UInt8, UInt8, UInt8, UInt8]
         --End
         -- now reject invalid entries
         class == {| elfclass_none |} is false
         data == {| elfdata_none |} is false
         version == {| elfversion_none |} is false
      SetStream nextstream

def Elf32Ident =
   block
      let r = ElfIdent
      r.class == {| elfclass_32 |} is true
      -- if we get this far don't backtrack into the other size
      commit
      ^ r

def Elf64Ident =
   block
      let r = ElfIdent
      r.class == {| elfclass_64 |} is true
      -- if we get this far don't backtrack into the other size
      commit
      ^ r

-- classes for the class field [HIST]
bitdata ElfClass where
   elfclass_none = { 0: uint 8 }
   elfclass_32   = { 1: uint 8 }
   elfclass_64   = { 2: uint 8 }

-- values for the data field (endianness) [HIST]
bitdata ElfData where
   elfdata_none  = { 0: uint 8 }
   elfdata_2lsb  = { 1: uint 8 }
   elfdata_2msb  = { 2: uint 8 }

-- values for the osabi field [gABI13]
bitdata ElfOsabi where
   elfosabi_sysv =      { 0: uint 8 }	-- no extensions or unspecified
   elfosabi_hpux =      { 1: uint 8 }
   elfosabi_netbsd =    { 2: uint 8 }
   elfosabi_gnu =       { 3: uint 8 }	-- originally linux
   elfosabi_hurd =      { 4: uint 8 }	-- GNU HURD  [NetBSD]
   elfosabi_86open =    { 5: uint 8 }	-- 86Open    [NetBSD]
   elfosabi_solaris =   { 6: uint 8 }
   elfosabi_aix =       { 7: uint 8 }
   elfosabi_irix =      { 8: uint 8 }
   elfosabi_freebsd =   { 9: uint 8 }
   elfosabi_tru64 =     { 10: uint 8 }  -- Compaq Tru64 (Digital Unix)
   elfosabi_modesto =   { 11: uint 8 }  -- Novell Modesto
   elfosabi_openbsd =   { 12: uint 8 }
   elfosabi_openvms =   { 13: uint 8 }
   elfosabi_nsk =       { 14: uint 8 }	-- HP Non-Stop Kernel
   elfosabi_aros =      { 15: uint 8 }	-- Amiga Research OS
   elfosabi_fenixos =   { 16: uint 8 }
   elfosabi_cloudabi =  { 17: uint 8 }	-- Nuxi CloudABI
   elfosabi_openvos =   { 18: uint 8 }	-- Stratus OpenVOS


------------------------------------------------------------
-- Sections
------------------------------------------------------------

-- Read the section header table.
--
-- The section header table appears at file offset SHOFF; it is
-- SHNUM headers each of size SHENTSIZE (which may be greater
-- than the amount of data we actually read out)
def ElfShdrTable basestream parse_shdr shoff shnum shentsize =
   block
      let tablestream = Take (shnum * shentsize) (Drop shoff basestream)
      SetStream tablestream
      Many shnum
         block
            let curstream = GetStream
            let shdrstream = Take shentsize curstream
            let nextstream = Drop shentsize curstream
            SetStream shdrstream
            let shdr = parse_shdr
            SetStream nextstream
            ^ shdr

def Elf32ShdrTable basestream header =
   block
      let shoff = header.shoff as uint 64
      let shnum = header.shnum as uint 64
      let shentsize = header.shentsize as uint 64
      let results = ElfShdrTable basestream Elf32Shdr shoff shnum shentsize
      -- entry 0 is reserved and must be (mostly) 0
      shdr32_null_p (Index results 0) is true
      ^ results

def Elf64ShdrTable basestream header =
   block
      let shoff = header.shoff as uint 64
      let shnum = header.shnum as uint 64
      let shentsize = header.shentsize as uint 64
      let results = ElfShdrTable basestream Elf64Shdr shoff shnum shentsize
      -- entry 0 is reserved and must be (mostly) 0
      shdr64_null_p (Index results 0) is true
      ^ results

-- Special section indexes
-- (reserved range is 0xff00 through 0xffff, and 0xff00-0xff1f are
-- for per-processor ones)
def shn_UNDEF		= 0: uint 16
def shn_ABS		= 0xfff1: uint 16  -- absolute symbols
def shn_COMMON		= 0xfff2: uint 16  -- common symbols
def shn_XINDEX		= 0xffff: uint 16  -- overflow, get real value elsewhere

-- Some invariants:
--
-- 1. All material in the file except for the following should be in a section:
--       - the ELF header
--       - the program header table
--       - the section header table
-- However, voids/cavities in the file are explicitly permitted.
--
-- 2. There is exactly one section header for each section in the file.
-- (There may be additional section headers that do not have contents.)
--
-- 3. Section contents are contiguous and sections may not overlap.
--
-- XXX: how do we validate these?

-- 32-bit section header [HIST]
def Elf32Shdr =
   block
      name = Elf32Word
      type = Elf32Word as? ElfSectionType
      flags = Elf32Word as? ElfSectionFlags
      addr = Elf32Addr
      offset = Elf32Off
      size = Elf32Word
      link = Elf32Word
      info = Elf32Word
      addralign = Elf32Word
      entsize = Elf32Word

-- 64-bit section header [gABI13]
def Elf64Shdr =
   block
      name = Elf64Word
      type = Elf64Word as? ElfSectionType
      flags = Elf64Xword as? uint 32 as? ElfSectionFlags
      addr = Elf64Addr
      offset = Elf64Off
      size = Elf64Xword
      link = Elf64Word
      info = Elf64Word
      addralign = Elf64Xword
      entsize = Elf64Xword

-- section types [HIST]
bitdata ElfSectionType where
   sht_null =           { 0: uint 32 }
   sht_progbits =       { 1: uint 32 }
   sht_symtab =         { 2: uint 32 }
   sht_strtab =         { 3: uint 32 }
   sht_rela =           { 4: uint 32 }
   sht_hash =           { 5: uint 32 }
   sht_dynamic =        { 6: uint 32 }
   sht_note =           { 7: uint 32 }
   sht_nobits =         { 8: uint 32 }
   sht_rel =            { 9: uint 32 }
   sht_shlib =          { 10: uint 32 }
   sht_dynsym =         { 11: uint 32 }
   sht_init_array =     { 14: uint 32 } -- [gABI13]
   sht_fini_array =     { 15: uint 32 } -- [gABI13]
   sht_preinit_array =  { 16: uint 32 } -- [gABI13]
   sht_group =          { 17: uint 32 } -- [gABI13]
   sht_symtab_shndx =   { 18: uint 32 } -- [gABI13]

-- section 0 [HIST]
-- all zero except:
--    - the size field may be nonzero in which case it contains
--      the actual number of section header entries [gABI13]
--    - the link field may be nonzero in which case it contains
--      the index of the section header string table section [gABI13]
--
-- Those substitutions are done separately when reading the file
-- header (above); here we just have to ignore the values if present.
def shdr32_null_p (shdr: Elf32Shdr) =
   shdr.name == 0 &&
   shdr.type == {| sht_null |} &&
   --shdr.flags == (0 as ElfSectionFlags) && -- XXX notyet: broken (issue #357)
   shdr.addr == 0 &&
   shdr.offset == 0 &&
   shdr.info == 0 &&
   shdr.addralign == 0 &&
   shdr.entsize == 0

-- it's unfortunate we seem to need two copies of this since they're
-- textually identical, but we'd probably need typeclasses to avoid it
def shdr64_null_p (shdr: Elf64Shdr) =
   shdr.name == 0 &&
   shdr.type == {| sht_null |} &&
   --shdr.flags == (0 as ElfSectionFlags) && -- XXX notyet: broken (issue #357)
   shdr.addr == 0 &&
   shdr.offset == 0 &&
   shdr.info == 0 &&
   shdr.addralign == 0 &&
   shdr.entsize == 0

-- section flags
bitdata ElfSectionFlags where
   ElfSectionFlags = {
      0: uint 20,                    -- 0xfffff000 not used
      shf_compressed: uint 1,        -- 0x00000800  [gABI13]
      shf_tls: uint 1,               -- 0x00000400  [gABI13]
      shf_group: uint 1,             -- 0x00000200  [gABI13]
      shf_os_nonconforming: uint 1,  -- 0x00000100  [gABI13]
      shf_link_order: uint 1,        -- 0x00000080  [gABI13]
      shf_info_link: uint 1,         -- 0x00000040  [gABI13]
      shf_strings: uint 1,           -- 0x00000020  [gABI13]
      shf_merge: uint 1,             -- 0x00000010  [gABI13]
      0: uint 1,                     -- 0x00000008 not used
      shf_execinstr: uint 1,         -- 0x00000004  [HIST]
      shf_alloc: uint 1,             -- 0x00000002  [HIST]
      shf_write: uint 1              -- 0x00000001  [HIST]
    }

-- compression header for sections tagged shf_compressed [gABI13]
def Elf32Chdr =
   block
      type = Elf32Word as? ElfChdrType
      size = Elf32Word
      addralign = Elf32Word

def Elf64Chdr =
   block
      type = Elf64Word as? ElfChdrType
      reserved = Elf64Word
      size = Elf64Xword
      addralign = Elf64Xword

-- types for chdr type field
bitdata ElfChdrType where
   elfcompress_zlib = 0x1: uint 32

-- Parser to retrieve a stream for section contents
-- XXX: should try to share more between the 32/64 versions
def Elf32SectionData basestream (shdr: Elf32Shdr) =
   block
      let rawstream =
         Take (shdr.size as uint 64) (Drop (shdr.offset as uint 64) basestream)
      let compressed =
         case shdr.flags of
            ElfSectionFlags flags -> flags.shf_compressed
      if compressed == 0
         then
            block
               shstream = rawstream
               size = shdr.size
               addralign = shdr.addralign
         else
            block
               SetStream rawstream
               let chdr = Elf32Chdr
               case chdr.type of
                  elfcompress_zlib ->
                     block
                        -- FUTURE: call out to zlib here
                        shstream = Fail "No zlib support yet"
                        size = chdr.size
                        addralign = chdr.addralign

def Elf64SectionData basestream (shdr: Elf64Shdr) =
   block
      let rawstream =
         Take (shdr.size as uint 64) (Drop (shdr.offset as uint 64) basestream)
      let compressed =
         case shdr.flags of
            ElfSectionFlags flags -> flags.shf_compressed
      if compressed == 0
         then
            block
               shstream = rawstream
               size = shdr.size
               addralign = shdr.addralign
         else
            block
               SetStream rawstream
               let chdr = Elf64Chdr
               case chdr.type of
                  elfcompress_zlib ->
                     block
                        -- FUTURE: call out to zlib here
                        shstream = Fail "No zlib support yet"
                        size = chdr.size
                        addralign = chdr.addralign

-- flags for the flag word of a section group [gABI13]
bitdata SectionGroupFlags where
   SectionGroupFlags = {
      0: uint 31,
      grp_comdat: uint 1   -- 0x1
    }

-- table of special sections [HIST, gABI13]
{- XXX notyet
def specialsections = [
   ".bss" ->           (sht_nobits,         Exact (shf_alloc|shf_write))
   ".comment" ->       (sht_progbits,	    Exact 0)
   ".data" ->          (sht_progbits,	    Exact (shf_alloc|shf_write))
   ".data1" ->         (sht_progbits,	    Exact (shf_alloc|shf_write))
   ".debug" ->         (sht_progbits,	    Exact 0)
   ".dynamic" ->       (sht_dynamic,	    RequireAllow shf_alloc shf_write)
   ".dynstr" ->        (sht_strtab,	    Exact shf_alloc)
   ".dynsym" ->        (sht_dynsym,	    Exact shf_alloc)
   ".fini" ->          (sht_progbits,	    Exact (shf_alloc|shf_execinstr))
   ".fini_array" ->    (sht_fini_array,	    Exact (shf_alloc|shf_write))
   ".got" ->           (sht_progbits,	    NoCheck)
   ".hash" ->          (sht_hash,	    Exact shf_alloc)
   ".init" ->          (sht_progbits,	    Exact (shf_alloc|shf_execinstr))
   ".init_array" ->    (sht_init_array,	    Exact (shf_alloc|shf_write))
   ".interp" ->        (sht_progbits,	    RequireAllow 0 shf_alloc)
   ".line" ->          (sht_progbits,	    Exact 0)
   ".note" ->          (sht_note,	    Exact 0)
   ".plt" ->           (sht_progbits,	    NoCheck)
   ".preinit_array" -> (sht_preinit_array,  Exact (shf_alloc|shf_write))
   ".relname" ->       (sht_rel,	    RequireAllow 0 shf_alloc)
   ".relaname" ->      (sht_rela,	    RequireAllow 0 shf_alloc)
   ".rodata" ->        (sht_progbits,	    Exact shf_alloc)
   ".rodata1" ->       (sht_progbits,	    Exact shf_alloc)
   ".shstrtab" ->      (sht_strtab,	    Exact 0)
   ".strtab" ->        (sht_strtab,	    RequireAllow 0 shf_alloc)
   ".symtab" ->        (sht_symtab,	    RequireAllow 0 shf_alloc)
   ".symtab_shndx" ->  (sht_symtab_shndx,   RequireAllow 0 shf_alloc)
   ".tbss" ->          (sht_nobits,	    Exact (shf_alloc|shf_write|shf_tls))
   ".tdata" ->         (sht_progbits,	    Exact (shf_alloc|shf_write|shf_tls))
   ".tdata1" ->        (sht_progbits,	    Exact (shf_alloc|shf_write|shf_tls))
   ".text" ->          (sht_progbits,	    Exact (shf_alloc|shf_execinstr))
]

def checksection name type_seen flags_seen =
   case lookup name specialsections in
       Nothing -> ()
       Just (type_required, NoCheck) ->
          (type_seen == type_required) is true
       Just (type_required, Exact flags_required) ->
          (type_seen == type_required && flags_seen == flags_required) is true
       Just (type_required, RequireAllow flags_req flags_allowed) ->
          (type_seen == type_required &&
           (flags_seen & flags_req) == flags_req &&
           (flags_seen & ~flags_allowed) == 0) is true
-}

------------------------------------------------------------
-- String tables
------------------------------------------------------------

-- string table [HIST]
def StringtableSection basestream offset size =
   if size == 0 then
      block
         bytes = ""
         size = 1
   else
      block
         let sectionstream = Take size (Drop offset basestream)
         SetStream sectionstream
         bytes = Many size UInt8
         (Index bytes 0 == 0) is true
         (Index bytes (size - 1) == 0) is true
         size = size

-- wrappers that unpack the section headers
def Stringtable32Section basestream shdr =
   block
      let data = Elf32SectionData basestream shdr
      let offset = shdr.offset as uint 64
      let size = data.size as uint 64
      StringtableSection data.shstream offset size

def Stringtable64Section basestream shdr =
   block
      let data = Elf64SectionData basestream shdr
      let offset = shdr.offset as uint 64
      let size = data.size as uint 64
      StringtableSection data.shstream offset size


------------------------------------------------------------
-- Symbol table
------------------------------------------------------------

-- validate the flags on a symbol table section
def symtab_valid_flags_p (flags0: ElfSectionFlags) =
  case flags0 of
     ElfSectionFlags flags ->
        flags.shf_write == 0 &&
        flags.shf_alloc == 0 &&
        flags.shf_execinstr == 0 &&
        flags.shf_merge == 0 &&
        flags.shf_strings == 0 &&
        flags.shf_info_link == 0 &&
        flags.shf_link_order == 0 &&
        flags.shf_group == 0 &&
        flags.shf_tls == 0

-- Read a symbol table.
--
-- The section header table appears at file offset SHOFF; it is
-- SHNUM headers each of size SHENTSIZE (which may be greater
-- than the amount of data we actually read out)
def ElfSymtab Parse_sym shstream stringtables size link info entsize =
   block
      -- validate the size vs. the entsize
      size % entsize == 0 is true
      let num = size / entsize
      -- the link is the string table, so there should be a table there
      Index stringtables link == nothing is false
      let strtab =
         case Index stringtables (link as uint 64) of
            nothing -> Fail "strings for symbol table missing or not a string table"
            just tab -> tab
      -- the info value is the index of the first non-local symbol (if any)
      -- so it must be <= the number of symbols we've got
      info <= num is true

      SetStream shstream
      Many num
         block
            let curstream = GetStream
            let symstream = Take entsize curstream
            let nextstream = Drop entsize curstream
            SetStream symstream
            let sym = Parse_sym strtab
            SetStream nextstream
            ^ sym

def Elf32Symtab basestream stringtables shdr =
   block
      -- validate the flags
      symtab_valid_flags_p shdr.flags is true
      -- address should be 0
      shdr.addr == 0 is true
      let data = Elf32SectionData basestream shdr
      let size = data.size as uint 64
      let link = shdr.link as uint 64
      let info = shdr.info as uint 64
      let entsize = shdr.entsize as uint 64
      let results =
         ElfSymtab Elf32Sym data.shstream stringtables size link info entsize
      -- entry 0 is reserved and must be 0
      sym32_null_p (Index results 0) is true
      -- entries up to but not including info must be local, entries
      -- after not
      Foralli (Sym32_ok_local_p info) results
      ^ results

def Elf64Symtab basestream stringtables shdr =
   block
      -- validate the flags
      symtab_valid_flags_p shdr.flags is true
      -- address should be 0
      shdr.addr == 0 is true
      let data = Elf64SectionData basestream shdr
      let size = data.size as uint 64
      let link = shdr.link as uint 64
      let info = shdr.info as uint 64
      let entsize = shdr.entsize as uint 64
      let results =
         ElfSymtab Elf64Sym data.shstream stringtables size link info entsize
      -- entry 0 is reserved and must be 0
      sym64_null_p (Index results 0) is true
      -- entries up to but not including info must be local, entries
      -- after not
      Foralli (Sym64_ok_local_p info) results
      ^ results

-- 32-bit symbol table entry [HIST]
-- XXX: this should validate that shndx (the section index) is in range
-- XXX: and also, if it's SHN_XINDEX, fetch the real value from the
--      sht_symtab_shndx section
def Elf32Sym namestrtab =
   block
      name = Elf32Word
      name as uint 64 <= namestrtab.size is true
      value = Elf32Addr
      size = Elf32Word
      info = UInt8 as? ElfSymInfo
      other = UInt8 as? ElfSymOther
      shndx = Elf32Half

-- 64-bit symbol table entry [gABI13]
-- XXX: this should validate that shndx (the section index) is in range
-- XXX: and also, if it's SHN_XINDEX, fetch the real value from the
--      sht_symtab_shndx section
def Elf64Sym namestrtab =
   block
      name = Elf64Word
      name as uint 64 <= namestrtab.size is true
      info = UInt8 as? ElfSymInfo
      other = UInt8 as? ElfSymOther
      shndx = Elf64Half
      value = Elf64Addr
      size = Elf64Xword

-- decoding of the info field [HIST]
bitdata ElfSymInfo where
   ElfSymInfo = {
      bind: ElfSymBinding,
      type: ElfSymType
    }

-- values for the bind field [HIST]
bitdata ElfSymBinding where
   stb_local = 0: uint 4
   stb_global = 1: uint 4
   stb_weak = 2: uint 4

-- values for the type field [HIST]
bitdata ElfSymType where
   stt_notype = 0: uint 4
   stt_object = 1: uint 4
   stt_func = 2: uint 4
   stt_section = 3: uint 4
   stt_file = 4: uint 4
   stt_common = 5: uint 4	-- [gABI13]
   stt_tls = 6: uint 4		-- [gABI13]

-- decoding of the other field [gABI13]
bitdata ElfSymOther where
   ElfSymOther = {
      0: uint 6,
      visibility: ElfSymVisibility
    }

-- values for the visibility field [gABI13]
bitdata ElfSymVisibility where
   stv_default = 0: uint 2
   stv_internal = 1: uint 2
   stv_hidden = 2: uint 2
   stv_protected = 3: uint 2

-- constraint on the 0th entry in the symbol table
def sym32_null_p (sym: Elf32Sym) =
   sym.name == 0 &&
   sym.value == 0 &&
   sym.size == 0 &&
   --sym.info == (0 as ElfSymInfo) &&  -- XXX notyet: broken (issue #357)
   --sym.other == (0 as ElfSymOther) &&  -- XXX notyet: broken (issue #357)
   sym.shndx == shn_UNDEF

def sym64_null_p (sym: Elf64Sym) =
   sym.name == 0 &&
   sym.value == 0 &&
   sym.size == 0 &&
   --sym.info == (0 as ElfSymInfo) &&  -- XXX notyet: broken (issue #357)
   --sym.other == (0 as ElfSymOther) &&  -- XXX notyet: broken (issue #357)
   sym.shndx == shn_UNDEF

-- check that local symbols come before globals and match the declared
-- threshold.
def Sym32_ok_local_p maxlocal i (sym: Elf32Sym) =
   case sym.info of
      ElfSymInfo info ->
         case info.bind of
            stb_local -> i < maxlocal
            _ -> i >= maxlocal

def Sym64_ok_local_p maxlocal i (sym: Elf64Sym) =
   case sym.info of
      ElfSymInfo info ->
         case info.bind of
            stb_local -> i < maxlocal
            _ -> i >= maxlocal

------------------------------------------------------------
-- Relocations
------------------------------------------------------------

-- validate the flags on a relocations section
def rels_valid_flags_p (flags0: ElfSectionFlags) =
   case flags0 of
      ElfSectionFlags flags ->
         flags.shf_write == 0 &&
         flags.shf_alloc == 0 &&
         flags.shf_execinstr == 0 &&
         flags.shf_merge == 0 &&
         flags.shf_strings == 0 &&
         flags.shf_info_link == 1 &&
         flags.shf_link_order == 0 &&
         flags.shf_group == 0 &&
         flags.shf_tls == 0

-- Read a relocations section table.
--
-- The section header table appears at file offset SHOFF; it is
-- SHNUM headers each of size SHENTSIZE (which may be greater
-- than the amount of data we actually read out)
def AnyrelSection Anyrel shstream symtabs size link info entsize =
   block
      -- validate the size vs. the entsize
      size % entsize == 0 is true
      let num = size / entsize
      -- the link is the symbol table, so there should be a symbol table there
      let symtab =
         case Index symtabs (link as uint 64) of
            nothing -> Fail "Symbol table for relocations missing or not a symbol table"
            just tab -> tab
      -- the info value is the section index of the section we're relocating,
      -- so it should be a section type where that makes sense
      -- (XXX not in a position to check this just yet, and if adding such
      -- logic remember that things like debug info routinely get relocated)

      SetStream shstream
      Many num
         block
            let curstream = GetStream
            let symstream = Take entsize curstream
            let nextstream = Drop entsize curstream
            SetStream symstream
            let rel = Anyrel {}
            SetStream nextstream
            ^ rel

def Anyrel32Section basestream Anyrel symtabs shdr =
   block
      -- validate the flags
      rels_valid_flags_p shdr.flags is true
      let data = Elf32SectionData basestream shdr
      let size = data.size as uint 64
      let link = shdr.link as uint 64
      let info = shdr.info as uint 64
      let entsize = shdr.entsize as uint 64
      AnyrelSection Anyrel data.shstream symtabs size link info entsize

def Rel32Section basestream symtabs shdr =
   Anyrel32Section basestream Elf32Rel symtabs shdr

def Rela32Section basestream symtabs shdr =
   Anyrel32Section basestream Elf32Rela symtabs shdr

def Anyrel64Section basestream Anyrel symtabs shdr =
   block
      -- validate the flags
      symtab_valid_flags_p shdr.flags is true
      let data = Elf64SectionData basestream shdr
      let size = data.size as uint 64
      let link = shdr.link as uint 64
      let info = shdr.info as uint 64
      let entsize = shdr.entsize as uint 64
      AnyrelSection Anyrel data.shstream symtabs size link info entsize

def Rel64Section basestream symtabs shdr =
   Anyrel64Section basestream Elf64Rel symtabs shdr

def Rela64Section basestream symtabs shdr =
   Anyrel64Section basestream Elf64Rela symtabs shdr

-- I have fused the rel and rela types (so that the addend is a Maybe)
-- because otherwise we can't have one list of relocation sections and
-- that makes a mess.

-- 32-bit relocation [HIST]
def Elf32Rel ignore =
   block
      offset = Elf32Addr
      info = Elf32Word
      addend = nothing

-- 32-bit relocation-with-addend [HIST]
def Elf32Rela ignore : Elf32Rel =
   block
      offset = Elf32Addr
      info = Elf32Word
      addend = just Elf32Sword

-- 64-bit relocation [gABI13]
def Elf64Rel ignore =
   block
      offset = Elf64Addr
      info = Elf64Xword
      addend = nothing

-- 64-bit relocation-with-addend [gABI13]
def Elf64Rela ignore : Elf64Rel =
   block
      offset = Elf64Addr
      info = Elf64Xword
      addend = just Elf64Xsword

-- decoding for the info field
bitdata Elf32RelInfo where
   Elf32RelInfo = { sym: uint 24, type: uint 8 }

bitdata Elf64RelInfo where
   Elf64RelInfo = { sym: uint 32, type: uint 32 }

-- Note that the type field is the relocation code and those are per-processor.

------------------------------------------------------------
-- Program headers
------------------------------------------------------------

-- Read the program header table.
--
-- Like the section header table, the program header (segment) table
-- appears at file offset PHOFF and contains PHNUM headers each of
-- size PHENTSIZE (which may be greater than the amount of data we
-- actually read out)
def ElfPhdrTable basestream Parse_phdr phoff phnum phentsize =
   block
      let tablestream = Take (phnum * phentsize) (Drop phoff basestream)
      SetStream tablestream
      Many phnum
         block
            let curstream = GetStream
            let phdrstream = Take phentsize curstream
            let nextstream = Drop phentsize curstream
            SetStream phdrstream
            let phdr = Parse_phdr {}
            SetStream nextstream
            ^ phdr

def Elf32PhdrTable basestream (header: Elf32Header) : [Elf32Phdr] =
   block
      let phoff = header.phoff as UInt64
      let phnum = header.phnum as UInt64
      let phentsize = header.phentsize as UInt64
      ElfPhdrTable basestream Elf32Phdr phoff phnum phentsize

def Elf64PhdrTable basestream (header: Elf64Header) : [Elf64Phdr] =
   block
      let phoff = header.phoff as UInt64
      let phnum = header.phnum as UInt64
      let phentsize = header.phentsize as UInt64
      ElfPhdrTable basestream Elf64Phdr phoff phnum phentsize

-- 32-bit program header [HIST]
def Elf32Phdr ignore =
   block
      type = Elf32Word as? ElfSegmentType
      offset = Elf32Off
      vaddr = Elf32Addr
      paddr = Elf32Addr
      filesz = Elf32Word
      memsz = Elf32Word
      flags = Elf32Word as? ElfSegmentFlags
      align = Elf32Word

-- 64-bit program header [gABI13]
def Elf64Phdr ignore =
   block
      type = Elf64Word as? ElfSegmentType
      flags = Elf64Word as? ElfSegmentFlags
      offset = Elf64Off
      vaddr = Elf64Addr
      paddr = Elf64Addr
      filesz = Elf64Xword
      memsz = Elf64Xword
      align = Elf64Xword

-- types for the type field [HIST]
bitdata ElfSegmentType where
   pt_null = 0: uint 32
   pt_load = 1: uint 32
   pt_dynamic = 2: uint 32
   pt_interp = 3: uint 32
   pt_note = 4: uint 32
   pt_shlib = 5: uint 32
   pt_phdr = 6: uint 32
   pt_tls = 7: uint 32  -- [gABI13]

-- flags for the flags field [HIST]
bitdata ElfSegmentFlags where
   ElfSegmentFlags = {
      0: uint 29,
      PF_R: uint 1,	-- 4
      PF_W: uint 1,	-- 2
      PF_X: uint 1	-- 1
    }

------------------------------------------------------------
-- notes
------------------------------------------------------------

-- XXX notyet
def NoteSection shstream offset size =
   [0: uint 32]

def Note32Section basestream shdr =
   block
      let data = Elf32SectionData basestream shdr
      let offset = shdr.offset as uint 64
      let size = data.size as uint 64
      NoteSection data.shstream offset size

def Note64Section basestream shdr =
   block
      let data = Elf64SectionData basestream shdr
      let offset = shdr.offset as uint 64
      let size = data.size as uint 64
      NoteSection data.shstream offset size

------------------------------------------------------------
-- top level
------------------------------------------------------------

def Elf32File =
   block
      let basestream = GetStream
      header = Elf32Header basestream
      let ?bigEndian =
         case header.ident.data of
            elfdata_2lsb -> false
            elfdata_2msb -> true

      phdrs = Elf32PhdrTable basestream header
      shdrs = Elf32ShdrTable basestream header

      -- we've checked that the first section header is null; make sure
      -- no others are
      for (seen = false; shdr in shdrs)
         if seen == false
            then true
            else
               block
                  shdr.type == {| sht_null |} is false
                  true
           
      -- load all the stringtables first
      stringtables = map (shdr in shdrs)
         case shdr.type of
            sht_strtab -> just (Stringtable32Section basestream shdr)
            _ -> nothing

      -- the shstrndx section must be a string table (it has the section names)
      shstrtab =
         case Index stringtables (header.shstrndx as uint 64) of
            nothing -> Fail "Section name strings missing or not a string table"
            just tab -> tab

      -- XXX we need to look for a section of type sht_symtab_shndx, and
      -- if found load it and provided to Elf32Symtab for resolving SHN_XINDEX
      -- overflow section numbers.

      -- load the symbol tables (regular and possibly also dynamic)
      symtabs = map (shdr in shdrs)
         case shdr.type of
            sht_symtab -> just (Elf32Symtab basestream stringtables shdr)
            sht_dynsym -> just (Elf32Symtab basestream stringtables shdr)
            _ -> nothing

      -- load relocations
      relocs = map (shdr in shdrs)
         case shdr.type of
            sht_rela -> just (Rela32Section basestream symtabs shdr)
            sht_rel -> just (Rel32Section basestream symtabs shdr)
            _ -> nothing

      -- load notes
      notes = map (shdr in shdrs)
         case shdr.type of
            sht_note -> just (Note32Section basestream shdr)
            _ -> nothing

      -- FUTURE/XXX: things left we haven't handled are:
      --    - dynamic section (sht_dynamic)
      --    - dynamic linker hash table (sht_hash)
      --    - shlib section (sht_shlib)
      --    - section groups (sht_group)

def Elf64File =
   block
      let basestream = GetStream
      header = Elf64Header basestream
      let ?bigEndian =
         case header.ident.data of
            elfdata_2lsb -> false
            elfdata_2msb -> true

      phdrs = Elf64PhdrTable basestream header
      shdrs = Elf64ShdrTable basestream header

      -- we've checked that the first section header is null; make sure
      -- no others are
      for (seen = false; shdr in shdrs)
         if seen == false
            then true
            else
               block
                  shdr.type == {| sht_null |} is false
                  true
           
      -- load all the stringtables first
      stringtables = map (shdr in shdrs)
         case shdr.type of
            sht_strtab -> just (Stringtable64Section basestream shdr)
            _ -> nothing

      -- the shstrndx section must be a string table (it has the section names)
      shstrtab =
         case Index stringtables (header.shstrndx as uint 64) of
            nothing -> Fail "Section name strings missing or not a string table"
            just tab -> tab

      -- XXX we need to look for a section of type sht_symtab_shndx, and
      -- if found load it and provided to Elf32Symtab for resolving SHN_XINDEX
      -- overflow section numbers.

      -- load the symbol tables (regular and possibly also dynamic)
      symtabs = map (shdr in shdrs)
         case shdr.type of
            sht_symtab -> just (Elf64Symtab basestream stringtables shdr)
            sht_dynsym -> just (Elf64Symtab basestream stringtables shdr)
            _ -> nothing

      -- load relocations
      relocs = map (shdr in shdrs)
         case shdr.type of
            sht_rela -> just (Rela64Section basestream symtabs shdr)
            sht_rel -> just (Rel64Section basestream symtabs shdr)
            _ -> nothing

      -- load notes
      -- load notes
      notes = map (shdr in shdrs)
         case shdr.type of
            sht_note -> just (Note64Section basestream shdr)
            _ -> nothing

      -- FUTURE/XXX: things left we haven't handled are:
      --    - dynamic section (sht_dynamic)
      --    - dynamic linker hash table (sht_hash)
      --    - shlib section (sht_shlib)
      --    - section groups (sht_group)

def ElfFile =
   Choose
      block
         let f = Elf32File
         ^ {| is32 = f |}
      block
         let f = Elf64File
         ^ {| is64 = f |}

def Main =
   ElfFile
