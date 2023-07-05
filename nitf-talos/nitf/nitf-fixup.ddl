
import nitf_main

def min x y = if x < y then x else y

def Zip xs ys = map (i in rangeUp 0 (min (length xs) (length ys))) { fst = Index xs i, snd = Index ys i }

def Mk off (nbytes : int) sz = block
  offset = ^ off
  nbytes = ^ nbytes
  size   = ^ sz

def Fixups = block
  @nitf = Main
  @hdr_fixup = Mk nitf.h.talos_fixup_offset 6 nitf.h.talos_fixup_bsize
  @img_fixups = map (i, l in nitf.h.li)
    Mk l.talos_fixup_offset 6 (Index nitf.img_segments i).imgHeader.talos_fixup_bsize
  @graph_fixups = map (i, l in nitf.h.graphlens)
    Mk l.talos_fixup_offset 4 (Index nitf.graph_segments i).graphHeader.talos_fixup_bsize
  @txt_fixups = map (i, l in nitf.h.textlens)
    Mk l.talos_fixup_offset 4 (Index nitf.txt_segments i).txtHeader.talos_fixup_bsize
  @dataext_fixups = map (i, l in nitf.h.dataextlens)
    Mk l.talos_fixup_offset 4 (Index nitf.dataExt_segments i).dataExtHeader.talos_fixup_bsize
  -- resext should be zero
  ^ concat [[hdr_fixup], img_fixups, graph_fixups, txt_fixups, dataext_fixups]

def T1 = Zip [(1 : int), 2] ['a', 'b', 'c']

def TestOn input P =
  block
    SetStream (arrayStream input)
    P
