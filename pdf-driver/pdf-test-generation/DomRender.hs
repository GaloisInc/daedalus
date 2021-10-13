{-# LANGUAGE LambdaCase, OverloadedStrings #-}

module DomRender where

import Data.Binary
import Data.List
import Text.Printf


import qualified Data.ByteString.Char8 as B8 
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Lazy  as BSL

import DomGeneration

---- rendering/etc -----------------------------------------------------------

-- | 2 styles of rendering:
data RenderType = RT_XRef_Trad | RT_XRef_Strm
                  deriving (Eq,Ord,Read,Show)

render_PDF_DOM :: RenderType -> PDF_DOM -> BS
render_PDF_DOM rtype (h1,h2,os,tr) =
  hdr `BS.append` render_Upd rtype (fromIntegral (BS.length hdr)) os tr
  where
  hdr = h1 <> h2
  
render_Upd :: RenderType -> Offset -> [TopDecl] -> Dict -> BS
render_Upd rtype offset decls trailer =
  BS.concat decls'
  `BS.append` render_XRefAndTrailer offset' xrefAndTrailer
    
  where
  xrefAndTrailer =
    case rtype of
      RT_XRef_Trad -> XRAT_Trad (map mkXRef_Trad xrefs) trailer
      RT_XRef_Strm -> XRAT_Strm
                        (map mkXRef_Strm xrefs ++ allType2s)
                        trailer
                      where
                      allType2s = concat [ getT2s ref ds | (ref,ObjStrm ds) <- decls ]
                      getT2s (oi,g) ds
                        | g /= 0    = error "g /= 0"
                        | otherwise = zipWith (\index (oid,_)-> (oid, XRES_InUse2 oi index))
                                          [0..]
                                          ds
                                        
  decls'            = map render_TopDecl decls    -- renderings
  (offset',offsets) = accumOffsets offset decls'
  xrefs             = zipWith (\(x,_) o-> (x,o)) decls offsets
    
  mkXRef_Trad ((i,g), off') = (i, XRR_InUse   off' g)
  mkXRef_Strm ((i,g), off') = (i, XRES_InUse1 off' g)

  
accumOffsets :: Offset -> [BS] -> (Offset, [Offset])
accumOffsets off os =
  mapAccumL addLength off os

  where
  addLength off' bs = ( off' + fromIntegral (BS.length bs)
                      , off'
                      )
  
render_XRefAndTrailer :: Offset -> XRefAndTrailer -> BS
render_XRefAndTrailer offset =
  \case
    XRAT_Trad xrefs d -> BS.concat
                           [ render_XRefTrad $
                               (0, XRR_Free 0 65535) : xrefs
                           , "\ntrailer\n"
                           , render_Value $ V_Dict d
                           , "\nstartxref\n"
                           , showAsByteString offset
                           , "\n%%EOF"
                           ]
                          
    XRAT_Strm xrefs d -> BS.concat
                           [ render_TopDecl
                               ( (30,0)  -- FIXME: matters?
                               , StrmObj $ mkXRefStreamObj
                                             ((0, XRES_Free 0 65535) : xrefs) d
                               )
                           , "startxref\n"
                           , showAsByteString offset
                           , "\n%%EOF"
                           ]
    
    -- FIXME: in both above, this "_Free 0 ..." is just for the *first* xref in file
    --        - with updates this should not be here.

mkXRefStreamObj :: [(OID, XRefEntry_Strm)] -> Dict -> StreamObj
mkXRefStreamObj xrefs dict =
  StreamObj ( [ ("Type" , V_Name "XRef")
              , ("Index", V_Array indices)
              , ("W"    , V_Array (map V_Int [1,2,2])) -- field sizes
              ]
              ++ dict)
             NoFilter -- ASCIIHexDecode
             streamData -- (showAsByteString xrefs)
               -- (for debugging: showAsByteString xrefs)
  where
  subsections = findRanges $ sortOn fst xrefs
  indices     = map V_Int $ concatMap (\(s,l,_)->[s,l]) subsections
  streamData  = BS.concat $ map renderSubSection subsections

  renderSubSection (_s,_l,es) = BS.concat (map renderEntry es)

  renderEntry :: XRefEntry_Strm -> BS
  renderEntry = BS.pack . BSL.unpack . encode

-- FIXME: ad hoc: the 1,2,2 is built-in!!
instance Binary XRefEntry_Strm where
  get = error "get: not implemented"
  put = \case
    XRES_Free   i g     -> putWord8 0 >> put16 i >> put16 g
    XRES_InUse1 o g     -> putWord8 1 >> put16 o >> put16 g
    XRES_InUse2 o index -> putWord8 2 >> put16 o >> put16 index
                
    where
    put16 x = put (fromIntegral x :: Word16)  -- FIXME: this big-endian?

render_TopDecl :: TopDecl -> BS
render_TopDecl (or', x) =
  render_ObjRef or' <> " obj\n" <> render_TopDeclDef x <> "endobj\n"

render_TopDeclDef :: TopDeclDef -> BS
render_TopDeclDef y =
  case y of
     Value v   -> render_Value v
     StrmObj x -> render_StreamObj x
     ObjStrm x -> render_ObjStrm NoFilter x
  <> "\n"
                     
render_Value :: Value -> BS
render_Value x = case x of
                   V_Name s      -> render_Name s
                   V_Int i       -> render_Int i
                   V_Dict d      -> render_Dict d
                   V_String s    -> render_String s
                   V_Null        -> "null"
                   V_Bool b      -> if b then "true" else "false"
                   V_Array vs    -> render_Array vs
                   V_Indirect o  -> render_ObjRef o <> " R"
                   V_Raw s       -> B8.pack s

render_XRefTrad items =
  "xref\n" <> BS.concat (map render_XRefTrad_Subsection subsections)

  where
  subsections = findRanges $ sortOn fst items
  
  render_XRefTrad_Subsection (s,l,es) =
       showAsByteString s
    <> " "
    <> showAsByteString l
    <> "\n"
    <>  BS.concat (map render_XRefRhs es)

  render_XRefRhs = \case
                      XRR_InUse off g -> B8.pack $ printf "%010d %05d n \n" off g 
                      XRR_Free  oid g -> B8.pack $ printf "%010d %05d f \n" oid g
    
---- basic rendering ---------------------------------------------------------

render_ObjStrm :: Filter -> [(OID,TopDeclDef)] -> BS
render_ObjStrm f decls =
    render_StreamObj
  $ StreamObj
      [ ("Type" , V_Name "ObjStm")
      , ("N"    , V_Int (length decls))
      , ("First", V_Int (BS.length renderOffsetTable))
      , ("UNUSED", V_Int 1234567890123456)
      ]
      f
      (BS.concat (renderOffsetTable : decls'))
    
  where
  decls'      = map (render_TopDeclDef . snd) decls    -- renderings

  (_,offsets) = accumOffsets 0 decls'
    
  renderOffsetTable = BS.intercalate " " (zipWith renderPair (map fst decls) offsets)
                      <> "\n"

  renderPair a b = BS.intercalate " " (map showAsByteString [a,b])

render_StreamObj :: StreamObj -> BS
render_StreamObj (StreamObj dict f bs) =
  BS.concat $ [ render_Dict (("Length", V_Int (BS.length bs')) : addFilter dict)
              , "\nstream\n"
              , bs'
              , "\nendstream"
              ]
  where
  addFilter d = case f of NoFilter       -> d
                          ASCIIHexDecode -> [("Filter", V_Name "ASCIIHexDecode")] ++ d
  bs' = case f of NoFilter       -> bs
                  ASCIIHexDecode -> BS.intercalate " " (map renderByteHex $ BS.unpack bs) <> " >"

  renderByteHex :: Word8 -> BS
  renderByteHex w = B8.pack $ printf "%02X" w
  
render_ObjRef (i,g) = BS.intercalate " " [showAsByteString i, showAsByteString g]

render_Name s = "/" <> B8.pack s  -- right?
render_Int i = showAsByteString i

render_Array :: [Value] -> BS
render_Array vs = BS.concat
              $ ["["] ++ [BS.intercalate " " (map render_Value vs)] ++ ["]"]
  -- FIXME parenthesis?!
  
render_Dict :: Dict -> BS
render_Dict d = 
  -- borrowing from HPDF no recursive indenting; whatsup with the loop-tying?!):
  BS.concat $ ["<<\n", convertLevel d, ">>"]
  
  where
  convertLevel :: Dict -> BS
  convertLevel _ = foldr convertItem BS.empty d
    where
    convertItem (key,value) current = BS.concat
                                        [ render_Name key
                                        , " "
                                        , render_Value value
                                        , "\n"
                                        , current
                                        ]

           
render_String s = showAsByteString s -- FIXME

---- library -----------------------------------------------------------------

findRanges :: (Show a, Ord a, Enum a, Num a) => [(a, b)] -> [(a, Int, [b])]
findRanges = map unRev . findRangesRev

  where
  unRev (i,bs) = (i - fromIntegral len + 1, len, reverse bs)
                 where
                 len = length bs
    
  findRangesRev []          = []
  findRangesRev ((i,b):ibs) = getRange i [b] ibs
  
  getRange i' bs []               = [(i',bs)]
  getRange i' bs ((i'',b''):ibs') = if i' >= i'' then
                                      error $ "findRanges: " ++ show (i',i'')
                                    else if i' == pred i'' then
                                      getRange i'' (b'':bs) ibs'
                                    else
                                      (i',bs) : findRangesRev ((i'',b''):ibs')


showAsByteString :: Show a => a -> BS
showAsByteString = B8.pack . show
