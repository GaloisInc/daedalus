import PdfDecl 
import PdfValue 

-- Encryption dictionary (Table 20 in S7.6.1) 
def EncryptionDict (eref : Ref) = { 
  @edict = (ResolveValRef eref) is dict; 

  encFilter = (Lookup "Filter" edict) is name; 
  encFilter == "Standard" is true; -- Other modes unsupported 

  encSubFilter = Optional ((Lookup "SubFilter" edict) is name); 

  encV = LookupNat "V" edict; 

  -- Fields for the Standard security handler (Table 21, S7.6.3.2)
  encR = LookupNat "R" edict; 
  encR == 3 || (encV == 4 && encR == 4) is true; -- Other modes unsupported 

  encO = (Lookup "O" edict) is string; 
  encU = (Lookup "U" edict) is string; 

  encP = { 
    @v = (Lookup "P" edict) is number; 
    ^ v.num;
  }; 

  ciph = ChooseCiph edict encV; 
} 

def ChooseCiph edict v = Choose1 { 
  v2RC4 = { 
    v == 2 is true; 
    @len = LookupNat "Length" edict; 
    len == 128 is true; 
  }; 
  v4RC4 = { 
    v == 4 is true;
    @stmFname = V4stmFname edict; 
    stmFname == "V2" is true;
  }; 
  v4AES = { 
    v == 4 is true;
    @stmFname = V4stmFname edict; 
    stmFname == "AESV2" is true;
  }; 
}

def V4stmFname edict = {
  @stmF = (Lookup "StmF" edict) is name; 
  @strF = (Lookup "StrF" edict) is name;       
  @cf = (Lookup "CF" edict) is dict; 
  
  -- Lookup stream filter 
  @stmFdict = (Lookup stmF cf) is dict; 
  @stmFname = (Lookup "CFM" stmFdict) is name; 
  @stmFLen = LookupNat "Length" stmFdict; 

  -- Lookup string filter 
  @strFdict = (Lookup strF cf) is dict; 
  @strFname = (Lookup "CFM" strFdict) is name; 
  @strFLen = LookupNat "Length" strFdict; 

  ^ stmFname; 
} 

{- 
def ChooseCiphV4 name = Choose1 { 
  v4RC4 = { 
    name == "V2" is true; 
  }; 
  v4AES = { 
    name == "AESV2" is true; 
  }; 
}
-} 

{- 
def MakeContext (t : TrailerDict) = Choose1 { 
  encryption = { 
    @enc = (Lookup "encrypt" t) is just;
    commit; 
    @r = EncryptionDict enc.eref; 

  }
  noencryption = {}; 
}
-} 

{-
makeEncContext :: Integral a => 
                      TrailerDict  
                  -> ObjIndex 
                  -> Input 
                  -> BS.ByteString 
                  -> IO ((a, a) -> Maybe EncContext)
makeEncContext trail refs topInput pwd = 
  case getField @"encrypt" trail of 
    Nothing -> pure $ const Nothing -- No encryption 
    Just e -> do 
      let eref = getField @"eref" e 
      enc <- handlePdfResult (runParser refs Nothing (pEncryptionDict eref) topInput) 
                              "Ambiguous encryption dictionary"
      let encO = vecToRep $ getField @"encO" enc 
          encP = fromIntegral $ getField @"encP" enc
          id0 = vecToRep $ getField @"id0" e 
          filekey = makeFileKey pwd encO encP id0
      pure $ \(ro, rg) -> 
        Just EncContext { key  = filekey, 
                          robj = fromIntegral ro, 
                          rgen = fromIntegral rg, 
                          ver  = fromIntegral $ getField @"encV" enc, 
                          ciph = chooseCipher $ getField @"ciph" enc  } 

chooseCipher :: ChooseCiph -> Cipher 
chooseCipher enc = 
  case enc of 
    ChooseCiph_v2 _ -> V2 
    ChooseCiph_v4 i -> 
      case i of 
        ChooseCiphV4_v4AES () -> V4AES 
        ChooseCiphV4_v4RC4 () -> V4RC4
-}