{-# LANGUAGE OverloadedStrings #-}

module DomExamples where

import qualified Data.ByteString.Char8 as B8 
import qualified Data.ByteString       as BS

import DomGeneration

---- examples ----------------------------------------------------------------

pdf1 :: PDF_DOM
pdf1 = mk_PDF_DOM (hdr1 "1.4") "CAVITY\n" 3 domObjs1

domObjs1 =
  [ vToTopDecl 1 (V_Int 5) -- dead object 
  , vToTopDecl 2 (V_Int 6) -- dead object 
  , vToTopDecl 3 (V_Dict [("Type" , V_Name "Catalog")
                         ,("Pages", V_Indirect (4,0))
                         ])
  , vToTopDecl 4 (V_Dict [("Type", V_Name "Pages")
                         ,("MediaBox", V_Raw "[0 0 842 595]")
                         ,("Kids", V_Array [V_Indirect (5,0)])
                         ,("Count", V_Int 1)
                         ])
  , vToTopDecl 5 (V_Dict [("Type", V_Name "Page")
                         ,("Parent", V_Indirect (4,0))
                         ,("Contents", V_Indirect (6,0))
                         ,("Resources", V_Indirect (9,0))
                          -- DEFACTO: no errors/warns when 'Resources' mispelled!
                         ])
  , ((6,0),  StrmObj (StreamObj []
                                NoFilter
                                (B8.pack helloWorldContentStream)))
  , vToTopDecl 7 $ (V_Int 7) -- dead
  , vToTopDecl 8 $ courierF1
  , vToTopDecl 9 $ resources

  -- more dead objects (want some 2 digit objects):
  , vToTopDecl 10 (V_Int 1000) 
  , vToTopDecl 11 (V_Raw "999 % dead object (01234567890123456789)")
  ]

---- pdf3 : syntax errors for values

pdf3a,pdf3b :: PDF_DOM

pdf3a = mk_PDF_DOM (hdr1 "1.4") "CAVITY\n" 3 domObjs3a
domObjs3a = vToTopDecl 1 (V_Raw "") : tail domObjs1    -- subtle syntax error.
                                            
pdf3b = mk_PDF_DOM (hdr1 "1.4") "CAVITY\n" 3 domObjs3b
domObjs3b = vToTopDecl 1 (V_Raw ">>") : tail domObjs1  -- blatant syntax error.

---- pdf2: one object stream -------------------------------------------------

pdf2 :: PDF_DOM
pdf2 = mk_PDF_DOM (hdr1 "1.5") "CAVITY\n" 3 domObjs2
    
domObjs2 =
  [ -- dead objects (potentially a reference to in edits)
    vToTopDecl 1 (V_Int 5)
  , vToTopDecl 2 (V_Int 28)
  
  , ( (7,0)   -- NOTE: 7 the stream object ID
    , ObjStrm
        [ vToCompDecl 3 $ V_Dict [("Type" , V_Name "Catalog")
                                 ,("Pages", V_Indirect (4,0))
                                 ]
        , vToCompDecl 4 $ V_Dict [("Type", V_Name "Pages")
                                 ,("MediaBox", V_Raw "[0 0 842 595]")
                                 ,("Kids", V_Array [V_Indirect (5,0)])
                                 ,("Count", V_Int 1)
                                 ]
        , vToCompDecl 5 $ V_Dict [("Type", V_Name "Page")
                                 ,("Parent", V_Indirect (4,0))
                                 ,("Resources", V_Indirect (9,0))
                                 ,("Contents", V_Indirect (10,0))
                                 ]
        -- dead objects (potentially a reference to after we edit): 
        , vToCompDecl 6  $ V_Int 5
        , vToCompDecl 11 $ V_Int 28
        ]
    )
    -- 7 used above
  , vToTopDecl 8 $ courierF1
  , vToTopDecl 9 $ resources
  , ((10,0),  StrmObj (StreamObj []
                                 NoFilter
                                 (B8.pack helloWorldContentStream)))
  ]

---- pdf4: two object streams ------------------------------------------------

pdf4 :: PDF_DOM
pdf4 = mk_PDF_DOM (hdr1 "1.7") "CAVITY\n" 3 domObjs4
    
domObjs4 =
  [ -- dead objects (potentially a reference to in edits)
    vToTopDecl 1 (V_Int 10001)
  , vToTopDecl 2 (V_Int 10002)
  
  , ( (7,0)   -- NOTE: 7 the stream object ID
    , ObjStrm
        [ vToCompDecl 3 $ V_Dict [("Type" , V_Name "Catalog")
                                 ,("Pages", V_Indirect (4,0))
                                 ]
        , vToCompDecl 4 $ V_Dict [("Type", V_Name "Pages")
                                 ,("MediaBox", V_Raw "[0 0 842 595]")
                                 ,("Kids", V_Array [V_Indirect (5,0)])
                                 ,("Count", V_Int 1)
                                 ]
        -- dead objects (potentially a reference to after we edit): 
        , vToCompDecl 6  $ V_Int 10006
        , vToCompDecl 11 $ V_Int 10011
        ]
    )
    -- 7 used above
  , vToTopDecl 8 $ courierF1
  , vToTopDecl 9 $ resources
  , ((10,0),  StrmObj (StreamObj []
                                 NoFilter
                                 (B8.pack helloWorldContentStream)))
  , ( (12,0)   -- NOTE: 12 the stream object ID
    , ObjStrm
        [ vToCompDecl 5 $ V_Dict [("Type", V_Name "Page")
                                 ,("Parent", V_Indirect (4,0))
                                 ,("Resources", V_Indirect (9,0))
                                 ,("Contents", V_Indirect (10,0))
                                 ]
        -- dead objects (potentially a reference to after we edit): 
        , vToCompDecl 13 $ V_Int 10013
        , vToCompDecl 14 $ V_Int 10014
        ]
    )
  ]

---- library -----------------------------------------------------------------

line2WhenBinary = BS.pack [0x25,0xE2,0xE3,0xCF,0xD3]  -- only necessary if file contains binary data

hdr1 s = BS.intercalate "\n"
           [ "%PDF-" <> s
           , line2WhenBinary
           , ""
           ]

resources = V_Raw "<</Font<</F1 8 0 R>> >>"
courierF1 = V_Raw "<</Type /Font  /Subtype /Type1 /Name/F1 /BaseFont /Courier >>"
helloWorldContentStream =
  unlines [ "BT"
          , "  /F1 24.0 Tf"
          , "  1 0 0 1 50 555 Tm"
          , "  12 TL"
          , "  T*(HELLO WORLD)Tj"
          , "ET"
          ]
