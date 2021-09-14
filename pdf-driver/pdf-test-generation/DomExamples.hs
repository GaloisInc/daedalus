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
  , vToTopDecl 7 $ (V_Int 7)
  , vToTopDecl 8 $ courierF1
  , vToTopDecl 9 $ resources

  -- more dead objects (want some 2 digit objects):
  , vToTopDecl 10 (V_Int 1000) 
  , vToTopDecl 11 (V_Raw "999 % dead object (01234567890123456789)")
  ]

pdf2 :: PDF_DOM
pdf2 = mk_PDF_DOM (hdr1 "1.5") "CAVITY\n" 3 domObjs2
    
domObjs2 =
  [ ( (6,0)
    , ObjStrm
        [ vToCompDecl 3 $ V_Dict [("Type" , V_Name "Catalog")
                                 ,("Pages", V_Indirect (4,0))
                                 ]
        , vToCompDecl 4 $ V_Dict [("Type", V_Name "Pages")
                                 ,("Kids", V_Array [V_Indirect (5,0)])
                                 ,("Count", V_Int 1)
                                 ]
        , vToCompDecl 9 $ V_Dict [("Type", V_Name "Page")
                                 ,("Parent", V_Indirect (4,0))
                                 ,("Resources", V_Dict [])
                                 ]
        ]
    )
    -- dead objects:
  , vToTopDecl 1 (V_Int 5)
  , vToTopDecl 2 (V_Int 6)
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
