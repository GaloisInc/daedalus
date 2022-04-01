import GlyphList1

def glyph (name : [uint 8])
          (std : uint 8) (mac : uint 8) (win : uint 8) (pdf : uint 8) =
  block
    glyph = name
    std   = std
    mac   = mac
    win   = win
    pdf   = pdf

def stdEncoding =
  for (enc = empty; x in latin)
    if x.std == undef  then enc else
    case lookup x.glyph glyphToUni of
      just u -> insert x.std u enc
      nothing -> enc

def winEncoding =
  for (enc = empty; x in latin)
    if x.win == undef  then enc else
    case lookup x.glyph glyphToUni of
      just u -> insert x.win u enc
      nothing -> enc

def macEncoding =
  for (enc = empty; x in latin)
    if x.mac == undef then enc else
    case lookup x.glyph glyphToUni of
      just u -> insert x.mac u enc
      nothing -> enc

def pdfEncoding =
  for (enc = empty; x in latin)
    if x.pdf == undef  then enc else
    case lookup x.glyph glyphToUni of
      just u -> insert x.pdf u enc
      nothing -> enc

def glyphToUni =
  for (enc = empty; x in glyphEncs)
    insert x.key x.value enc


def undef  : uint 8  = 0o000
def uundef : uint 16 = 0x0000



-- These are chunked up like this because at the moment
-- it takes too long to compile the kind of code we generate with
-- functions that have many function calls with lots of values persisting
-- accoress them.
def latin = concat [latin1,latin2
                   ,latin3,latin4
                   ,latin5,latin6
                   ,latin7,latin8
                   ,latin9,latin10
                   ]

-- Table D2
def latin1 =

  -- Page 847
  [ glyph "A"             0o101 0o101 0o101 0o101
  , glyph "AE"            0o341 0o256 0o306 0o306
  , glyph "Aacute"        undef 0o347 0o301 0o301
  , glyph "Acircumflex"   undef 0o345 0o302 0o302
  , glyph "Adieresis"     undef 0o200 0o304 0o304
  , glyph "Agrave"        undef 0o313 0o300 0o300
  , glyph "Aring"         undef 0o201 0o305 0o305
  , glyph "Atilde"        undef 0o314 0o303 0o303
  , glyph "B"             0o102 0o102 0o102 0o102
  , glyph "C"             0o103 0o103 0o103 0o103
  , glyph "Ccedilla"      undef 0o202 0o307 0o307
  , glyph "D"             0o104 0o104 0o104 0o104
  , glyph "E"             0o105 0o105 0o105 0o105
  , glyph "Eacute"        undef 0o203 0o311 0o311
  ]

def latin2 =

  [ glyph "OE"            0o352 0o316 0o214 0o226
  , glyph "Oacute"        undef 0o356 0o323 0o323
  , glyph "Ocircumflex"   undef 0o357 0o324 0o324
  , glyph "Odieresis"     undef 0o205 0o326 0o326
  , glyph "Ograve"        undef 0o361 0o322 0o322
  , glyph "Oslash"        0o351 0o257 0o330 0o330
  , glyph "Otilde"        undef 0o315 0o325 0o325
  , glyph "P"             0o120 0o120 0o120 0o120
  , glyph "Q"             0o121 0o121 0o121 0o121
  , glyph "R"             0o122 0o122 0o122 0o122
  , glyph "S"             0o123 0o123 0o123 0o123
  , glyph "Scaron"        undef undef 0o212 0o227
  , glyph "T"             0o124 0o124 0o124 0o124
  , glyph "Thorn"         undef undef 0o336 0o336
  ]

def latin3 =

  -- Page 848
  [ glyph "Ecircumflex"   undef 0o346 0o312 0o312
  , glyph "Edieresis"     undef 0o350 0o313 0o313
  , glyph "Egrave"        undef 0o351 0o310 0o310
  , glyph "Eth"           undef undef 0o320 0o320
  , glyph "Euro"          undef undef 0o200 0o240
  , glyph "F"             0o106 0o106 0o106 0o106
  , glyph "G"             0o107 0o107 0o107 0o107
  , glyph "H"             0o110 0o110 0o110 0o110
  , glyph "I"             0o111 0o111 0o111 0o111
  , glyph "Iacute"        undef 0o352 0o315 0o315
  , glyph "Icircumflex"   undef 0o353 0o316 0o316
  , glyph "Idieresis"     undef 0o354 0o317 0o317
  , glyph "Igrave"        undef 0o355 0o314 0o314
  , glyph "J"             0o112 0o112 0o112 0o112
  , glyph "K"             0o113 0o113 0o113 0o113
  , glyph "L"             0o114 0o114 0o114 0o114
  , glyph "Lslash"        0o350 undef undef 0o225
  , glyph "M"             0o115 0o115 0o115 0o115
  , glyph "N"             0o116 0o116 0o116 0o116
  , glyph "Ntilde"        undef 0o204 0o321 0o321
  , glyph "O"             0o117 0o117 0o117 0o117
  , glyph "aring"         undef 0o214 0o345 0o345
  , glyph "asciicircum"   0o136 0o136 0o136 0o136
  , glyph "asciitilde"    0o176 0o176 0o176 0o176
  , glyph "asterisk"      0o052 0o052 0o052 0o052
  , glyph "at"            0o100 0o100 0o100 0o100
  , glyph "atilde"        undef 0o213 0o343 0o343
  ]

def latin4 =

  [ glyph "U"             0o125 0o125 0o125 0o125
  , glyph "Uacute"        undef 0o362 0o332 0o332
  , glyph "Ucircumflex"   undef 0o363 0o333 0o333
  , glyph "Udieresis"     undef 0o206 0o334 0o334
  , glyph "Ugrave"        undef 0o364 0o331 0o331
  , glyph "V"             0o126 0o126 0o126 0o126
  , glyph "W"             0o127 0o127 0o127 0o127
  , glyph "X"             0o130 0o130 0o130 0o130
  , glyph "Y"             0o131 0o131 0o131 0o131
  , glyph "Yacute"        undef undef 0o335 0o335
  , glyph "Ydieresis"     undef 0o331 0o237 0o230
  , glyph "Z"             0o132 0o132 0o132 0o132
  , glyph "Zcaron"        undef undef 0o216 0o231
  , glyph "a"             0o141 0o141 0o141 0o141
  , glyph "aacute"        undef 0o207 0o341 0o341
  , glyph "acircumflex"   undef 0o211 0o342 0o342
  , glyph "acute"         0o302 0o253 0o264 0o264
  , glyph "adieresis"     undef 0o212 0o344 0o344
  , glyph "ae"            0o361 0o276 0o346 0o346
  , glyph "agrave"        undef 0o210 0o340 0o340
  , glyph "ampersand"     0o046 0o046 0o046 0o046
  , glyph "eth"           undef undef 0o360 0o360
  , glyph "exclam"        0o041 0o041 0o041 0o041
  , glyph "exclamdown"    0o241 0o301 0o241 0o241
  , glyph "f"             0o146 0o146 0o146 0o146
  , glyph "fi"            0o256 0o336 undef 0o223
  , glyph "five"          0o065 0o065 0o065 0o065
  ]

def latin5 =

  -- Page 849
  [ glyph "b"             0o142 0o142 0o142 0o142
  , glyph "backslash"     0o134 0o134 0o134 0o134
  , glyph "bar"           0o174 0o174 0o174 0o174
  , glyph "braceleft"     0o173 0o173 0o173 0o173
  , glyph "braceright"    0o175 0o175 0o175 0o175
  , glyph "bracketleft"   0o133 0o133 0o133 0o133
  , glyph "bracketright"  0o135 0o135 0o135 0o135
  , glyph "breve"         0o306 0o371 undef 0o030
  , glyph "brokenbar"     undef undef 0o246 0o246
  , glyph "bullet"        0o267 0o245 0o225 0o200
  , glyph "c"             0o143 0o143 0o143 0o143
  , glyph "caron"         0o317 0o377 undef 0o031
  , glyph "ccedilla"      undef 0o215 0o347 0o347
  , glyph "cedilla"       0o313 0o374 0o270 0o270
  , glyph "cent"          0o242 0o242 0o242 0o242
  , glyph "circumflex"    0o303 0o366 0o210 0o032
  , glyph "colon"         0o072 0o072 0o072 0o072
  , glyph "comma"         0o054 0o054 0o054 0o054
  , glyph "copyright"     undef 0o251 0o251 0o251
  , glyph "currency"      0o250 0o333 0o244 0o244
  , glyph "dagger"        0o262 0o240 0o206 0o201
  , glyph "daggerdbl"     0o263 0o340 0o207 0o202
  , glyph "degree"        undef 0o241 0o260 0o260
  , glyph "dieresis"      0o310 0o254 0o250 0o250
  , glyph "divide"        undef 0o326 0o367 0o367
  , glyph "dollar"        0o044 0o044 0o044 0o044
  ]

def latin6 =
  [ glyph "fl"            0o257 0o337 undef 0o224
  , glyph "florin"        0o246 0o304 0o203 0o206
  , glyph "four"          0o064 0o064 0o064 0o064
  , glyph "fraction"      0o244 0o332 undef 0o207
  , glyph "g"             0o147 0o147 0o147 0o147
  , glyph "germandbls"    0o373 0o247 0o337 0o337
  , glyph "grave"         0o301 0o140 0o140 0o140
  , glyph "greater"       0o076 0o076 0o076 0o076
  , glyph "guillemotleft" 0o253 0o307 0o253 0o253
  , glyph "guillemotright"0o273 0o310 0o273 0o273
  , glyph "guilsinglleft" 0o254 0o334 0o213 0o210
  , glyph "guilsinglright"0o255 0o335 0o233 0o211
  , glyph "h"             0o150 0o150 0o150 0o150
  , glyph "hungarumlaut"  0o315 0o375 undef 0o034
  , glyph "hyphen"        0o055 0o055 0o055 0o055
  , glyph "i"             0o151 0o151 0o151 0o151
  , glyph "iacute"        undef 0o222 0o355 0o355
  , glyph "icircumflex"   undef 0o224 0o356 0o356
  , glyph "idieresis"     undef 0o225 0o357 0o357
  , glyph "igrave"        undef 0o223 0o354 0o354
  , glyph "j"             0o152 0o152 0o152 0o152
  , glyph "k"             0o153 0o153 0o153 0o153
  , glyph "l"             0o154 0o154 0o154 0o154
  , glyph "less"          0o074 0o074 0o074 0o074
  , glyph "logicalnot"    undef 0o302 0o254 0o254
  , glyph "lslash"        0o370 undef undef 0o233
  , glyph "m"             0o155 0o155 0o155 0o155
  ]

  -- Page 850
def latin7 =
  [ glyph "dotaccent"     0o307 0o372 undef 0o033
  , glyph "dotlessi"      0o365 0o365 undef 0o232
  , glyph "e"             0o145 0o145 0o145 0o145
  , glyph "eacute"        undef 0o216 0o351 0o351
  , glyph "ecircumflex"   undef 0o220 0o352 0o352
  , glyph "edieresis"     undef 0o221 0o353 0o353
  , glyph "egrave"        undef 0o217 0o350 0o350
  , glyph "eight"         0o070 0o070 0o070 0o070
  , glyph "ellipsis"      0o274 0o311 0o205 0o203
  , glyph "emdash"        0o320 0o321 0o227 0o204
  , glyph "endash"        0o261 0o320 0o226 0o205
  , glyph "equal"         0o075 0o075 0o075 0o075
  , glyph "oe"            0o372 0o317 0o234 0o234
  , glyph "ogonek"        0o316 0o376 undef 0o035
  , glyph "ograve"        undef 0o230 0o362 0o362
  , glyph "one"           0o061 0o061 0o061 0o061
  , glyph "onehalf"       undef undef 0o275 0o275
  , glyph "onequarter"    undef undef 0o274 0o274
  , glyph "onesuperior"   undef undef 0o271 0o271
  , glyph "ordfeminine"   0o343 0o273 0o252 0o252
  , glyph "ordmasculine"  0o353 0o274 0o272 0o272
  , glyph "oslash"        0o371 0o277 0o370 0o370
  , glyph "otilde"        undef 0o233 0o365 0o365
  , glyph "p"             0o160 0o160 0o160 0o160
  , glyph "paragraph"     0o266 0o246 0o266 0o266
  , glyph "parenleft"     0o050 0o050 0o050 0o050
  , glyph "parenright"    0o051 0o051 0o051 0o051
  ]

def latin8 =
  [ glyph "macron"        0o305 0o370 0o257 0o257
  , glyph "minus"         undef undef undef 0o212
  , glyph "mu"            undef 0o265 0o265 0o265
  , glyph "multiply"      undef undef 0o327 0o327
  , glyph "n"             0o156 0o156 0o156 0o156
  , glyph "nine"          0o071 0o071 0o071 0o071
  , glyph "ntilde"        undef 0o226 0o361 0o361
  , glyph "numbersign"    0o043 0o043 0o043 0o043
  , glyph "o"             0o157 0o157 0o157 0o157
  , glyph "oacute"        undef 0o227 0o363 0o363
  , glyph "ocircumflex"   undef 0o231 0o364 0o364
  , glyph "odieresis"     undef 0o232 0o366 0o366
  , glyph "s"             0o163 0o163 0o163 0o163
  , glyph "scaron"        undef undef 0o232 0o235
  , glyph "section"       0o247 0o244 0o247 0o247
  , glyph "semicolon"     0o073 0o073 0o073 0o073
  , glyph "seven"         0o067 0o067 0o067 0o067
  , glyph "six"           0o066 0o066 0o066 0o066
  , glyph "slash"         0o057 0o057 0o057 0o057
  , glyph "space"         0o040 0o040 0o040 0o040
  , glyph "sterling"      0o243 0o243 0o243 0o243
  , glyph "t"             0o164 0o164 0o164 0o164
  , glyph "thorn"         undef undef 0o376 0o376
  , glyph "three"         0o063 0o063 0o063 0o063
  , glyph "threequarters" undef undef 0o276 0o276
  , glyph "threesuperior" undef undef 0o263 0o263
  , glyph "tilde"         0o304 0o367 0o230 0o037
  ]


-- Page 851
def latin9 =
  [ glyph "percent"       0o045 0o045 0o045 0o045
  , glyph "period"        0o056 0o056 0o056 0o056
  , glyph "periodcentered"0o264 0o341 0o267 0o267
  , glyph "perthousand"   0o275 0o344 0o211 0o213
  , glyph "plus"          0o053 0o053 0o053 0o053
  , glyph "plusminus"     undef 0o261 0o261 0o261
  , glyph "q"             0o161 0o161 0o161 0o161
  , glyph "question"      0o077 0o077 0o077 0o077
  , glyph "questiondown"  0o277 0o300 0o277 0o277
  , glyph "quotedbl"      0o042 0o042 0o042 0o042
  , glyph "quotedblbase"  0o271 0o343 0o204 0o214
  , glyph "quotedblleft"  0o252 0o322 0o223 0o215
  , glyph "quotedblright" 0o272 0o323 0o224 0o216
  , glyph "quoteleft"     0o140 0o324 0o221 0o217
  , glyph "quoteright"    0o047 0o325 0o222 0o220
  , glyph "quotesinglbase"0o270 0o342 0o202 0o221
  , glyph "quotesingle"   0o251 0o047 0o047 0o047
  , glyph "r"             0o162 0o162 0o162 0o162
  , glyph "registered"    undef 0o250 0o256 0o256
  , glyph "ring"          0o312 0o373 undef 0o036
  ]

def latin10 =
  [ glyph "trademark"     undef 0o252 0o231 0o222
  , glyph "two"           0o062 0o062 0o062 0o062
  , glyph "twosuperior"   undef undef 0o262 0o262
  , glyph "u"             0o165 0o165 0o165 0o165
  , glyph "uacute"        undef 0o234 0o372 0o372
  , glyph "ucircumflex"   undef 0o236 0o373 0o373
  , glyph "udieresis"     undef 0o237 0o374 0o374
  , glyph "ugrave"        undef 0o235 0o371 0o371
  , glyph "underscore"    0o137 0o137 0o137 0o137
  , glyph "v"             0o166 0o166 0o166 0o166
  , glyph "w"             0o167 0o167 0o167 0o167
  , glyph "x"             0o170 0o170 0o170 0o170
  , glyph "y"             0o171 0o171 0o171 0o171
  , glyph "yacute"        undef undef 0o375 0o375
  , glyph "ydieresis"     undef 0o330 0o377 0o377
  , glyph "yen"           0o245 0o264 0o245 0o245
  , glyph "z"             0o172 0o172 0o172 0o172
  , glyph "zcaron"        undef undef 0o236 0o236
  , glyph "zero"          0o060 0o060 0o060 0o060
  ]



  -- Page 851





