module Types where

data Mod = Mod
  { mName :: Int
  , mSpecs :: [Spec]    -- mutually recursive so can't split
  , mImports :: [Int]
  }

data Spec = Spec
  { sName   :: String
  , sShapes :: [StructType]
  , sFields :: [Field]
  } deriving Show

data Field = Field
  { fName :: FieldPat
  , fType :: Type
  , fSince :: Float
  , fDeprecated :: Maybe Float
  , fRequired :: Bool
  , fIndirect :: Bool
  , fDefaultValue :: Maybe Expr
  , fValNeeded :: Bool
  } deriving Show

data FieldPat =
    PFieldName String
  | PArrayIx Integer
  | PArrayAny
    deriving (Show, Eq, Ord)


-- These can have named specifications
data StructType =
    TStream
  | TArray
  | TDictionary
  | TNumberTree
  | TNameTree
    deriving (Show,Eq,Ord)

-- These can produce a value that can be examined in other fields
-- or refined with constraints
data PrimType =
    TInteger
  | TNumber
  | TName
  | TBool
  | TStringText
  | TStringByte
  | TStringAscii
  | TString
  | TRectangle
  | TDate
  | TNull
    deriving Show

data Type =
    TOr Type Type
  | TPrim   PrimType (Maybe Constraint)
  | TStruct StructType (Maybe String)
  deriving Show

data Constraint = Equals Expr
                | Interval Double (Maybe Double)
                | IsLessThan Expr
                | IsGreaterThan Expr
                | Orc Constraint Constraint
                  deriving Show

data FieldIx = ArrayIx Integer | FieldIx String
  deriving (Show,Eq,Ord)

data Expr = ELitI Integer
          | ELit Double
          | ELitR Rational
          | EBool Bool
          | ELitStr String
          | ELitName String
          | EArr [Expr]
          | ValueOf FieldIx
            deriving Show

