
def Main =
  block
    Many $[! $recordTerminator]; $recordTerminator     -- Skip header
    $$ = Many Field
    END


def $fieldSeparator   = '\t'
def $recordTerminator = '\n'

def Field =
  block
    key               = FreeText;             $fieldSeparator
    type              = FieldType;            $fieldSeparator
    sinceVersion      = Version;              $fieldSeparator
    deprecatedIn      = Optional Version;     $fieldSeparator
    required          = IsRequired;           $fieldSeparator
    indirectReference = Alts IsIndirect;      $fieldSeparator
    inheritable       = IsInheritable;        $fieldSeparator
    defaultValue      = Alts DefaultValue;    $fieldSeparator
    possibleValues    = FreeText;             $fieldSeparator
    specialCase       = FreeText;             $fieldSeparator
    link              = FreeText;             $fieldSeparator
    note              = FreeText;             $recordTerminator

def FreeText  = Many $[! ($fieldSeparator | $recordTerminator)]

def FieldType = SepBy (KW ";") FieldTypeExpression

def Alts P =
  First
    SepBy (KW ";") { KW "["; $$ = P; KW "]" }   -- must be first
    [ P ]

def FieldTypeExpression =
  First
    TType       = PrimitiveType
    TSince      = FnSinceVersion PrimitiveType
    TDeprecated = FnDeprecated PrimitiveType

def Version =
  block
    major   = Integer
    $['.']
    minor   = Integer

def PrimitiveType =
  First
    TArray       = @Match "array"
    TBitmask     = @Match "bitmask"
    TBoolean     = @Match "boolean"
    TDate        = @Match "date"
    TDictionary  = @Match "dictionary"
    TInteger     = @Match "integer"
    TMap         = @Match "matrix"
    TNameTree    = @Match "name-tree"
    TName        = @Match "name"
    TNull        = @Match "null"
    TNumberTree  = @Match "number-tree"
    TNumber      = @Match "number"
    TRectangle   = @Match "rectangle"
    TStream      = @Match "stream"
    TStringASCII = @Match "string-ascii"
    TStringByte  = @Match "string-byte"
    TStringText  = @Match "string-text"
    TString      = @Match "string"


def IsRequired : BoolExpr =
  First
    BoolExpr
    Fun1 "IsRequired" BoolExpr

{- This encode 3 possible values:
    * direct-only
    * indirect-only
    * either -}
def IsIndirect =
  First
    IndirectIf = BoolExpr
    DirectIf   = {| TRUE = Fun0 "MustBeDirect" |}
    DirectIf   = Fun1 "MustBeDirect" BoolExpr
    IndirectIf = Fun1 "MustBeIndirect" BoolExpr
    IndirectIf = {| TRUE = KW "IndirectReference" |}

def IsInheritable : BoolExpr =
  First
    {| TRUE = KW "Inheritable" |}
    BoolExpr

def DefaultValue =
  First
    Conditional = ConditionalDefaultCases
    Default     = Term
    NoDefault   = Accept

def ConditionalDefaultCases =
  First
    Fun1 "Eval" (SepBy (KW "||") ConditionalDefault)
    [ ConditionalDefault ]

def ConditionalDefault =
  First

    block
      let f = Fun2 "IsPresent" FieldName Term
      condition = {| IsPresent = f.arg1 |} : BoolExpr
      value     = f.arg2

    block
      let f = Fun2 "DefaultValue" BoolExpr Term
      condition = f.arg1
      value     = f.arg2

--------------------------------------------------------------------------------
-- Boolean Expressions

def BoolExpr =
  block
    First
      {| OR  = BoolInfixExpr "||" |}
      {| AND = BoolInfixExpr "&&" |}
      BoolAtomExpr

def BoolInfixExpr kw =
  block
    let first  = emit builder BoolAtomExpr
    let second = emit first (BoolInfixAtomExpr kw)
    let rest   = many (s = second) (emit s (BoolInfixAtomExpr kw))
    build rest

def BoolInfixAtomExpr kw =
  block
    KW kw
    BoolAtomExpr

def BoolAtomExpr : BoolExpr =
  First

    block
      KW "("
      $$ = BoolExpr
      KW ")"


    {| TRUE  = KW "TRUE"  |}
    {| FALSE = KW "FALSE" |}

    {| FontHasLatinChars = Fun0 "FontHasLatinChars" |}
    {| NotStandard14Font = Fun0 "NotStandard14Font" |}

    {| IsAssociatedFile   = Fun0 "IsAssociatedFile" |}
    {| IsPDFTagged        = Fun0 "IsPDFTagged" |}
    {| IsEncryptedWrapper = Fun0 "IsEncryptedWrapper" |}
    {| PageContainsStructContentItems = Fun0 "PageContainsStructContentItems" |}
    {| ImageIsStructContentItem = Fun0 "ImageIsStructContentItem" |}

    {| NotPresent        = Fun1 "NotPresent" FieldName |}
    {| IsPresent         = Fun1 "IsPresent" FieldName |}
    {| InMap             = Fun1 "InMap" FieldName |}

    {| SinceVersion      = Fun1 "SinceVersion" Version |}
    {| BeforeVersion     = Fun1 "BeforeVersion" Version |}
    {| IsPDFVersion      = Fun1 "IsPDFVersion" Version |}

    -- This is actually a guarded term, and not a predicate,
    -- See rules for default values
    {| ConditionalDefault =
          block
            let f = Fun2 "DefaultValue" BoolExpr Term
            condition = f.arg1
            value     = f.arg2
          : ConditionalDefault
          |}

    {| EQ = BinPred "==" |}
    {| EQ = BinPred "!=" |}
    {| LT = BinPred "<"  |}
    {| GT = BinPred ">"  |}



def BinPred op =
  block
    lhs = Term
    KW op
    rhs = Term


def SimpleFieldName =
  First
    Text = Token (Many (1 .. ) $['A' .. 'Z', 'a' .. 'z'])
    Wild = $['*']

def FieldName = SepBy (Match "::") SimpleFieldName

def NameValue = Token (Many (1 .. ) $['A' .. 'Z', 'a' .. 'z', '0' .. '9', '.', '_'])

def ValueOf =
  block
    parent = length (Many (Match "parent::"))
    $['@']
    field  = FieldName

def RepeatArray =
  block
    KW "["
    v1 = Term
    v2 = Term
    v1 == Term is true
    v2 == Term is true
    KW "..."
    KW "]"



def Term =
  First
    {| ValueOf    = ValueOf |}

    {| Float      = Token FloatValue |}

    {| Integer    = Token Integer |}    -- Must be aftre Float

    {| Bool       = {| TRUE   = KW "true" |} |}
    {| Bool       = {| FALSE  = KW "false" |} |}
    {| Bool       = Fun1 "Eval" BoolExpr |}

    {| Null       = KW "null" |}

    {| Array      = block
                      KW "["
                      $$ = Many Term
                      KW "]"
                  |}

    {| Array      = block
                      KW "["
                      $$ = SepBy (KW ",") Term
                      KW "]"
                  |}

    {| RepeatArray = RepeatArray |}


    {| String     = block
                      $['\'']
                      $$ = Many $[!'\'']
                      Token $['\'']
                  |}

    {| RectWidth  = Fun1 "RectWidth"  Term |}

    {| RectHeight = Fun1 "RectHeight" Term |}

    {| ImplementationDependent = Fun0 "ImplementationDependent" |}

    {| Name       = NameValue |}    -- Needs to be after the functions and Integer


def Token P =
  block
    $$ = P
    Many $[' ']

def KW x = Token (@Match x)

--------------------------------------------------------------------------------
-- Functions


def Fun0 f =
  block
    Match "fn:"
    Match f
    KW "("
    KW ")"
    Accept

def Fun1 f Arg =
  block
    Match "fn:"
    Match f
    KW "("
    $$ = Arg
    KW ")"

def Fun2 f Arg1 Arg2 =
  block
    Match "fn:"
    Match f
    KW "("
    arg1 = Arg1
    KW ","
    arg2 = Arg2
    KW ")"

def FnSinceVersion Arg =
  block
    let f = Fun2 "SinceVersion" Version Arg
    since = f.arg1
    value = f.arg2

def FnDeprecated Arg =
  block
    let f = Fun2 "Deprecated" Version Arg
    deprecatedIn = f.arg1
    value        = f.arg2


--------------------------------------------------------------------------------

def Digit   = $['0' .. '9'] - '0' as int
def Natural = many (s = Digit) (10 * s + Digit)
def Integer =
  First
    { $['-']; - Natural }
    Natural

-- Leave as text for now
def FloatValue =
  block
    whole = Many (1..) Digit
    $['.']
    frac  = Many (1..) Digit


-- One or more Q, separated by P
def SepBy Sep Thing =
  build (many (s = emit builder Thing) { Sep; emit s Thing })



