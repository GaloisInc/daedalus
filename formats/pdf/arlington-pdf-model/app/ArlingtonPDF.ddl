
def Main =
  block
    Many $[! $recordTerminator]; $recordTerminator     -- Skip header
    $$ = Many Field
    END


def $alpha            = 'a' .. 'z' | 'A' .. 'Z'
def $digit            = '0' .. '9'
def $fieldSeparator   = '\t'
def $recordTerminator = '\n'

def Field =
  block
    key               = FreeText;                 $fieldSeparator
    type              = FieldType;                $fieldSeparator
    sinceVersion      = Version;                  $fieldSeparator
    deprecatedIn      = Optional Version;         $fieldSeparator
    required          = IsRequired;               $fieldSeparator
    indirectReference = Alts IsIndirect;          $fieldSeparator
    inheritable       = IsInheritable;            $fieldSeparator
    defaultValue      = Alts DefaultValue;        $fieldSeparator
    possibleValues    = MultiAlts PossibleValue;  $fieldSeparator
    specialCase       = MultiAlts Check;          $fieldSeparator
    link              = MultiAlts Link;           $fieldSeparator
    note              = FreeText;                 $recordTerminator

def FreeText  = Many $[! ($fieldSeparator | $recordTerminator)]

def FieldType = SepBy (KW ";") FieldTypeExpression

def Bracketed P =
  block
    KW "["
    $$ = P
    KW "]"

def Alts P =
  First
    SepBy (KW ";") (Bracketed (Optional P)) -- must be first
    [ Optional P ]

def MultiAlts P =
  Optional (SepBy (KW ";") (Bracketed (Optional (SepBy (KW ",") P))))

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

def Test = Alts DefaultValue

def DefaultValue =
  First
    Conditional = ConditionalDefaultCases
    Default     = Term

def ConditionalDefaultCases =
  First
    FnEval (SepBy (KW "||") ConditionalDefault)
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


def PossibleValue =
  First
    Deprecated    = FnDeprecated PossibleValue
    SinceVersion  = FnSinceVersion Term
    Conditional   = ConditionalValue
    Value         = Term
    Wild          = KW "*"

def ConditionalValue =
  block
    let f = Fun2 "RequiredValue" BoolExpr Term
    condition = f.arg1
    value     = f.arg2

def Link =
  First
    Deprecated    = FnDeprecated Link
    SinceVersion  = FnSinceVersion TypeName
    InVersion     = FnIsPDFVersion TypeName
    Link          = TypeName

def TypeName  = Many (1..) $[ $alpha, $digit, '_' ]

def Check =
  First
    Check      = BoolExpr
    Ignore     = Fun1 "Ignore" BoolExpr --??
    IgnoreF    = Fun1 "Ignore" FieldName --??
    Meaningful = Fun1 "IsMeaningful" BoolExpr

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
    {| NOT   = Fun1 "Not" BoolExpr |}

    FnEval BoolExpr

    {| FontHasLatinChars = Fun0 "FontHasLatinChars" |}
    {| NotStandard14Font = Fun0 "NotStandard14Font" |}
    {| KeyNameIsColorant = Fun0 "KeyNameIsColorant" |}

    {| IsAssociatedFile   = Fun0 "IsAssociatedFile" |}
    {| IsPDFTagged        = Fun0 "IsPDFTagged" |}
    {| IsEncryptedWrapper = Fun0 "IsEncryptedWrapper" |}
    {| PageContainsStructContentItems = Fun0 "PageContainsStructContentItems" |}
    {| ImageIsStructContentItem = Fun0 "ImageIsStructContentItem" |}

    {| IsPresent         = Fun1 "IsPresent" FieldName |}
    {| InMap             = Fun1 "InMap" FieldName |}
    {| Contains          = Fun2 "Contains" Term Term |}

    {| SinceVersion      = FnSinceVersion BoolExpr |}
    {| BeforeVersion     = FnBeforeVersion BoolExpr |}
    {| IsAtLeastVersion  = Fun1 "SinceVersion" Version |}
    {| IsBeforeVersion   = Fun1 "BeforeVersion" Version |}
    {| IsPDFVersion      = Fun1 "IsPDFVersion" Version |}

    {| BitClear          = Fun1 "BitClear" Term |}
    {| BitsClear         = Fun2 "BitsClear" Term Term |}
    {| BitSet            = Fun1 "BitSet" Term |}


    -- This is actually a guarded term, and not a predicate,
    -- See rules for default values
    {| ConditionalDefault =
          block
            let f = Fun2 "DefaultValue" BoolExpr Term
            condition = f.arg1
            value     = f.arg2
          : ConditionalDefault
          |}

    {| EQ   = BinPred "==" |}
    {| NEQ  = BinPred "!=" |}
    {| LT   = BinPred "<"  |}
    {| GT   = BinPred ">"  |}
    {| LEQ  = BinPred "<=" |}
    {| GEQ  = BinPred ">=" |}



def BinPred op =
  block
    lhs = Term
    KW op
    rhs = Term


def SimpleFieldName =
  First
    Number = Natural
    Text   = NameValue
    Wild   = $['*']

def FieldName = SepBy (Match "::") SimpleFieldName

def NameValue = Token (Many (1 .. ) $[ $alpha, $digit, '.', '_', '-'])

def ValueOf =
  block
    qualifier = Optional { $$ = FieldName; Match "::" }
    $['@']
    field  = SimpleFieldName

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
    {| add = BinOp "+" |}
    {| sub = BinOp "- " |}
        -- the space is to avoid conflict with names like a-b
    {| mul = BinOp "*" |}  -- XXX: probably ambiguous with wild cards
    {| mod = BinOp "mod" |}
    TermAtom

def BinOp op =
  block
    lhs = TermAtom
    KW op
    rhs = TermAtom

def TermAtom : Term =
  First
    block
      KW "("
      $$ = Term
      KW ")"

    {| ValueOf    = ValueOf |}

    {| Float      = Token FloatValue |}

    {| Integer    = Token Integer |}    -- Must be aftre Float

    {| Bool       = { KW "true";  true } |}
    {| Bool       = { KW "false"; false } |}

    {| Null       = KW "null" |}

    {| Array      = block
                      KW "["
                      $$ = Many Term
                      KW "]"
                  |}

    -- XXX
    {| Array      = block
                      KW "["
                      $$ = SepBy (KW ",") Term
                      KW "]"
                  |}

    {| RepeatArray = RepeatArray |}


    {| String     = block
                      $['\'']
                      $$ = Many $[!'\'']
                      KW "'"
                  |}

    {| RectWidth    = Fun1 "RectWidth"  Term |}
    {| RectHeight   = Fun1 "RectHeight" Term |}
    {| FileSize     = Fun0 "FileSize" |}
    {| ArrayLength  = Fun1 "ArrayLength" Term |}
    {| StringLength = Fun1 "StringLength" Term |}

    {| ImplementationDependent = Fun0 "ImplementationDependent" |}

    {| Name = NameValue |} -- Needs to be after the functions and Integer

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

def FnBeforeVersion Arg =
  block
    let f = Fun2 "BeforeVersion" Version Arg
    before = f.arg1
    value = f.arg2

def FnDeprecated Arg =
  block
    let f = Fun2 "Deprecated" Version Arg
    deprecatedIn = f.arg1
    value        = f.arg2

def FnIsPDFVersion Arg =
  block
    let f = Fun2 "IsPDFVersion" Version Arg
    version = f.arg1
    value   = f.arg2

def FnEval Arg = Fun1 "Eval" Arg


--------------------------------------------------------------------------------

def Digit   = $digit - '0' as int

def Natural =
  block
    $$ = many (s = Digit) (10 * s + Digit)
    -- can't be followed my a letter
    case Optional $alpha of
      just    -> Fail "Expected number, found identifier"
      nothing -> Accept

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

def Token P =
  block
    $$ = P
    Many $[' ']

def KW x = Token (@Match x)




