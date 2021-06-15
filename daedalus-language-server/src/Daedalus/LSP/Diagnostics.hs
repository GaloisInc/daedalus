
-- | Implements core diagnostics (parse errors, type errors, etc.)
module Daedalus.LSP.Diagnostics where

import qualified Data.Text as Text
import Data.Text (Text)

import           Language.LSP.Diagnostics
import qualified Language.LSP.Types            as J
import           Language.LSP.Types (Diagnostic(..))
import           System.Log.Logger
import qualified Language.LSP.Types.Lens as J

import Daedalus.LSP.Monad
import Daedalus.Parser (ParseError(..), parseFromText)
import Daedalus.Scope (ScopeError(..), resolveModule)
import Daedalus.Type.Monad (TypeError(..), runMTypeM)

import Daedalus.SourceRange (SourceRange(..), SourcePos(..))
import Daedalus.PP
import Daedalus.Type.AST (Located(..))
import Language.LSP.Server
import Control.Monad.IO.Class (liftIO)
import Language.LSP.VFS
import Data.Maybe (fromMaybe)
import Daedalus.Type (inferRules)
import Control.Lens hiding (Iso)

-- ---------------------------------------------------------------------
-- Diagnostics

handleChangedDocument :: J.NormalizedUri -> J.TextDocumentVersion -> ServerM ()
handleChangedDocument uri version = do
  liftIO $ debugM "reactor.handleChangedDocument" ("Handling " ++ show uri)
  mdoc <- getVirtualFile uri
  case (mdoc, version) of
    -- Can we race the server thread?  It is possible to imagine that
    -- if the reactor is a bit slow then the server might update the
    -- doc before we start, so we check here and do nothing if we are
    -- out of date.
    (Just vf, Just version') | virtualFileVersion vf == version' -> goParse (virtualFileText vf)
    (Just vf, Nothing)  -> goParse (virtualFileText vf)
    _ ->  liftIO $ debugM "reactor.handle" $ "Didn't find anything in the VFS for: " ++ show uri
  where
    mn = Text.pack $ fromMaybe "Main" (J.uriToFilePath (J.fromNormalizedUri uri))
    maxDiags = 100
    reportDiags = publishDiagnostics maxDiags uri version . partitionBySource 

    goParse txt = 
      case parseFromText (J.getUri (J.fromNormalizedUri uri))  mn txt of
        Left err -> reportDiags (toDiagnostics err)
        Right m  -> goScope m

    goScope m = do
      r <- liftPassM (resolveModule mempty m)
      case r of
        Left err -> reportDiags (toDiagnostics err)
        Right (m', _gs) -> goTC m'

    goTC m = do
      r <- liftPassM (runMTypeM mempty mempty (inferRules m))
      case r of
        Left err  -> reportDiags (toDiagnostics err)
        Right _m' -> reportDiags [] -- report no diagnostics to clear diagnostics on the client

sourcePosToPosition :: SourcePos -> J.Position
sourcePosToPosition sp = J.Position (sourceLine sp - 1) (sourceColumn sp - 1)

sourcePosToRange :: SourcePos -> J.Range
sourcePosToRange sp = J.Range pos pos -- FIXME: same point? 
  where
    pos = sourcePosToPosition sp

sourceRangeToRange :: SourceRange -> J.Range
sourceRangeToRange sr = J.Range (sourcePosToPosition (sourceFrom sr))
                                (over J.character (+ 1) (sourcePosToPosition (sourceTo sr)))

-- FIXME: we should never really need these? (from haskell-language-server)
noRange :: J.Range
noRange =  J.Range (J.Position 0 0) (J.Position 1 0)

makeDiagnosticText :: J.Range -> Text -> Diagnostic
makeDiagnosticText r msg = 
  Diagnostic { _range    = r
             , _severity = Just J.DsError
             , _source   = Nothing -- ??
             , _message  = msg
             , _code     = Nothing
             , _relatedInformation = Nothing
             , _tags     = Nothing
             }

makeDiagnostic :: PP a => J.Range -> a -> Diagnostic
makeDiagnostic r msg = makeDiagnosticText r (Text.pack (showPP msg))

class ToDiagnostics a where
  toDiagnostics :: a -> [Diagnostic]

instance ToDiagnostics ParseError where
  toDiagnostics (ParseError { errorLoc = sp, errorMsg = msg }) = [ makeDiagnosticText (sourcePosToRange sp) (Text.pack msg) ]

instance ToDiagnostics ScopeError where
  toDiagnostics serr = [ makeDiagnostic noRange serr ]

instance ToDiagnostics TypeError where
  toDiagnostics (TypeError l) = [ makeDiagnosticText (sourceRangeToRange $ thingRange l) (Text.pack (show (thingValue l))) ]

-- daedalusErrorToDiagnostics :: DaedalusError -> [Diagnostic]
-- daedalusErrorToDiagnostics err =
--   case err of
--     AParseError (ParseError { errorLoc = sp, errorMsg = msg }) ->
--       [ makeDiagnosticText (sourcePosToRange sp) (Text.pack msg) ]
      
--     -- FIXME: we should add positions to these!
--     AModuleError merr -> [ makeDiagnostic noRange merr ]

--     -- FIXME: we should add positions to these!                         
--     AScopeError  serr -> [ makeDiagnostic noRange serr ]
    
--     ASpecializeError str -> [ makeDiagnosticText noRange (Text.pack str) ]
--     ADriverError     str -> [ makeDiagnosticText noRange (Text.pack str) ]


