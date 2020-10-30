{-# Language DataKinds #-}
{-# Language KindSignatures #-}
{-# Language TypeOperators #-}
{-# Language MultiParamTypeClasses #-}
{-# Language FlexibleInstances #-}
{-# Language StandaloneDeriving #-}
{-# Language ScopedTypeVariables #-}
{-# Language FlexibleContexts #-}
{-# Language AllowAmbiguousTypes #-}
{-# Language OverloadedStrings #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
module Midi where
 
import RTS.Parser
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data Header_0
  = Header_0_multi_song ()
  | Header_0_multi_track ()
  | Header_0_single_track ()
  
 
deriving instance HS.Eq Header_0
 
deriving instance HS.Ord Header_0
 
deriving instance HS.Show Header_0
 
instance RTS.DDL Header_0 where
 
instance HS.HasField "multi_song" Header_0 (HS.Maybe ()) where
  getField (Header_0_multi_song x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "multi_track" Header_0 (HS.Maybe ()) where
  getField (Header_0_multi_track x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "single_track" Header_0 (HS.Maybe ()) where
  getField (Header_0_single_track x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Header_1
  = Header_1_quarter_len (RTS.UInt 15)
  | Header_1_smtpe (RTS.SInt 15)
  
 
deriving instance HS.Eq Header_1
 
deriving instance HS.Ord Header_1
 
deriving instance HS.Show Header_1
 
instance RTS.DDL Header_1 where
 
instance HS.HasField "quarter_len" Header_1
           (HS.Maybe (RTS.UInt 15)) where
  getField (Header_1_quarter_len x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "smtpe" Header_1
           (HS.Maybe (RTS.SInt 15)) where
  getField (Header_1_smtpe x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Meta_0
  = Meta_0 (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 8)
      (RTS.UInt 8)
  
 
deriving instance HS.Eq Meta_0
 
deriving instance HS.Ord Meta_0
 
deriving instance HS.Show Meta_0
 
instance RTS.DDL Meta_0 where
 
instance HS.HasField "hh" Meta_0 (RTS.UInt 8) where
  getField (Meta_0 x _ _ _ _) = x
 
instance HS.HasField "mm" Meta_0 (RTS.UInt 8) where
  getField (Meta_0 _ x _ _ _) = x
 
instance HS.HasField "ss" Meta_0 (RTS.UInt 8) where
  getField (Meta_0 _ _ x _ _) = x
 
instance HS.HasField "fr" Meta_0 (RTS.UInt 8) where
  getField (Meta_0 _ _ _ x _) = x
 
instance HS.HasField "ff" Meta_0 (RTS.UInt 8) where
  getField (Meta_0 _ _ _ _ x) = x
 
data Meta_1
  = Meta_1 (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 8)
  
 
deriving instance HS.Eq Meta_1
 
deriving instance HS.Ord Meta_1
 
deriving instance HS.Show Meta_1
 
instance RTS.DDL Meta_1 where
 
instance HS.HasField "nn" Meta_1 (RTS.UInt 8) where
  getField (Meta_1 x _ _ _) = x
 
instance HS.HasField "dd" Meta_1 (RTS.UInt 8) where
  getField (Meta_1 _ x _ _) = x
 
instance HS.HasField "cc" Meta_1 (RTS.UInt 8) where
  getField (Meta_1 _ _ x _) = x
 
instance HS.HasField "bb" Meta_1 (RTS.UInt 8) where
  getField (Meta_1 _ _ _ x) = x
 
data Meta_2
  = Meta_2_major (RTS.UInt 8)
  | Meta_2_minor (RTS.UInt 8)
  
 
deriving instance HS.Eq Meta_2
 
deriving instance HS.Ord Meta_2
 
deriving instance HS.Show Meta_2
 
instance RTS.DDL Meta_2 where
 
instance HS.HasField "major" Meta_2 (HS.Maybe (RTS.UInt 8)) where
  getField (Meta_2_major x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "minor" Meta_2 (HS.Maybe (RTS.UInt 8)) where
  getField (Meta_2_minor x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Meta_3
  = Meta_3 (RTS.UInt 8) Meta_2
  
 
deriving instance HS.Eq Meta_3
 
deriving instance HS.Ord Meta_3
 
deriving instance HS.Show Meta_3
 
instance RTS.DDL Meta_3 where
 
instance HS.HasField "key" Meta_3 (RTS.UInt 8) where
  getField (Meta_3 x _) = x
 
instance HS.HasField "mode" Meta_3 Meta_2 where
  getField (Meta_3 _ x) = x
 
data Meta_4
  = Meta_4 (RTS.UInt 16) RTS.Input
  
 
deriving instance HS.Eq Meta_4
 
deriving instance HS.Ord Meta_4
 
deriving instance HS.Show Meta_4
 
instance RTS.DDL Meta_4 where
 
instance HS.HasField "manufacturer" Meta_4 (RTS.UInt 16) where
  getField (Meta_4 x _) = x
 
instance HS.HasField "data" Meta_4 RTS.Input where
  getField (Meta_4 _ x) = x
 
data Meta_5
  = Meta_5 (RTS.UInt 8) RTS.Input
  
 
deriving instance HS.Eq Meta_5
 
deriving instance HS.Ord Meta_5
 
deriving instance HS.Show Meta_5
 
instance RTS.DDL Meta_5 where
 
instance HS.HasField "type" Meta_5 (RTS.UInt 8) where
  getField (Meta_5 x _) = x
 
instance HS.HasField "data" Meta_5 RTS.Input where
  getField (Meta_5 _ x) = x
 
data VoiceMessage_0
  = VoiceMessage_0 (RTS.UInt 7) (RTS.UInt 7)
  
 
deriving instance HS.Eq VoiceMessage_0
 
deriving instance HS.Ord VoiceMessage_0
 
deriving instance HS.Show VoiceMessage_0
 
instance RTS.DDL VoiceMessage_0 where
 
instance HS.HasField "key" VoiceMessage_0 (RTS.UInt 7) where
  getField (VoiceMessage_0 x _) = x
 
instance HS.HasField "velocity" VoiceMessage_0 (RTS.UInt 7) where
  getField (VoiceMessage_0 _ x) = x
 
data VoiceMessage_1
  = VoiceMessage_1 (RTS.UInt 7) (RTS.UInt 7)
  
 
deriving instance HS.Eq VoiceMessage_1
 
deriving instance HS.Ord VoiceMessage_1
 
deriving instance HS.Show VoiceMessage_1
 
instance RTS.DDL VoiceMessage_1 where
 
instance HS.HasField "key" VoiceMessage_1 (RTS.UInt 7) where
  getField (VoiceMessage_1 x _) = x
 
instance HS.HasField "velocity" VoiceMessage_1 (RTS.UInt 7) where
  getField (VoiceMessage_1 _ x) = x
 
data VoiceMessage_2
  = VoiceMessage_2 (RTS.UInt 7) (RTS.UInt 7)
  
 
deriving instance HS.Eq VoiceMessage_2
 
deriving instance HS.Ord VoiceMessage_2
 
deriving instance HS.Show VoiceMessage_2
 
instance RTS.DDL VoiceMessage_2 where
 
instance HS.HasField "key" VoiceMessage_2 (RTS.UInt 7) where
  getField (VoiceMessage_2 x _) = x
 
instance HS.HasField "pressure" VoiceMessage_2 (RTS.UInt 7) where
  getField (VoiceMessage_2 _ x) = x
 
data VoiceMessage_3
  = VoiceMessage_3 (RTS.UInt 7) (RTS.UInt 7)
  
 
deriving instance HS.Eq VoiceMessage_3
 
deriving instance HS.Ord VoiceMessage_3
 
deriving instance HS.Show VoiceMessage_3
 
instance RTS.DDL VoiceMessage_3 where
 
instance HS.HasField "controller" VoiceMessage_3 (RTS.UInt 7) where
  getField (VoiceMessage_3 x _) = x
 
instance HS.HasField "value" VoiceMessage_3 (RTS.UInt 7) where
  getField (VoiceMessage_3 _ x) = x
 
data Delta c
  = Delta HS.Integer c
  
 
deriving instance HS.Eq c => HS.Eq (Delta c)
 
deriving instance HS.Ord c => HS.Ord (Delta c)
 
deriving instance HS.Show c => HS.Show (Delta c)
 
instance RTS.DDL c => RTS.DDL (Delta c) where
 
instance HS.HasField "after" (Delta c) HS.Integer where
  getField (Delta x _) = x
 
instance HS.HasField "event" (Delta c) c where
  getField (Delta _ x) = x
 
data Meta
  = Meta_channel (RTS.UInt 8)
  | Meta_copyright RTS.Input
  | Meta_cue RTS.Input
  | Meta_end_track ()
  | Meta_instrument RTS.Input
  | Meta_key_sig Meta_3
  | Meta_lyrics RTS.Input
  | Meta_marker RTS.Input
  | Meta_name RTS.Input
  | Meta_seq_specifiec Meta_4
  | Meta_sequence (RTS.UInt 16)
  | Meta_smtpe_offset Meta_0
  | Meta_tempo (RTS.UInt 24)
  | Meta_text RTS.Input
  | Meta_time_sig Meta_1
  | Meta_unknown Meta_5
  
 
deriving instance HS.Eq Meta
 
deriving instance HS.Ord Meta
 
deriving instance HS.Show Meta
 
instance RTS.DDL Meta where
 
instance HS.HasField "channel" Meta (HS.Maybe (RTS.UInt 8)) where
  getField (Meta_channel x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "copyright" Meta (HS.Maybe RTS.Input) where
  getField (Meta_copyright x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "cue" Meta (HS.Maybe RTS.Input) where
  getField (Meta_cue x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "end_track" Meta (HS.Maybe ()) where
  getField (Meta_end_track x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "instrument" Meta (HS.Maybe RTS.Input) where
  getField (Meta_instrument x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "key_sig" Meta (HS.Maybe Meta_3) where
  getField (Meta_key_sig x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lyrics" Meta (HS.Maybe RTS.Input) where
  getField (Meta_lyrics x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "marker" Meta (HS.Maybe RTS.Input) where
  getField (Meta_marker x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "name" Meta (HS.Maybe RTS.Input) where
  getField (Meta_name x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "seq_specifiec" Meta (HS.Maybe Meta_4) where
  getField (Meta_seq_specifiec x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "sequence" Meta (HS.Maybe (RTS.UInt 16)) where
  getField (Meta_sequence x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "smtpe_offset" Meta (HS.Maybe Meta_0) where
  getField (Meta_smtpe_offset x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "tempo" Meta (HS.Maybe (RTS.UInt 24)) where
  getField (Meta_tempo x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "text" Meta (HS.Maybe RTS.Input) where
  getField (Meta_text x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "time_sig" Meta (HS.Maybe Meta_1) where
  getField (Meta_time_sig x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "unknown" Meta (HS.Maybe Meta_5) where
  getField (Meta_unknown x) = HS.Just x
   
  getField _ = HS.Nothing
 
data ModeMessage
  = ModeMessage_all_notes_off ()
  | ModeMessage_all_sound_off ()
  | ModeMessage_local_control_off ()
  | ModeMessage_local_control_on ()
  | ModeMessage_mono_on (RTS.UInt 8)
  | ModeMessage_omni_off ()
  | ModeMessage_omni_on ()
  | ModeMessage_poly_on ()
  | ModeMessage_reset_controllers ()
  
 
deriving instance HS.Eq ModeMessage
 
deriving instance HS.Ord ModeMessage
 
deriving instance HS.Show ModeMessage
 
instance RTS.DDL ModeMessage where
 
instance HS.HasField "all_notes_off" ModeMessage
           (HS.Maybe ()) where
  getField (ModeMessage_all_notes_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "all_sound_off" ModeMessage
           (HS.Maybe ()) where
  getField (ModeMessage_all_sound_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "local_control_off" ModeMessage
           (HS.Maybe ()) where
  getField (ModeMessage_local_control_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "local_control_on" ModeMessage
           (HS.Maybe ()) where
  getField (ModeMessage_local_control_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "mono_on" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_mono_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "omni_off" ModeMessage (HS.Maybe ()) where
  getField (ModeMessage_omni_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "omni_on" ModeMessage (HS.Maybe ()) where
  getField (ModeMessage_omni_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "poly_on" ModeMessage (HS.Maybe ()) where
  getField (ModeMessage_poly_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "reset_controllers" ModeMessage
           (HS.Maybe ()) where
  getField (ModeMessage_reset_controllers x) = HS.Just x
   
  getField _ = HS.Nothing
 
data ModeMessages
  = ModeMessages (RTS.UInt 4) ModeMessage
      (Vector.Vector (Delta ModeMessage))
  
 
deriving instance HS.Eq ModeMessages
 
deriving instance HS.Ord ModeMessages
 
deriving instance HS.Show ModeMessages
 
instance RTS.DDL ModeMessages where
 
instance HS.HasField "channel" ModeMessages (RTS.UInt 4) where
  getField (ModeMessages x _ _) = x
 
instance HS.HasField "messages" ModeMessages ModeMessage where
  getField (ModeMessages _ x _) = x
 
instance HS.HasField "extra" ModeMessages
           (Vector.Vector (Delta ModeMessage)) where
  getField (ModeMessages _ _ x) = x
 
data SysEx
  = SysEx_add_f0 RTS.Input
  | SysEx_as_is RTS.Input
  
 
deriving instance HS.Eq SysEx
 
deriving instance HS.Ord SysEx
 
deriving instance HS.Show SysEx
 
instance RTS.DDL SysEx where
 
instance HS.HasField "add_f0" SysEx (HS.Maybe RTS.Input) where
  getField (SysEx_add_f0 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "as_is" SysEx (HS.Maybe RTS.Input) where
  getField (SysEx_as_is x) = HS.Just x
   
  getField _ = HS.Nothing
 
data VoiceMessage
  = VoiceMessage_aftertouch VoiceMessage_2
  | VoiceMessage_channel_pressure (RTS.UInt 7)
  | VoiceMessage_controller_change VoiceMessage_3
  | VoiceMessage_note_off VoiceMessage_0
  | VoiceMessage_note_on VoiceMessage_1
  | VoiceMessage_pitch_bend (RTS.UInt 14)
  | VoiceMessage_program_change (RTS.UInt 7)
  
 
deriving instance HS.Eq VoiceMessage
 
deriving instance HS.Ord VoiceMessage
 
deriving instance HS.Show VoiceMessage
 
instance RTS.DDL VoiceMessage where
 
instance HS.HasField "aftertouch" VoiceMessage
           (HS.Maybe VoiceMessage_2) where
  getField (VoiceMessage_aftertouch x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "channel_pressure" VoiceMessage
           (HS.Maybe (RTS.UInt 7)) where
  getField (VoiceMessage_channel_pressure x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "controller_change" VoiceMessage
           (HS.Maybe VoiceMessage_3) where
  getField (VoiceMessage_controller_change x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "note_off" VoiceMessage
           (HS.Maybe VoiceMessage_0) where
  getField (VoiceMessage_note_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "note_on" VoiceMessage
           (HS.Maybe VoiceMessage_1) where
  getField (VoiceMessage_note_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "pitch_bend" VoiceMessage
           (HS.Maybe (RTS.UInt 14)) where
  getField (VoiceMessage_pitch_bend x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "program_change" VoiceMessage
           (HS.Maybe (RTS.UInt 7)) where
  getField (VoiceMessage_program_change x) = HS.Just x
   
  getField _ = HS.Nothing
 
data VoiceMessages
  = VoiceMessages (RTS.UInt 4) VoiceMessage
      (Vector.Vector (Delta VoiceMessage))
  
 
deriving instance HS.Eq VoiceMessages
 
deriving instance HS.Ord VoiceMessages
 
deriving instance HS.Show VoiceMessages
 
instance RTS.DDL VoiceMessages where
 
instance HS.HasField "channel" VoiceMessages (RTS.UInt 4) where
  getField (VoiceMessages x _ _) = x
 
instance HS.HasField "message" VoiceMessages VoiceMessage where
  getField (VoiceMessages _ x _) = x
 
instance HS.HasField "extra" VoiceMessages
           (Vector.Vector (Delta VoiceMessage)) where
  getField (VoiceMessages _ _ x) = x
 
data Event
  = Event_meta Meta
  | Event_modeMessage ModeMessages
  | Event_sysEx SysEx
  | Event_voiceMessage VoiceMessages
  
 
deriving instance HS.Eq Event
 
deriving instance HS.Ord Event
 
deriving instance HS.Show Event
 
instance RTS.DDL Event where
 
instance HS.HasField "meta" Event (HS.Maybe Meta) where
  getField (Event_meta x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "modeMessage" Event
           (HS.Maybe ModeMessages) where
  getField (Event_modeMessage x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "sysEx" Event (HS.Maybe SysEx) where
  getField (Event_sysEx x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "voiceMessage" Event
           (HS.Maybe VoiceMessages) where
  getField (Event_voiceMessage x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Header
  = Header Header_0 (RTS.UInt 16) Header_1
  
 
deriving instance HS.Eq Header
 
deriving instance HS.Ord Header
 
deriving instance HS.Show Header
 
instance RTS.DDL Header where
 
instance HS.HasField "format" Header Header_0 where
  getField (Header x _ _) = x
 
instance HS.HasField "track_num" Header (RTS.UInt 16) where
  getField (Header _ x _) = x
 
instance HS.HasField "time_unit" Header Header_1 where
  getField (Header _ _ x) = x
 
data Main
  = Main Header (Vector.Vector (Vector.Vector (Delta Event)))
  
 
deriving instance HS.Eq Main
 
deriving instance HS.Ord Main
 
deriving instance HS.Show Main
 
instance RTS.DDL Main where
 
instance HS.HasField "header" Main Header where
  getField (Main x _) = x
 
instance HS.HasField "tracks" Main
           (Vector.Vector (Vector.Vector (Delta Event))) where
  getField (Main _ x) = x
 
pBE16 :: Parser (RTS.UInt 16)
 
pBE16 =
  do (_0 :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "150:19--150:23"
     (_1 :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "150:27--150:31"
     HS.pure (RTS.cat _0 _1)
 
pBE24 :: Parser (RTS.UInt 24)
 
pBE24 =
  do (_2 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     (_3 :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "151:27--151:31"
     HS.pure (RTS.cat _2 _3)
 
pBE32 :: Parser (RTS.UInt 32)
 
pBE32 =
  do (_4 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     (_5 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     HS.pure (RTS.cat _4 _5)
 
pBlock ::
  forall e. RTS.DDL e => HS.Integer -> (Parser e -> Parser e)
 
pBlock (n :: HS.Integer) (pP :: Parser e) =
  do (cur :: RTS.Input) <- RTS.pPeek
     do (_6 :: RTS.Input) <-
          RTS.pIsJust "161:16--161:21" "Not enough bytes"
            (RTS.limitLen n cur)
        RTS.pSetInput _6
     (__ :: e) <- pP
     do (_7 :: RTS.Input) <-
          RTS.pIsJust "163:16--163:21" "Not enough bytes"
            (RTS.advanceBy n cur)
        RTS.pSetInput _7
     HS.pure __
 
pOnly :: forall b. RTS.DDL b => Parser b -> Parser b
 
pOnly (pP :: Parser b) =
  do (__ :: b) <- pP
     RTS.pEnd "166:24--166:26"
     HS.pure __
 
_Block ::
  forall e. RTS.DDL e => HS.Integer -> (Parser () -> Parser ())
 
_Block (n :: HS.Integer) (_P :: Parser ()) =
  do (cur :: RTS.Input) <- RTS.pPeek
     do (_6 :: RTS.Input) <-
          RTS.pIsJust "161:16--161:21" "Not enough bytes"
            (RTS.limitLen n cur)
        RTS.pSetInput _6
     _P
     (_7 :: RTS.Input) <-
       RTS.pIsJust "163:16--163:21" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput _7
 
_Only :: forall b. RTS.DDL b => Parser () -> Parser ()
 
_Only (_P :: Parser ()) =
  do _P
     RTS.pEnd "166:24--166:26"
 
pChunk ::
  forall e i.
    (RTS.DDL e, RTS.DDL i) => Parser e -> (Parser i -> Parser i)
 
pChunk (pTy :: Parser e) (pP :: Parser i) =
  do RTS.pEnter "midi._Block"
       (_Block @e (RTS.lit 4 :: HS.Integer)
          (RTS.pEnter "midi._Only"
             (_Only @e
                (do HS.void pTy
                    HS.pure ()))))
     (__ :: i) <-
       do (_9 :: HS.Integer) <-
            do (_8 :: RTS.UInt 32) <- RTS.pEnter "midi.BE32" pBE32
               RTS.pIsJust "19:12--19:22" "Value does not fit in target type"
                 (RTS.convertMaybe _8 :: HS.Maybe HS.Integer)
          RTS.pEnter "midi.Block"
            (pBlock @i _9 (RTS.pEnter "midi.Only" (pOnly @i pP)))
     HS.pure __
 
pUInt7 :: Parser (RTS.UInt 7)
 
pUInt7 =
  do (_10 :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "157:19--157:23"
     RTS.pIsJust "157:19--157:33" "Value does not fit in target type"
       (RTS.convertMaybe _10 :: HS.Maybe (RTS.UInt 7))
 
_Guard :: HS.Bool -> Parser ()
 
_Guard (p :: HS.Bool) =
  RTS.pGuard "168:15--168:23" "guard failed" p
 
getBit ::
  forall b.
    (RTS.DDL b, RTS.Numeric b, RTS.Convert b (RTS.UInt 1)) =>
      HS.Integer -> (b -> RTS.UInt 1)
 
getBit (n :: HS.Integer) (b :: b) =
  RTS.convert (RTS.shiftr b n) :: RTS.UInt 1
 
pVarQ :: Parser HS.Integer
 
pVarQ =
  do (lead :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pMany (RTS.<||)
         (do (b :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "11:25--11:29"
             RTS.pEnter "midi._Guard"
               (_Guard
                  (getBit @(RTS.UInt 8) (RTS.lit 7 :: HS.Integer) b
                     HS.== (RTS.lit 1 :: RTS.UInt 1)))
             (__ :: RTS.UInt 7) <- HS.pure (RTS.convert b :: RTS.UInt 7)
             HS.pure __)
     (last :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
     (__ :: HS.Integer) <-
       HS.pure
         (RTS.lcat
            (RTS.loopFold
               (\(v :: HS.Integer) (l :: RTS.UInt 7) -> RTS.lcat v l)
               (RTS.lit 0 :: HS.Integer)
               lead)
            last)
     HS.pure __
 
pDelta :: forall c. RTS.DDL c => Parser c -> Parser (Delta c)
 
pDelta (pE :: Parser c) =
  do (after :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
     (event :: c) <- pE
     HS.pure (Delta after event)
 
pMeta :: Parser Meta
 
pMeta =
  do HS.const ()
       HS.<$> RTS.pMatch1 "108:5--108:15" (RTS.bcSingle (RTS.uint8 255))
     (_type :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "109:13--109:17"
     RTS.pEnter "midi._Guard"
       (_Guard (_type HS.<= (RTS.lit 127 :: RTS.UInt 8)))
     (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
     (__ :: Meta) <-
       RTS.pEnter "midi.Block"
         (pBlock @Meta len
            ((RTS.<||)
               (RTS.pEnter "sequence"
                  (do (_11 :: RTS.UInt 16) <-
                        do RTS.pEnter "midi._Guard"
                             (_Guard (_type HS.== (RTS.lit 0 :: RTS.UInt 8)))
                           (__ :: RTS.UInt 16) <-
                             RTS.pEnter "midi.Only"
                               (pOnly @(RTS.UInt 16) (RTS.pEnter "midi.BE16" pBE16))
                           HS.pure __
                      HS.pure (Meta_sequence _11)))
               ((RTS.<||)
                  (RTS.pEnter "text"
                     (do (_12 :: RTS.Input) <-
                           do RTS.pEnter "midi._Guard"
                                (_Guard (_type HS.== (RTS.lit 1 :: RTS.UInt 8)))
                              (__ :: RTS.Input) <- RTS.pPeek
                              HS.pure __
                         HS.pure (Meta_text _12)))
                  ((RTS.<||)
                     (RTS.pEnter "copyright"
                        (do (_13 :: RTS.Input) <-
                              do RTS.pEnter "midi._Guard"
                                   (_Guard (_type HS.== (RTS.lit 2 :: RTS.UInt 8)))
                                 (__ :: RTS.Input) <- RTS.pPeek
                                 HS.pure __
                            HS.pure (Meta_copyright _13)))
                     ((RTS.<||)
                        (RTS.pEnter "name"
                           (do (_14 :: RTS.Input) <-
                                 do RTS.pEnter "midi._Guard"
                                      (_Guard (_type HS.== (RTS.lit 3 :: RTS.UInt 8)))
                                    (__ :: RTS.Input) <- RTS.pPeek
                                    HS.pure __
                               HS.pure (Meta_name _14)))
                        ((RTS.<||)
                           (RTS.pEnter "instrument"
                              (do (_15 :: RTS.Input) <-
                                    do RTS.pEnter "midi._Guard"
                                         (_Guard (_type HS.== (RTS.lit 4 :: RTS.UInt 8)))
                                       (__ :: RTS.Input) <- RTS.pPeek
                                       HS.pure __
                                  HS.pure (Meta_instrument _15)))
                           ((RTS.<||)
                              (RTS.pEnter "lyrics"
                                 (do (_16 :: RTS.Input) <-
                                       do RTS.pEnter "midi._Guard"
                                            (_Guard (_type HS.== (RTS.lit 5 :: RTS.UInt 8)))
                                          (__ :: RTS.Input) <- RTS.pPeek
                                          HS.pure __
                                     HS.pure (Meta_lyrics _16)))
                              ((RTS.<||)
                                 (RTS.pEnter "marker"
                                    (do (_17 :: RTS.Input) <-
                                          do RTS.pEnter "midi._Guard"
                                               (_Guard (_type HS.== (RTS.lit 6 :: RTS.UInt 8)))
                                             (__ :: RTS.Input) <- RTS.pPeek
                                             HS.pure __
                                        HS.pure (Meta_marker _17)))
                                 ((RTS.<||)
                                    (RTS.pEnter "cue"
                                       (do (_18 :: RTS.Input) <-
                                             do RTS.pEnter "midi._Guard"
                                                  (_Guard (_type HS.== (RTS.lit 7 :: RTS.UInt 8)))
                                                (__ :: RTS.Input) <- RTS.pPeek
                                                HS.pure __
                                           HS.pure (Meta_cue _18)))
                                    ((RTS.<||)
                                       (RTS.pEnter "channel"
                                          (do (_19 :: RTS.UInt 8) <-
                                                do RTS.pEnter "midi._Guard"
                                                     (_Guard
                                                        (_type HS.== (RTS.lit 32 :: RTS.UInt 8)))
                                                   (__ :: RTS.UInt 8) <-
                                                     RTS.pEnter "midi.Only"
                                                       (pOnly @(RTS.UInt 8)
                                                          (RTS.uint8
                                                             HS.<$> RTS.pByte "121:53--121:57"))
                                                   HS.pure __
                                              HS.pure (Meta_channel _19)))
                                       ((RTS.<||)
                                          (RTS.pEnter "end_track"
                                             (do (_20 :: ()) <-
                                                   do RTS.pEnter "midi._Guard"
                                                        (_Guard
                                                           (_type HS.== (RTS.lit 47 :: RTS.UInt 8)))
                                                      (__ :: ()) <- RTS.pEnd "122:48--122:50"
                                                      HS.pure __
                                                 HS.pure (Meta_end_track _20)))
                                          ((RTS.<||)
                                             (RTS.pEnter "tempo"
                                                (do (_21 :: RTS.UInt 24) <-
                                                      do RTS.pEnter "midi._Guard"
                                                           (_Guard
                                                              (_type
                                                                 HS.== (RTS.lit 81 :: RTS.UInt 8)))
                                                         (__ :: RTS.UInt 24) <-
                                                           RTS.pEnter "midi.Only"
                                                             (pOnly @(RTS.UInt 24)
                                                                (RTS.pEnter "midi.BE24" pBE24))
                                                         HS.pure __
                                                    HS.pure (Meta_tempo _21)))
                                             ((RTS.<||)
                                                (RTS.pEnter "smtpe_offset"
                                                   (do (_22 :: Meta_0) <-
                                                         do RTS.pEnter "midi._Guard"
                                                              (_Guard
                                                                 (_type
                                                                    HS.== (RTS.lit 84
                                                                             :: RTS.UInt 8)))
                                                            (hh :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "125:31--125:35"
                                                            (mm :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "125:43--125:47"
                                                            (ss :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "125:55--125:59"
                                                            (fr :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "125:67--125:71"
                                                            (ff :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "126:31--126:35"
                                                            RTS.pEnd "126:38--126:40"
                                                            HS.pure (Meta_0 hh mm ss fr ff)
                                                       HS.pure (Meta_smtpe_offset _22)))
                                                ((RTS.<||)
                                                   (RTS.pEnter "time_sig"
                                                      (do (_23 :: Meta_1) <-
                                                            do RTS.pEnter "midi._Guard"
                                                                 (_Guard
                                                                    (_type
                                                                       HS.== (RTS.lit 88
                                                                                :: RTS.UInt 8)))
                                                               (nn :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "128:31--128:35"
                                                               (dd :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "128:43--128:47"
                                                               (cc :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "128:55--128:59"
                                                               (bb :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "129:31--129:35"
                                                               RTS.pEnd "130:26--130:28"
                                                               HS.pure (Meta_1 nn dd cc bb)
                                                          HS.pure (Meta_time_sig _23)))
                                                   ((RTS.<||)
                                                      (RTS.pEnter "key_sig"
                                                         (do (_26 :: Meta_3) <-
                                                               do RTS.pEnter "midi._Guard"
                                                                    (_Guard
                                                                       (_type
                                                                          HS.== (RTS.lit 89
                                                                                   :: RTS.UInt 8)))
                                                                  (key :: RTS.UInt 8) <-
                                                                    RTS.uint8
                                                                      HS.<$> RTS.pByte
                                                                               "133:33--133:37"
                                                                  (mode :: Meta_2) <-
                                                                    (RTS.|||)
                                                                      (RTS.pEnter "major"
                                                                         (do (_24 :: RTS.UInt 8) <-
                                                                               RTS.uint8
                                                                                 HS.<$> RTS.pMatch1
                                                                                          "135:50--135:57"
                                                                                          (RTS.bcSingle
                                                                                             (RTS.uint8
                                                                                                0))
                                                                             HS.pure
                                                                               (Meta_2_major _24)))
                                                                      (RTS.pEnter "minor"
                                                                         (do (_25 :: RTS.UInt 8) <-
                                                                               RTS.uint8
                                                                                 HS.<$> RTS.pMatch1
                                                                                          "135:68--135:75"
                                                                                          (RTS.bcSingle
                                                                                             (RTS.uint8
                                                                                                1))
                                                                             HS.pure
                                                                               (Meta_2_minor _25)))
                                                                  RTS.pEnd "136:26--136:28"
                                                                  HS.pure (Meta_3 key mode)
                                                             HS.pure (Meta_key_sig _26)))
                                                      ((RTS.<||)
                                                         (RTS.pEnter "seq_specifiec"
                                                            (do (_27 :: Meta_4) <-
                                                                  do RTS.pEnter "midi._Guard"
                                                                       (_Guard
                                                                          (_type
                                                                             HS.== (RTS.lit 127
                                                                                      :: RTS.UInt
                                                                                           8)))
                                                                     (manufacturer
                                                                        :: RTS.UInt 16) <-
                                                                       (RTS.<||)
                                                                         (do HS.const ()
                                                                               HS.<$> RTS.pMatch1
                                                                                        "139:44--139:51"
                                                                                        (RTS.bcSingle
                                                                                           (RTS.uint8
                                                                                              0))
                                                                             (__ :: RTS.UInt 16) <-
                                                                               RTS.pEnter
                                                                                 "midi.BE16"
                                                                                 pBE16
                                                                             HS.pure __)
                                                                         (do (b :: RTS.UInt 8) <-
                                                                               RTS.uint8
                                                                                 HS.<$> RTS.pByte
                                                                                          "140:49--140:53"
                                                                             (__ :: RTS.UInt 16) <-
                                                                               HS.pure
                                                                                 (RTS.convert b
                                                                                    :: RTS.UInt 16)
                                                                             HS.pure __)
                                                                     (_data :: RTS.Input) <-
                                                                       RTS.pPeek
                                                                     HS.pure
                                                                       (Meta_4 manufacturer _data)
                                                                HS.pure (Meta_seq_specifiec _27)))
                                                         (RTS.pEnter "unknown"
                                                            (do (_28 :: Meta_5) <-
                                                                  do (_type :: RTS.UInt 8) <-
                                                                       HS.pure _type
                                                                     (_data :: RTS.Input) <-
                                                                       RTS.pPeek
                                                                     HS.pure (Meta_5 _type _data)
                                                                HS.pure
                                                                  (Meta_unknown
                                                                     _28)))))))))))))))))))
     HS.pure __
 
pModeMessage :: Parser ModeMessage
 
pModeMessage =
  (RTS.|||)
    (RTS.pEnter "all_sound_off"
       (do (_29 :: ()) <-
             HS.const ()
               HS.<$> RTS.pMatch "88:26--88:45"
                        (Vector.fromList
                           [RTS.lit 120 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
           HS.pure (ModeMessage_all_sound_off _29)))
    ((RTS.|||)
       (RTS.pEnter "reset_controllers"
          (do (_30 :: ()) <-
                HS.const ()
                  HS.<$> RTS.pMatch "89:26--89:45"
                           (Vector.fromList
                              [RTS.lit 121 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
              HS.pure (ModeMessage_reset_controllers _30)))
       ((RTS.|||)
          (RTS.pEnter "local_control_off"
             (do (_31 :: ()) <-
                   HS.const ()
                     HS.<$> RTS.pMatch "90:26--90:45"
                              (Vector.fromList
                                 [RTS.lit 122 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
                 HS.pure (ModeMessage_local_control_off _31)))
          ((RTS.|||)
             (RTS.pEnter "local_control_on"
                (do (_32 :: ()) <-
                      HS.const ()
                        HS.<$> RTS.pMatch "91:26--91:45"
                                 (Vector.fromList
                                    [RTS.lit 122 :: RTS.UInt 8, RTS.lit 127 :: RTS.UInt 8])
                    HS.pure (ModeMessage_local_control_on _32)))
             ((RTS.|||)
                (RTS.pEnter "all_notes_off"
                   (do (_33 :: ()) <-
                         HS.const ()
                           HS.<$> RTS.pMatch "92:26--92:45"
                                    (Vector.fromList
                                       [RTS.lit 123 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
                       HS.pure (ModeMessage_all_notes_off _33)))
                ((RTS.|||)
                   (RTS.pEnter "omni_off"
                      (do (_34 :: ()) <-
                            HS.const ()
                              HS.<$> RTS.pMatch "93:26--93:45"
                                       (Vector.fromList
                                          [RTS.lit 124 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
                          HS.pure (ModeMessage_omni_off _34)))
                   ((RTS.|||)
                      (RTS.pEnter "omni_on"
                         (do (_35 :: ()) <-
                               HS.const ()
                                 HS.<$> RTS.pMatch "94:26--94:45"
                                          (Vector.fromList
                                             [RTS.lit 125 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
                             HS.pure (ModeMessage_omni_on _35)))
                      ((RTS.|||)
                         (RTS.pEnter "mono_on"
                            (do (_36 :: RTS.UInt 8) <-
                                  do HS.const ()
                                       HS.<$> RTS.pMatch1 "95:27--95:37"
                                                (RTS.bcSingle (RTS.uint8 126))
                                     (__ :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "95:45--95:49"
                                     RTS.pEnter "midi._Guard"
                                       (_Guard (__ HS.<= (RTS.lit 16 :: RTS.UInt 8)))
                                     HS.pure __
                                HS.pure (ModeMessage_mono_on _36)))
                         (RTS.pEnter "poly_on"
                            (do (_37 :: ()) <-
                                  HS.const ()
                                    HS.<$> RTS.pMatch "96:26--96:45"
                                             (Vector.fromList
                                                [RTS.lit 127 :: RTS.UInt 8,
                                                 RTS.lit 0 :: RTS.UInt 8])
                                HS.pure (ModeMessage_poly_on _37))))))))))
 
pModeMessages :: Parser ModeMessages
 
pModeMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "78:15--78:19"
     (tag :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr status (RTS.lit 4 :: HS.Integer))
            :: RTS.UInt 4)
     RTS.pEnter "midi._Guard"
       (_Guard (tag HS.== (RTS.lit 11 :: RTS.UInt 4)))
     (channel :: RTS.UInt 4) <-
       HS.pure (RTS.convert status :: RTS.UInt 4)
     (messages :: ModeMessage) <-
       RTS.pEnter "midi.ModeMessage" pModeMessage
     (extra :: Vector.Vector (Delta ModeMessage)) <-
       RTS.pMany (RTS.<||)
         (RTS.pEnter "midi.Delta"
            (pDelta @ModeMessage (RTS.pEnter "midi.ModeMessage" pModeMessage)))
     HS.pure (ModeMessages channel messages extra)
 
pSysEx :: Parser SysEx
 
pSysEx =
  (RTS.|||)
    (RTS.pEnter "add_f0"
       (do (_38 :: RTS.Input) <-
             do HS.const ()
                  HS.<$> RTS.pMatch1 "102:16--102:26" (RTS.bcSingle (RTS.uint8 240))
                (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
                (__ :: RTS.Input) <-
                  RTS.pEnter "midi.Block" (pBlock @RTS.Input len RTS.pPeek)
                HS.pure __
           HS.pure (SysEx_add_f0 _38)))
    (RTS.pEnter "as_is"
       (do (_39 :: RTS.Input) <-
             do HS.const ()
                  HS.<$> RTS.pMatch1 "103:16--103:26" (RTS.bcSingle (RTS.uint8 247))
                (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
                (__ :: RTS.Input) <-
                  RTS.pEnter "midi.Block" (pBlock @RTS.Input len RTS.pPeek)
                HS.pure __
           HS.pure (SysEx_as_is _39)))
 
pVoiceMessage :: RTS.UInt 4 -> Parser VoiceMessage
 
pVoiceMessage (tag :: RTS.UInt 4) =
  (RTS.|||)
    (RTS.pEnter "note_off"
       (do (_40 :: VoiceMessage_0) <-
             do RTS.pEnter "midi._Guard"
                  (_Guard (tag HS.== (RTS.lit 8 :: RTS.UInt 4)))
                (key :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                (velocity :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                HS.pure (VoiceMessage_0 key velocity)
           HS.pure (VoiceMessage_note_off _40)))
    ((RTS.|||)
       (RTS.pEnter "note_on"
          (do (_41 :: VoiceMessage_1) <-
                do RTS.pEnter "midi._Guard"
                     (_Guard (tag HS.== (RTS.lit 9 :: RTS.UInt 4)))
                   (key :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                   (velocity :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                   HS.pure (VoiceMessage_1 key velocity)
              HS.pure (VoiceMessage_note_on _41)))
       ((RTS.|||)
          (RTS.pEnter "aftertouch"
             (do (_42 :: VoiceMessage_2) <-
                   do RTS.pEnter "midi._Guard"
                        (_Guard (tag HS.== (RTS.lit 10 :: RTS.UInt 4)))
                      (key :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                      (pressure :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                      HS.pure (VoiceMessage_2 key pressure)
                 HS.pure (VoiceMessage_aftertouch _42)))
          ((RTS.|||)
             (RTS.pEnter "controller_change"
                (do (_43 :: VoiceMessage_3) <-
                      do RTS.pEnter "midi._Guard"
                           (_Guard (tag HS.== (RTS.lit 11 :: RTS.UInt 4)))
                         (controller :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                         RTS.pEnter "midi._Guard"
                           (_Guard (controller HS.<= (RTS.lit 119 :: RTS.UInt 7)))
                         (value :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                         HS.pure (VoiceMessage_3 controller value)
                    HS.pure (VoiceMessage_controller_change _43)))
             ((RTS.|||)
                (RTS.pEnter "program_change"
                   (do (_44 :: RTS.UInt 7) <-
                         do RTS.pEnter "midi._Guard"
                              (_Guard (tag HS.== (RTS.lit 12 :: RTS.UInt 4)))
                            (__ :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                            HS.pure __
                       HS.pure (VoiceMessage_program_change _44)))
                ((RTS.|||)
                   (RTS.pEnter "channel_pressure"
                      (do (_45 :: RTS.UInt 7) <-
                            do RTS.pEnter "midi._Guard"
                                 (_Guard (tag HS.== (RTS.lit 13 :: RTS.UInt 4)))
                               (__ :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                               HS.pure __
                          HS.pure (VoiceMessage_channel_pressure _45)))
                   (RTS.pEnter "pitch_bend"
                      (do (_46 :: RTS.UInt 14) <-
                            do RTS.pEnter "midi._Guard"
                                 (_Guard (tag HS.== (RTS.lit 14 :: RTS.UInt 4)))
                               (lsb :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                               (msb :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                               (__ :: RTS.UInt 14) <- HS.pure (RTS.cat msb lsb)
                               HS.pure __
                          HS.pure (VoiceMessage_pitch_bend _46))))))))
 
pVoiceMessages :: Parser VoiceMessages
 
pVoiceMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "56:16--56:20"
     (tag :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr status (RTS.lit 4 :: HS.Integer))
            :: RTS.UInt 4)
     (channel :: RTS.UInt 4) <-
       HS.pure (RTS.convert status :: RTS.UInt 4)
     (message :: VoiceMessage) <-
       RTS.pEnter "midi.VoiceMessage" (pVoiceMessage tag)
     (extra :: Vector.Vector (Delta VoiceMessage)) <-
       RTS.pMany (RTS.<||)
         (RTS.pEnter "midi.Delta"
            (pDelta @VoiceMessage
               (RTS.pEnter "midi.VoiceMessage" (pVoiceMessage tag))))
     HS.pure (VoiceMessages channel message extra)
 
pEvent :: Parser Event
 
pEvent =
  (RTS.|||)
    (RTS.pEnter "voiceMessage"
       (do (_47 :: VoiceMessages) <-
             RTS.pEnter "midi.VoiceMessages" pVoiceMessages
           HS.pure (Event_voiceMessage _47)))
    ((RTS.|||)
       (RTS.pEnter "modeMessage"
          (do (_48 :: ModeMessages) <-
                RTS.pEnter "midi.ModeMessages" pModeMessages
              HS.pure (Event_modeMessage _48)))
       ((RTS.|||)
          (RTS.pEnter "sysEx"
             (do (_49 :: SysEx) <- RTS.pEnter "midi.SysEx" pSysEx
                 HS.pure (Event_sysEx _49)))
          (RTS.pEnter "meta"
             (do (_50 :: Meta) <- RTS.pEnter "midi.Meta" pMeta
                 HS.pure (Event_meta _50)))))
 
pGuard :: HS.Bool -> Parser ()
 
pGuard (p :: HS.Bool) =
  RTS.pGuard "168:15--168:23" "guard failed" p
 
pTAG16 :: RTS.UInt 16 -> Parser ()
 
pTAG16 (n :: RTS.UInt 16) =
  do (_52 :: HS.Bool) <-
       do (_51 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
          HS.pure (_51 HS.== n)
     RTS.pEnter "midi.Guard" (pGuard _52)
 
pHeader :: Parser Header
 
pHeader =
  RTS.pEnter "midi.Chunk"
    (pChunk @(Vector.Vector (RTS.UInt 8)) @Header
       (RTS.pMatch "25:10--25:21" (Vector.vecFromRep "MThd"))
       (do (format :: Header_0) <-
             (RTS.|||)
               (RTS.pEnter "single_track"
                  (do (_53 :: ()) <-
                        RTS.pEnter "midi.TAG16" (pTAG16 (RTS.lit 0 :: RTS.UInt 16))
                      HS.pure (Header_0_single_track _53)))
               ((RTS.|||)
                  (RTS.pEnter "multi_track"
                     (do (_54 :: ()) <-
                           RTS.pEnter "midi.TAG16" (pTAG16 (RTS.lit 1 :: RTS.UInt 16))
                         HS.pure (Header_0_multi_track _54)))
                  (RTS.pEnter "multi_song"
                     (do (_55 :: ()) <-
                           RTS.pEnter "midi.TAG16" (pTAG16 (RTS.lit 2 :: RTS.UInt 16))
                         HS.pure (Header_0_multi_song _55))))
           (track_num :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
           (time_unit :: Header_1) <-
             do (w :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
                (tag :: RTS.UInt 1) <-
                  HS.pure (getBit @(RTS.UInt 16) (RTS.lit 15 :: HS.Integer) w)
                (__ :: Header_1) <-
                  (RTS.|||)
                    (RTS.pEnter "quarter_len"
                       (do (_56 :: RTS.UInt 15) <-
                             do RTS.pEnter "midi._Guard"
                                  (_Guard (tag HS.== (RTS.lit 0 :: RTS.UInt 1)))
                                (__ :: RTS.UInt 15) <- HS.pure (RTS.convert w :: RTS.UInt 15)
                                HS.pure __
                           HS.pure (Header_1_quarter_len _56)))
                    (RTS.pEnter "smtpe"
                       (do (_57 :: RTS.SInt 15) <-
                             do RTS.pEnter "midi._Guard"
                                  (_Guard (tag HS.== (RTS.lit 1 :: RTS.UInt 1)))
                                (__ :: RTS.SInt 15) <- HS.pure (RTS.convert w :: RTS.SInt 15)
                                HS.pure __
                           HS.pure (Header_1_smtpe _57)))
                HS.pure __
           HS.pure (Header format track_num time_unit)))
 
pTrack :: Parser (Vector.Vector (Delta Event))
 
pTrack =
  RTS.pEnter "midi.Chunk"
    (pChunk @(Vector.Vector (RTS.UInt 8))
       @(Vector.Vector (Delta Event))
       (RTS.pMatch "43:20--43:31" (Vector.vecFromRep "MTrk"))
       (RTS.pMany (RTS.<||)
          (RTS.pEnter "midi.Delta"
             (pDelta @Event (RTS.pEnter "midi.Event" pEvent)))))
 
pMain :: Parser Main
 
pMain =
  do (header :: Header) <- RTS.pEnter "midi.Header" pHeader
     (n :: HS.Integer) <-
       RTS.pIsJust "4:14--4:36" "Value does not fit in target type"
         (RTS.convertMaybe (HS.getField @"track_num" header)
            :: HS.Maybe HS.Integer)
     (tracks :: Vector.Vector (Vector.Vector (Delta Event))) <-
       Vector.replicateM n (RTS.pEnter "midi.Track" pTrack)
     HS.pure (Main header tracks)
 
_BE16 :: Parser ()
 
_BE16 =
  do HS.const () HS.<$> RTS.pByte "150:19--150:23"
     HS.const () HS.<$> RTS.pByte "150:27--150:31"
 
_BE24 :: Parser ()
 
_BE24 =
  do RTS.pEnter "midi._BE16" _BE16
     HS.const () HS.<$> RTS.pByte "151:27--151:31"
 
_BE32 :: Parser ()
 
_BE32 =
  do RTS.pEnter "midi._BE16" _BE16
     RTS.pEnter "midi._BE16" _BE16
 
_Chunk ::
  forall e i.
    (RTS.DDL e, RTS.DDL i) => Parser () -> (Parser () -> Parser ())
 
_Chunk (_P :: Parser ()) (_Ty :: Parser ()) =
  do RTS.pEnter "midi._Block"
       (_Block @e (RTS.lit 4 :: HS.Integer)
          (RTS.pEnter "midi._Only" (_Only @e _Ty)))
     (_9 :: HS.Integer) <-
       do (_8 :: RTS.UInt 32) <- RTS.pEnter "midi.BE32" pBE32
          RTS.pIsJust "19:12--19:22" "Value does not fit in target type"
            (RTS.convertMaybe _8 :: HS.Maybe HS.Integer)
     RTS.pEnter "midi._Block"
       (_Block @i _9 (RTS.pEnter "midi._Only" (_Only @i _P)))
 
_UInt7 :: Parser ()
 
_UInt7 =
  do (_10 :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "157:19--157:23"
     RTS.pIsJust_ "157:19--157:33" "Value does not fit in target type"
       (RTS.convertMaybe _10 :: HS.Maybe (RTS.UInt 7))
 
_VarQ :: Parser ()
 
_VarQ =
  do RTS.pSkipMany (RTS.<||)
       (do (b :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "11:25--11:29"
           RTS.pEnter "midi._Guard"
             (_Guard
                (getBit @(RTS.UInt 8) (RTS.lit 7 :: HS.Integer) b
                   HS.== (RTS.lit 1 :: RTS.UInt 1))))
     RTS.pEnter "midi._UInt7" _UInt7
 
_Delta :: forall c. RTS.DDL c => Parser () -> Parser ()
 
_Delta (_E :: Parser ()) =
  do RTS.pEnter "midi._VarQ" _VarQ
     _E
 
_Meta :: Parser ()
 
_Meta =
  do HS.const ()
       HS.<$> RTS.pMatch1 "108:5--108:15" (RTS.bcSingle (RTS.uint8 255))
     (_type :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "109:13--109:17"
     RTS.pEnter "midi._Guard"
       (_Guard (_type HS.<= (RTS.lit 127 :: RTS.UInt 8)))
     (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
     RTS.pEnter "midi._Block"
       (_Block @Meta len
          ((RTS.<||)
             (RTS.pEnter "sequence"
                (do RTS.pEnter "midi._Guard"
                      (_Guard (_type HS.== (RTS.lit 0 :: RTS.UInt 8)))
                    RTS.pEnter "midi._Only"
                      (_Only @(RTS.UInt 16) (RTS.pEnter "midi._BE16" _BE16))))
             ((RTS.<||)
                (RTS.pEnter "text"
                   (RTS.pEnter "midi._Guard"
                      (_Guard (_type HS.== (RTS.lit 1 :: RTS.UInt 8)))))
                ((RTS.<||)
                   (RTS.pEnter "copyright"
                      (RTS.pEnter "midi._Guard"
                         (_Guard (_type HS.== (RTS.lit 2 :: RTS.UInt 8)))))
                   ((RTS.<||)
                      (RTS.pEnter "name"
                         (RTS.pEnter "midi._Guard"
                            (_Guard (_type HS.== (RTS.lit 3 :: RTS.UInt 8)))))
                      ((RTS.<||)
                         (RTS.pEnter "instrument"
                            (RTS.pEnter "midi._Guard"
                               (_Guard (_type HS.== (RTS.lit 4 :: RTS.UInt 8)))))
                         ((RTS.<||)
                            (RTS.pEnter "lyrics"
                               (RTS.pEnter "midi._Guard"
                                  (_Guard (_type HS.== (RTS.lit 5 :: RTS.UInt 8)))))
                            ((RTS.<||)
                               (RTS.pEnter "marker"
                                  (RTS.pEnter "midi._Guard"
                                     (_Guard (_type HS.== (RTS.lit 6 :: RTS.UInt 8)))))
                               ((RTS.<||)
                                  (RTS.pEnter "cue"
                                     (RTS.pEnter "midi._Guard"
                                        (_Guard (_type HS.== (RTS.lit 7 :: RTS.UInt 8)))))
                                  ((RTS.<||)
                                     (RTS.pEnter "channel"
                                        (do RTS.pEnter "midi._Guard"
                                              (_Guard (_type HS.== (RTS.lit 32 :: RTS.UInt 8)))
                                            RTS.pEnter "midi._Only"
                                              (_Only @(RTS.UInt 8)
                                                 (HS.const () HS.<$> RTS.pByte "121:53--121:57"))))
                                     ((RTS.<||)
                                        (RTS.pEnter "end_track"
                                           (do RTS.pEnter "midi._Guard"
                                                 (_Guard (_type HS.== (RTS.lit 47 :: RTS.UInt 8)))
                                               RTS.pEnd "122:48--122:50"))
                                        ((RTS.<||)
                                           (RTS.pEnter "tempo"
                                              (do RTS.pEnter "midi._Guard"
                                                    (_Guard
                                                       (_type HS.== (RTS.lit 81 :: RTS.UInt 8)))
                                                  RTS.pEnter "midi._Only"
                                                    (_Only @(RTS.UInt 24)
                                                       (RTS.pEnter "midi._BE24" _BE24))))
                                           ((RTS.<||)
                                              (RTS.pEnter "smtpe_offset"
                                                 (do RTS.pEnter "midi._Guard"
                                                       (_Guard
                                                          (_type HS.== (RTS.lit 84 :: RTS.UInt 8)))
                                                     HS.const () HS.<$> RTS.pByte "125:31--125:35"
                                                     HS.const () HS.<$> RTS.pByte "125:43--125:47"
                                                     HS.const () HS.<$> RTS.pByte "125:55--125:59"
                                                     HS.const () HS.<$> RTS.pByte "125:67--125:71"
                                                     HS.const () HS.<$> RTS.pByte "126:31--126:35"
                                                     RTS.pEnd "126:38--126:40"))
                                              ((RTS.<||)
                                                 (RTS.pEnter "time_sig"
                                                    (do RTS.pEnter "midi._Guard"
                                                          (_Guard
                                                             (_type
                                                                HS.== (RTS.lit 88 :: RTS.UInt 8)))
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "128:31--128:35"
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "128:43--128:47"
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "128:55--128:59"
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "129:31--129:35"
                                                        RTS.pEnd "130:26--130:28"))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "key_sig"
                                                       (do RTS.pEnter "midi._Guard"
                                                             (_Guard
                                                                (_type
                                                                   HS.== (RTS.lit 89
                                                                            :: RTS.UInt 8)))
                                                           HS.const ()
                                                             HS.<$> RTS.pByte "133:33--133:37"
                                                           (RTS.|||)
                                                             (RTS.pEnter "major"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch1
                                                                            "135:50--135:57"
                                                                            (RTS.bcSingle
                                                                               (RTS.uint8 0))))
                                                             (RTS.pEnter "minor"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch1
                                                                            "135:68--135:75"
                                                                            (RTS.bcSingle
                                                                               (RTS.uint8 1))))
                                                           RTS.pEnd "136:26--136:28"))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "seq_specifiec"
                                                          (do RTS.pEnter "midi._Guard"
                                                                (_Guard
                                                                   (_type
                                                                      HS.== (RTS.lit 127
                                                                               :: RTS.UInt 8)))
                                                              (RTS.<||)
                                                                (do HS.const ()
                                                                      HS.<$> RTS.pMatch1
                                                                               "139:44--139:51"
                                                                               (RTS.bcSingle
                                                                                  (RTS.uint8 0))
                                                                    RTS.pEnter "midi._BE16" _BE16)
                                                                (HS.const ()
                                                                   HS.<$> RTS.pByte
                                                                            "140:49--140:53")))
                                                       (RTS.pEnter "unknown"
                                                          (HS.pure ()))))))))))))))))))
 
_ModeMessage :: Parser ()
 
_ModeMessage =
  (RTS.|||)
    (RTS.pEnter "all_sound_off"
       (HS.const ()
          HS.<$> RTS.pMatch "88:26--88:45"
                   (Vector.fromList
                      [RTS.lit 120 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
    ((RTS.|||)
       (RTS.pEnter "reset_controllers"
          (HS.const ()
             HS.<$> RTS.pMatch "89:26--89:45"
                      (Vector.fromList
                         [RTS.lit 121 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
       ((RTS.|||)
          (RTS.pEnter "local_control_off"
             (HS.const ()
                HS.<$> RTS.pMatch "90:26--90:45"
                         (Vector.fromList
                            [RTS.lit 122 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
          ((RTS.|||)
             (RTS.pEnter "local_control_on"
                (HS.const ()
                   HS.<$> RTS.pMatch "91:26--91:45"
                            (Vector.fromList
                               [RTS.lit 122 :: RTS.UInt 8, RTS.lit 127 :: RTS.UInt 8])))
             ((RTS.|||)
                (RTS.pEnter "all_notes_off"
                   (HS.const ()
                      HS.<$> RTS.pMatch "92:26--92:45"
                               (Vector.fromList
                                  [RTS.lit 123 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
                ((RTS.|||)
                   (RTS.pEnter "omni_off"
                      (HS.const ()
                         HS.<$> RTS.pMatch "93:26--93:45"
                                  (Vector.fromList
                                     [RTS.lit 124 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
                   ((RTS.|||)
                      (RTS.pEnter "omni_on"
                         (HS.const ()
                            HS.<$> RTS.pMatch "94:26--94:45"
                                     (Vector.fromList
                                        [RTS.lit 125 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
                      ((RTS.|||)
                         (RTS.pEnter "mono_on"
                            (do HS.const ()
                                  HS.<$> RTS.pMatch1 "95:27--95:37" (RTS.bcSingle (RTS.uint8 126))
                                (__ :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "95:45--95:49"
                                RTS.pEnter "midi._Guard"
                                  (_Guard (__ HS.<= (RTS.lit 16 :: RTS.UInt 8)))))
                         (RTS.pEnter "poly_on"
                            (HS.const ()
                               HS.<$> RTS.pMatch "96:26--96:45"
                                        (Vector.fromList
                                           [RTS.lit 127 :: RTS.UInt 8,
                                            RTS.lit 0 :: RTS.UInt 8]))))))))))
 
_ModeMessages :: Parser ()
 
_ModeMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "78:15--78:19"
     (tag :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr status (RTS.lit 4 :: HS.Integer))
            :: RTS.UInt 4)
     RTS.pEnter "midi._Guard"
       (_Guard (tag HS.== (RTS.lit 11 :: RTS.UInt 4)))
     RTS.pEnter "midi._ModeMessage" _ModeMessage
     RTS.pSkipMany (RTS.<||)
       (RTS.pEnter "midi._Delta"
          (_Delta @ModeMessage
             (RTS.pEnter "midi._ModeMessage" _ModeMessage)))
 
_SysEx :: Parser ()
 
_SysEx =
  (RTS.|||)
    (RTS.pEnter "add_f0"
       (do HS.const ()
             HS.<$> RTS.pMatch1 "102:16--102:26" (RTS.bcSingle (RTS.uint8 240))
           (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
           RTS.pEnter "midi._Block" (_Block @RTS.Input len (HS.pure ()))))
    (RTS.pEnter "as_is"
       (do HS.const ()
             HS.<$> RTS.pMatch1 "103:16--103:26" (RTS.bcSingle (RTS.uint8 247))
           (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
           RTS.pEnter "midi._Block" (_Block @RTS.Input len (HS.pure ()))))
 
_VoiceMessage :: RTS.UInt 4 -> Parser ()
 
_VoiceMessage (tag :: RTS.UInt 4) =
  (RTS.|||)
    (RTS.pEnter "note_off"
       (do RTS.pEnter "midi._Guard"
             (_Guard (tag HS.== (RTS.lit 8 :: RTS.UInt 4)))
           RTS.pEnter "midi._UInt7" _UInt7
           RTS.pEnter "midi._UInt7" _UInt7))
    ((RTS.|||)
       (RTS.pEnter "note_on"
          (do RTS.pEnter "midi._Guard"
                (_Guard (tag HS.== (RTS.lit 9 :: RTS.UInt 4)))
              RTS.pEnter "midi._UInt7" _UInt7
              RTS.pEnter "midi._UInt7" _UInt7))
       ((RTS.|||)
          (RTS.pEnter "aftertouch"
             (do RTS.pEnter "midi._Guard"
                   (_Guard (tag HS.== (RTS.lit 10 :: RTS.UInt 4)))
                 RTS.pEnter "midi._UInt7" _UInt7
                 RTS.pEnter "midi._UInt7" _UInt7))
          ((RTS.|||)
             (RTS.pEnter "controller_change"
                (do RTS.pEnter "midi._Guard"
                      (_Guard (tag HS.== (RTS.lit 11 :: RTS.UInt 4)))
                    (controller :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                    RTS.pEnter "midi._Guard"
                      (_Guard (controller HS.<= (RTS.lit 119 :: RTS.UInt 7)))
                    RTS.pEnter "midi._UInt7" _UInt7))
             ((RTS.|||)
                (RTS.pEnter "program_change"
                   (do RTS.pEnter "midi._Guard"
                         (_Guard (tag HS.== (RTS.lit 12 :: RTS.UInt 4)))
                       RTS.pEnter "midi._UInt7" _UInt7))
                ((RTS.|||)
                   (RTS.pEnter "channel_pressure"
                      (do RTS.pEnter "midi._Guard"
                            (_Guard (tag HS.== (RTS.lit 13 :: RTS.UInt 4)))
                          RTS.pEnter "midi._UInt7" _UInt7))
                   (RTS.pEnter "pitch_bend"
                      (do RTS.pEnter "midi._Guard"
                            (_Guard (tag HS.== (RTS.lit 14 :: RTS.UInt 4)))
                          RTS.pEnter "midi._UInt7" _UInt7
                          RTS.pEnter "midi._UInt7" _UInt7)))))))
 
_VoiceMessages :: Parser ()
 
_VoiceMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "56:16--56:20"
     (tag :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr status (RTS.lit 4 :: HS.Integer))
            :: RTS.UInt 4)
     RTS.pEnter "midi._VoiceMessage" (_VoiceMessage tag)
     RTS.pSkipMany (RTS.<||)
       (RTS.pEnter "midi._Delta"
          (_Delta @VoiceMessage
             (RTS.pEnter "midi._VoiceMessage" (_VoiceMessage tag))))
 
_Event :: Parser ()
 
_Event =
  (RTS.|||)
    (RTS.pEnter "voiceMessage"
       (RTS.pEnter "midi._VoiceMessages" _VoiceMessages))
    ((RTS.|||)
       (RTS.pEnter "modeMessage"
          (RTS.pEnter "midi._ModeMessages" _ModeMessages))
       ((RTS.|||) (RTS.pEnter "sysEx" (RTS.pEnter "midi._SysEx" _SysEx))
          (RTS.pEnter "meta" (RTS.pEnter "midi._Meta" _Meta))))
 
_TAG16 :: RTS.UInt 16 -> Parser ()
 
_TAG16 (n :: RTS.UInt 16) =
  do (_52 :: HS.Bool) <-
       do (_51 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
          HS.pure (_51 HS.== n)
     RTS.pEnter "midi._Guard" (_Guard _52)
 
_Header :: Parser ()
 
_Header =
  RTS.pEnter "midi._Chunk"
    (_Chunk @(Vector.Vector (RTS.UInt 8)) @Header
       (do (RTS.|||)
             (RTS.pEnter "single_track"
                (RTS.pEnter "midi._TAG16" (_TAG16 (RTS.lit 0 :: RTS.UInt 16))))
             ((RTS.|||)
                (RTS.pEnter "multi_track"
                   (RTS.pEnter "midi._TAG16" (_TAG16 (RTS.lit 1 :: RTS.UInt 16))))
                (RTS.pEnter "multi_song"
                   (RTS.pEnter "midi._TAG16" (_TAG16 (RTS.lit 2 :: RTS.UInt 16)))))
           RTS.pEnter "midi._BE16" _BE16
           (w :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
           (tag :: RTS.UInt 1) <-
             HS.pure (getBit @(RTS.UInt 16) (RTS.lit 15 :: HS.Integer) w)
           (RTS.|||)
             (RTS.pEnter "quarter_len"
                (RTS.pEnter "midi._Guard"
                   (_Guard (tag HS.== (RTS.lit 0 :: RTS.UInt 1)))))
             (RTS.pEnter "smtpe"
                (RTS.pEnter "midi._Guard"
                   (_Guard (tag HS.== (RTS.lit 1 :: RTS.UInt 1))))))
       (HS.const ()
          HS.<$> RTS.pMatch "25:10--25:21" (Vector.vecFromRep "MThd")))
 
_Track :: Parser ()
 
_Track =
  RTS.pEnter "midi._Chunk"
    (_Chunk @(Vector.Vector (RTS.UInt 8))
       @(Vector.Vector (Delta Event))
       (RTS.pSkipMany (RTS.<||)
          (RTS.pEnter "midi._Delta"
             (_Delta @Event (RTS.pEnter "midi._Event" _Event))))
       (HS.const ()
          HS.<$> RTS.pMatch "43:20--43:31" (Vector.vecFromRep "MTrk")))
 
_Main :: Parser ()
 
_Main =
  do (header :: Header) <- RTS.pEnter "midi.Header" pHeader
     (n :: HS.Integer) <-
       RTS.pIsJust "4:14--4:36" "Value does not fit in target type"
         (RTS.convertMaybe (HS.getField @"track_num" header)
            :: HS.Maybe HS.Integer)
     RTS.pSkipExact n (RTS.pEnter "midi._Track" _Track)