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
import qualified RTS.Vector as Vector
import qualified RTS.Map as Map
 
 
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
 
data ModeMessage_0
  = ModeMessage_0_off ()
  | ModeMessage_0_on ()
  
 
deriving instance HS.Eq ModeMessage_0
 
deriving instance HS.Ord ModeMessage_0
 
deriving instance HS.Show ModeMessage_0
 
instance RTS.DDL ModeMessage_0 where
 
instance HS.HasField "off" ModeMessage_0 (HS.Maybe ()) where
  getField (ModeMessage_0_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "on" ModeMessage_0 (HS.Maybe ()) where
  getField (ModeMessage_0_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
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
 
data Delta a
  = Delta HS.Integer a
  
 
deriving instance HS.Eq a => HS.Eq (Delta a)
 
deriving instance HS.Ord a => HS.Ord (Delta a)
 
deriving instance HS.Show a => HS.Show (Delta a)
 
instance RTS.DDL a => RTS.DDL (Delta a) where
 
instance HS.HasField "after" (Delta a) HS.Integer where
  getField (Delta x _) = x
 
instance HS.HasField "event" (Delta a) a where
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
  = ModeMessage_all_notes_off (RTS.UInt 8)
  | ModeMessage_all_sound_off (RTS.UInt 8)
  | ModeMessage_local_control ModeMessage_0
  | ModeMessage_mono_on (RTS.UInt 8)
  | ModeMessage_omni_off (RTS.UInt 8)
  | ModeMessage_omni_on (RTS.UInt 8)
  | ModeMessage_poly_on (RTS.UInt 8)
  | ModeMessage_reset_controllers (RTS.UInt 8)
  
 
deriving instance HS.Eq ModeMessage
 
deriving instance HS.Ord ModeMessage
 
deriving instance HS.Show ModeMessage
 
instance RTS.DDL ModeMessage where
 
instance HS.HasField "all_notes_off" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_all_notes_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "all_sound_off" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_all_sound_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "local_control" ModeMessage
           (HS.Maybe ModeMessage_0) where
  getField (ModeMessage_local_control x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "mono_on" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_mono_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "omni_off" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_omni_off x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "omni_on" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_omni_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "poly_on" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
  getField (ModeMessage_poly_on x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "reset_controllers" ModeMessage
           (HS.Maybe (RTS.UInt 8)) where
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
  do (b1 :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "149:27--149:31"
     (b2 :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "149:40--149:44"
     (__ :: RTS.UInt 16) <- HS.pure (RTS.cat b1 b2)
     HS.pure __
 
pBE24 :: Parser (RTS.UInt 24)
 
pBE24 =
  do (w1 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     (b2 :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "150:40--150:44"
     (__ :: RTS.UInt 24) <- HS.pure (RTS.cat w1 b2)
     HS.pure __
 
pBE32 :: Parser (RTS.UInt 32)
 
pBE32 =
  do (w1 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     (w2 :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     (__ :: RTS.UInt 32) <- HS.pure (RTS.cat w1 w2)
     HS.pure __
 
pBlock ::
  forall b. RTS.DDL b => HS.Integer -> (Parser b -> Parser b)
 
pBlock (n :: HS.Integer) (pP :: Parser b) =
  do (cur :: RTS.Input) <- RTS.pPeek
     (this :: RTS.Input) <-
       RTS.pIsJust "160:13--160:18" "Not enough bytes"
         (RTS.limitLen n cur)
     RTS.pSetInput this
     (__ :: b) <- pP
     (next :: RTS.Input) <-
       RTS.pIsJust "163:13--163:18" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput next
     HS.pure __
 
pOnly :: forall a. RTS.DDL a => Parser a -> Parser a
 
pOnly (pP :: Parser a) =
  do (__ :: a) <- pP
     RTS.pEnd "167:24--167:26"
     HS.pure __
 
_Block ::
  forall b. RTS.DDL b => HS.Integer -> (Parser () -> Parser ())
 
_Block (n :: HS.Integer) (_P :: Parser ()) =
  do (cur :: RTS.Input) <- RTS.pPeek
     (this :: RTS.Input) <-
       RTS.pIsJust "160:13--160:18" "Not enough bytes"
         (RTS.limitLen n cur)
     RTS.pSetInput this
     _P
     (next :: RTS.Input) <-
       RTS.pIsJust "163:13--163:18" "Not enough bytes"
         (RTS.advanceBy n cur)
     RTS.pSetInput next
 
_Only :: forall a. RTS.DDL a => Parser () -> Parser ()
 
_Only (_P :: Parser ()) =
  do _P
     RTS.pEnd "167:24--167:26"
 
pChunk ::
  forall a b.
    (RTS.DDL a, RTS.DDL b) => Parser a -> (Parser b -> Parser b)
 
pChunk (pTy :: Parser a) (pP :: Parser b) =
  do RTS.pEnter "midi._Block"
       (_Block @a (RTS.lit 4 :: HS.Integer)
          (RTS.pEnter "midi._Only"
             (_Only @a
                (do HS.void pTy
                    HS.pure ()))))
     (len :: RTS.UInt 32) <- RTS.pEnter "midi.BE32" pBE32
     (__ :: b) <-
       RTS.pEnter "midi.Block"
         (pBlock @b (RTS.convert len :: HS.Integer)
            (RTS.pEnter "midi.Only" (pOnly @b pP)))
     HS.pure __
 
pUInt7 :: Parser (RTS.UInt 7)
 
pUInt7 =
  do (b :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "156:26--156:30"
     (__ :: RTS.UInt 7) <-
       RTS.pIsJust "156:33--156:43" "Value does not fit in target type"
         (RTS.convertMaybe b :: HS.Maybe (RTS.UInt 7))
     HS.pure __
 
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
             RTS.pGuard "11:32--11:46" "guard failed"
               (getBit @(RTS.UInt 8) (RTS.lit 7 :: HS.Integer) b
                  HS.== (RTS.lit 1 :: RTS.UInt 1))
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
 
pDelta :: forall a. RTS.DDL a => Parser a -> Parser (Delta a)
 
pDelta (pE :: Parser a) =
  do (after :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
     (event :: a) <- pE
     HS.pure (Delta after event)
 
pMeta :: Parser Meta
 
pMeta =
  do HS.const ()
       HS.<$> RTS.pMatch1 "107:5--107:8" (RTS.bcSingle (RTS.uint8 255))
     (_type :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "108:13--108:17"
     RTS.pGuard "108:20--108:31" "guard failed"
       (_type HS.<= (RTS.lit 127 :: RTS.UInt 8))
     (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
     (__ :: Meta) <-
       RTS.pEnter "midi.Block"
         (pBlock @Meta len
            ((RTS.<||)
               (RTS.pEnter "sequence"
                  (do (_0 :: RTS.UInt 16) <-
                        do RTS.pGuard "112:26--112:37" "guard failed"
                             (_type HS.== (RTS.lit 0 :: RTS.UInt 8))
                           (__ :: RTS.UInt 16) <-
                             RTS.pEnter "midi.Only"
                               (pOnly @(RTS.UInt 16) (RTS.pEnter "midi.BE16" pBE16))
                           HS.pure __
                      HS.pure (Meta_sequence _0)))
               ((RTS.<||)
                  (RTS.pEnter "text"
                     (do (_1 :: RTS.Input) <-
                           do RTS.pGuard "113:26--113:37" "guard failed"
                                (_type HS.== (RTS.lit 1 :: RTS.UInt 8))
                              (__ :: RTS.Input) <- RTS.pPeek
                              HS.pure __
                         HS.pure (Meta_text _1)))
                  ((RTS.<||)
                     (RTS.pEnter "copyright"
                        (do (_2 :: RTS.Input) <-
                              do RTS.pGuard "114:26--114:37" "guard failed"
                                   (_type HS.== (RTS.lit 2 :: RTS.UInt 8))
                                 (__ :: RTS.Input) <- RTS.pPeek
                                 HS.pure __
                            HS.pure (Meta_copyright _2)))
                     ((RTS.<||)
                        (RTS.pEnter "name"
                           (do (_3 :: RTS.Input) <-
                                 do RTS.pGuard "115:26--115:37" "guard failed"
                                      (_type HS.== (RTS.lit 3 :: RTS.UInt 8))
                                    (__ :: RTS.Input) <- RTS.pPeek
                                    HS.pure __
                               HS.pure (Meta_name _3)))
                        ((RTS.<||)
                           (RTS.pEnter "instrument"
                              (do (_4 :: RTS.Input) <-
                                    do RTS.pGuard "116:26--116:37" "guard failed"
                                         (_type HS.== (RTS.lit 4 :: RTS.UInt 8))
                                       (__ :: RTS.Input) <- RTS.pPeek
                                       HS.pure __
                                  HS.pure (Meta_instrument _4)))
                           ((RTS.<||)
                              (RTS.pEnter "lyrics"
                                 (do (_5 :: RTS.Input) <-
                                       do RTS.pGuard "117:26--117:37" "guard failed"
                                            (_type HS.== (RTS.lit 5 :: RTS.UInt 8))
                                          (__ :: RTS.Input) <- RTS.pPeek
                                          HS.pure __
                                     HS.pure (Meta_lyrics _5)))
                              ((RTS.<||)
                                 (RTS.pEnter "marker"
                                    (do (_6 :: RTS.Input) <-
                                          do RTS.pGuard "118:26--118:37" "guard failed"
                                               (_type HS.== (RTS.lit 6 :: RTS.UInt 8))
                                             (__ :: RTS.Input) <- RTS.pPeek
                                             HS.pure __
                                        HS.pure (Meta_marker _6)))
                                 ((RTS.<||)
                                    (RTS.pEnter "cue"
                                       (do (_7 :: RTS.Input) <-
                                             do RTS.pGuard "119:26--119:37" "guard failed"
                                                  (_type HS.== (RTS.lit 7 :: RTS.UInt 8))
                                                (__ :: RTS.Input) <- RTS.pPeek
                                                HS.pure __
                                           HS.pure (Meta_cue _7)))
                                    ((RTS.<||)
                                       (RTS.pEnter "channel"
                                          (do (_8 :: RTS.UInt 8) <-
                                                do RTS.pGuard "120:26--120:37" "guard failed"
                                                     (_type HS.== (RTS.lit 32 :: RTS.UInt 8))
                                                   (__ :: RTS.UInt 8) <-
                                                     RTS.pEnter "midi.Only"
                                                       (pOnly @(RTS.UInt 8)
                                                          (RTS.uint8
                                                             HS.<$> RTS.pByte "120:45--120:49"))
                                                   HS.pure __
                                              HS.pure (Meta_channel _8)))
                                       ((RTS.<||)
                                          (RTS.pEnter "end_track"
                                             (do (_9 :: ()) <-
                                                   do RTS.pGuard "121:26--121:37" "guard failed"
                                                        (_type HS.== (RTS.lit 47 :: RTS.UInt 8))
                                                      (__ :: ()) <- RTS.pEnd "121:40--121:42"
                                                      HS.pure __
                                                 HS.pure (Meta_end_track _9)))
                                          ((RTS.<||)
                                             (RTS.pEnter "tempo"
                                                (do (_10 :: RTS.UInt 24) <-
                                                      do RTS.pGuard "122:26--122:37" "guard failed"
                                                           (_type HS.== (RTS.lit 81 :: RTS.UInt 8))
                                                         (__ :: RTS.UInt 24) <-
                                                           RTS.pEnter "midi.Only"
                                                             (pOnly @(RTS.UInt 24)
                                                                (RTS.pEnter "midi.BE24" pBE24))
                                                         HS.pure __
                                                    HS.pure (Meta_tempo _10)))
                                             ((RTS.<||)
                                                (RTS.pEnter "smtpe_offset"
                                                   (do (_11 :: Meta_0) <-
                                                         do RTS.pGuard "123:26--123:37"
                                                              "guard failed"
                                                              (_type
                                                                 HS.== (RTS.lit 84 :: RTS.UInt 8))
                                                            (hh :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "124:31--124:35"
                                                            (mm :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "124:43--124:47"
                                                            (ss :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "124:55--124:59"
                                                            (fr :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "124:67--124:71"
                                                            (ff :: RTS.UInt 8) <-
                                                              RTS.uint8
                                                                HS.<$> RTS.pByte "125:31--125:35"
                                                            RTS.pEnd "125:38--125:40"
                                                            HS.pure (Meta_0 hh mm ss fr ff)
                                                       HS.pure (Meta_smtpe_offset _11)))
                                                ((RTS.<||)
                                                   (RTS.pEnter "time_sig"
                                                      (do (_12 :: Meta_1) <-
                                                            do RTS.pGuard "126:26--126:37"
                                                                 "guard failed"
                                                                 (_type
                                                                    HS.== (RTS.lit 88
                                                                             :: RTS.UInt 8))
                                                               (nn :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "127:31--127:35"
                                                               (dd :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "127:43--127:47"
                                                               (cc :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "127:55--127:59"
                                                               (bb :: RTS.UInt 8) <-
                                                                 RTS.uint8
                                                                   HS.<$> RTS.pByte "128:31--128:35"
                                                               RTS.pEnd "129:26--129:28"
                                                               HS.pure (Meta_1 nn dd cc bb)
                                                          HS.pure (Meta_time_sig _12)))
                                                   ((RTS.<||)
                                                      (RTS.pEnter "key_sig"
                                                         (do (_15 :: Meta_3) <-
                                                               do RTS.pGuard "131:26--131:37"
                                                                    "guard failed"
                                                                    (_type
                                                                       HS.== (RTS.lit 89
                                                                                :: RTS.UInt 8))
                                                                  (key :: RTS.UInt 8) <-
                                                                    RTS.uint8
                                                                      HS.<$> RTS.pByte
                                                                               "132:33--132:37"
                                                                  (mode :: Meta_2) <-
                                                                    (RTS.|||)
                                                                      (RTS.pEnter "major"
                                                                         (do (_13 :: RTS.UInt 8) <-
                                                                               RTS.uint8
                                                                                 HS.<$> RTS.pMatch1
                                                                                          "134:50--134:50"
                                                                                          (RTS.bcSingle
                                                                                             (RTS.uint8
                                                                                                0))
                                                                             HS.pure
                                                                               (Meta_2_major _13)))
                                                                      (RTS.pEnter "minor"
                                                                         (do (_14 :: RTS.UInt 8) <-
                                                                               RTS.uint8
                                                                                 HS.<$> RTS.pMatch1
                                                                                          "134:61--134:61"
                                                                                          (RTS.bcSingle
                                                                                             (RTS.uint8
                                                                                                1))
                                                                             HS.pure
                                                                               (Meta_2_minor _14)))
                                                                  RTS.pEnd "135:26--135:28"
                                                                  HS.pure (Meta_3 key mode)
                                                             HS.pure (Meta_key_sig _15)))
                                                      ((RTS.<||)
                                                         (RTS.pEnter "seq_specifiec"
                                                            (do (_16 :: Meta_4) <-
                                                                  do RTS.pGuard "137:27--137:38"
                                                                       "guard failed"
                                                                       (_type
                                                                          HS.== (RTS.lit 127
                                                                                   :: RTS.UInt 8))
                                                                     (manufacturer
                                                                        :: RTS.UInt 16) <-
                                                                       (RTS.<||)
                                                                         (do HS.const ()
                                                                               HS.<$> RTS.pMatch1
                                                                                        "138:44--138:44"
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
                                                                                          "139:49--139:53"
                                                                             (__ :: RTS.UInt 16) <-
                                                                               HS.pure
                                                                                 (RTS.convert b
                                                                                    :: RTS.UInt 16)
                                                                             HS.pure __)
                                                                     (_data :: RTS.Input) <-
                                                                       RTS.pPeek
                                                                     HS.pure
                                                                       (Meta_4 manufacturer _data)
                                                                HS.pure (Meta_seq_specifiec _16)))
                                                         (RTS.pEnter "unknown"
                                                            (do (_17 :: Meta_5) <-
                                                                  do (_type :: RTS.UInt 8) <-
                                                                       HS.pure _type
                                                                     (_data :: RTS.Input) <-
                                                                       RTS.pPeek
                                                                     HS.pure (Meta_5 _type _data)
                                                                HS.pure
                                                                  (Meta_unknown
                                                                     _17)))))))))))))))))))
     HS.pure __
 
pModeMessage :: Parser ModeMessage
 
pModeMessage =
  (RTS.|||)
    (RTS.pEnter "all_sound_off"
       (do (_18 :: RTS.UInt 8) <-
             do HS.const ()
                  HS.<$> RTS.pMatch1 "88:27--88:30" (RTS.bcSingle (RTS.uint8 120))
                (__ :: RTS.UInt 8) <-
                  RTS.uint8
                    HS.<$> RTS.pMatch1 "88:33--88:36" (RTS.bcSingle (RTS.uint8 0))
                HS.pure __
           HS.pure (ModeMessage_all_sound_off _18)))
    ((RTS.|||)
       (RTS.pEnter "reset_controllers"
          (do (_19 :: RTS.UInt 8) <-
                do HS.const ()
                     HS.<$> RTS.pMatch1 "89:27--89:30" (RTS.bcSingle (RTS.uint8 121))
                   (__ :: RTS.UInt 8) <-
                     RTS.uint8
                       HS.<$> RTS.pMatch1 "89:33--89:36" (RTS.bcSingle (RTS.uint8 0))
                   HS.pure __
              HS.pure (ModeMessage_reset_controllers _19)))
       ((RTS.|||)
          (RTS.pEnter "local_control"
             (do (_22 :: ModeMessage_0) <-
                   do HS.const ()
                        HS.<$> RTS.pMatch1 "90:27--90:30" (RTS.bcSingle (RTS.uint8 122))
                      (__ :: ModeMessage_0) <-
                        (RTS.|||)
                          (RTS.pEnter "off"
                             (do (_20 :: ()) <-
                                   HS.const ()
                                     HS.<$> RTS.pMatch1 "90:49--90:52" (RTS.bcSingle (RTS.uint8 0))
                                 HS.pure (ModeMessage_0_off _20)))
                          (RTS.pEnter "on"
                             (do (_21 :: ()) <-
                                   HS.const ()
                                     HS.<$> RTS.pMatch1 "90:61--90:64"
                                              (RTS.bcSingle (RTS.uint8 127))
                                 HS.pure (ModeMessage_0_on _21)))
                      HS.pure __
                 HS.pure (ModeMessage_local_control _22)))
          ((RTS.|||)
             (RTS.pEnter "all_notes_off"
                (do (_23 :: RTS.UInt 8) <-
                      do HS.const ()
                           HS.<$> RTS.pMatch1 "91:27--91:30" (RTS.bcSingle (RTS.uint8 123))
                         (__ :: RTS.UInt 8) <-
                           RTS.uint8
                             HS.<$> RTS.pMatch1 "91:33--91:36" (RTS.bcSingle (RTS.uint8 0))
                         HS.pure __
                    HS.pure (ModeMessage_all_notes_off _23)))
             ((RTS.|||)
                (RTS.pEnter "omni_off"
                   (do (_24 :: RTS.UInt 8) <-
                         do HS.const ()
                              HS.<$> RTS.pMatch1 "92:27--92:30" (RTS.bcSingle (RTS.uint8 124))
                            (__ :: RTS.UInt 8) <-
                              RTS.uint8
                                HS.<$> RTS.pMatch1 "92:33--92:36" (RTS.bcSingle (RTS.uint8 0))
                            HS.pure __
                       HS.pure (ModeMessage_omni_off _24)))
                ((RTS.|||)
                   (RTS.pEnter "omni_on"
                      (do (_25 :: RTS.UInt 8) <-
                            do HS.const ()
                                 HS.<$> RTS.pMatch1 "93:27--93:30" (RTS.bcSingle (RTS.uint8 125))
                               (__ :: RTS.UInt 8) <-
                                 RTS.uint8
                                   HS.<$> RTS.pMatch1 "93:33--93:36" (RTS.bcSingle (RTS.uint8 0))
                               HS.pure __
                          HS.pure (ModeMessage_omni_on _25)))
                   ((RTS.|||)
                      (RTS.pEnter "mono_on"
                         (do (_26 :: RTS.UInt 8) <-
                               do HS.const ()
                                    HS.<$> RTS.pMatch1 "94:27--94:30" (RTS.bcSingle (RTS.uint8 126))
                                  (__ :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "94:38--94:42"
                                  RTS.pGuard "94:45--94:54" "guard failed"
                                    (__ HS.<= (RTS.lit 16 :: RTS.UInt 8))
                                  HS.pure __
                             HS.pure (ModeMessage_mono_on _26)))
                      (RTS.pEnter "poly_on"
                         (do (_27 :: RTS.UInt 8) <-
                               do HS.const ()
                                    HS.<$> RTS.pMatch1 "95:27--95:30" (RTS.bcSingle (RTS.uint8 127))
                                  (__ :: RTS.UInt 8) <-
                                    RTS.uint8
                                      HS.<$> RTS.pMatch1 "95:33--95:36" (RTS.bcSingle (RTS.uint8 0))
                                  HS.pure __
                             HS.pure (ModeMessage_poly_on _27)))))))))
 
pModeMessages :: Parser ModeMessages
 
pModeMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "78:15--78:19"
     (tag :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr status (RTS.lit 4 :: HS.Integer))
            :: RTS.UInt 4)
     RTS.pGuard "80:5--80:14" "guard failed"
       (tag HS.== (RTS.lit 11 :: RTS.UInt 4))
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
       (do (_28 :: RTS.Input) <-
             do HS.const ()
                  HS.<$> RTS.pMatch1 "101:16--101:19" (RTS.bcSingle (RTS.uint8 240))
                (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
                (__ :: RTS.Input) <-
                  RTS.pEnter "midi.Block" (pBlock @RTS.Input len RTS.pPeek)
                HS.pure __
           HS.pure (SysEx_add_f0 _28)))
    (RTS.pEnter "as_is"
       (do (_29 :: RTS.Input) <-
             do HS.const ()
                  HS.<$> RTS.pMatch1 "102:16--102:19" (RTS.bcSingle (RTS.uint8 247))
                (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
                (__ :: RTS.Input) <-
                  RTS.pEnter "midi.Block" (pBlock @RTS.Input len RTS.pPeek)
                HS.pure __
           HS.pure (SysEx_as_is _29)))
 
pVoiceMessage :: RTS.UInt 4 -> Parser VoiceMessage
 
pVoiceMessage (tag :: RTS.UInt 4) =
  (RTS.|||)
    (RTS.pEnter "note_off"
       (do (_30 :: VoiceMessage_0) <-
             do RTS.pGuard "66:27--66:36" "guard failed"
                  (tag HS.== (RTS.lit 8 :: RTS.UInt 4))
                (key :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                (velocity :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                HS.pure (VoiceMessage_0 key velocity)
           HS.pure (VoiceMessage_note_off _30)))
    ((RTS.|||)
       (RTS.pEnter "note_on"
          (do (_31 :: VoiceMessage_1) <-
                do RTS.pGuard "67:27--67:36" "guard failed"
                     (tag HS.== (RTS.lit 9 :: RTS.UInt 4))
                   (key :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                   (velocity :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                   HS.pure (VoiceMessage_1 key velocity)
              HS.pure (VoiceMessage_note_on _31)))
       ((RTS.|||)
          (RTS.pEnter "aftertouch"
             (do (_32 :: VoiceMessage_2) <-
                   do RTS.pGuard "68:27--68:36" "guard failed"
                        (tag HS.== (RTS.lit 10 :: RTS.UInt 4))
                      (key :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                      (pressure :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                      HS.pure (VoiceMessage_2 key pressure)
                 HS.pure (VoiceMessage_aftertouch _32)))
          ((RTS.|||)
             (RTS.pEnter "controller_change"
                (do (_33 :: VoiceMessage_3) <-
                      do RTS.pGuard "69:27--69:36" "guard failed"
                           (tag HS.== (RTS.lit 11 :: RTS.UInt 4))
                         (controller :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                         RTS.pGuard "70:47--70:64" "guard failed"
                           (controller HS.<= (RTS.lit 119 :: RTS.UInt 7))
                         (value :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                         HS.pure (VoiceMessage_3 controller value)
                    HS.pure (VoiceMessage_controller_change _33)))
             ((RTS.|||)
                (RTS.pEnter "program_change"
                   (do (_34 :: RTS.UInt 7) <-
                         do RTS.pGuard "72:27--72:36" "guard failed"
                              (tag HS.== (RTS.lit 12 :: RTS.UInt 4))
                            (__ :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                            HS.pure __
                       HS.pure (VoiceMessage_program_change _34)))
                ((RTS.|||)
                   (RTS.pEnter "channel_pressure"
                      (do (_35 :: RTS.UInt 7) <-
                            do RTS.pGuard "73:27--73:36" "guard failed"
                                 (tag HS.== (RTS.lit 13 :: RTS.UInt 4))
                               (__ :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                               HS.pure __
                          HS.pure (VoiceMessage_channel_pressure _35)))
                   (RTS.pEnter "pitch_bend"
                      (do (_36 :: RTS.UInt 14) <-
                            do RTS.pGuard "74:27--74:36" "guard failed"
                                 (tag HS.== (RTS.lit 14 :: RTS.UInt 4))
                               (lsb :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                               (msb :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                               (__ :: RTS.UInt 14) <- HS.pure (RTS.cat msb lsb)
                               HS.pure __
                          HS.pure (VoiceMessage_pitch_bend _36))))))))
 
pVoiceMessages :: Parser VoiceMessages
 
pVoiceMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "57:16--57:20"
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
       (do (_37 :: VoiceMessages) <-
             RTS.pEnter "midi.VoiceMessages" pVoiceMessages
           HS.pure (Event_voiceMessage _37)))
    ((RTS.|||)
       (RTS.pEnter "modeMessage"
          (do (_38 :: ModeMessages) <-
                RTS.pEnter "midi.ModeMessages" pModeMessages
              HS.pure (Event_modeMessage _38)))
       ((RTS.|||)
          (RTS.pEnter "sysEx"
             (do (_39 :: SysEx) <- RTS.pEnter "midi.SysEx" pSysEx
                 HS.pure (Event_sysEx _39)))
          (RTS.pEnter "meta"
             (do (_40 :: Meta) <- RTS.pEnter "midi.Meta" pMeta
                 HS.pure (Event_meta _40)))))
 
pTAG16 :: RTS.UInt 16 -> Parser ()
 
pTAG16 (n :: RTS.UInt 16) =
  do (b :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     (__ :: ()) <-
       RTS.pGuard "152:33--152:38" "guard failed" (b HS.== n)
     HS.pure __
 
pHeader :: Parser Header
 
pHeader =
  RTS.pEnter "midi.Chunk"
    (pChunk @(Vector.Vector (RTS.UInt 8)) @Header
       (RTS.pMatch "26:9--26:14" (Vector.vecFromRep "MThd"))
       (do (format :: Header_0) <-
             (RTS.|||)
               (RTS.pEnter "single_track"
                  (do (_41 :: ()) <-
                        RTS.pEnter "midi.TAG16" (pTAG16 (RTS.lit 0 :: RTS.UInt 16))
                      HS.pure (Header_0_single_track _41)))
               ((RTS.|||)
                  (RTS.pEnter "multi_track"
                     (do (_42 :: ()) <-
                           RTS.pEnter "midi.TAG16" (pTAG16 (RTS.lit 1 :: RTS.UInt 16))
                         HS.pure (Header_0_multi_track _42)))
                  (RTS.pEnter "multi_song"
                     (do (_43 :: ()) <-
                           RTS.pEnter "midi.TAG16" (pTAG16 (RTS.lit 2 :: RTS.UInt 16))
                         HS.pure (Header_0_multi_song _43))))
           (track_num :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
           (time_unit :: Header_1) <-
             do (w :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
                (tag :: RTS.UInt 1) <-
                  HS.pure (getBit @(RTS.UInt 16) (RTS.lit 15 :: HS.Integer) w)
                (__ :: Header_1) <-
                  (RTS.|||)
                    (RTS.pEnter "quarter_len"
                       (do (_44 :: RTS.UInt 15) <-
                             do RTS.pGuard "35:34--35:41" "guard failed"
                                  (tag HS.== (RTS.lit 0 :: RTS.UInt 1))
                                (__ :: RTS.UInt 15) <- HS.pure (RTS.convert w :: RTS.UInt 15)
                                HS.pure __
                           HS.pure (Header_1_quarter_len _44)))
                    (RTS.pEnter "smtpe"
                       (do (_45 :: RTS.SInt 15) <-
                             do RTS.pGuard "36:34--36:41" "guard failed"
                                  (tag HS.== (RTS.lit 1 :: RTS.UInt 1))
                                (__ :: RTS.SInt 15) <- HS.pure (RTS.convert w :: RTS.SInt 15)
                                HS.pure __
                           HS.pure (Header_1_smtpe _45)))
                HS.pure __
           HS.pure (Header format track_num time_unit)))
 
pTrack :: Parser (Vector.Vector (Delta Event))
 
pTrack =
  RTS.pEnter "midi.Chunk"
    (pChunk @(Vector.Vector (RTS.UInt 8))
       @(Vector.Vector (Delta Event))
       (RTS.pMatch "44:19--44:24" (Vector.vecFromRep "MTrk"))
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
  do HS.const () HS.<$> RTS.pByte "149:27--149:31"
     HS.const () HS.<$> RTS.pByte "149:40--149:44"
 
_BE24 :: Parser ()
 
_BE24 =
  do RTS.pEnter "midi._BE16" _BE16
     HS.const () HS.<$> RTS.pByte "150:40--150:44"
 
_BE32 :: Parser ()
 
_BE32 =
  do RTS.pEnter "midi._BE16" _BE16
     RTS.pEnter "midi._BE16" _BE16
 
_Chunk ::
  forall a b.
    (RTS.DDL a, RTS.DDL b) => Parser () -> (Parser () -> Parser ())
 
_Chunk (_P :: Parser ()) (_Ty :: Parser ()) =
  do RTS.pEnter "midi._Block"
       (_Block @a (RTS.lit 4 :: HS.Integer)
          (RTS.pEnter "midi._Only" (_Only @a _Ty)))
     (len :: RTS.UInt 32) <- RTS.pEnter "midi.BE32" pBE32
     RTS.pEnter "midi._Block"
       (_Block @b (RTS.convert len :: HS.Integer)
          (RTS.pEnter "midi._Only" (_Only @b _P)))
 
_UInt7 :: Parser ()
 
_UInt7 =
  do (b :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "156:26--156:30"
     RTS.pIsJust_ "156:33--156:43" "Value does not fit in target type"
       (RTS.convertMaybe b :: HS.Maybe (RTS.UInt 7))
 
_VarQ :: Parser ()
 
_VarQ =
  do RTS.pSkipMany (RTS.<||)
       (do (b :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "11:25--11:29"
           RTS.pGuard "11:32--11:46" "guard failed"
             (getBit @(RTS.UInt 8) (RTS.lit 7 :: HS.Integer) b
                HS.== (RTS.lit 1 :: RTS.UInt 1)))
     RTS.pEnter "midi._UInt7" _UInt7
 
_Delta :: forall a. RTS.DDL a => Parser () -> Parser ()
 
_Delta (_E :: Parser ()) =
  do RTS.pEnter "midi._VarQ" _VarQ
     _E
 
_Meta :: Parser ()
 
_Meta =
  do HS.const ()
       HS.<$> RTS.pMatch1 "107:5--107:8" (RTS.bcSingle (RTS.uint8 255))
     (_type :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "108:13--108:17"
     RTS.pGuard "108:20--108:31" "guard failed"
       (_type HS.<= (RTS.lit 127 :: RTS.UInt 8))
     (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
     RTS.pEnter "midi._Block"
       (_Block @Meta len
          ((RTS.<||)
             (RTS.pEnter "sequence"
                (do RTS.pGuard "112:26--112:37" "guard failed"
                      (_type HS.== (RTS.lit 0 :: RTS.UInt 8))
                    RTS.pEnter "midi._Only"
                      (_Only @(RTS.UInt 16) (RTS.pEnter "midi._BE16" _BE16))))
             ((RTS.<||)
                (RTS.pEnter "text"
                   (RTS.pGuard "113:26--113:37" "guard failed"
                      (_type HS.== (RTS.lit 1 :: RTS.UInt 8))))
                ((RTS.<||)
                   (RTS.pEnter "copyright"
                      (RTS.pGuard "114:26--114:37" "guard failed"
                         (_type HS.== (RTS.lit 2 :: RTS.UInt 8))))
                   ((RTS.<||)
                      (RTS.pEnter "name"
                         (RTS.pGuard "115:26--115:37" "guard failed"
                            (_type HS.== (RTS.lit 3 :: RTS.UInt 8))))
                      ((RTS.<||)
                         (RTS.pEnter "instrument"
                            (RTS.pGuard "116:26--116:37" "guard failed"
                               (_type HS.== (RTS.lit 4 :: RTS.UInt 8))))
                         ((RTS.<||)
                            (RTS.pEnter "lyrics"
                               (RTS.pGuard "117:26--117:37" "guard failed"
                                  (_type HS.== (RTS.lit 5 :: RTS.UInt 8))))
                            ((RTS.<||)
                               (RTS.pEnter "marker"
                                  (RTS.pGuard "118:26--118:37" "guard failed"
                                     (_type HS.== (RTS.lit 6 :: RTS.UInt 8))))
                               ((RTS.<||)
                                  (RTS.pEnter "cue"
                                     (RTS.pGuard "119:26--119:37" "guard failed"
                                        (_type HS.== (RTS.lit 7 :: RTS.UInt 8))))
                                  ((RTS.<||)
                                     (RTS.pEnter "channel"
                                        (do RTS.pGuard "120:26--120:37" "guard failed"
                                              (_type HS.== (RTS.lit 32 :: RTS.UInt 8))
                                            RTS.pEnter "midi._Only"
                                              (_Only @(RTS.UInt 8)
                                                 (HS.const () HS.<$> RTS.pByte "120:45--120:49"))))
                                     ((RTS.<||)
                                        (RTS.pEnter "end_track"
                                           (do RTS.pGuard "121:26--121:37" "guard failed"
                                                 (_type HS.== (RTS.lit 47 :: RTS.UInt 8))
                                               RTS.pEnd "121:40--121:42"))
                                        ((RTS.<||)
                                           (RTS.pEnter "tempo"
                                              (do RTS.pGuard "122:26--122:37" "guard failed"
                                                    (_type HS.== (RTS.lit 81 :: RTS.UInt 8))
                                                  RTS.pEnter "midi._Only"
                                                    (_Only @(RTS.UInt 24)
                                                       (RTS.pEnter "midi._BE24" _BE24))))
                                           ((RTS.<||)
                                              (RTS.pEnter "smtpe_offset"
                                                 (do RTS.pGuard "123:26--123:37" "guard failed"
                                                       (_type HS.== (RTS.lit 84 :: RTS.UInt 8))
                                                     HS.const () HS.<$> RTS.pByte "124:31--124:35"
                                                     HS.const () HS.<$> RTS.pByte "124:43--124:47"
                                                     HS.const () HS.<$> RTS.pByte "124:55--124:59"
                                                     HS.const () HS.<$> RTS.pByte "124:67--124:71"
                                                     HS.const () HS.<$> RTS.pByte "125:31--125:35"
                                                     RTS.pEnd "125:38--125:40"))
                                              ((RTS.<||)
                                                 (RTS.pEnter "time_sig"
                                                    (do RTS.pGuard "126:26--126:37" "guard failed"
                                                          (_type HS.== (RTS.lit 88 :: RTS.UInt 8))
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "127:31--127:35"
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "127:43--127:47"
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "127:55--127:59"
                                                        HS.const ()
                                                          HS.<$> RTS.pByte "128:31--128:35"
                                                        RTS.pEnd "129:26--129:28"))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "key_sig"
                                                       (do RTS.pGuard "131:26--131:37"
                                                             "guard failed"
                                                             (_type
                                                                HS.== (RTS.lit 89 :: RTS.UInt 8))
                                                           HS.const ()
                                                             HS.<$> RTS.pByte "132:33--132:37"
                                                           (RTS.|||)
                                                             (RTS.pEnter "major"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch1
                                                                            "134:50--134:50"
                                                                            (RTS.bcSingle
                                                                               (RTS.uint8 0))))
                                                             (RTS.pEnter "minor"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch1
                                                                            "134:61--134:61"
                                                                            (RTS.bcSingle
                                                                               (RTS.uint8 1))))
                                                           RTS.pEnd "135:26--135:28"))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "seq_specifiec"
                                                          (do RTS.pGuard "137:27--137:38"
                                                                "guard failed"
                                                                (_type
                                                                   HS.== (RTS.lit 127
                                                                            :: RTS.UInt 8))
                                                              (RTS.<||)
                                                                (do HS.const ()
                                                                      HS.<$> RTS.pMatch1
                                                                               "138:44--138:44"
                                                                               (RTS.bcSingle
                                                                                  (RTS.uint8 0))
                                                                    RTS.pEnter "midi._BE16" _BE16)
                                                                (HS.const ()
                                                                   HS.<$> RTS.pByte
                                                                            "139:49--139:53")))
                                                       (RTS.pEnter "unknown"
                                                          (HS.pure ()))))))))))))))))))
 
_ModeMessage :: Parser ()
 
_ModeMessage =
  (RTS.|||)
    (RTS.pEnter "all_sound_off"
       (do HS.const ()
             HS.<$> RTS.pMatch1 "88:27--88:30" (RTS.bcSingle (RTS.uint8 120))
           HS.const ()
             HS.<$> RTS.pMatch1 "88:33--88:36" (RTS.bcSingle (RTS.uint8 0))))
    ((RTS.|||)
       (RTS.pEnter "reset_controllers"
          (do HS.const ()
                HS.<$> RTS.pMatch1 "89:27--89:30" (RTS.bcSingle (RTS.uint8 121))
              HS.const ()
                HS.<$> RTS.pMatch1 "89:33--89:36" (RTS.bcSingle (RTS.uint8 0))))
       ((RTS.|||)
          (RTS.pEnter "local_control"
             (do HS.const ()
                   HS.<$> RTS.pMatch1 "90:27--90:30" (RTS.bcSingle (RTS.uint8 122))
                 (RTS.|||)
                   (RTS.pEnter "off"
                      (HS.const ()
                         HS.<$> RTS.pMatch1 "90:49--90:52" (RTS.bcSingle (RTS.uint8 0))))
                   (RTS.pEnter "on"
                      (HS.const ()
                         HS.<$> RTS.pMatch1 "90:61--90:64"
                                  (RTS.bcSingle (RTS.uint8 127))))))
          ((RTS.|||)
             (RTS.pEnter "all_notes_off"
                (do HS.const ()
                      HS.<$> RTS.pMatch1 "91:27--91:30" (RTS.bcSingle (RTS.uint8 123))
                    HS.const ()
                      HS.<$> RTS.pMatch1 "91:33--91:36" (RTS.bcSingle (RTS.uint8 0))))
             ((RTS.|||)
                (RTS.pEnter "omni_off"
                   (do HS.const ()
                         HS.<$> RTS.pMatch1 "92:27--92:30" (RTS.bcSingle (RTS.uint8 124))
                       HS.const ()
                         HS.<$> RTS.pMatch1 "92:33--92:36" (RTS.bcSingle (RTS.uint8 0))))
                ((RTS.|||)
                   (RTS.pEnter "omni_on"
                      (do HS.const ()
                            HS.<$> RTS.pMatch1 "93:27--93:30" (RTS.bcSingle (RTS.uint8 125))
                          HS.const ()
                            HS.<$> RTS.pMatch1 "93:33--93:36" (RTS.bcSingle (RTS.uint8 0))))
                   ((RTS.|||)
                      (RTS.pEnter "mono_on"
                         (do HS.const ()
                               HS.<$> RTS.pMatch1 "94:27--94:30" (RTS.bcSingle (RTS.uint8 126))
                             (__ :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "94:38--94:42"
                             RTS.pGuard "94:45--94:54" "guard failed"
                               (__ HS.<= (RTS.lit 16 :: RTS.UInt 8))))
                      (RTS.pEnter "poly_on"
                         (do HS.const ()
                               HS.<$> RTS.pMatch1 "95:27--95:30" (RTS.bcSingle (RTS.uint8 127))
                             HS.const ()
                               HS.<$> RTS.pMatch1 "95:33--95:36"
                                        (RTS.bcSingle (RTS.uint8 0))))))))))
 
_ModeMessages :: Parser ()
 
_ModeMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "78:15--78:19"
     (tag :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr status (RTS.lit 4 :: HS.Integer))
            :: RTS.UInt 4)
     RTS.pGuard "80:5--80:14" "guard failed"
       (tag HS.== (RTS.lit 11 :: RTS.UInt 4))
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
             HS.<$> RTS.pMatch1 "101:16--101:19" (RTS.bcSingle (RTS.uint8 240))
           (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
           RTS.pEnter "midi._Block" (_Block @RTS.Input len (HS.pure ()))))
    (RTS.pEnter "as_is"
       (do HS.const ()
             HS.<$> RTS.pMatch1 "102:16--102:19" (RTS.bcSingle (RTS.uint8 247))
           (len :: HS.Integer) <- RTS.pEnter "midi.VarQ" pVarQ
           RTS.pEnter "midi._Block" (_Block @RTS.Input len (HS.pure ()))))
 
_VoiceMessage :: RTS.UInt 4 -> Parser ()
 
_VoiceMessage (tag :: RTS.UInt 4) =
  (RTS.|||)
    (RTS.pEnter "note_off"
       (do RTS.pGuard "66:27--66:36" "guard failed"
             (tag HS.== (RTS.lit 8 :: RTS.UInt 4))
           RTS.pEnter "midi._UInt7" _UInt7
           RTS.pEnter "midi._UInt7" _UInt7))
    ((RTS.|||)
       (RTS.pEnter "note_on"
          (do RTS.pGuard "67:27--67:36" "guard failed"
                (tag HS.== (RTS.lit 9 :: RTS.UInt 4))
              RTS.pEnter "midi._UInt7" _UInt7
              RTS.pEnter "midi._UInt7" _UInt7))
       ((RTS.|||)
          (RTS.pEnter "aftertouch"
             (do RTS.pGuard "68:27--68:36" "guard failed"
                   (tag HS.== (RTS.lit 10 :: RTS.UInt 4))
                 RTS.pEnter "midi._UInt7" _UInt7
                 RTS.pEnter "midi._UInt7" _UInt7))
          ((RTS.|||)
             (RTS.pEnter "controller_change"
                (do RTS.pGuard "69:27--69:36" "guard failed"
                      (tag HS.== (RTS.lit 11 :: RTS.UInt 4))
                    (controller :: RTS.UInt 7) <- RTS.pEnter "midi.UInt7" pUInt7
                    RTS.pGuard "70:47--70:64" "guard failed"
                      (controller HS.<= (RTS.lit 119 :: RTS.UInt 7))
                    RTS.pEnter "midi._UInt7" _UInt7))
             ((RTS.|||)
                (RTS.pEnter "program_change"
                   (do RTS.pGuard "72:27--72:36" "guard failed"
                         (tag HS.== (RTS.lit 12 :: RTS.UInt 4))
                       RTS.pEnter "midi._UInt7" _UInt7))
                ((RTS.|||)
                   (RTS.pEnter "channel_pressure"
                      (do RTS.pGuard "73:27--73:36" "guard failed"
                            (tag HS.== (RTS.lit 13 :: RTS.UInt 4))
                          RTS.pEnter "midi._UInt7" _UInt7))
                   (RTS.pEnter "pitch_bend"
                      (do RTS.pGuard "74:27--74:36" "guard failed"
                            (tag HS.== (RTS.lit 14 :: RTS.UInt 4))
                          RTS.pEnter "midi._UInt7" _UInt7
                          RTS.pEnter "midi._UInt7" _UInt7)))))))
 
_VoiceMessages :: Parser ()
 
_VoiceMessages =
  do (status :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "57:16--57:20"
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
  do (b :: RTS.UInt 16) <- RTS.pEnter "midi.BE16" pBE16
     RTS.pGuard "152:33--152:38" "guard failed" (b HS.== n)
 
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
                (RTS.pGuard "35:34--35:41" "guard failed"
                   (tag HS.== (RTS.lit 0 :: RTS.UInt 1))))
             (RTS.pEnter "smtpe"
                (RTS.pGuard "36:34--36:41" "guard failed"
                   (tag HS.== (RTS.lit 1 :: RTS.UInt 1)))))
       (HS.const ()
          HS.<$> RTS.pMatch "26:9--26:14" (Vector.vecFromRep "MThd")))
 
_Track :: Parser ()
 
_Track =
  RTS.pEnter "midi._Chunk"
    (_Chunk @(Vector.Vector (RTS.UInt 8))
       @(Vector.Vector (Delta Event))
       (RTS.pSkipMany (RTS.<||)
          (RTS.pEnter "midi._Delta"
             (_Delta @Event (RTS.pEnter "midi._Event" _Event))))
       (HS.const ()
          HS.<$> RTS.pMatch "44:19--44:24" (Vector.vecFromRep "MTrk")))
 
_Main :: Parser ()
 
_Main =
  do (header :: Header) <- RTS.pEnter "midi.Header" pHeader
     (n :: HS.Integer) <-
       RTS.pIsJust "4:14--4:36" "Value does not fit in target type"
         (RTS.convertMaybe (HS.getField @"track_num" header)
            :: HS.Maybe HS.Integer)
     RTS.pSkipExact n (RTS.pEnter "midi._Track" _Track)