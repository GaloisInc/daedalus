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
{-# Language ViewPatterns #-}
module ICC where
 
import qualified RTS.Parser as RTS
import qualified Prelude as HS
import qualified GHC.TypeLits as HS
import qualified GHC.Records as HS
import qualified Control.Monad as HS
import qualified RTS as RTS
import qualified RTS.Input as RTS
import qualified RTS.Map as Map
import qualified RTS.Vector as Vector
 
 
data VersionField
  = VersionField (RTS.UInt 8) (RTS.UInt 4) (RTS.UInt 4)
  
 
deriving instance HS.Eq VersionField
 
deriving instance HS.Ord VersionField
 
deriving instance HS.Show VersionField
 
instance RTS.DDL VersionField where
 
instance HS.HasField "major" VersionField (RTS.UInt 8) where
  getField (VersionField x _ _) = x
 
instance HS.HasField "minor" VersionField (RTS.UInt 4) where
  getField (VersionField _ x _) = x
 
instance HS.HasField "bugfix" VersionField (RTS.UInt 4) where
  getField (VersionField _ _ x) = x
 
data ProfileClasses
  = ProfileClasses_abstract_profile ()
  | ProfileClasses_color_space_profile ()
  | ProfileClasses_device_link_profile ()
  | ProfileClasses_display_device_profile ()
  | ProfileClasses_input_device_profile ()
  | ProfileClasses_named_color_profile ()
  | ProfileClasses_output_device_profile ()
  
 
deriving instance HS.Eq ProfileClasses
 
deriving instance HS.Ord ProfileClasses
 
deriving instance HS.Show ProfileClasses
 
instance RTS.DDL ProfileClasses where
 
instance HS.HasField "abstract_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_abstract_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "color_space_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_color_space_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "device_link_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_device_link_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "display_device_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_display_device_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "input_device_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_input_device_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "named_color_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_named_color_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "output_device_profile" ProfileClasses
           (HS.Maybe ()) where
  getField (ProfileClasses_output_device_profile x) = HS.Just x
   
  getField _ = HS.Nothing
 
data DataColorSpaces
  = DataColorSpaces_cielab_or_pcslab ()
  | DataColorSpaces_cieluv ()
  | DataColorSpaces_cieyxy ()
  | DataColorSpaces_cmy ()
  | DataColorSpaces_cmyk ()
  | DataColorSpaces_eight_colour ()
  | DataColorSpaces_eleven_colour ()
  | DataColorSpaces_fifteen_colour ()
  | DataColorSpaces_five_colour ()
  | DataColorSpaces_four_colour ()
  | DataColorSpaces_fourteen_colour ()
  | DataColorSpaces_gray ()
  | DataColorSpaces_hls ()
  | DataColorSpaces_hsv ()
  | DataColorSpaces_nciexyz_or_pcsxyz ()
  | DataColorSpaces_nine_colour ()
  | DataColorSpaces_rgb ()
  | DataColorSpaces_seven_colour ()
  | DataColorSpaces_six_colour ()
  | DataColorSpaces_ten_colour ()
  | DataColorSpaces_thirteen_colour ()
  | DataColorSpaces_three_colour ()
  | DataColorSpaces_twelve_colour ()
  | DataColorSpaces_two_colour ()
  | DataColorSpaces_ycbcr ()
  
 
deriving instance HS.Eq DataColorSpaces
 
deriving instance HS.Ord DataColorSpaces
 
deriving instance HS.Show DataColorSpaces
 
instance RTS.DDL DataColorSpaces where
 
instance HS.HasField "cielab_or_pcslab" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_cielab_or_pcslab x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "cieluv" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_cieluv x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "cieyxy" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_cieyxy x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "cmy" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_cmy x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "cmyk" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_cmyk x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "eight_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_eight_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "eleven_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_eleven_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fifteen_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_fifteen_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "five_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_five_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "four_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_four_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "fourteen_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_fourteen_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "gray" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_gray x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "hls" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_hls x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "hsv" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_hsv x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "nciexyz_or_pcsxyz" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_nciexyz_or_pcsxyz x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "nine_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_nine_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "rgb" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_rgb x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "seven_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_seven_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "six_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_six_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "ten_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_ten_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "thirteen_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_thirteen_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "three_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_three_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "twelve_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_twelve_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "two_colour" DataColorSpaces
           (HS.Maybe ()) where
  getField (DataColorSpaces_two_colour x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "ycbcr" DataColorSpaces (HS.Maybe ()) where
  getField (DataColorSpaces_ycbcr x) = HS.Just x
   
  getField _ = HS.Nothing
 
data PrimaryPlatforms
  = PrimaryPlatforms_apple_computer_inc ()
  | PrimaryPlatforms_microsoft_corporation ()
  | PrimaryPlatforms_none ()
  | PrimaryPlatforms_silicon_graphics_inc ()
  | PrimaryPlatforms_sun_microsystems ()
  
 
deriving instance HS.Eq PrimaryPlatforms
 
deriving instance HS.Ord PrimaryPlatforms
 
deriving instance HS.Show PrimaryPlatforms
 
instance RTS.DDL PrimaryPlatforms where
 
instance HS.HasField "apple_computer_inc" PrimaryPlatforms
           (HS.Maybe ()) where
  getField (PrimaryPlatforms_apple_computer_inc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "microsoft_corporation" PrimaryPlatforms
           (HS.Maybe ()) where
  getField (PrimaryPlatforms_microsoft_corporation x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "none" PrimaryPlatforms (HS.Maybe ()) where
  getField (PrimaryPlatforms_none x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "silicon_graphics_inc" PrimaryPlatforms
           (HS.Maybe ()) where
  getField (PrimaryPlatforms_silicon_graphics_inc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "sun_microsystems" PrimaryPlatforms
           (HS.Maybe ()) where
  getField (PrimaryPlatforms_sun_microsystems x) = HS.Just x
   
  getField _ = HS.Nothing
 
data RenderingIntent
  = RenderingIntent_icc_absolute_colorimetric ()
  | RenderingIntent_media_relative_colorimetric ()
  | RenderingIntent_perceptual ()
  | RenderingIntent_saturation ()
  
 
deriving instance HS.Eq RenderingIntent
 
deriving instance HS.Ord RenderingIntent
 
deriving instance HS.Show RenderingIntent
 
instance RTS.DDL RenderingIntent where
 
instance HS.HasField "icc_absolute_colorimetric" RenderingIntent
           (HS.Maybe ()) where
  getField (RenderingIntent_icc_absolute_colorimetric x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "media_relative_colorimetric" RenderingIntent
           (HS.Maybe ()) where
  getField (RenderingIntent_media_relative_colorimetric x) =
    HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "perceptual" RenderingIntent
           (HS.Maybe ()) where
  getField (RenderingIntent_perceptual x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "saturation" RenderingIntent
           (HS.Maybe ()) where
  getField (RenderingIntent_saturation x) = HS.Just x
   
  getField _ = HS.Nothing
 
data XYZNumber
  = XYZNumber (RTS.UInt 32) (RTS.UInt 32) (RTS.UInt 32)
  
 
deriving instance HS.Eq XYZNumber
 
deriving instance HS.Ord XYZNumber
 
deriving instance HS.Show XYZNumber
 
instance RTS.DDL XYZNumber where
 
instance HS.HasField "x" XYZNumber (RTS.UInt 32) where
  getField (XYZNumber x _ _) = x
 
instance HS.HasField "y" XYZNumber (RTS.UInt 32) where
  getField (XYZNumber _ x _) = x
 
instance HS.HasField "z" XYZNumber (RTS.UInt 32) where
  getField (XYZNumber _ _ x) = x
 
data DateTimeNumber
  = DateTimeNumber (RTS.UInt 16) (RTS.UInt 16) (RTS.UInt 16)
      (RTS.UInt 16)
      (RTS.UInt 16)
      (RTS.UInt 16)
  
 
deriving instance HS.Eq DateTimeNumber
 
deriving instance HS.Ord DateTimeNumber
 
deriving instance HS.Show DateTimeNumber
 
instance RTS.DDL DateTimeNumber where
 
instance HS.HasField "year" DateTimeNumber (RTS.UInt 16) where
  getField (DateTimeNumber x _ _ _ _ _) = x
 
instance HS.HasField "month" DateTimeNumber (RTS.UInt 16) where
  getField (DateTimeNumber _ x _ _ _ _) = x
 
instance HS.HasField "day" DateTimeNumber (RTS.UInt 16) where
  getField (DateTimeNumber _ _ x _ _ _) = x
 
instance HS.HasField "hour" DateTimeNumber (RTS.UInt 16) where
  getField (DateTimeNumber _ _ _ x _ _) = x
 
instance HS.HasField "minute" DateTimeNumber (RTS.UInt 16) where
  getField (DateTimeNumber _ _ _ _ x _) = x
 
instance HS.HasField "second" DateTimeNumber (RTS.UInt 16) where
  getField (DateTimeNumber _ _ _ _ _ x) = x
 
data ProfileHeader
  = ProfileHeader (RTS.UInt 32) (RTS.UInt 32) VersionField
      ProfileClasses
      DataColorSpaces
      DataColorSpaces
      DateTimeNumber
      PrimaryPlatforms
      (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 64)
      RenderingIntent
      XYZNumber
      (RTS.UInt 32)
      (Vector.Vector (RTS.UInt 8))
      (Vector.Vector (RTS.UInt 8))
  
 
deriving instance HS.Eq ProfileHeader
 
deriving instance HS.Ord ProfileHeader
 
deriving instance HS.Show ProfileHeader
 
instance RTS.DDL ProfileHeader where
 
instance HS.HasField "size" ProfileHeader (RTS.UInt 32) where
  getField (ProfileHeader x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "preferred_cmm_type" ProfileHeader
           (RTS.UInt 32) where
  getField (ProfileHeader _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "version" ProfileHeader VersionField where
  getField (ProfileHeader _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "devce_class" ProfileHeader
           ProfileClasses where
  getField (ProfileHeader _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "color_space" ProfileHeader
           DataColorSpaces where
  getField (ProfileHeader _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "pcs" ProfileHeader DataColorSpaces where
  getField (ProfileHeader _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "creation_date_time" ProfileHeader
           DateTimeNumber where
  getField (ProfileHeader _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "primary_platform" ProfileHeader
           PrimaryPlatforms where
  getField (ProfileHeader _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "profile_flags" ProfileHeader
           (RTS.UInt 32) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "device_manufacturer" ProfileHeader
           (RTS.UInt 32) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _ _) = x
 
instance HS.HasField "device_model" ProfileHeader
           (RTS.UInt 32) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _ _) = x
 
instance HS.HasField "device_attributes" ProfileHeader
           (RTS.UInt 64) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _ _) = x
 
instance HS.HasField "rendering_intent" ProfileHeader
           RenderingIntent where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ _ _ x _ _ _ _) = x
 
instance HS.HasField "illuminant" ProfileHeader XYZNumber where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ _ _ _ x _ _ _) = x
 
instance HS.HasField "creatior" ProfileHeader (RTS.UInt 32) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ x _ _) = x
 
instance HS.HasField "identifier" ProfileHeader
           (Vector.Vector (RTS.UInt 8)) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x _) = x
 
instance HS.HasField "reserved_data" ProfileHeader
           (Vector.Vector (RTS.UInt 8)) where
  getField (ProfileHeader _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ x) = x
 
data TagEntry
  = TagEntry (Vector.Vector (RTS.UInt 8)) (RTS.UInt 32) (RTS.UInt 32)
  
 
deriving instance HS.Eq TagEntry
 
deriving instance HS.Ord TagEntry
 
deriving instance HS.Show TagEntry
 
instance RTS.DDL TagEntry where
 
instance HS.HasField "tag_signature" TagEntry
           (Vector.Vector (RTS.UInt 8)) where
  getField (TagEntry x _ _) = x
 
instance HS.HasField "offset_to_data_element" TagEntry
           (RTS.UInt 32) where
  getField (TagEntry _ x _) = x
 
instance HS.HasField "size_of_data_element" TagEntry
           (RTS.UInt 32) where
  getField (TagEntry _ _ x) = x
 
data Main
  = Main ProfileHeader (Vector.Vector TagEntry)
  
 
deriving instance HS.Eq Main
 
deriving instance HS.Ord Main
 
deriving instance HS.Show Main
 
instance RTS.DDL Main where
 
instance HS.HasField "profileHeader" Main ProfileHeader where
  getField (Main x _) = x
 
instance HS.HasField "tagTable" Main (Vector.Vector TagEntry) where
  getField (Main _ x) = x
 
data XYNumber
  = XYNumber (RTS.UInt 32) (RTS.UInt 32)
  
 
deriving instance HS.Eq XYNumber
 
deriving instance HS.Ord XYNumber
 
deriving instance HS.Show XYNumber
 
instance RTS.DDL XYNumber where
 
instance HS.HasField "x" XYNumber (RTS.UInt 32) where
  getField (XYNumber x _) = x
 
instance HS.HasField "y" XYNumber (RTS.UInt 32) where
  getField (XYNumber _ x) = x
 
data PositionNumber
  = PositionNumber (RTS.UInt 32) (RTS.UInt 32)
  
 
deriving instance HS.Eq PositionNumber
 
deriving instance HS.Ord PositionNumber
 
deriving instance HS.Show PositionNumber
 
instance RTS.DDL PositionNumber where
 
instance HS.HasField "offset" PositionNumber (RTS.UInt 32) where
  getField (PositionNumber x _) = x
 
instance HS.HasField "size" PositionNumber (RTS.UInt 32) where
  getField (PositionNumber _ x) = x
 
data Response16Number
  = Response16Number (RTS.UInt 16) (RTS.UInt 32)
  
 
deriving instance HS.Eq Response16Number
 
deriving instance HS.Ord Response16Number
 
deriving instance HS.Show Response16Number
 
instance RTS.DDL Response16Number where
 
instance HS.HasField "device" Response16Number (RTS.UInt 16) where
  getField (Response16Number x _) = x
 
instance HS.HasField "measurement" Response16Number
           (RTS.UInt 32) where
  getField (Response16Number _ x) = x
 
data Lut8Type
  = Lut8Type (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 8)
      (Vector.Vector (RTS.SInt 32))
      RTS.Input
      RTS.Input
      RTS.Input
  
 
deriving instance HS.Eq Lut8Type
 
deriving instance HS.Ord Lut8Type
 
deriving instance HS.Show Lut8Type
 
instance RTS.DDL Lut8Type where
 
instance HS.HasField "number_of_input_channels" Lut8Type
           (RTS.UInt 8) where
  getField (Lut8Type x _ _ _ _ _ _) = x
 
instance HS.HasField "number_of_output_channels" Lut8Type
           (RTS.UInt 8) where
  getField (Lut8Type _ x _ _ _ _ _) = x
 
instance HS.HasField "number_of_clut_grid_points" Lut8Type
           (RTS.UInt 8) where
  getField (Lut8Type _ _ x _ _ _ _) = x
 
instance HS.HasField "encoded_e_parameters" Lut8Type
           (Vector.Vector (RTS.SInt 32)) where
  getField (Lut8Type _ _ _ x _ _ _) = x
 
instance HS.HasField "input_tables" Lut8Type RTS.Input where
  getField (Lut8Type _ _ _ _ x _ _) = x
 
instance HS.HasField "clut_values" Lut8Type RTS.Input where
  getField (Lut8Type _ _ _ _ _ x _) = x
 
instance HS.HasField "output_tables" Lut8Type RTS.Input where
  getField (Lut8Type _ _ _ _ _ _ x) = x
 
data Lut16Type
  = Lut16Type (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 8)
      (Vector.Vector (RTS.SInt 32))
      (RTS.UInt 32)
      (RTS.UInt 32)
      RTS.Input
      RTS.Input
      RTS.Input
  
 
deriving instance HS.Eq Lut16Type
 
deriving instance HS.Ord Lut16Type
 
deriving instance HS.Show Lut16Type
 
instance RTS.DDL Lut16Type where
 
instance HS.HasField "number_of_input_channels" Lut16Type
           (RTS.UInt 8) where
  getField (Lut16Type x _ _ _ _ _ _ _ _) = x
 
instance HS.HasField "number_of_output_channels" Lut16Type
           (RTS.UInt 8) where
  getField (Lut16Type _ x _ _ _ _ _ _ _) = x
 
instance HS.HasField "number_of_clut_grid_points" Lut16Type
           (RTS.UInt 8) where
  getField (Lut16Type _ _ x _ _ _ _ _ _) = x
 
instance HS.HasField "encoded_e_parameters" Lut16Type
           (Vector.Vector (RTS.SInt 32)) where
  getField (Lut16Type _ _ _ x _ _ _ _ _) = x
 
instance HS.HasField "number_of_input_table_entries" Lut16Type
           (RTS.UInt 32) where
  getField (Lut16Type _ _ _ _ x _ _ _ _) = x
 
instance HS.HasField "number_of_output_table_entries" Lut16Type
           (RTS.UInt 32) where
  getField (Lut16Type _ _ _ _ _ x _ _ _) = x
 
instance HS.HasField "input_tables" Lut16Type RTS.Input where
  getField (Lut16Type _ _ _ _ _ _ x _ _) = x
 
instance HS.HasField "clut_values" Lut16Type RTS.Input where
  getField (Lut16Type _ _ _ _ _ _ _ x _) = x
 
instance HS.HasField "output_tables" Lut16Type RTS.Input where
  getField (Lut16Type _ _ _ _ _ _ _ _ x) = x
 
data LutAToBType
  = LutAToBType (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 32) (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 32)
      RTS.Input
  
 
deriving instance HS.Eq LutAToBType
 
deriving instance HS.Ord LutAToBType
 
deriving instance HS.Show LutAToBType
 
instance RTS.DDL LutAToBType where
 
instance HS.HasField "number_of_input_channels" LutAToBType
           (RTS.UInt 8) where
  getField (LutAToBType x _ _ _ _ _ _ _) = x
 
instance HS.HasField "number_of_output_channels" LutAToBType
           (RTS.UInt 8) where
  getField (LutAToBType _ x _ _ _ _ _ _) = x
 
instance HS.HasField "offset_first_B_curve" LutAToBType
           (RTS.UInt 32) where
  getField (LutAToBType _ _ x _ _ _ _ _) = x
 
instance HS.HasField "offset_to_matrix" LutAToBType
           (RTS.UInt 32) where
  getField (LutAToBType _ _ _ x _ _ _ _) = x
 
instance HS.HasField "offset_to_first_M_curve" LutAToBType
           (RTS.UInt 32) where
  getField (LutAToBType _ _ _ _ x _ _ _) = x
 
instance HS.HasField "offset_to_CLUT" LutAToBType
           (RTS.UInt 32) where
  getField (LutAToBType _ _ _ _ _ x _ _) = x
 
instance HS.HasField "offset_to_first_A_curve" LutAToBType
           (RTS.UInt 32) where
  getField (LutAToBType _ _ _ _ _ _ x _) = x
 
instance HS.HasField "data" LutAToBType RTS.Input where
  getField (LutAToBType _ _ _ _ _ _ _ x) = x
 
data LutBToAType
  = LutBToAType (RTS.UInt 8) (RTS.UInt 8) (RTS.UInt 32) (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 32)
      RTS.Input
  
 
deriving instance HS.Eq LutBToAType
 
deriving instance HS.Ord LutBToAType
 
deriving instance HS.Show LutBToAType
 
instance RTS.DDL LutBToAType where
 
instance HS.HasField "number_of_input_channels" LutBToAType
           (RTS.UInt 8) where
  getField (LutBToAType x _ _ _ _ _ _ _) = x
 
instance HS.HasField "number_of_output_channels" LutBToAType
           (RTS.UInt 8) where
  getField (LutBToAType _ x _ _ _ _ _ _) = x
 
instance HS.HasField "offset_first_B_curve" LutBToAType
           (RTS.UInt 32) where
  getField (LutBToAType _ _ x _ _ _ _ _) = x
 
instance HS.HasField "offset_to_matrix" LutBToAType
           (RTS.UInt 32) where
  getField (LutBToAType _ _ _ x _ _ _ _) = x
 
instance HS.HasField "offset_to_first_M_curve" LutBToAType
           (RTS.UInt 32) where
  getField (LutBToAType _ _ _ _ x _ _ _) = x
 
instance HS.HasField "offset_to_CLUT" LutBToAType
           (RTS.UInt 32) where
  getField (LutBToAType _ _ _ _ _ x _ _) = x
 
instance HS.HasField "offset_to_first_A_curve" LutBToAType
           (RTS.UInt 32) where
  getField (LutBToAType _ _ _ _ _ _ x _) = x
 
instance HS.HasField "data" LutBToAType RTS.Input where
  getField (LutBToAType _ _ _ _ _ _ _ x) = x
 
data Lut_8_16_AB_BA
  = Lut_8_16_AB_BA_lut16 Lut16Type
  | Lut_8_16_AB_BA_lut8 Lut8Type
  | Lut_8_16_AB_BA_lutAB LutAToBType
  | Lut_8_16_AB_BA_lutBA LutBToAType
  
 
deriving instance HS.Eq Lut_8_16_AB_BA
 
deriving instance HS.Ord Lut_8_16_AB_BA
 
deriving instance HS.Show Lut_8_16_AB_BA
 
instance RTS.DDL Lut_8_16_AB_BA where
 
instance HS.HasField "lut16" Lut_8_16_AB_BA
           (HS.Maybe Lut16Type) where
  getField (Lut_8_16_AB_BA_lut16 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lut8" Lut_8_16_AB_BA
           (HS.Maybe Lut8Type) where
  getField (Lut_8_16_AB_BA_lut8 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lutAB" Lut_8_16_AB_BA
           (HS.Maybe LutAToBType) where
  getField (Lut_8_16_AB_BA_lutAB x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lutBA" Lut_8_16_AB_BA
           (HS.Maybe LutBToAType) where
  getField (Lut_8_16_AB_BA_lutBA x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Lut_8_16_AB
  = Lut_8_16_AB_lut16 Lut16Type
  | Lut_8_16_AB_lut8 Lut8Type
  | Lut_8_16_AB_lutAB LutAToBType
  
 
deriving instance HS.Eq Lut_8_16_AB
 
deriving instance HS.Ord Lut_8_16_AB
 
deriving instance HS.Show Lut_8_16_AB
 
instance RTS.DDL Lut_8_16_AB where
 
instance HS.HasField "lut16" Lut_8_16_AB (HS.Maybe Lut16Type) where
  getField (Lut_8_16_AB_lut16 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lut8" Lut_8_16_AB (HS.Maybe Lut8Type) where
  getField (Lut_8_16_AB_lut8 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lutAB" Lut_8_16_AB
           (HS.Maybe LutAToBType) where
  getField (Lut_8_16_AB_lutAB x) = HS.Just x
   
  getField _ = HS.Nothing
 
data Lut_8_16_BA
  = Lut_8_16_BA_lut16 Lut16Type
  | Lut_8_16_BA_lut8 Lut8Type
  | Lut_8_16_BA_lutBA LutBToAType
  
 
deriving instance HS.Eq Lut_8_16_BA
 
deriving instance HS.Ord Lut_8_16_BA
 
deriving instance HS.Show Lut_8_16_BA
 
instance RTS.DDL Lut_8_16_BA where
 
instance HS.HasField "lut16" Lut_8_16_BA (HS.Maybe Lut16Type) where
  getField (Lut_8_16_BA_lut16 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lut8" Lut_8_16_BA (HS.Maybe Lut8Type) where
  getField (Lut_8_16_BA_lut8 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "lutBA" Lut_8_16_BA
           (HS.Maybe LutBToAType) where
  getField (Lut_8_16_BA_lutBA x) = HS.Just x
   
  getField _ = HS.Nothing
 
data ParametricCurveType
  = ParametricCurveType (RTS.UInt 16) (Vector.Vector (RTS.UInt 32))
  
 
deriving instance HS.Eq ParametricCurveType
 
deriving instance HS.Ord ParametricCurveType
 
deriving instance HS.Show ParametricCurveType
 
instance RTS.DDL ParametricCurveType where
 
instance HS.HasField "function" ParametricCurveType
           (RTS.UInt 16) where
  getField (ParametricCurveType x _) = x
 
instance HS.HasField "parameters" ParametricCurveType
           (Vector.Vector (RTS.UInt 32)) where
  getField (ParametricCurveType _ x) = x
 
data SomeCurve
  = SomeCurve_curve (Vector.Vector (RTS.UInt 16))
  | SomeCurve_parametric_curve ParametricCurveType
  
 
deriving instance HS.Eq SomeCurve
 
deriving instance HS.Ord SomeCurve
 
deriving instance HS.Show SomeCurve
 
instance RTS.DDL SomeCurve where
 
instance HS.HasField "curve" SomeCurve
           (HS.Maybe (Vector.Vector (RTS.UInt 16))) where
  getField (SomeCurve_curve x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "parametric_curve" SomeCurve
           (HS.Maybe ParametricCurveType) where
  getField (SomeCurve_parametric_curve x) = HS.Just x
   
  getField _ = HS.Nothing
 
data UnicodeRecord
  = UnicodeRecord (RTS.UInt 16) (RTS.UInt 16) RTS.Input
  
 
deriving instance HS.Eq UnicodeRecord
 
deriving instance HS.Ord UnicodeRecord
 
deriving instance HS.Show UnicodeRecord
 
instance RTS.DDL UnicodeRecord where
 
instance HS.HasField "language" UnicodeRecord (RTS.UInt 16) where
  getField (UnicodeRecord x _ _) = x
 
instance HS.HasField "country" UnicodeRecord (RTS.UInt 16) where
  getField (UnicodeRecord _ x _) = x
 
instance HS.HasField "data" UnicodeRecord RTS.Input where
  getField (UnicodeRecord _ _ x) = x
 
data Colorant
  = Colorant (Vector.Vector (RTS.UInt 7))
      (Vector.Vector (RTS.UInt 16))
  
 
deriving instance HS.Eq Colorant
 
deriving instance HS.Ord Colorant
 
deriving instance HS.Show Colorant
 
instance RTS.DDL Colorant where
 
instance HS.HasField "name" Colorant
           (Vector.Vector (RTS.UInt 7)) where
  getField (Colorant x _) = x
 
instance HS.HasField "pcs" Colorant
           (Vector.Vector (RTS.UInt 16)) where
  getField (Colorant _ x) = x
 
data ResponseCurve
  = ResponseCurve (RTS.UInt 32) (Vector.Vector XYNumber)
      (Vector.Vector (Vector.Vector Response16Number))
  
 
deriving instance HS.Eq ResponseCurve
 
deriving instance HS.Ord ResponseCurve
 
deriving instance HS.Show ResponseCurve
 
instance RTS.DDL ResponseCurve where
 
instance HS.HasField "measurement_unit" ResponseCurve
           (RTS.UInt 32) where
  getField (ResponseCurve x _ _) = x
 
instance HS.HasField "pcxyzs" ResponseCurve
           (Vector.Vector XYNumber) where
  getField (ResponseCurve _ x _) = x
 
instance HS.HasField "response_arrays" ResponseCurve
           (Vector.Vector (Vector.Vector Response16Number)) where
  getField (ResponseCurve _ _ x) = x
 
data MultiProcessElementsType
  = MultiProcessElementsType (RTS.UInt 16) (RTS.UInt 16)
      (RTS.UInt 32)
      (RTS.UInt 64)
      (Vector.Vector RTS.Input)
  
 
deriving instance HS.Eq MultiProcessElementsType
 
deriving instance HS.Ord MultiProcessElementsType
 
deriving instance HS.Show MultiProcessElementsType
 
instance RTS.DDL MultiProcessElementsType where
 
instance HS.HasField "number_of_input_channels"
           MultiProcessElementsType
           (RTS.UInt 16) where
  getField (MultiProcessElementsType x _ _ _ _) = x
 
instance HS.HasField "number_of_output_channels"
           MultiProcessElementsType
           (RTS.UInt 16) where
  getField (MultiProcessElementsType _ x _ _ _) = x
 
instance HS.HasField "number_of_processing_elements"
           MultiProcessElementsType
           (RTS.UInt 32) where
  getField (MultiProcessElementsType _ _ x _ _) = x
 
instance HS.HasField "n" MultiProcessElementsType
           (RTS.UInt 64) where
  getField (MultiProcessElementsType _ _ _ x _) = x
 
instance HS.HasField "elements" MultiProcessElementsType
           (Vector.Vector RTS.Input) where
  getField (MultiProcessElementsType _ _ _ _ x) = x
 
data MeasurementType
  = MeasurementType (RTS.UInt 32) XYZNumber (RTS.UInt 32)
      (RTS.UInt 32)
      (RTS.UInt 32)
  
 
deriving instance HS.Eq MeasurementType
 
deriving instance HS.Ord MeasurementType
 
deriving instance HS.Show MeasurementType
 
instance RTS.DDL MeasurementType where
 
instance HS.HasField "standard_observer" MeasurementType
           (RTS.UInt 32) where
  getField (MeasurementType x _ _ _ _) = x
 
instance HS.HasField "nCIEXYZ" MeasurementType XYZNumber where
  getField (MeasurementType _ x _ _ _) = x
 
instance HS.HasField "geometry" MeasurementType (RTS.UInt 32) where
  getField (MeasurementType _ _ x _ _) = x
 
instance HS.HasField "flare" MeasurementType (RTS.UInt 32) where
  getField (MeasurementType _ _ _ x _) = x
 
instance HS.HasField "illuminant" MeasurementType
           (RTS.UInt 32) where
  getField (MeasurementType _ _ _ _ x) = x
 
data ColorName
  = ColorName (Vector.Vector (RTS.UInt 7))
      (Vector.Vector (RTS.UInt 16))
      (Vector.Vector (RTS.UInt 16))
  
 
deriving instance HS.Eq ColorName
 
deriving instance HS.Ord ColorName
 
deriving instance HS.Show ColorName
 
instance RTS.DDL ColorName where
 
instance HS.HasField "name_root" ColorName
           (Vector.Vector (RTS.UInt 7)) where
  getField (ColorName x _ _) = x
 
instance HS.HasField "pcs_coords" ColorName
           (Vector.Vector (RTS.UInt 16)) where
  getField (ColorName _ x _) = x
 
instance HS.HasField "device_coords" ColorName
           (Vector.Vector (RTS.UInt 16)) where
  getField (ColorName _ _ x) = x
 
data NamedColor2Type
  = NamedColor2Type (RTS.UInt 32) (Vector.Vector (RTS.UInt 7))
      (Vector.Vector (RTS.UInt 7))
      (Vector.Vector ColorName)
  
 
deriving instance HS.Eq NamedColor2Type
 
deriving instance HS.Ord NamedColor2Type
 
deriving instance HS.Show NamedColor2Type
 
instance RTS.DDL NamedColor2Type where
 
instance HS.HasField "vendor_specific" NamedColor2Type
           (RTS.UInt 32) where
  getField (NamedColor2Type x _ _ _) = x
 
instance HS.HasField "prefix" NamedColor2Type
           (Vector.Vector (RTS.UInt 7)) where
  getField (NamedColor2Type _ x _ _) = x
 
instance HS.HasField "suffix" NamedColor2Type
           (Vector.Vector (RTS.UInt 7)) where
  getField (NamedColor2Type _ _ x _) = x
 
instance HS.HasField "names" NamedColor2Type
           (Vector.Vector ColorName) where
  getField (NamedColor2Type _ _ _ x) = x
 
data ViewConditionsType
  = ViewConditionsType XYZNumber XYZNumber (RTS.UInt 32)
  
 
deriving instance HS.Eq ViewConditionsType
 
deriving instance HS.Ord ViewConditionsType
 
deriving instance HS.Show ViewConditionsType
 
instance RTS.DDL ViewConditionsType where
 
instance HS.HasField "illuminantXYZ" ViewConditionsType
           XYZNumber where
  getField (ViewConditionsType x _ _) = x
 
instance HS.HasField "surroundXYZ" ViewConditionsType
           XYZNumber where
  getField (ViewConditionsType _ x _) = x
 
instance HS.HasField "illuminant" ViewConditionsType
           (RTS.UInt 32) where
  getField (ViewConditionsType _ _ x) = x
 
data Tag
  = Tag_AToB0 Lut_8_16_AB
  | Tag_AToB1 Lut_8_16_AB
  | Tag_AToB2 Lut_8_16_AB
  | Tag_BToA0 Lut_8_16_BA
  | Tag_BToA1 Lut_8_16_BA
  | Tag_BToA2 Lut_8_16_BA
  | Tag_BToD0 MultiProcessElementsType
  | Tag_BToD1 MultiProcessElementsType
  | Tag_BToD2 MultiProcessElementsType
  | Tag_BToD3 MultiProcessElementsType
  | Tag_DToB0 MultiProcessElementsType
  | Tag_DToB1 MultiProcessElementsType
  | Tag_DToB2 MultiProcessElementsType
  | Tag_DToB3 MultiProcessElementsType
  | Tag_blueMatrixColumn (Vector.Vector XYZNumber)
  | Tag_blueTRC SomeCurve
  | Tag_calibrationDateTime DateTimeNumber
  | Tag_charTarget (Vector.Vector (RTS.UInt 7))
  | Tag_chromaticAdaptation (Vector.Vector (RTS.UInt 32))
  | Tag_colorantOrder (Vector.Vector (RTS.UInt 8))
  | Tag_colorantTable (Vector.Vector Colorant)
  | Tag_colorantTableOut (Vector.Vector Colorant)
  | Tag_colorimetricIntentImageState (Vector.Vector (RTS.UInt 8))
  | Tag_copyright (Vector.Vector UnicodeRecord)
  | Tag_deviceMfgDesc (Vector.Vector UnicodeRecord)
  | Tag_deviceModelDesc (Vector.Vector UnicodeRecord)
  | Tag_gamut Lut_8_16_BA
  | Tag_grayTRC SomeCurve
  | Tag_greenMatrixColumn (Vector.Vector XYZNumber)
  | Tag_greenTRC SomeCurve
  | Tag_luminance (Vector.Vector XYZNumber)
  | Tag_measurement MeasurementType
  | Tag_mediaWhitePoint (Vector.Vector XYZNumber)
  | Tag_namedColor2 NamedColor2Type
  | Tag_outputResponse (Vector.Vector ResponseCurve)
  | Tag_perceptualRenderingIntentGamut (Vector.Vector (RTS.UInt 8))
  | Tag_preview0 Lut_8_16_AB_BA
  | Tag_preview1 Lut_8_16_BA
  | Tag_preview2 Lut_8_16_BA
  | Tag_profileDescription (Vector.Vector UnicodeRecord)
  | Tag_profileSequenceDesc (Vector.Vector (RTS.UInt 8))
  | Tag_profileSequenceIdentifier ()
  | Tag_redMatrixColumn (Vector.Vector XYZNumber)
  | Tag_redTRC SomeCurve
  | Tag_saturationRenderingIntentGamut (Vector.Vector (RTS.UInt 8))
  | Tag_technology (Vector.Vector (RTS.UInt 8))
  | Tag_viewCondDesc (Vector.Vector UnicodeRecord)
  | Tag_viewConditions ViewConditionsType
  
 
deriving instance HS.Eq Tag
 
deriving instance HS.Ord Tag
 
deriving instance HS.Show Tag
 
instance RTS.DDL Tag where
 
instance HS.HasField "AToB0" Tag (HS.Maybe Lut_8_16_AB) where
  getField (Tag_AToB0 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "AToB1" Tag (HS.Maybe Lut_8_16_AB) where
  getField (Tag_AToB1 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "AToB2" Tag (HS.Maybe Lut_8_16_AB) where
  getField (Tag_AToB2 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToA0" Tag (HS.Maybe Lut_8_16_BA) where
  getField (Tag_BToA0 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToA1" Tag (HS.Maybe Lut_8_16_BA) where
  getField (Tag_BToA1 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToA2" Tag (HS.Maybe Lut_8_16_BA) where
  getField (Tag_BToA2 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToD0" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_BToD0 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToD1" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_BToD1 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToD2" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_BToD2 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "BToD3" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_BToD3 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "DToB0" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_DToB0 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "DToB1" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_DToB1 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "DToB2" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_DToB2 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "DToB3" Tag
           (HS.Maybe MultiProcessElementsType) where
  getField (Tag_DToB3 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "blueMatrixColumn" Tag
           (HS.Maybe (Vector.Vector XYZNumber)) where
  getField (Tag_blueMatrixColumn x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "blueTRC" Tag (HS.Maybe SomeCurve) where
  getField (Tag_blueTRC x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "calibrationDateTime" Tag
           (HS.Maybe DateTimeNumber) where
  getField (Tag_calibrationDateTime x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "charTarget" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 7))) where
  getField (Tag_charTarget x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "chromaticAdaptation" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 32))) where
  getField (Tag_chromaticAdaptation x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "colorantOrder" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Tag_colorantOrder x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "colorantTable" Tag
           (HS.Maybe (Vector.Vector Colorant)) where
  getField (Tag_colorantTable x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "colorantTableOut" Tag
           (HS.Maybe (Vector.Vector Colorant)) where
  getField (Tag_colorantTableOut x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "colorimetricIntentImageState" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Tag_colorimetricIntentImageState x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "copyright" Tag
           (HS.Maybe (Vector.Vector UnicodeRecord)) where
  getField (Tag_copyright x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "deviceMfgDesc" Tag
           (HS.Maybe (Vector.Vector UnicodeRecord)) where
  getField (Tag_deviceMfgDesc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "deviceModelDesc" Tag
           (HS.Maybe (Vector.Vector UnicodeRecord)) where
  getField (Tag_deviceModelDesc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "gamut" Tag (HS.Maybe Lut_8_16_BA) where
  getField (Tag_gamut x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "grayTRC" Tag (HS.Maybe SomeCurve) where
  getField (Tag_grayTRC x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "greenMatrixColumn" Tag
           (HS.Maybe (Vector.Vector XYZNumber)) where
  getField (Tag_greenMatrixColumn x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "greenTRC" Tag (HS.Maybe SomeCurve) where
  getField (Tag_greenTRC x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "luminance" Tag
           (HS.Maybe (Vector.Vector XYZNumber)) where
  getField (Tag_luminance x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "measurement" Tag
           (HS.Maybe MeasurementType) where
  getField (Tag_measurement x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "mediaWhitePoint" Tag
           (HS.Maybe (Vector.Vector XYZNumber)) where
  getField (Tag_mediaWhitePoint x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "namedColor2" Tag
           (HS.Maybe NamedColor2Type) where
  getField (Tag_namedColor2 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "outputResponse" Tag
           (HS.Maybe (Vector.Vector ResponseCurve)) where
  getField (Tag_outputResponse x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "perceptualRenderingIntentGamut" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Tag_perceptualRenderingIntentGamut x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "preview0" Tag (HS.Maybe Lut_8_16_AB_BA) where
  getField (Tag_preview0 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "preview1" Tag (HS.Maybe Lut_8_16_BA) where
  getField (Tag_preview1 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "preview2" Tag (HS.Maybe Lut_8_16_BA) where
  getField (Tag_preview2 x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "profileDescription" Tag
           (HS.Maybe (Vector.Vector UnicodeRecord)) where
  getField (Tag_profileDescription x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "profileSequenceDesc" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Tag_profileSequenceDesc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "profileSequenceIdentifier" Tag
           (HS.Maybe ()) where
  getField (Tag_profileSequenceIdentifier x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "redMatrixColumn" Tag
           (HS.Maybe (Vector.Vector XYZNumber)) where
  getField (Tag_redMatrixColumn x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "redTRC" Tag (HS.Maybe SomeCurve) where
  getField (Tag_redTRC x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "saturationRenderingIntentGamut" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Tag_saturationRenderingIntentGamut x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "technology" Tag
           (HS.Maybe (Vector.Vector (RTS.UInt 8))) where
  getField (Tag_technology x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "viewCondDesc" Tag
           (HS.Maybe (Vector.Vector UnicodeRecord)) where
  getField (Tag_viewCondDesc x) = HS.Just x
   
  getField _ = HS.Nothing
 
instance HS.HasField "viewConditions" Tag
           (HS.Maybe ViewConditionsType) where
  getField (Tag_viewConditions x) = HS.Just x
   
  getField _ = HS.Nothing
 
data ChromaticityType
  = ChromaticityType (RTS.UInt 16) (Vector.Vector XYNumber)
  
 
deriving instance HS.Eq ChromaticityType
 
deriving instance HS.Ord ChromaticityType
 
deriving instance HS.Show ChromaticityType
 
instance RTS.DDL ChromaticityType where
 
instance HS.HasField "phosphor_or_colorant" ChromaticityType
           (RTS.UInt 16) where
  getField (ChromaticityType x _) = x
 
instance HS.HasField "cie_coords" ChromaticityType
           (Vector.Vector XYNumber) where
  getField (ChromaticityType _ x) = x
 
pVersionField :: RTS.Parser VersionField
 
pVersionField =
  do (major :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "36:18--36:22"
     (min_bf :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "37:18--37:22"
     (minor :: RTS.UInt 4) <-
       HS.pure
         (RTS.convert (RTS.shiftr min_bf (RTS.lit 4 :: RTS.UInt 64))
            :: RTS.UInt 4)
     (bugfix :: RTS.UInt 4) <-
       HS.pure (RTS.convert min_bf :: RTS.UInt 4)
     HS.const ()
       HS.<$> RTS.pMatch "40:3--40:20"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     HS.pure (VersionField major minor bugfix)
 
pProfileClasses :: RTS.Parser ProfileClasses
 
pProfileClasses =
  do (__ :: ProfileClasses) <-
       (RTS.<||)
         (RTS.pEnter "input_device_profile"
            (do (_262 :: ()) <-
                  HS.const ()
                    HS.<$> RTS.pMatch "45:31--45:42" (Vector.vecFromRep "scnr")
                HS.pure (ProfileClasses_input_device_profile _262)))
         ((RTS.<||)
            (RTS.pEnter "display_device_profile"
               (do (_263 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch "46:31--46:42" (Vector.vecFromRep "mntr")
                   HS.pure (ProfileClasses_display_device_profile _263)))
            ((RTS.<||)
               (RTS.pEnter "output_device_profile"
                  (do (_264 :: ()) <-
                        HS.const ()
                          HS.<$> RTS.pMatch "47:31--47:42" (Vector.vecFromRep "prtr")
                      HS.pure (ProfileClasses_output_device_profile _264)))
               ((RTS.<||)
                  (RTS.pEnter "device_link_profile"
                     (do (_265 :: ()) <-
                           HS.const ()
                             HS.<$> RTS.pMatch "48:31--48:42" (Vector.vecFromRep "link")
                         HS.pure (ProfileClasses_device_link_profile _265)))
                  ((RTS.<||)
                     (RTS.pEnter "color_space_profile"
                        (do (_266 :: ()) <-
                              HS.const ()
                                HS.<$> RTS.pMatch "49:31--49:42" (Vector.vecFromRep "spac")
                            HS.pure (ProfileClasses_color_space_profile _266)))
                     ((RTS.<||)
                        (RTS.pEnter "abstract_profile"
                           (do (_267 :: ()) <-
                                 HS.const ()
                                   HS.<$> RTS.pMatch "50:31--50:42" (Vector.vecFromRep "abst")
                               HS.pure (ProfileClasses_abstract_profile _267)))
                        (RTS.pEnter "named_color_profile"
                           (do (_268 :: ()) <-
                                 HS.const ()
                                   HS.<$> RTS.pMatch "51:31--51:42" (Vector.vecFromRep "nmcl")
                               HS.pure (ProfileClasses_named_color_profile _268))))))))
     HS.pure __
 
pDataColorSpaces :: RTS.Parser DataColorSpaces
 
pDataColorSpaces =
  do (__ :: DataColorSpaces) <-
       (RTS.<||)
         (RTS.pEnter "nciexyz_or_pcsxyz"
            (do (_270 :: ()) <-
                  HS.const ()
                    HS.<$> RTS.pMatch "57:26--57:37" (Vector.vecFromRep "XYZ ")
                HS.pure (DataColorSpaces_nciexyz_or_pcsxyz _270)))
         ((RTS.<||)
            (RTS.pEnter "cielab_or_pcslab"
               (do (_271 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch "58:26--58:37" (Vector.vecFromRep "Lab ")
                   HS.pure (DataColorSpaces_cielab_or_pcslab _271)))
            ((RTS.<||)
               (RTS.pEnter "cieluv"
                  (do (_272 :: ()) <-
                        HS.const ()
                          HS.<$> RTS.pMatch "59:26--59:37" (Vector.vecFromRep "Luv ")
                      HS.pure (DataColorSpaces_cieluv _272)))
               ((RTS.<||)
                  (RTS.pEnter "ycbcr"
                     (do (_273 :: ()) <-
                           HS.const ()
                             HS.<$> RTS.pMatch "60:26--60:37" (Vector.vecFromRep "Ycbr")
                         HS.pure (DataColorSpaces_ycbcr _273)))
                  ((RTS.<||)
                     (RTS.pEnter "cieyxy"
                        (do (_274 :: ()) <-
                              HS.const ()
                                HS.<$> RTS.pMatch "61:26--61:37" (Vector.vecFromRep "Yxy ")
                            HS.pure (DataColorSpaces_cieyxy _274)))
                     ((RTS.<||)
                        (RTS.pEnter "rgb"
                           (do (_275 :: ()) <-
                                 HS.const ()
                                   HS.<$> RTS.pMatch "62:26--62:37" (Vector.vecFromRep "RGB ")
                               HS.pure (DataColorSpaces_rgb _275)))
                        ((RTS.<||)
                           (RTS.pEnter "gray"
                              (do (_276 :: ()) <-
                                    HS.const ()
                                      HS.<$> RTS.pMatch "63:26--63:37" (Vector.vecFromRep "GRAY")
                                  HS.pure (DataColorSpaces_gray _276)))
                           ((RTS.<||)
                              (RTS.pEnter "hsv"
                                 (do (_277 :: ()) <-
                                       HS.const ()
                                         HS.<$> RTS.pMatch "64:26--64:37" (Vector.vecFromRep "HSV ")
                                     HS.pure (DataColorSpaces_hsv _277)))
                              ((RTS.<||)
                                 (RTS.pEnter "hls"
                                    (do (_278 :: ()) <-
                                          HS.const ()
                                            HS.<$> RTS.pMatch "65:26--65:37"
                                                     (Vector.vecFromRep "HLS ")
                                        HS.pure (DataColorSpaces_hls _278)))
                                 ((RTS.<||)
                                    (RTS.pEnter "cmyk"
                                       (do (_279 :: ()) <-
                                             HS.const ()
                                               HS.<$> RTS.pMatch "66:26--66:37"
                                                        (Vector.vecFromRep "CMYK")
                                           HS.pure (DataColorSpaces_cmyk _279)))
                                    ((RTS.<||)
                                       (RTS.pEnter "cmy"
                                          (do (_280 :: ()) <-
                                                HS.const ()
                                                  HS.<$> RTS.pMatch "67:26--67:37"
                                                           (Vector.vecFromRep "CMY ")
                                              HS.pure (DataColorSpaces_cmy _280)))
                                       ((RTS.<||)
                                          (RTS.pEnter "two_colour"
                                             (do (_281 :: ()) <-
                                                   HS.const ()
                                                     HS.<$> RTS.pMatch "68:26--68:37"
                                                              (Vector.vecFromRep "2CLR")
                                                 HS.pure (DataColorSpaces_two_colour _281)))
                                          ((RTS.<||)
                                             (RTS.pEnter "three_colour"
                                                (do (_282 :: ()) <-
                                                      HS.const ()
                                                        HS.<$> RTS.pMatch "69:26--69:37"
                                                                 (Vector.vecFromRep "3CLR")
                                                    HS.pure (DataColorSpaces_three_colour _282)))
                                             ((RTS.<||)
                                                (RTS.pEnter "four_colour"
                                                   (do (_283 :: ()) <-
                                                         HS.const ()
                                                           HS.<$> RTS.pMatch "70:26--70:37"
                                                                    (Vector.vecFromRep "4CLR")
                                                       HS.pure (DataColorSpaces_four_colour _283)))
                                                ((RTS.<||)
                                                   (RTS.pEnter "five_colour"
                                                      (do (_284 :: ()) <-
                                                            HS.const ()
                                                              HS.<$> RTS.pMatch "71:26--71:37"
                                                                       (Vector.vecFromRep "5CLR")
                                                          HS.pure
                                                            (DataColorSpaces_five_colour _284)))
                                                   ((RTS.<||)
                                                      (RTS.pEnter "six_colour"
                                                         (do (_285 :: ()) <-
                                                               HS.const ()
                                                                 HS.<$> RTS.pMatch "72:26--72:37"
                                                                          (Vector.vecFromRep "6CLR")
                                                             HS.pure
                                                               (DataColorSpaces_six_colour _285)))
                                                      ((RTS.<||)
                                                         (RTS.pEnter "seven_colour"
                                                            (do (_286 :: ()) <-
                                                                  HS.const ()
                                                                    HS.<$> RTS.pMatch "73:26--73:37"
                                                                             (Vector.vecFromRep
                                                                                "7CLR")
                                                                HS.pure
                                                                  (DataColorSpaces_seven_colour
                                                                     _286)))
                                                         ((RTS.<||)
                                                            (RTS.pEnter "eight_colour"
                                                               (do (_287 :: ()) <-
                                                                     HS.const ()
                                                                       HS.<$> RTS.pMatch
                                                                                "74:26--74:37"
                                                                                (Vector.vecFromRep
                                                                                   "8CLR")
                                                                   HS.pure
                                                                     (DataColorSpaces_eight_colour
                                                                        _287)))
                                                            ((RTS.<||)
                                                               (RTS.pEnter "nine_colour"
                                                                  (do (_288 :: ()) <-
                                                                        HS.const ()
                                                                          HS.<$> RTS.pMatch
                                                                                   "75:26--75:37"
                                                                                   (Vector.vecFromRep
                                                                                      "9CLR")
                                                                      HS.pure
                                                                        (DataColorSpaces_nine_colour
                                                                           _288)))
                                                               ((RTS.<||)
                                                                  (RTS.pEnter "ten_colour"
                                                                     (do (_289 :: ()) <-
                                                                           HS.const ()
                                                                             HS.<$> RTS.pMatch
                                                                                      "76:26--76:37"
                                                                                      (Vector.vecFromRep
                                                                                         "ACLR")
                                                                         HS.pure
                                                                           (DataColorSpaces_ten_colour
                                                                              _289)))
                                                                  ((RTS.<||)
                                                                     (RTS.pEnter "eleven_colour"
                                                                        (do (_290 :: ()) <-
                                                                              HS.const ()
                                                                                HS.<$> RTS.pMatch
                                                                                         "77:26--77:37"
                                                                                         (Vector.vecFromRep
                                                                                            "BCLR")
                                                                            HS.pure
                                                                              (DataColorSpaces_eleven_colour
                                                                                 _290)))
                                                                     ((RTS.<||)
                                                                        (RTS.pEnter "twelve_colour"
                                                                           (do (_291 :: ()) <-
                                                                                 HS.const ()
                                                                                   HS.<$> RTS.pMatch
                                                                                            "78:26--78:37"
                                                                                            (Vector.vecFromRep
                                                                                               "CCLR")
                                                                               HS.pure
                                                                                 (DataColorSpaces_twelve_colour
                                                                                    _291)))
                                                                        ((RTS.<||)
                                                                           (RTS.pEnter
                                                                              "thirteen_colour"
                                                                              (do (_292 :: ()) <-
                                                                                    HS.const ()
                                                                                      HS.<$> RTS.pMatch
                                                                                               "79:26--79:37"
                                                                                               (Vector.vecFromRep
                                                                                                  "DCLR")
                                                                                  HS.pure
                                                                                    (DataColorSpaces_thirteen_colour
                                                                                       _292)))
                                                                           ((RTS.<||)
                                                                              (RTS.pEnter
                                                                                 "fourteen_colour"
                                                                                 (do (_293 :: ()) <-
                                                                                       HS.const ()
                                                                                         HS.<$> RTS.pMatch
                                                                                                  "80:26--80:37"
                                                                                                  (Vector.vecFromRep
                                                                                                     "ECLR")
                                                                                     HS.pure
                                                                                       (DataColorSpaces_fourteen_colour
                                                                                          _293)))
                                                                              (RTS.pEnter
                                                                                 "fifteen_colour"
                                                                                 (do (_294 :: ()) <-
                                                                                       HS.const ()
                                                                                         HS.<$> RTS.pMatch
                                                                                                  "81:26--81:37"
                                                                                                  (Vector.vecFromRep
                                                                                                     "FCLR")
                                                                                     HS.pure
                                                                                       (DataColorSpaces_fifteen_colour
                                                                                          _294))))))))))))))))))))))))))
     HS.pure __
 
pPrimaryPlatforms :: RTS.Parser PrimaryPlatforms
 
pPrimaryPlatforms =
  do (__ :: PrimaryPlatforms) <-
       (RTS.<||)
         (RTS.pEnter "none"
            (do (_296 :: ()) <-
                  HS.const ()
                    HS.<$> RTS.pMatch "87:30--87:44"
                             (Vector.fromList
                                [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                                 RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
                HS.pure (PrimaryPlatforms_none _296)))
         ((RTS.<||)
            (RTS.pEnter "apple_computer_inc"
               (do (_297 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch "88:30--88:41" (Vector.vecFromRep "APPL")
                   HS.pure (PrimaryPlatforms_apple_computer_inc _297)))
            ((RTS.<||)
               (RTS.pEnter "microsoft_corporation"
                  (do (_298 :: ()) <-
                        HS.const ()
                          HS.<$> RTS.pMatch "89:30--89:41" (Vector.vecFromRep "MSFT")
                      HS.pure (PrimaryPlatforms_microsoft_corporation _298)))
               ((RTS.<||)
                  (RTS.pEnter "silicon_graphics_inc"
                     (do (_299 :: ()) <-
                           HS.const ()
                             HS.<$> RTS.pMatch "90:30--90:41" (Vector.vecFromRep "SGI ")
                         HS.pure (PrimaryPlatforms_silicon_graphics_inc _299)))
                  (RTS.pEnter "sun_microsystems"
                     (do (_300 :: ()) <-
                           HS.const ()
                             HS.<$> RTS.pMatch "91:30--91:41" (Vector.vecFromRep "SUNW")
                         HS.pure (PrimaryPlatforms_sun_microsystems _300))))))
     HS.pure __
 
pRenderingIntent :: RTS.Parser RenderingIntent
 
pRenderingIntent =
  do (__ :: RenderingIntent) <-
       (RTS.<||)
         (RTS.pEnter "perceptual"
            (do (_302 :: ()) <-
                  HS.const ()
                    HS.<$> RTS.pMatch "98:36--98:50"
                             (Vector.fromList
                                [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                                 RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
                HS.pure (RenderingIntent_perceptual _302)))
         ((RTS.<||)
            (RTS.pEnter "media_relative_colorimetric"
               (do (_303 :: ()) <-
                     HS.const ()
                       HS.<$> RTS.pMatch "99:36--99:50"
                                (Vector.fromList
                                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                                    RTS.lit 0 :: RTS.UInt 8, RTS.lit 1 :: RTS.UInt 8])
                   HS.pure (RenderingIntent_media_relative_colorimetric _303)))
            ((RTS.<||)
               (RTS.pEnter "saturation"
                  (do (_304 :: ()) <-
                        HS.const ()
                          HS.<$> RTS.pMatch "100:36--100:50"
                                   (Vector.fromList
                                      [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                                       RTS.lit 0 :: RTS.UInt 8, RTS.lit 2 :: RTS.UInt 8])
                      HS.pure (RenderingIntent_saturation _304)))
               (RTS.pEnter "icc_absolute_colorimetric"
                  (do (_305 :: ()) <-
                        HS.const ()
                          HS.<$> RTS.pMatch "101:36--101:50"
                                   (Vector.fromList
                                      [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                                       RTS.lit 0 :: RTS.UInt 8, RTS.lit 3 :: RTS.UInt 8])
                      HS.pure (RenderingIntent_icc_absolute_colorimetric _305)))))
     HS.pure __
 
pBE16 :: RTS.Parser (RTS.UInt 16)
 
pBE16 =
  do (_306 :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "479:12--479:16"
     (_307 :: RTS.UInt 8) <- RTS.uint8 HS.<$> RTS.pByte "479:20--479:24"
     HS.pure (RTS.cat _306 _307)
 
pBE32 :: RTS.Parser (RTS.UInt 32)
 
pBE32 =
  do (_308 :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (_309 :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     HS.pure (RTS.cat _308 _309)
 
pXYZNumber :: RTS.Parser XYZNumber
 
pXYZNumber =
  do (x :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (y :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (z :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure (XYZNumber x y z)
 
pDateTimeNumber :: RTS.Parser DateTimeNumber
 
pDateTimeNumber =
  do (year :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (month :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (day :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (hour :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (minute :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (second :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     HS.pure (DateTimeNumber year month day hour minute second)
 
pBE64 :: RTS.Parser (RTS.UInt 64)
 
pBE64 =
  do (_310 :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (_311 :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure (RTS.cat _310 _311)
 
pProfileHeader :: RTS.Parser ProfileHeader
 
pProfileHeader =
  do (size :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (preferred_cmm_type :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (version :: VersionField) <-
       RTS.pEnter "ICC.VersionField" pVersionField
     (devce_class :: ProfileClasses) <-
       RTS.pEnter "ICC.ProfileClasses" pProfileClasses
     (color_space :: DataColorSpaces) <-
       RTS.pEnter "ICC.DataColorSpaces" pDataColorSpaces
     (pcs :: DataColorSpaces) <-
       RTS.pEnter "ICC.DataColorSpaces" pDataColorSpaces
     (creation_date_time :: DateTimeNumber) <-
       RTS.pEnter "ICC.DateTimeNumber" pDateTimeNumber
     HS.const ()
       HS.<$> RTS.pMatch "22:3--22:14" (Vector.vecFromRep "acsp")
     (primary_platform :: PrimaryPlatforms) <-
       RTS.pEnter "ICC.PrimaryPlatforms" pPrimaryPlatforms
     (profile_flags :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (device_manufacturer :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (device_model :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (device_attributes :: RTS.UInt 64) <- RTS.pEnter "ICC.BE64" pBE64
     (rendering_intent :: RenderingIntent) <-
       RTS.pEnter "ICC.RenderingIntent" pRenderingIntent
     (illuminant :: XYZNumber) <- RTS.pEnter "ICC.XYZNumber" pXYZNumber
     (creatior :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (identifier :: Vector.Vector (RTS.UInt 8)) <-
       Vector.replicateM (RTS.lit 16 :: RTS.UInt 64)
         (RTS.uint8 HS.<$> RTS.pByte "31:33--31:37")
     (reserved_data :: Vector.Vector (RTS.UInt 8)) <-
       Vector.replicateM (RTS.lit 28 :: RTS.UInt 64)
         (RTS.uint8
            HS.<$> RTS.pMatch1 "32:34--32:41" (RTS.bcSingle (RTS.uint8 0)))
     HS.pure
       (ProfileHeader size preferred_cmm_type version devce_class
          color_space
          pcs
          creation_date_time
          primary_platform
          profile_flags
          device_manufacturer
          device_model
          device_attributes
          rendering_intent
          illuminant
          creatior
          identifier
          reserved_data)
 
pTagEntry :: RTS.Parser TagEntry
 
pTagEntry =
  do (tag_signature :: Vector.Vector (RTS.UInt 8)) <-
       Vector.replicateM (RTS.lit 4 :: RTS.UInt 64)
         (RTS.uint8 HS.<$> RTS.pByte "159:36--159:40")
     (offset_to_data_element :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (size_of_data_element :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     HS.pure
       (TagEntry tag_signature offset_to_data_element
          size_of_data_element)
 
pTagTable :: RTS.Parser (Vector.Vector TagEntry)
 
pTagTable =
  do (tag_count :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (__ :: Vector.Vector TagEntry) <-
       Vector.replicateM (RTS.convert tag_count :: RTS.UInt 64)
         (RTS.pEnter "ICC.TagEntry" pTagEntry)
     HS.pure __
 
pMain :: RTS.Parser Main
 
pMain =
  do (profileHeader :: ProfileHeader) <-
       RTS.pEnter "ICC.ProfileHeader" pProfileHeader
     (tagTable :: Vector.Vector TagEntry) <-
       RTS.pEnter "ICC.TagTable" pTagTable
     HS.pure (Main profileHeader tagTable)
 
pXYNumber :: RTS.Parser XYNumber
 
pXYNumber =
  do (x :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (y :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure (XYNumber x y)
 
pPositionNumber :: RTS.Parser PositionNumber
 
pPositionNumber =
  do (offset :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (size :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure (PositionNumber offset size)
 
pASCII7 :: RTS.Parser (Vector.Vector (RTS.UInt 7))
 
pASCII7 =
  do (__ :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pMany (RTS.<||)
         (do (_313 :: RTS.UInt 8) <-
               RTS.uint8
                 HS.<$> RTS.pMatch1 "136:14--136:24"
                          (RTS.bcRange (RTS.lit 1 :: RTS.UInt 8) (RTS.lit 255 :: RTS.UInt 8))
             RTS.pIsJust "136:14--136:35" "Value does not fit in target type"
               (RTS.convertMaybe _313 :: HS.Maybe (RTS.UInt 7)))
     (RTS.<||)
       (RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: RTS.UInt 64)
          (HS.const ()
             HS.<$> RTS.pMatch1 "137:17--137:24" (RTS.bcSingle (RTS.uint8 0))))
       (RTS.pError RTS.FromUser "137:30--137:59"
          (Vector.vecToString (Vector.vecFromRep "Non 0 string terminator")))
     HS.pure __
 
pResponse16Number :: RTS.Parser Response16Number
 
pResponse16Number =
  do (device :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     HS.const ()
       HS.<$> RTS.pMatch "142:3--142:13"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     (measurement :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure (Response16Number device measurement)
 
_GotoRel :: RTS.Input -> (RTS.UInt 64 -> RTS.Parser ())
 
_GotoRel (s :: RTS.Input) (n :: RTS.UInt 64) =
  do (s1 :: RTS.Input) <-
       RTS.pIsJust "493:9--493:14" "Not enough bytes" (RTS.advanceBy n s)
     RTS.pSetInput s1
 
_Goto :: RTS.UInt 64 -> RTS.Parser ()
 
_Goto (n :: RTS.UInt 64) =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._GotoRel" (_GotoRel s n)
 
pChunk :: RTS.UInt 64 -> RTS.Parser RTS.Input
 
pChunk (sz :: RTS.UInt 64) =
  do (s :: RTS.Input) <- RTS.pPeek
     (__ :: RTS.Input) <-
       RTS.pIsJust "500:8--500:14" "Not enough bytes" (RTS.limitLen sz s)
     RTS.pEnter "ICC._Goto" (_Goto sz)
     HS.pure __
 
exp ::
  forall a g.
    (RTS.DDL a, RTS.DDL g, RTS.Literal 1 a, RTS.Literal 0 g,
     RTS.Literal 1 g, RTS.Numeric g, RTS.Numeric a) =>
      a -> (g -> a)
 
exp (b :: a) (e :: g) =
  RTS.loopFold (\(x :: a) (i :: g) -> RTS.mul x b) (RTS.lit 1 :: a)
    (Vector.rangeUp (RTS.lit 0 :: g) e (RTS.lit 1 :: g))
 
_StartTag :: Vector.Vector (RTS.UInt 8) -> RTS.Parser ()
 
_StartTag (x :: Vector.Vector (RTS.UInt 8)) =
  do HS.const () HS.<$> RTS.pMatch "264:20--264:26" x
     RTS.pErrorMode RTS.Abort
       (HS.const ()
          HS.<$> RTS.pMatch "264:37--264:51"
                   (Vector.fromList
                      [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                       RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8]))
 
pLut8Type :: RTS.Parser Lut8Type
 
pLut8Type =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mft1"))
     (number_of_input_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "352:30--352:34"
     (i :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_input_channels :: RTS.UInt 64)
     (number_of_output_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "354:31--354:35"
     (o :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_output_channels :: RTS.UInt 64)
     (number_of_clut_grid_points :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "356:32--356:36"
     (g :: RTS.UInt 64) <-
       RTS.pIsJust "357:8--357:41" "Value does not fit in target type"
         (RTS.convertMaybe number_of_clut_grid_points
            :: HS.Maybe (RTS.UInt 64))
     HS.const ()
       HS.<$> RTS.pMatch1 "358:3--358:13" (RTS.bcSingle (RTS.uint8 0))
     (encoded_e_parameters :: Vector.Vector (RTS.SInt 32)) <-
       Vector.replicateM (RTS.lit 9 :: RTS.UInt 64)
         (do (x :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
             (__ :: RTS.SInt 32) <- HS.pure (RTS.convert x :: RTS.SInt 32)
             HS.pure __)
     (input_tables :: RTS.Input) <-
       RTS.pEnter "ICC.Chunk"
         (pChunk (RTS.mul (RTS.lit 256 :: RTS.UInt 64) i))
     (clut_values :: RTS.Input) <-
       RTS.pEnter "ICC.Chunk"
         (pChunk (RTS.mul (exp @(RTS.UInt 64) @(RTS.UInt 64) g i) o))
     (output_tables :: RTS.Input) <-
       RTS.pEnter "ICC.Chunk"
         (pChunk (RTS.mul (RTS.lit 256 :: RTS.UInt 64) o))
     HS.pure
       (Lut8Type number_of_input_channels number_of_output_channels
          number_of_clut_grid_points
          encoded_e_parameters
          input_tables
          clut_values
          output_tables)
 
pLut16Type :: RTS.Parser Lut16Type
 
pLut16Type =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mft2"))
     (number_of_input_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "367:30--367:34"
     (i :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_input_channels :: RTS.UInt 64)
     (number_of_output_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "369:31--369:35"
     (o :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_output_channels :: RTS.UInt 64)
     (number_of_clut_grid_points :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "371:32--371:36"
     (g :: RTS.UInt 64) <-
       RTS.pIsJust "372:8--372:41" "Value does not fit in target type"
         (RTS.convertMaybe number_of_clut_grid_points
            :: HS.Maybe (RTS.UInt 64))
     HS.const ()
       HS.<$> RTS.pMatch1 "373:3--373:13" (RTS.bcSingle (RTS.uint8 0))
     (encoded_e_parameters :: Vector.Vector (RTS.SInt 32)) <-
       Vector.replicateM (RTS.lit 9 :: RTS.UInt 64)
         (do (x :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
             (__ :: RTS.SInt 32) <- HS.pure (RTS.convert x :: RTS.SInt 32)
             HS.pure __)
     (number_of_input_table_entries :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (n :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_input_table_entries :: RTS.UInt 64)
     (number_of_output_table_entries :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (m :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_output_table_entries :: RTS.UInt 64)
     (input_tables :: RTS.Input) <-
       RTS.pEnter "ICC.Chunk"
         (pChunk (RTS.mul (RTS.mul (RTS.lit 256 :: RTS.UInt 64) n) i))
     (clut_values :: RTS.Input) <-
       RTS.pEnter "ICC.Chunk"
         (pChunk
            (RTS.mul
               (RTS.mul (RTS.lit 2 :: RTS.UInt 64)
                  (exp @(RTS.UInt 64) @(RTS.UInt 64) g i))
               o))
     (output_tables :: RTS.Input) <-
       RTS.pEnter "ICC.Chunk"
         (pChunk (RTS.mul (RTS.mul (RTS.lit 2 :: RTS.UInt 64) m) o))
     HS.pure
       (Lut16Type number_of_input_channels number_of_output_channels
          number_of_clut_grid_points
          encoded_e_parameters
          number_of_input_table_entries
          number_of_output_table_entries
          input_tables
          clut_values
          output_tables)
 
pLutAToBType :: RTS.Parser LutAToBType
 
pLutAToBType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mAB "))
     (number_of_input_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "386:31--386:35"
     (number_of_output_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "387:31--387:35"
     HS.const ()
       HS.<$> RTS.pMatch "388:3--388:13"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     (offset_first_B_curve :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (offset_to_matrix :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (offset_to_first_M_curve :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (offset_to_CLUT :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (offset_to_first_A_curve :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (_data :: RTS.Input) <- RTS.pPeek
     HS.pure
       (LutAToBType number_of_input_channels number_of_output_channels
          offset_first_B_curve
          offset_to_matrix
          offset_to_first_M_curve
          offset_to_CLUT
          offset_to_first_A_curve
          _data)
 
pLutBToAType :: RTS.Parser LutBToAType
 
pLutBToAType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mBA "))
     (number_of_input_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "400:31--400:35"
     (number_of_output_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "401:31--401:35"
     HS.const ()
       HS.<$> RTS.pMatch "402:3--402:13"
                (Vector.fromList
                   [RTS.lit 2 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     (offset_first_B_curve :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (offset_to_matrix :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (offset_to_first_M_curve :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (offset_to_CLUT :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (offset_to_first_A_curve :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (_data :: RTS.Input) <- RTS.pPeek
     HS.pure
       (LutBToAType number_of_input_channels number_of_output_channels
          offset_first_B_curve
          offset_to_matrix
          offset_to_first_M_curve
          offset_to_CLUT
          offset_to_first_A_curve
          _data)
 
pLut_8_16_AB_BA :: RTS.Parser Lut_8_16_AB_BA
 
pLut_8_16_AB_BA =
  (RTS.<||)
    (RTS.pEnter "lut8"
       (do (_319 :: Lut8Type) <- RTS.pEnter "ICC.Lut8Type" pLut8Type
           HS.pure (Lut_8_16_AB_BA_lut8 _319)))
    ((RTS.<||)
       (RTS.pEnter "lut16"
          (do (_320 :: Lut16Type) <- RTS.pEnter "ICC.Lut16Type" pLut16Type
              HS.pure (Lut_8_16_AB_BA_lut16 _320)))
       ((RTS.<||)
          (RTS.pEnter "lutAB"
             (do (_321 :: LutAToBType) <-
                   RTS.pEnter "ICC.LutAToBType" pLutAToBType
                 HS.pure (Lut_8_16_AB_BA_lutAB _321)))
          (RTS.pEnter "lutBA"
             (do (_322 :: LutBToAType) <-
                   RTS.pEnter "ICC.LutBToAType" pLutBToAType
                 HS.pure (Lut_8_16_AB_BA_lutBA _322)))))
 
pLut_8_16_AB :: RTS.Parser Lut_8_16_AB
 
pLut_8_16_AB =
  (RTS.<||)
    (RTS.pEnter "lut8"
       (do (_323 :: Lut8Type) <- RTS.pEnter "ICC.Lut8Type" pLut8Type
           HS.pure (Lut_8_16_AB_lut8 _323)))
    ((RTS.<||)
       (RTS.pEnter "lut16"
          (do (_324 :: Lut16Type) <- RTS.pEnter "ICC.Lut16Type" pLut16Type
              HS.pure (Lut_8_16_AB_lut16 _324)))
       (RTS.pEnter "lutAB"
          (do (_325 :: LutAToBType) <-
                RTS.pEnter "ICC.LutAToBType" pLutAToBType
              HS.pure (Lut_8_16_AB_lutAB _325))))
 
pLut_8_16_BA :: RTS.Parser Lut_8_16_BA
 
pLut_8_16_BA =
  (RTS.<||)
    (RTS.pEnter "lut8"
       (do (_326 :: Lut8Type) <- RTS.pEnter "ICC.Lut8Type" pLut8Type
           HS.pure (Lut_8_16_BA_lut8 _326)))
    ((RTS.<||)
       (RTS.pEnter "lut16"
          (do (_327 :: Lut16Type) <- RTS.pEnter "ICC.Lut16Type" pLut16Type
              HS.pure (Lut_8_16_BA_lut16 _327)))
       (RTS.pEnter "lutBA"
          (do (_328 :: LutBToAType) <-
                RTS.pEnter "ICC.LutBToAType" pLutBToAType
              HS.pure (Lut_8_16_BA_lutBA _328))))
 
pCurveType :: RTS.Parser (Vector.Vector (RTS.UInt 16))
 
pCurveType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "curv"))
     (n :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (__ :: Vector.Vector (RTS.UInt 16)) <-
       Vector.replicateM (RTS.convert n :: RTS.UInt 64)
         (RTS.pEnter "ICC.BE16" pBE16)
     HS.pure __
 
pParametricCurveType :: RTS.Parser ParametricCurveType
 
pParametricCurveType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "para"))
     (function :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     HS.const ()
       HS.<$> RTS.pMatch "327:3--327:13"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     (parameters :: Vector.Vector (RTS.UInt 32)) <-
       RTS.pMany (RTS.<||) (RTS.pEnter "ICC.BE32" pBE32)
     HS.pure (ParametricCurveType function parameters)
 
pSomeCurve :: RTS.Parser SomeCurve
 
pSomeCurve =
  (RTS.<||)
    (RTS.pEnter "curve"
       (do (_330 :: Vector.Vector (RTS.UInt 16)) <-
             RTS.pEnter "ICC.CurveType" pCurveType
           HS.pure (SomeCurve_curve _330)))
    (RTS.pEnter "parametric_curve"
       (do (_331 :: ParametricCurveType) <-
             RTS.pEnter "ICC.ParametricCurveType" pParametricCurveType
           HS.pure (SomeCurve_parametric_curve _331)))
 
pDateTimeType :: RTS.Parser DateTimeNumber
 
pDateTimeType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "dtim"))
     (__ :: DateTimeNumber) <-
       RTS.pEnter "ICC.DateTimeNumber" pDateTimeNumber
     HS.pure __
 
pOnly :: forall b. RTS.DDL b => RTS.Parser b -> RTS.Parser b
 
pOnly (pP :: RTS.Parser b) =
  do (__ :: b) <- pP
     RTS.pEnd "535:24--535:26"
     HS.pure __
 
pTextType :: RTS.Parser (Vector.Vector (RTS.UInt 7))
 
pTextType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "text"))
     (__ :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pEnter "ICC.Only"
         (pOnly @(Vector.Vector (RTS.UInt 7))
            (RTS.pEnter "ICC.ASCII7" pASCII7))
     HS.pure __
 
pSignatureType :: RTS.Parser (Vector.Vector (RTS.UInt 8))
 
pSignatureType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "sig "))
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       Vector.replicateM (RTS.lit 4 :: RTS.UInt 64)
         (RTS.uint8 HS.<$> RTS.pByte "270:47--270:51")
     HS.pure __
 
pRemote :: forall c. RTS.DDL c => RTS.Parser c -> RTS.Parser c
 
pRemote (pP :: RTS.Parser c) =
  do (s :: RTS.Input) <- RTS.pPeek
     (__ :: c) <- pP
     RTS.pSetInput s
     HS.pure __
 
pChunkRelativeTo ::
      RTS.Input -> (RTS.UInt 64 -> (RTS.UInt 64 -> RTS.Parser RTS.Input))
 
pChunkRelativeTo (s :: RTS.Input) (off :: RTS.UInt 64)
  (sz :: RTS.UInt 64) =
  do RTS.pEnter "ICC._GotoRel" (_GotoRel s off)
     (__ :: RTS.Input) <- RTS.pEnter "ICC.Chunk" (pChunk sz)
     HS.pure __
 
pUnicodeRecord :: RTS.Input -> RTS.Parser UnicodeRecord
 
pUnicodeRecord (s :: RTS.Input) =
  do (language :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (country :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (size :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (offset :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (_data :: RTS.Input) <-
       RTS.pEnter "ICC.Remote"
         (pRemote @RTS.Input
            (RTS.pEnter "ICC.ChunkRelativeTo"
               (pChunkRelativeTo s (RTS.convert offset :: RTS.UInt 64)
                  (RTS.convert size :: RTS.UInt 64))))
     HS.pure (UnicodeRecord language country _data)
 
_Guard :: HS.Bool -> RTS.Parser ()
 
_Guard (p :: HS.Bool) =
  RTS.pGuard "537:15--537:23" "guard failed" p
 
pMultiLocalizedUnicodeType ::
      RTS.Parser (Vector.Vector UnicodeRecord)
 
pMultiLocalizedUnicodeType =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "mluc"))
     (record_number :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (record_size :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pEnter "ICC._Guard"
       (_Guard (record_size HS.== (RTS.lit 12 :: RTS.UInt 32)))
     (__ :: Vector.Vector UnicodeRecord) <-
       Vector.replicateM (RTS.convert record_number :: RTS.UInt 64)
         (RTS.pEnter "ICC.UnicodeRecord" (pUnicodeRecord s))
     HS.pure __
 
pS15Fixed16ArrayType :: RTS.Parser (Vector.Vector (RTS.UInt 32))
 
pS15Fixed16ArrayType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "sf32"))
     (__ :: Vector.Vector (RTS.UInt 32)) <-
       RTS.pMany (RTS.<||) (RTS.pEnter "ICC.BE32" pBE32)
     HS.pure __
 
_BE16 :: RTS.Parser ()
 
_BE16 =
  do HS.const () HS.<$> RTS.pByte "479:12--479:16"
     HS.const () HS.<$> RTS.pByte "479:20--479:24"
 
_BE32 :: RTS.Parser ()
 
_BE32 =
  do RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
 
pColorantOrderType :: RTS.Parser (Vector.Vector (RTS.UInt 8))
 
pColorantOrderType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "clro"))
     RTS.pEnter "ICC._BE32" _BE32
     (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pMany (RTS.<||) (RTS.uint8 HS.<$> RTS.pByte "304:8--304:12")
     HS.pure __
 
pParseChunk ::
  forall e.
    RTS.DDL e => RTS.UInt 64 -> (RTS.Parser e -> RTS.Parser e)
 
pParseChunk (sz :: RTS.UInt 64) (pP :: RTS.Parser e) =
  do (s :: RTS.Input) <- RTS.pPeek
     (s1 :: RTS.Input) <-
       RTS.pIsJust "520:9--520:15" "Not enough bytes" (RTS.limitLen sz s)
     RTS.pSetInput s1
     (__ :: e) <- pP
     (s2 :: RTS.Input) <-
       RTS.pIsJust "523:9--523:15" "Not enough bytes" (RTS.advanceBy sz s)
     RTS.pSetInput s2
     HS.pure __
 
pColorant :: RTS.Parser Colorant
 
pColorant =
  do (name :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pEnter "ICC.ParseChunk"
         (pParseChunk @(Vector.Vector (RTS.UInt 7))
            (RTS.lit 32 :: RTS.UInt 64)
            (RTS.pEnter "ICC.Only"
               (pOnly @(Vector.Vector (RTS.UInt 7))
                  (RTS.pEnter "ICC.ASCII7" pASCII7))))
     (pcs :: Vector.Vector (RTS.UInt 16)) <-
       Vector.replicateM (RTS.lit 3 :: RTS.UInt 64)
         (RTS.pEnter "ICC.BE16" pBE16)
     HS.pure (Colorant name pcs)
 
pColorantTableType :: RTS.Parser (Vector.Vector Colorant)
 
pColorantTableType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "clrt"))
     (count_of_colorant :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (__ :: Vector.Vector Colorant) <-
       Vector.replicateM (RTS.convert count_of_colorant :: RTS.UInt 64)
         (RTS.pEnter "ICC.Colorant" pColorant)
     HS.pure __
 
pResponseCurve :: RTS.UInt 64 -> RTS.Parser ResponseCurve
 
pResponseCurve (n :: RTS.UInt 64) =
  do (measurement_unit :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (counts :: Vector.Vector (RTS.UInt 32)) <-
       Vector.replicateM n (RTS.pEnter "ICC.BE32" pBE32)
     (pcxyzs :: Vector.Vector XYNumber) <-
       Vector.replicateM n (RTS.pEnter "ICC.XYNumber" pXYNumber)
     (response_arrays
        :: Vector.Vector (Vector.Vector Response16Number)) <-
       RTS.loopMapM
         (\(qi :: RTS.UInt 32) ->
            Vector.replicateM (RTS.convert qi :: RTS.UInt 64)
              (RTS.pEnter "ICC.Response16Number" pResponse16Number))
         counts
         :: RTS.Parser (Vector.Vector (Vector.Vector Response16Number))
     HS.pure (ResponseCurve measurement_unit pcxyzs response_arrays)
 
pResponseCurveSet16Type :: RTS.Parser (Vector.Vector ResponseCurve)
 
pResponseCurveSet16Type =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "rcs2"))
     (number_of_channels :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (count :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (__ :: Vector.Vector ResponseCurve) <-
       Vector.replicateM (RTS.convert count :: RTS.UInt 64)
         (do (off :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
             (__ :: ResponseCurve) <-
               RTS.pEnter "ICC.Remote"
                 (pRemote @ResponseCurve
                    (do RTS.pEnter "ICC._GotoRel"
                          (_GotoRel s (RTS.convert off :: RTS.UInt 64))
                        (__ :: ResponseCurve) <-
                          RTS.pEnter "ICC.ResponseCurve"
                            (pResponseCurve (RTS.convert number_of_channels :: RTS.UInt 64))
                        HS.pure __))
             HS.pure __)
     HS.pure __
 
pMultiProcessElementsType :: RTS.Parser MultiProcessElementsType
 
pMultiProcessElementsType =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "mpet"))
     (number_of_input_channels :: RTS.UInt 16) <-
       RTS.pEnter "ICC.BE16" pBE16
     (number_of_output_channels :: RTS.UInt 16) <-
       RTS.pEnter "ICC.BE16" pBE16
     (number_of_processing_elements :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (n :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_processing_elements :: RTS.UInt 64)
     RTS.pEnter "ICC._Guard"
       (_Guard ((RTS.lit 0 :: RTS.UInt 64) HS.< n))
     (els :: Vector.Vector PositionNumber) <-
       Vector.replicateM n
         (RTS.pEnter "ICC.PositionNumber" pPositionNumber)
     (elements :: Vector.Vector RTS.Input) <-
       RTS.loopMapM
         (\(e :: PositionNumber) ->
            RTS.pEnter "ICC.ChunkRelativeTo"
              (pChunkRelativeTo s
                 (RTS.convert (HS.getField @"offset" e) :: RTS.UInt 64)
                 (RTS.convert (HS.getField @"size" e) :: RTS.UInt 64)))
         els
         :: RTS.Parser (Vector.Vector RTS.Input)
     HS.pure
       (MultiProcessElementsType number_of_input_channels
          number_of_output_channels
          number_of_processing_elements
          n
          elements)
 
pXYZType :: RTS.Parser (Vector.Vector XYZNumber)
 
pXYZType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "XYZ "))
     (__ :: Vector.Vector XYZNumber) <-
       RTS.pMany (RTS.<||) (RTS.pEnter "ICC.XYZNumber" pXYZNumber)
     HS.pure __
 
pMeasurementType :: RTS.Parser MeasurementType
 
pMeasurementType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "meas"))
     (standard_observer :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (nCIEXYZ :: XYZNumber) <- RTS.pEnter "ICC.XYZNumber" pXYZNumber
     (geometry :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (flare :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (illuminant :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure
       (MeasurementType standard_observer nCIEXYZ geometry flare
          illuminant)
 
pColorName :: RTS.UInt 64 -> RTS.Parser ColorName
 
pColorName (m :: RTS.UInt 64) =
  do (name_root :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pEnter "ICC.ParseChunk"
         (pParseChunk @(Vector.Vector (RTS.UInt 7))
            (RTS.lit 32 :: RTS.UInt 64)
            (RTS.pEnter "ICC.ASCII7" pASCII7))
     (pcs_coords :: Vector.Vector (RTS.UInt 16)) <-
       Vector.replicateM (RTS.lit 3 :: RTS.UInt 64)
         (RTS.pEnter "ICC.BE16" pBE16)
     (device_coords :: Vector.Vector (RTS.UInt 16)) <-
       Vector.replicateM m (RTS.pEnter "ICC.BE16" pBE16)
     HS.pure (ColorName name_root pcs_coords device_coords)
 
pNamedColor2Type :: RTS.Parser NamedColor2Type
 
pNamedColor2Type =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "ncl2"))
     (vendor_specific :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (count :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (number_of_coords :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (prefix :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pEnter "ICC.ParseChunk"
         (pParseChunk @(Vector.Vector (RTS.UInt 7))
            (RTS.lit 32 :: RTS.UInt 64)
            (RTS.pEnter "ICC.Only"
               (pOnly @(Vector.Vector (RTS.UInt 7))
                  (RTS.pEnter "ICC.ASCII7" pASCII7))))
     (suffix :: Vector.Vector (RTS.UInt 7)) <-
       RTS.pEnter "ICC.ParseChunk"
         (pParseChunk @(Vector.Vector (RTS.UInt 7))
            (RTS.lit 32 :: RTS.UInt 64)
            (RTS.pEnter "ICC.Only"
               (pOnly @(Vector.Vector (RTS.UInt 7))
                  (RTS.pEnter "ICC.ASCII7" pASCII7))))
     (names :: Vector.Vector ColorName) <-
       Vector.replicateM (RTS.convert count :: RTS.UInt 64)
         (RTS.pEnter "ICC.ColorName"
            (pColorName (RTS.convert number_of_coords :: RTS.UInt 64)))
     HS.pure (NamedColor2Type vendor_specific prefix suffix names)
 
pStartTag ::
      Vector.Vector (RTS.UInt 8)
        -> RTS.Parser (Vector.Vector (RTS.UInt 8))
 
pStartTag (x :: Vector.Vector (RTS.UInt 8)) =
  do HS.const () HS.<$> RTS.pMatch "264:20--264:26" x
     RTS.pErrorMode RTS.Abort
       (do (__ :: Vector.Vector (RTS.UInt 8)) <-
             RTS.pMatch "264:37--264:51"
               (Vector.fromList
                  [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                   RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
           HS.pure __)
 
pProfileSequenceDescType :: RTS.Parser (Vector.Vector (RTS.UInt 8))
 
pProfileSequenceDescType =
  do (__ :: Vector.Vector (RTS.UInt 8)) <-
       RTS.pEnter "ICC.StartTag" (pStartTag (Vector.vecFromRep "pseq"))
     HS.pure __
 
pViewConditionsType :: RTS.Parser ViewConditionsType
 
pViewConditionsType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "view"))
     (illuminantXYZ :: XYZNumber) <-
       RTS.pEnter "ICC.XYZNumber" pXYZNumber
     (surroundXYZ :: XYZNumber) <- RTS.pEnter "ICC.XYZNumber" pXYZNumber
     (illuminant :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     HS.pure (ViewConditionsType illuminantXYZ surroundXYZ illuminant)
 
pTag :: Vector.Vector (RTS.UInt 8) -> RTS.Parser Tag
 
pTag (sig :: Vector.Vector (RTS.UInt 8)) =
  (RTS.<||)
    ((RTS.<||)
       (RTS.pEnter "AToB0"
          (do (_345 :: Lut_8_16_AB) <-
                do RTS.pEnter "ICC._Guard"
                     (_Guard (sig HS.== Vector.vecFromRep "A2B0"))
                   RTS.pErrorMode RTS.Abort
                     (do (__ :: Lut_8_16_AB) <-
                           RTS.pEnter "ICC.Lut_8_16_AB" pLut_8_16_AB
                         HS.pure __)
              HS.pure (Tag_AToB0 _345)))
       ((RTS.<||)
          (RTS.pEnter "AToB1"
             (do (_347 :: Lut_8_16_AB) <-
                   do RTS.pEnter "ICC._Guard"
                        (_Guard (sig HS.== Vector.vecFromRep "A2B1"))
                      RTS.pErrorMode RTS.Abort
                        (do (__ :: Lut_8_16_AB) <-
                              RTS.pEnter "ICC.Lut_8_16_AB" pLut_8_16_AB
                            HS.pure __)
                 HS.pure (Tag_AToB1 _347)))
          ((RTS.<||)
             (RTS.pEnter "AToB2"
                (do (_349 :: Lut_8_16_AB) <-
                      do RTS.pEnter "ICC._Guard"
                           (_Guard (sig HS.== Vector.vecFromRep "A2B2"))
                         RTS.pErrorMode RTS.Abort
                           (do (__ :: Lut_8_16_AB) <-
                                 RTS.pEnter "ICC.Lut_8_16_AB" pLut_8_16_AB
                               HS.pure __)
                    HS.pure (Tag_AToB2 _349)))
             ((RTS.<||)
                (RTS.pEnter "blueMatrixColumn"
                   (do (_351 :: Vector.Vector XYZNumber) <-
                         do RTS.pEnter "ICC._Guard"
                              (_Guard (sig HS.== Vector.vecFromRep "bXYZ"))
                            RTS.pErrorMode RTS.Abort
                              (do (__ :: Vector.Vector XYZNumber) <-
                                    RTS.pEnter "ICC.XYZType" pXYZType
                                  HS.pure __)
                       HS.pure (Tag_blueMatrixColumn _351)))
                ((RTS.<||)
                   (RTS.pEnter "blueTRC"
                      (do (_353 :: SomeCurve) <-
                            do RTS.pEnter "ICC._Guard"
                                 (_Guard (sig HS.== Vector.vecFromRep "bTRC"))
                               RTS.pErrorMode RTS.Abort
                                 (do (__ :: SomeCurve) <- RTS.pEnter "ICC.SomeCurve" pSomeCurve
                                     HS.pure __)
                          HS.pure (Tag_blueTRC _353)))
                   ((RTS.<||)
                      (RTS.pEnter "BToA0"
                         (do (_355 :: Lut_8_16_BA) <-
                               do RTS.pEnter "ICC._Guard"
                                    (_Guard (sig HS.== Vector.vecFromRep "B2A0"))
                                  RTS.pErrorMode RTS.Abort
                                    (do (__ :: Lut_8_16_BA) <-
                                          RTS.pEnter "ICC.Lut_8_16_BA" pLut_8_16_BA
                                        HS.pure __)
                             HS.pure (Tag_BToA0 _355)))
                      ((RTS.<||)
                         (RTS.pEnter "BToA1"
                            (do (_357 :: Lut_8_16_BA) <-
                                  do RTS.pEnter "ICC._Guard"
                                       (_Guard (sig HS.== Vector.vecFromRep "B2A1"))
                                     RTS.pErrorMode RTS.Abort
                                       (do (__ :: Lut_8_16_BA) <-
                                             RTS.pEnter "ICC.Lut_8_16_BA" pLut_8_16_BA
                                           HS.pure __)
                                HS.pure (Tag_BToA1 _357)))
                         ((RTS.<||)
                            (RTS.pEnter "BToA2"
                               (do (_359 :: Lut_8_16_BA) <-
                                     do RTS.pEnter "ICC._Guard"
                                          (_Guard (sig HS.== Vector.vecFromRep "B2A2"))
                                        RTS.pErrorMode RTS.Abort
                                          (do (__ :: Lut_8_16_BA) <-
                                                RTS.pEnter "ICC.Lut_8_16_BA" pLut_8_16_BA
                                              HS.pure __)
                                   HS.pure (Tag_BToA2 _359)))
                            ((RTS.<||)
                               (RTS.pEnter "BToD0"
                                  (do (_361 :: MultiProcessElementsType) <-
                                        do RTS.pEnter "ICC._Guard"
                                             (_Guard (sig HS.== Vector.vecFromRep "B2D0"))
                                           RTS.pErrorMode RTS.Abort
                                             (do (__ :: MultiProcessElementsType) <-
                                                   RTS.pEnter "ICC.MultiProcessElementsType"
                                                     pMultiProcessElementsType
                                                 HS.pure __)
                                      HS.pure (Tag_BToD0 _361)))
                               ((RTS.<||)
                                  (RTS.pEnter "BToD1"
                                     (do (_363 :: MultiProcessElementsType) <-
                                           do RTS.pEnter "ICC._Guard"
                                                (_Guard (sig HS.== Vector.vecFromRep "B2D1"))
                                              RTS.pErrorMode RTS.Abort
                                                (do (__ :: MultiProcessElementsType) <-
                                                      RTS.pEnter "ICC.MultiProcessElementsType"
                                                        pMultiProcessElementsType
                                                    HS.pure __)
                                         HS.pure (Tag_BToD1 _363)))
                                  ((RTS.<||)
                                     (RTS.pEnter "BToD2"
                                        (do (_365 :: MultiProcessElementsType) <-
                                              do RTS.pEnter "ICC._Guard"
                                                   (_Guard (sig HS.== Vector.vecFromRep "B2D2"))
                                                 RTS.pErrorMode RTS.Abort
                                                   (do (__ :: MultiProcessElementsType) <-
                                                         RTS.pEnter "ICC.MultiProcessElementsType"
                                                           pMultiProcessElementsType
                                                       HS.pure __)
                                            HS.pure (Tag_BToD2 _365)))
                                     ((RTS.<||)
                                        (RTS.pEnter "BToD3"
                                           (do (_367 :: MultiProcessElementsType) <-
                                                 do RTS.pEnter "ICC._Guard"
                                                      (_Guard (sig HS.== Vector.vecFromRep "B2D3"))
                                                    RTS.pErrorMode RTS.Abort
                                                      (do (__ :: MultiProcessElementsType) <-
                                                            RTS.pEnter
                                                              "ICC.MultiProcessElementsType"
                                                              pMultiProcessElementsType
                                                          HS.pure __)
                                               HS.pure (Tag_BToD3 _367)))
                                        ((RTS.<||)
                                           (RTS.pEnter "calibrationDateTime"
                                              (do (_369 :: DateTimeNumber) <-
                                                    do RTS.pEnter "ICC._Guard"
                                                         (_Guard
                                                            (sig HS.== Vector.vecFromRep "calt"))
                                                       RTS.pErrorMode RTS.Abort
                                                         (do (__ :: DateTimeNumber) <-
                                                               RTS.pEnter "ICC.DateTimeType"
                                                                 pDateTimeType
                                                             HS.pure __)
                                                  HS.pure (Tag_calibrationDateTime _369)))
                                           ((RTS.<||)
                                              (RTS.pEnter "charTarget"
                                                 (do (_371 :: Vector.Vector (RTS.UInt 7)) <-
                                                       do RTS.pEnter "ICC._Guard"
                                                            (_Guard
                                                               (sig HS.== Vector.vecFromRep "targ"))
                                                          RTS.pErrorMode RTS.Abort
                                                            (do (__
                                                                   :: Vector.Vector (RTS.UInt 7)) <-
                                                                  RTS.pEnter "ICC.TextType"
                                                                    pTextType
                                                                HS.pure __)
                                                     HS.pure (Tag_charTarget _371)))
                                              ((RTS.<||)
                                                 (RTS.pEnter "chromaticAdaptation"
                                                    (do (_373 :: Vector.Vector (RTS.UInt 32)) <-
                                                          do RTS.pEnter "ICC._Guard"
                                                               (_Guard
                                                                  (sig
                                                                     HS.== Vector.vecFromRep
                                                                             "chad"))
                                                             RTS.pErrorMode RTS.Abort
                                                               (do (__
                                                                      :: Vector.Vector
                                                                           (RTS.UInt 32)) <-
                                                                     RTS.pEnter
                                                                       "ICC.S15Fixed16ArrayType"
                                                                       pS15Fixed16ArrayType
                                                                   HS.pure __)
                                                        HS.pure (Tag_chromaticAdaptation _373)))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "colorantOrder"
                                                       (do (_375 :: Vector.Vector (RTS.UInt 8)) <-
                                                             do RTS.pEnter "ICC._Guard"
                                                                  (_Guard
                                                                     (sig
                                                                        HS.== Vector.vecFromRep
                                                                                "clro"))
                                                                RTS.pErrorMode RTS.Abort
                                                                  (do (__
                                                                         :: Vector.Vector
                                                                              (RTS.UInt 8)) <-
                                                                        RTS.pEnter
                                                                          "ICC.ColorantOrderType"
                                                                          pColorantOrderType
                                                                      HS.pure __)
                                                           HS.pure (Tag_colorantOrder _375)))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "colorantTable"
                                                          (do (_377 :: Vector.Vector Colorant) <-
                                                                do RTS.pEnter "ICC._Guard"
                                                                     (_Guard
                                                                        (sig
                                                                           HS.== Vector.vecFromRep
                                                                                   "clrt"))
                                                                   RTS.pErrorMode RTS.Abort
                                                                     (do (__
                                                                            :: Vector.Vector
                                                                                 Colorant) <-
                                                                           RTS.pEnter
                                                                             "ICC.ColorantTableType"
                                                                             pColorantTableType
                                                                         HS.pure __)
                                                              HS.pure (Tag_colorantTable _377)))
                                                       ((RTS.<||)
                                                          (RTS.pEnter "colorantTableOut"
                                                             (do (_379 :: Vector.Vector Colorant) <-
                                                                   do RTS.pEnter "ICC._Guard"
                                                                        (_Guard
                                                                           (sig
                                                                              HS.== Vector.vecFromRep
                                                                                      "clot"))
                                                                      RTS.pErrorMode RTS.Abort
                                                                        (do (__
                                                                               :: Vector.Vector
                                                                                    Colorant) <-
                                                                              RTS.pEnter
                                                                                "ICC.ColorantTableType"
                                                                                pColorantTableType
                                                                            HS.pure __)
                                                                 HS.pure
                                                                   (Tag_colorantTableOut _379)))
                                                          ((RTS.<||)
                                                             (RTS.pEnter
                                                                "colorimetricIntentImageState"
                                                                (do (_381
                                                                       :: Vector.Vector
                                                                            (RTS.UInt 8)) <-
                                                                      do RTS.pEnter "ICC._Guard"
                                                                           (_Guard
                                                                              (sig
                                                                                 HS.== Vector.vecFromRep
                                                                                         "ciis"))
                                                                         RTS.pErrorMode RTS.Abort
                                                                           (do (__
                                                                                  :: Vector.Vector
                                                                                       (RTS.UInt
                                                                                          8)) <-
                                                                                 RTS.pEnter
                                                                                   "ICC.SignatureType"
                                                                                   pSignatureType
                                                                               HS.pure __)
                                                                    HS.pure
                                                                      (Tag_colorimetricIntentImageState
                                                                         _381)))
                                                             ((RTS.<||)
                                                                (RTS.pEnter "copyright"
                                                                   (do (_383
                                                                          :: Vector.Vector
                                                                               UnicodeRecord) <-
                                                                         do RTS.pEnter "ICC._Guard"
                                                                              (_Guard
                                                                                 (sig
                                                                                    HS.== Vector.vecFromRep
                                                                                            "cprt"))
                                                                            RTS.pErrorMode RTS.Abort
                                                                              (do (__
                                                                                     :: Vector.Vector
                                                                                          UnicodeRecord) <-
                                                                                    RTS.pEnter
                                                                                      "ICC.MultiLocalizedUnicodeType"
                                                                                      pMultiLocalizedUnicodeType
                                                                                  HS.pure __)
                                                                       HS.pure
                                                                         (Tag_copyright _383)))
                                                                ((RTS.<||)
                                                                   (RTS.pEnter "deviceMfgDesc"
                                                                      (do (_385
                                                                             :: Vector.Vector
                                                                                  UnicodeRecord) <-
                                                                            do RTS.pEnter
                                                                                 "ICC._Guard"
                                                                                 (_Guard
                                                                                    (sig
                                                                                       HS.== Vector.vecFromRep
                                                                                               "dmnd"))
                                                                               RTS.pErrorMode
                                                                                 RTS.Abort
                                                                                 (do (__
                                                                                        :: Vector.Vector
                                                                                             UnicodeRecord) <-
                                                                                       RTS.pEnter
                                                                                         "ICC.MultiLocalizedUnicodeType"
                                                                                         pMultiLocalizedUnicodeType
                                                                                     HS.pure __)
                                                                          HS.pure
                                                                            (Tag_deviceMfgDesc
                                                                               _385)))
                                                                   ((RTS.<||)
                                                                      (RTS.pEnter "deviceModelDesc"
                                                                         (do (_387
                                                                                :: Vector.Vector
                                                                                     UnicodeRecord) <-
                                                                               do RTS.pEnter
                                                                                    "ICC._Guard"
                                                                                    (_Guard
                                                                                       (sig
                                                                                          HS.== Vector.vecFromRep
                                                                                                  "dmdd"))
                                                                                  RTS.pErrorMode
                                                                                    RTS.Abort
                                                                                    (do (__
                                                                                           :: Vector.Vector
                                                                                                UnicodeRecord) <-
                                                                                          RTS.pEnter
                                                                                            "ICC.MultiLocalizedUnicodeType"
                                                                                            pMultiLocalizedUnicodeType
                                                                                        HS.pure __)
                                                                             HS.pure
                                                                               (Tag_deviceModelDesc
                                                                                  _387)))
                                                                      ((RTS.<||)
                                                                         (RTS.pEnter "DToB0"
                                                                            (do (_389
                                                                                   :: MultiProcessElementsType) <-
                                                                                  do RTS.pEnter
                                                                                       "ICC._Guard"
                                                                                       (_Guard
                                                                                          (sig
                                                                                             HS.== Vector.vecFromRep
                                                                                                     "D2B0"))
                                                                                     RTS.pErrorMode
                                                                                       RTS.Abort
                                                                                       (do (__
                                                                                              :: MultiProcessElementsType) <-
                                                                                             RTS.pEnter
                                                                                               "ICC.MultiProcessElementsType"
                                                                                               pMultiProcessElementsType
                                                                                           HS.pure
                                                                                             __)
                                                                                HS.pure
                                                                                  (Tag_DToB0 _389)))
                                                                         ((RTS.<||)
                                                                            (RTS.pEnter "DToB1"
                                                                               (do (_391
                                                                                      :: MultiProcessElementsType) <-
                                                                                     do RTS.pEnter
                                                                                          "ICC._Guard"
                                                                                          (_Guard
                                                                                             (sig
                                                                                                HS.== Vector.vecFromRep
                                                                                                        "D2B1"))
                                                                                        RTS.pErrorMode
                                                                                          RTS.Abort
                                                                                          (do (__
                                                                                                 :: MultiProcessElementsType) <-
                                                                                                RTS.pEnter
                                                                                                  "ICC.MultiProcessElementsType"
                                                                                                  pMultiProcessElementsType
                                                                                              HS.pure
                                                                                                __)
                                                                                   HS.pure
                                                                                     (Tag_DToB1
                                                                                        _391)))
                                                                            ((RTS.<||)
                                                                               (RTS.pEnter "DToB2"
                                                                                  (do (_393
                                                                                         :: MultiProcessElementsType) <-
                                                                                        do RTS.pEnter
                                                                                             "ICC._Guard"
                                                                                             (_Guard
                                                                                                (sig
                                                                                                   HS.== Vector.vecFromRep
                                                                                                           "D2B2"))
                                                                                           RTS.pErrorMode
                                                                                             RTS.Abort
                                                                                             (do (__
                                                                                                    :: MultiProcessElementsType) <-
                                                                                                   RTS.pEnter
                                                                                                     "ICC.MultiProcessElementsType"
                                                                                                     pMultiProcessElementsType
                                                                                                 HS.pure
                                                                                                   __)
                                                                                      HS.pure
                                                                                        (Tag_DToB2
                                                                                           _393)))
                                                                               ((RTS.<||)
                                                                                  (RTS.pEnter
                                                                                     "DToB3"
                                                                                     (do (_395
                                                                                            :: MultiProcessElementsType) <-
                                                                                           do RTS.pEnter
                                                                                                "ICC._Guard"
                                                                                                (_Guard
                                                                                                   (sig
                                                                                                      HS.== Vector.vecFromRep
                                                                                                              "D2B3"))
                                                                                              RTS.pErrorMode
                                                                                                RTS.Abort
                                                                                                (do (__
                                                                                                       :: MultiProcessElementsType) <-
                                                                                                      RTS.pEnter
                                                                                                        "ICC.MultiProcessElementsType"
                                                                                                        pMultiProcessElementsType
                                                                                                    HS.pure
                                                                                                      __)
                                                                                         HS.pure
                                                                                           (Tag_DToB3
                                                                                              _395)))
                                                                                  ((RTS.<||)
                                                                                     (RTS.pEnter
                                                                                        "gamut"
                                                                                        (do (_397
                                                                                               :: Lut_8_16_BA) <-
                                                                                              do RTS.pEnter
                                                                                                   "ICC._Guard"
                                                                                                   (_Guard
                                                                                                      (sig
                                                                                                         HS.== Vector.vecFromRep
                                                                                                                 "gamt"))
                                                                                                 RTS.pErrorMode
                                                                                                   RTS.Abort
                                                                                                   (do (__
                                                                                                          :: Lut_8_16_BA) <-
                                                                                                         RTS.pEnter
                                                                                                           "ICC.Lut_8_16_BA"
                                                                                                           pLut_8_16_BA
                                                                                                       HS.pure
                                                                                                         __)
                                                                                            HS.pure
                                                                                              (Tag_gamut
                                                                                                 _397)))
                                                                                     ((RTS.<||)
                                                                                        (RTS.pEnter
                                                                                           "grayTRC"
                                                                                           (do (_399
                                                                                                  :: SomeCurve) <-
                                                                                                 do RTS.pEnter
                                                                                                      "ICC._Guard"
                                                                                                      (_Guard
                                                                                                         (sig
                                                                                                            HS.== Vector.vecFromRep
                                                                                                                    "kTRC"))
                                                                                                    RTS.pErrorMode
                                                                                                      RTS.Abort
                                                                                                      (do (__
                                                                                                             :: SomeCurve) <-
                                                                                                            RTS.pEnter
                                                                                                              "ICC.SomeCurve"
                                                                                                              pSomeCurve
                                                                                                          HS.pure
                                                                                                            __)
                                                                                               HS.pure
                                                                                                 (Tag_grayTRC
                                                                                                    _399)))
                                                                                        ((RTS.<||)
                                                                                           (RTS.pEnter
                                                                                              "greenMatrixColumn"
                                                                                              (do (_401
                                                                                                     :: Vector.Vector
                                                                                                          XYZNumber) <-
                                                                                                    do RTS.pEnter
                                                                                                         "ICC._Guard"
                                                                                                         (_Guard
                                                                                                            (sig
                                                                                                               HS.== Vector.vecFromRep
                                                                                                                       "gXYZ"))
                                                                                                       RTS.pErrorMode
                                                                                                         RTS.Abort
                                                                                                         (do (__
                                                                                                                :: Vector.Vector
                                                                                                                     XYZNumber) <-
                                                                                                               RTS.pEnter
                                                                                                                 "ICC.XYZType"
                                                                                                                 pXYZType
                                                                                                             HS.pure
                                                                                                               __)
                                                                                                  HS.pure
                                                                                                    (Tag_greenMatrixColumn
                                                                                                       _401)))
                                                                                           ((RTS.<||)
                                                                                              (RTS.pEnter
                                                                                                 "greenTRC"
                                                                                                 (do (_403
                                                                                                        :: SomeCurve) <-
                                                                                                       do RTS.pEnter
                                                                                                            "ICC._Guard"
                                                                                                            (_Guard
                                                                                                               (sig
                                                                                                                  HS.== Vector.vecFromRep
                                                                                                                          "gTRC"))
                                                                                                          RTS.pErrorMode
                                                                                                            RTS.Abort
                                                                                                            (do (__
                                                                                                                   :: SomeCurve) <-
                                                                                                                  RTS.pEnter
                                                                                                                    "ICC.SomeCurve"
                                                                                                                    pSomeCurve
                                                                                                                HS.pure
                                                                                                                  __)
                                                                                                     HS.pure
                                                                                                       (Tag_greenTRC
                                                                                                          _403)))
                                                                                              ((RTS.<||)
                                                                                                 (RTS.pEnter
                                                                                                    "luminance"
                                                                                                    (do (_405
                                                                                                           :: Vector.Vector
                                                                                                                XYZNumber) <-
                                                                                                          do RTS.pEnter
                                                                                                               "ICC._Guard"
                                                                                                               (_Guard
                                                                                                                  (sig
                                                                                                                     HS.== Vector.vecFromRep
                                                                                                                             "lumi"))
                                                                                                             RTS.pErrorMode
                                                                                                               RTS.Abort
                                                                                                               (do (__
                                                                                                                      :: Vector.Vector
                                                                                                                           XYZNumber) <-
                                                                                                                     RTS.pEnter
                                                                                                                       "ICC.XYZType"
                                                                                                                       pXYZType
                                                                                                                   HS.pure
                                                                                                                     __)
                                                                                                        HS.pure
                                                                                                          (Tag_luminance
                                                                                                             _405)))
                                                                                                 ((RTS.<||)
                                                                                                    (RTS.pEnter
                                                                                                       "measurement"
                                                                                                       (do (_407
                                                                                                              :: MeasurementType) <-
                                                                                                             do RTS.pEnter
                                                                                                                  "ICC._Guard"
                                                                                                                  (_Guard
                                                                                                                     (sig
                                                                                                                        HS.== Vector.vecFromRep
                                                                                                                                "meas"))
                                                                                                                RTS.pErrorMode
                                                                                                                  RTS.Abort
                                                                                                                  (do (__
                                                                                                                         :: MeasurementType) <-
                                                                                                                        RTS.pEnter
                                                                                                                          "ICC.MeasurementType"
                                                                                                                          pMeasurementType
                                                                                                                      HS.pure
                                                                                                                        __)
                                                                                                           HS.pure
                                                                                                             (Tag_measurement
                                                                                                                _407)))
                                                                                                    ((RTS.<||)
                                                                                                       (RTS.pEnter
                                                                                                          "mediaWhitePoint"
                                                                                                          (do (_409
                                                                                                                 :: Vector.Vector
                                                                                                                      XYZNumber) <-
                                                                                                                do RTS.pEnter
                                                                                                                     "ICC._Guard"
                                                                                                                     (_Guard
                                                                                                                        (sig
                                                                                                                           HS.== Vector.vecFromRep
                                                                                                                                   "wtpt"))
                                                                                                                   RTS.pErrorMode
                                                                                                                     RTS.Abort
                                                                                                                     (do (__
                                                                                                                            :: Vector.Vector
                                                                                                                                 XYZNumber) <-
                                                                                                                           RTS.pEnter
                                                                                                                             "ICC.XYZType"
                                                                                                                             pXYZType
                                                                                                                         HS.pure
                                                                                                                           __)
                                                                                                              HS.pure
                                                                                                                (Tag_mediaWhitePoint
                                                                                                                   _409)))
                                                                                                       ((RTS.<||)
                                                                                                          (RTS.pEnter
                                                                                                             "namedColor2"
                                                                                                             (do (_411
                                                                                                                    :: NamedColor2Type) <-
                                                                                                                   do RTS.pEnter
                                                                                                                        "ICC._Guard"
                                                                                                                        (_Guard
                                                                                                                           (sig
                                                                                                                              HS.== Vector.vecFromRep
                                                                                                                                      "ncl2"))
                                                                                                                      RTS.pErrorMode
                                                                                                                        RTS.Abort
                                                                                                                        (do (__
                                                                                                                               :: NamedColor2Type) <-
                                                                                                                              RTS.pEnter
                                                                                                                                "ICC.NamedColor2Type"
                                                                                                                                pNamedColor2Type
                                                                                                                            HS.pure
                                                                                                                              __)
                                                                                                                 HS.pure
                                                                                                                   (Tag_namedColor2
                                                                                                                      _411)))
                                                                                                          ((RTS.<||)
                                                                                                             (RTS.pEnter
                                                                                                                "outputResponse"
                                                                                                                (do (_413
                                                                                                                       :: Vector.Vector
                                                                                                                            ResponseCurve) <-
                                                                                                                      do RTS.pEnter
                                                                                                                           "ICC._Guard"
                                                                                                                           (_Guard
                                                                                                                              (sig
                                                                                                                                 HS.== Vector.vecFromRep
                                                                                                                                         "resp"))
                                                                                                                         RTS.pErrorMode
                                                                                                                           RTS.Abort
                                                                                                                           (do (__
                                                                                                                                  :: Vector.Vector
                                                                                                                                       ResponseCurve) <-
                                                                                                                                 RTS.pEnter
                                                                                                                                   "ICC.ResponseCurveSet16Type"
                                                                                                                                   pResponseCurveSet16Type
                                                                                                                               HS.pure
                                                                                                                                 __)
                                                                                                                    HS.pure
                                                                                                                      (Tag_outputResponse
                                                                                                                         _413)))
                                                                                                             ((RTS.<||)
                                                                                                                (RTS.pEnter
                                                                                                                   "perceptualRenderingIntentGamut"
                                                                                                                   (do (_415
                                                                                                                          :: Vector.Vector
                                                                                                                               (RTS.UInt
                                                                                                                                  8)) <-
                                                                                                                         do RTS.pEnter
                                                                                                                              "ICC._Guard"
                                                                                                                              (_Guard
                                                                                                                                 (sig
                                                                                                                                    HS.== Vector.vecFromRep
                                                                                                                                            "rig0"))
                                                                                                                            RTS.pErrorMode
                                                                                                                              RTS.Abort
                                                                                                                              (do (__
                                                                                                                                     :: Vector.Vector
                                                                                                                                          (RTS.UInt
                                                                                                                                             8)) <-
                                                                                                                                    RTS.pEnter
                                                                                                                                      "ICC.SignatureType"
                                                                                                                                      pSignatureType
                                                                                                                                  HS.pure
                                                                                                                                    __)
                                                                                                                       HS.pure
                                                                                                                         (Tag_perceptualRenderingIntentGamut
                                                                                                                            _415)))
                                                                                                                ((RTS.<||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "preview0"
                                                                                                                      (do (_417
                                                                                                                             :: Lut_8_16_AB_BA) <-
                                                                                                                            do RTS.pEnter
                                                                                                                                 "ICC._Guard"
                                                                                                                                 (_Guard
                                                                                                                                    (sig
                                                                                                                                       HS.== Vector.vecFromRep
                                                                                                                                               "pre0"))
                                                                                                                               RTS.pErrorMode
                                                                                                                                 RTS.Abort
                                                                                                                                 (do (__
                                                                                                                                        :: Lut_8_16_AB_BA) <-
                                                                                                                                       RTS.pEnter
                                                                                                                                         "ICC.Lut_8_16_AB_BA"
                                                                                                                                         pLut_8_16_AB_BA
                                                                                                                                     HS.pure
                                                                                                                                       __)
                                                                                                                          HS.pure
                                                                                                                            (Tag_preview0
                                                                                                                               _417)))
                                                                                                                   ((RTS.<||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "preview1"
                                                                                                                         (do (_419
                                                                                                                                :: Lut_8_16_BA) <-
                                                                                                                               do RTS.pEnter
                                                                                                                                    "ICC._Guard"
                                                                                                                                    (_Guard
                                                                                                                                       (sig
                                                                                                                                          HS.== Vector.vecFromRep
                                                                                                                                                  "pre1"))
                                                                                                                                  RTS.pErrorMode
                                                                                                                                    RTS.Abort
                                                                                                                                    (do (__
                                                                                                                                           :: Lut_8_16_BA) <-
                                                                                                                                          RTS.pEnter
                                                                                                                                            "ICC.Lut_8_16_BA"
                                                                                                                                            pLut_8_16_BA
                                                                                                                                        HS.pure
                                                                                                                                          __)
                                                                                                                             HS.pure
                                                                                                                               (Tag_preview1
                                                                                                                                  _419)))
                                                                                                                      ((RTS.<||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "preview2"
                                                                                                                            (do (_421
                                                                                                                                   :: Lut_8_16_BA) <-
                                                                                                                                  do RTS.pEnter
                                                                                                                                       "ICC._Guard"
                                                                                                                                       (_Guard
                                                                                                                                          (sig
                                                                                                                                             HS.== Vector.vecFromRep
                                                                                                                                                     "pre2"))
                                                                                                                                     RTS.pErrorMode
                                                                                                                                       RTS.Abort
                                                                                                                                       (do (__
                                                                                                                                              :: Lut_8_16_BA) <-
                                                                                                                                             RTS.pEnter
                                                                                                                                               "ICC.Lut_8_16_BA"
                                                                                                                                               pLut_8_16_BA
                                                                                                                                           HS.pure
                                                                                                                                             __)
                                                                                                                                HS.pure
                                                                                                                                  (Tag_preview2
                                                                                                                                     _421)))
                                                                                                                         ((RTS.<||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "profileDescription"
                                                                                                                               (do (_423
                                                                                                                                      :: Vector.Vector
                                                                                                                                           UnicodeRecord) <-
                                                                                                                                     do RTS.pEnter
                                                                                                                                          "ICC._Guard"
                                                                                                                                          (_Guard
                                                                                                                                             (sig
                                                                                                                                                HS.== Vector.vecFromRep
                                                                                                                                                        "desc"))
                                                                                                                                        RTS.pErrorMode
                                                                                                                                          RTS.Abort
                                                                                                                                          (do (__
                                                                                                                                                 :: Vector.Vector
                                                                                                                                                      UnicodeRecord) <-
                                                                                                                                                RTS.pEnter
                                                                                                                                                  "ICC.MultiLocalizedUnicodeType"
                                                                                                                                                  pMultiLocalizedUnicodeType
                                                                                                                                              HS.pure
                                                                                                                                                __)
                                                                                                                                   HS.pure
                                                                                                                                     (Tag_profileDescription
                                                                                                                                        _423)))
                                                                                                                            ((RTS.<||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "profileSequenceDesc"
                                                                                                                                  (do (_425
                                                                                                                                         :: Vector.Vector
                                                                                                                                              (RTS.UInt
                                                                                                                                                 8)) <-
                                                                                                                                        do RTS.pEnter
                                                                                                                                             "ICC._Guard"
                                                                                                                                             (_Guard
                                                                                                                                                (sig
                                                                                                                                                   HS.== Vector.vecFromRep
                                                                                                                                                           "pseq"))
                                                                                                                                           RTS.pErrorMode
                                                                                                                                             RTS.Abort
                                                                                                                                             (do (__
                                                                                                                                                    :: Vector.Vector
                                                                                                                                                         (RTS.UInt
                                                                                                                                                            8)) <-
                                                                                                                                                   RTS.pEnter
                                                                                                                                                     "ICC.ProfileSequenceDescType"
                                                                                                                                                     pProfileSequenceDescType
                                                                                                                                                 HS.pure
                                                                                                                                                   __)
                                                                                                                                      HS.pure
                                                                                                                                        (Tag_profileSequenceDesc
                                                                                                                                           _425)))
                                                                                                                               ((RTS.<||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "profileSequenceIdentifier"
                                                                                                                                     (do (_427
                                                                                                                                            :: ()) <-
                                                                                                                                           do RTS.pEnter
                                                                                                                                                "ICC._Guard"
                                                                                                                                                (_Guard
                                                                                                                                                   (sig
                                                                                                                                                      HS.== Vector.vecFromRep
                                                                                                                                                              "psid"))
                                                                                                                                              RTS.pErrorMode
                                                                                                                                                RTS.Abort
                                                                                                                                                (do (__
                                                                                                                                                       :: ()) <-
                                                                                                                                                      HS.pure
                                                                                                                                                        ()
                                                                                                                                                    HS.pure
                                                                                                                                                      __)
                                                                                                                                         HS.pure
                                                                                                                                           (Tag_profileSequenceIdentifier
                                                                                                                                              _427)))
                                                                                                                                  ((RTS.<||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "redMatrixColumn"
                                                                                                                                        (do (_429
                                                                                                                                               :: Vector.Vector
                                                                                                                                                    XYZNumber) <-
                                                                                                                                              do RTS.pEnter
                                                                                                                                                   "ICC._Guard"
                                                                                                                                                   (_Guard
                                                                                                                                                      (sig
                                                                                                                                                         HS.== Vector.vecFromRep
                                                                                                                                                                 "rXYZ"))
                                                                                                                                                 RTS.pErrorMode
                                                                                                                                                   RTS.Abort
                                                                                                                                                   (do (__
                                                                                                                                                          :: Vector.Vector
                                                                                                                                                               XYZNumber) <-
                                                                                                                                                         RTS.pEnter
                                                                                                                                                           "ICC.XYZType"
                                                                                                                                                           pXYZType
                                                                                                                                                       HS.pure
                                                                                                                                                         __)
                                                                                                                                            HS.pure
                                                                                                                                              (Tag_redMatrixColumn
                                                                                                                                                 _429)))
                                                                                                                                     ((RTS.<||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "redTRC"
                                                                                                                                           (do (_431
                                                                                                                                                  :: SomeCurve) <-
                                                                                                                                                 do RTS.pEnter
                                                                                                                                                      "ICC._Guard"
                                                                                                                                                      (_Guard
                                                                                                                                                         (sig
                                                                                                                                                            HS.== Vector.vecFromRep
                                                                                                                                                                    "rTRC"))
                                                                                                                                                    RTS.pErrorMode
                                                                                                                                                      RTS.Abort
                                                                                                                                                      (do (__
                                                                                                                                                             :: SomeCurve) <-
                                                                                                                                                            RTS.pEnter
                                                                                                                                                              "ICC.SomeCurve"
                                                                                                                                                              pSomeCurve
                                                                                                                                                          HS.pure
                                                                                                                                                            __)
                                                                                                                                               HS.pure
                                                                                                                                                 (Tag_redTRC
                                                                                                                                                    _431)))
                                                                                                                                        ((RTS.<||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "saturationRenderingIntentGamut"
                                                                                                                                              (do (_433
                                                                                                                                                     :: Vector.Vector
                                                                                                                                                          (RTS.UInt
                                                                                                                                                             8)) <-
                                                                                                                                                    do RTS.pEnter
                                                                                                                                                         "ICC._Guard"
                                                                                                                                                         (_Guard
                                                                                                                                                            (sig
                                                                                                                                                               HS.== Vector.vecFromRep
                                                                                                                                                                       "rig2"))
                                                                                                                                                       RTS.pErrorMode
                                                                                                                                                         RTS.Abort
                                                                                                                                                         (do (__
                                                                                                                                                                :: Vector.Vector
                                                                                                                                                                     (RTS.UInt
                                                                                                                                                                        8)) <-
                                                                                                                                                               RTS.pEnter
                                                                                                                                                                 "ICC.SignatureType"
                                                                                                                                                                 pSignatureType
                                                                                                                                                             HS.pure
                                                                                                                                                               __)
                                                                                                                                                  HS.pure
                                                                                                                                                    (Tag_saturationRenderingIntentGamut
                                                                                                                                                       _433)))
                                                                                                                                           ((RTS.<||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "technology"
                                                                                                                                                 (do (_435
                                                                                                                                                        :: Vector.Vector
                                                                                                                                                             (RTS.UInt
                                                                                                                                                                8)) <-
                                                                                                                                                       do RTS.pEnter
                                                                                                                                                            "ICC._Guard"
                                                                                                                                                            (_Guard
                                                                                                                                                               (sig
                                                                                                                                                                  HS.== Vector.vecFromRep
                                                                                                                                                                          "tech"))
                                                                                                                                                          RTS.pErrorMode
                                                                                                                                                            RTS.Abort
                                                                                                                                                            (do (__
                                                                                                                                                                   :: Vector.Vector
                                                                                                                                                                        (RTS.UInt
                                                                                                                                                                           8)) <-
                                                                                                                                                                  RTS.pEnter
                                                                                                                                                                    "ICC.SignatureType"
                                                                                                                                                                    pSignatureType
                                                                                                                                                                HS.pure
                                                                                                                                                                  __)
                                                                                                                                                     HS.pure
                                                                                                                                                       (Tag_technology
                                                                                                                                                          _435)))
                                                                                                                                              ((RTS.<||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "viewCondDesc"
                                                                                                                                                    (do (_437
                                                                                                                                                           :: Vector.Vector
                                                                                                                                                                UnicodeRecord) <-
                                                                                                                                                          do RTS.pEnter
                                                                                                                                                               "ICC._Guard"
                                                                                                                                                               (_Guard
                                                                                                                                                                  (sig
                                                                                                                                                                     HS.== Vector.vecFromRep
                                                                                                                                                                             "vued"))
                                                                                                                                                             RTS.pErrorMode
                                                                                                                                                               RTS.Abort
                                                                                                                                                               (do (__
                                                                                                                                                                      :: Vector.Vector
                                                                                                                                                                           UnicodeRecord) <-
                                                                                                                                                                     RTS.pEnter
                                                                                                                                                                       "ICC.MultiLocalizedUnicodeType"
                                                                                                                                                                       pMultiLocalizedUnicodeType
                                                                                                                                                                   HS.pure
                                                                                                                                                                     __)
                                                                                                                                                        HS.pure
                                                                                                                                                          (Tag_viewCondDesc
                                                                                                                                                             _437)))
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "viewConditions"
                                                                                                                                                    (do (_439
                                                                                                                                                           :: ViewConditionsType) <-
                                                                                                                                                          do RTS.pEnter
                                                                                                                                                               "ICC._Guard"
                                                                                                                                                               (_Guard
                                                                                                                                                                  (sig
                                                                                                                                                                     HS.== Vector.vecFromRep
                                                                                                                                                                             "view"))
                                                                                                                                                             RTS.pErrorMode
                                                                                                                                                               RTS.Abort
                                                                                                                                                               (do (__
                                                                                                                                                                      :: ViewConditionsType) <-
                                                                                                                                                                     RTS.pEnter
                                                                                                                                                                       "ICC.ViewConditionsType"
                                                                                                                                                                       pViewConditionsType
                                                                                                                                                                   HS.pure
                                                                                                                                                                     __)
                                                                                                                                                        HS.pure
                                                                                                                                                          (Tag_viewConditions
                                                                                                                                                             _439))))))))))))))))))))))))))))))))))))))))))))))))))
    (RTS.pError RTS.FromUser "228:6--228:46"
       (Vector.vecToString
          (Vector.concat
             (Vector.fromList [Vector.vecFromRep "Unregonized tag: ", sig]))))
 
pParseTag :: TagEntry -> RTS.Parser Tag
 
pParseTag (t :: TagEntry) =
  do RTS.pEnter "ICC._Goto"
       (_Goto
          (RTS.convert (HS.getField @"offset_to_data_element" t)
             :: RTS.UInt 64))
     (__ :: Tag) <-
       RTS.pEnter "ICC.ParseChunk"
         (pParseChunk @Tag
            (RTS.convert (HS.getField @"size_of_data_element" t)
               :: RTS.UInt 64)
            (RTS.pEnter "ICC.Tag" (pTag (HS.getField @"tag_signature" t))))
     HS.pure __
 
pChromaticityType :: RTS.Parser ChromaticityType
 
pChromaticityType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "chrm"))
     (number_of_device_channels :: RTS.UInt 16) <-
       RTS.pEnter "ICC.BE16" pBE16
     (phosphor_or_colorant :: RTS.UInt 16) <-
       RTS.pEnter "ICC.BE16" pBE16
     (cie_coords :: Vector.Vector XYNumber) <-
       Vector.replicateM
         (RTS.convert number_of_device_channels :: RTS.UInt 64)
         (RTS.pEnter "ICC.XYNumber" pXYNumber)
     HS.pure (ChromaticityType phosphor_or_colorant cie_coords)
 
getBit ::
  forall b.
    (RTS.DDL b, RTS.Numeric b, RTS.Convert b (RTS.UInt 1)) =>
      RTS.UInt 64 -> (b -> RTS.UInt 1)
 
getBit (n :: RTS.UInt 64) (b :: b) =
  RTS.convert (RTS.shiftr b n) :: RTS.UInt 1
 
pGotoRel :: RTS.Input -> (RTS.UInt 64 -> RTS.Parser ())
 
pGotoRel (s :: RTS.Input) (n :: RTS.UInt 64) =
  do (s1 :: RTS.Input) <-
       RTS.pIsJust "493:9--493:14" "Not enough bytes" (RTS.advanceBy n s)
     (__ :: ()) <- RTS.pSetInput s1
     HS.pure __
 
pGoto :: RTS.UInt 64 -> RTS.Parser ()
 
pGoto (n :: RTS.UInt 64) =
  do (s :: RTS.Input) <- RTS.pPeek
     (__ :: ()) <- RTS.pEnter "ICC.GotoRel" (pGotoRel s n)
     HS.pure __
 
pValidateArray ::
  forall g.
    RTS.DDL g =>
      Vector.Vector (RTS.UInt 8) -> (RTS.Parser g -> RTS.Parser ())
 
pValidateArray (arr :: Vector.Vector (RTS.UInt 8))
  (pP :: RTS.Parser g) =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pSetInput (RTS.arrayStream (Vector.vecFromRep "array") arr)
     do HS.void pP
        HS.pure ()
     RTS.pEnd "531:3--531:5"
     (__ :: ()) <- RTS.pSetInput s
     HS.pure __
 
pGuard :: HS.Bool -> RTS.Parser ()
 
pGuard (p :: HS.Bool) =
  RTS.pGuard "537:15--537:23" "guard failed" p
 
_VersionField :: RTS.Parser ()
 
_VersionField =
  do HS.const () HS.<$> RTS.pByte "36:18--36:22"
     HS.const () HS.<$> RTS.pByte "37:18--37:22"
     HS.const ()
       HS.<$> RTS.pMatch "40:3--40:20"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
 
_ProfileClasses :: RTS.Parser ()
 
_ProfileClasses =
  (RTS.<||)
    (RTS.pEnter "input_device_profile"
       (HS.const ()
          HS.<$> RTS.pMatch "45:31--45:42" (Vector.vecFromRep "scnr")))
    ((RTS.<||)
       (RTS.pEnter "display_device_profile"
          (HS.const ()
             HS.<$> RTS.pMatch "46:31--46:42" (Vector.vecFromRep "mntr")))
       ((RTS.<||)
          (RTS.pEnter "output_device_profile"
             (HS.const ()
                HS.<$> RTS.pMatch "47:31--47:42" (Vector.vecFromRep "prtr")))
          ((RTS.<||)
             (RTS.pEnter "device_link_profile"
                (HS.const ()
                   HS.<$> RTS.pMatch "48:31--48:42" (Vector.vecFromRep "link")))
             ((RTS.<||)
                (RTS.pEnter "color_space_profile"
                   (HS.const ()
                      HS.<$> RTS.pMatch "49:31--49:42" (Vector.vecFromRep "spac")))
                ((RTS.<||)
                   (RTS.pEnter "abstract_profile"
                      (HS.const ()
                         HS.<$> RTS.pMatch "50:31--50:42" (Vector.vecFromRep "abst")))
                   (RTS.pEnter "named_color_profile"
                      (HS.const ()
                         HS.<$> RTS.pMatch "51:31--51:42" (Vector.vecFromRep "nmcl"))))))))
 
_DataColorSpaces :: RTS.Parser ()
 
_DataColorSpaces =
  (RTS.<||)
    (RTS.pEnter "nciexyz_or_pcsxyz"
       (HS.const ()
          HS.<$> RTS.pMatch "57:26--57:37" (Vector.vecFromRep "XYZ ")))
    ((RTS.<||)
       (RTS.pEnter "cielab_or_pcslab"
          (HS.const ()
             HS.<$> RTS.pMatch "58:26--58:37" (Vector.vecFromRep "Lab ")))
       ((RTS.<||)
          (RTS.pEnter "cieluv"
             (HS.const ()
                HS.<$> RTS.pMatch "59:26--59:37" (Vector.vecFromRep "Luv ")))
          ((RTS.<||)
             (RTS.pEnter "ycbcr"
                (HS.const ()
                   HS.<$> RTS.pMatch "60:26--60:37" (Vector.vecFromRep "Ycbr")))
             ((RTS.<||)
                (RTS.pEnter "cieyxy"
                   (HS.const ()
                      HS.<$> RTS.pMatch "61:26--61:37" (Vector.vecFromRep "Yxy ")))
                ((RTS.<||)
                   (RTS.pEnter "rgb"
                      (HS.const ()
                         HS.<$> RTS.pMatch "62:26--62:37" (Vector.vecFromRep "RGB ")))
                   ((RTS.<||)
                      (RTS.pEnter "gray"
                         (HS.const ()
                            HS.<$> RTS.pMatch "63:26--63:37" (Vector.vecFromRep "GRAY")))
                      ((RTS.<||)
                         (RTS.pEnter "hsv"
                            (HS.const ()
                               HS.<$> RTS.pMatch "64:26--64:37" (Vector.vecFromRep "HSV ")))
                         ((RTS.<||)
                            (RTS.pEnter "hls"
                               (HS.const ()
                                  HS.<$> RTS.pMatch "65:26--65:37" (Vector.vecFromRep "HLS ")))
                            ((RTS.<||)
                               (RTS.pEnter "cmyk"
                                  (HS.const ()
                                     HS.<$> RTS.pMatch "66:26--66:37" (Vector.vecFromRep "CMYK")))
                               ((RTS.<||)
                                  (RTS.pEnter "cmy"
                                     (HS.const ()
                                        HS.<$> RTS.pMatch "67:26--67:37"
                                                 (Vector.vecFromRep "CMY ")))
                                  ((RTS.<||)
                                     (RTS.pEnter "two_colour"
                                        (HS.const ()
                                           HS.<$> RTS.pMatch "68:26--68:37"
                                                    (Vector.vecFromRep "2CLR")))
                                     ((RTS.<||)
                                        (RTS.pEnter "three_colour"
                                           (HS.const ()
                                              HS.<$> RTS.pMatch "69:26--69:37"
                                                       (Vector.vecFromRep "3CLR")))
                                        ((RTS.<||)
                                           (RTS.pEnter "four_colour"
                                              (HS.const ()
                                                 HS.<$> RTS.pMatch "70:26--70:37"
                                                          (Vector.vecFromRep "4CLR")))
                                           ((RTS.<||)
                                              (RTS.pEnter "five_colour"
                                                 (HS.const ()
                                                    HS.<$> RTS.pMatch "71:26--71:37"
                                                             (Vector.vecFromRep "5CLR")))
                                              ((RTS.<||)
                                                 (RTS.pEnter "six_colour"
                                                    (HS.const ()
                                                       HS.<$> RTS.pMatch "72:26--72:37"
                                                                (Vector.vecFromRep "6CLR")))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "seven_colour"
                                                       (HS.const ()
                                                          HS.<$> RTS.pMatch "73:26--73:37"
                                                                   (Vector.vecFromRep "7CLR")))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "eight_colour"
                                                          (HS.const ()
                                                             HS.<$> RTS.pMatch "74:26--74:37"
                                                                      (Vector.vecFromRep "8CLR")))
                                                       ((RTS.<||)
                                                          (RTS.pEnter "nine_colour"
                                                             (HS.const ()
                                                                HS.<$> RTS.pMatch "75:26--75:37"
                                                                         (Vector.vecFromRep
                                                                            "9CLR")))
                                                          ((RTS.<||)
                                                             (RTS.pEnter "ten_colour"
                                                                (HS.const ()
                                                                   HS.<$> RTS.pMatch "76:26--76:37"
                                                                            (Vector.vecFromRep
                                                                               "ACLR")))
                                                             ((RTS.<||)
                                                                (RTS.pEnter "eleven_colour"
                                                                   (HS.const ()
                                                                      HS.<$> RTS.pMatch
                                                                               "77:26--77:37"
                                                                               (Vector.vecFromRep
                                                                                  "BCLR")))
                                                                ((RTS.<||)
                                                                   (RTS.pEnter "twelve_colour"
                                                                      (HS.const ()
                                                                         HS.<$> RTS.pMatch
                                                                                  "78:26--78:37"
                                                                                  (Vector.vecFromRep
                                                                                     "CCLR")))
                                                                   ((RTS.<||)
                                                                      (RTS.pEnter "thirteen_colour"
                                                                         (HS.const ()
                                                                            HS.<$> RTS.pMatch
                                                                                     "79:26--79:37"
                                                                                     (Vector.vecFromRep
                                                                                        "DCLR")))
                                                                      ((RTS.<||)
                                                                         (RTS.pEnter
                                                                            "fourteen_colour"
                                                                            (HS.const ()
                                                                               HS.<$> RTS.pMatch
                                                                                        "80:26--80:37"
                                                                                        (Vector.vecFromRep
                                                                                           "ECLR")))
                                                                         (RTS.pEnter
                                                                            "fifteen_colour"
                                                                            (HS.const ()
                                                                               HS.<$> RTS.pMatch
                                                                                        "81:26--81:37"
                                                                                        (Vector.vecFromRep
                                                                                           "FCLR"))))))))))))))))))))))))))
 
_PrimaryPlatforms :: RTS.Parser ()
 
_PrimaryPlatforms =
  (RTS.<||)
    (RTS.pEnter "none"
       (HS.const ()
          HS.<$> RTS.pMatch "87:30--87:44"
                   (Vector.fromList
                      [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                       RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
    ((RTS.<||)
       (RTS.pEnter "apple_computer_inc"
          (HS.const ()
             HS.<$> RTS.pMatch "88:30--88:41" (Vector.vecFromRep "APPL")))
       ((RTS.<||)
          (RTS.pEnter "microsoft_corporation"
             (HS.const ()
                HS.<$> RTS.pMatch "89:30--89:41" (Vector.vecFromRep "MSFT")))
          ((RTS.<||)
             (RTS.pEnter "silicon_graphics_inc"
                (HS.const ()
                   HS.<$> RTS.pMatch "90:30--90:41" (Vector.vecFromRep "SGI ")))
             (RTS.pEnter "sun_microsystems"
                (HS.const ()
                   HS.<$> RTS.pMatch "91:30--91:41" (Vector.vecFromRep "SUNW"))))))
 
_RenderingIntent :: RTS.Parser ()
 
_RenderingIntent =
  (RTS.<||)
    (RTS.pEnter "perceptual"
       (HS.const ()
          HS.<$> RTS.pMatch "98:36--98:50"
                   (Vector.fromList
                      [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                       RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])))
    ((RTS.<||)
       (RTS.pEnter "media_relative_colorimetric"
          (HS.const ()
             HS.<$> RTS.pMatch "99:36--99:50"
                      (Vector.fromList
                         [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                          RTS.lit 0 :: RTS.UInt 8, RTS.lit 1 :: RTS.UInt 8])))
       ((RTS.<||)
          (RTS.pEnter "saturation"
             (HS.const ()
                HS.<$> RTS.pMatch "100:36--100:50"
                         (Vector.fromList
                            [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                             RTS.lit 0 :: RTS.UInt 8, RTS.lit 2 :: RTS.UInt 8])))
          (RTS.pEnter "icc_absolute_colorimetric"
             (HS.const ()
                HS.<$> RTS.pMatch "101:36--101:50"
                         (Vector.fromList
                            [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8,
                             RTS.lit 0 :: RTS.UInt 8, RTS.lit 3 :: RTS.UInt 8])))))
 
_XYZNumber :: RTS.Parser ()
 
_XYZNumber =
  do RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_DateTimeNumber :: RTS.Parser ()
 
_DateTimeNumber =
  do RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
 
_BE64 :: RTS.Parser ()
 
_BE64 =
  do RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_ProfileHeader :: RTS.Parser ()
 
_ProfileHeader =
  do RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._VersionField" _VersionField
     RTS.pEnter "ICC._ProfileClasses" _ProfileClasses
     RTS.pEnter "ICC._DataColorSpaces" _DataColorSpaces
     RTS.pEnter "ICC._DataColorSpaces" _DataColorSpaces
     RTS.pEnter "ICC._DateTimeNumber" _DateTimeNumber
     HS.const ()
       HS.<$> RTS.pMatch "22:3--22:14" (Vector.vecFromRep "acsp")
     RTS.pEnter "ICC._PrimaryPlatforms" _PrimaryPlatforms
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE64" _BE64
     RTS.pEnter "ICC._RenderingIntent" _RenderingIntent
     RTS.pEnter "ICC._XYZNumber" _XYZNumber
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pSkipExact (RTS.lit 16 :: RTS.UInt 64)
       (HS.const () HS.<$> RTS.pByte "31:33--31:37")
     RTS.pSkipExact (RTS.lit 28 :: RTS.UInt 64)
       (HS.const ()
          HS.<$> RTS.pMatch1 "32:34--32:41" (RTS.bcSingle (RTS.uint8 0)))
 
_TagEntry :: RTS.Parser ()
 
_TagEntry =
  do RTS.pSkipExact (RTS.lit 4 :: RTS.UInt 64)
       (HS.const () HS.<$> RTS.pByte "159:36--159:40")
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_TagTable :: RTS.Parser ()
 
_TagTable =
  do (tag_count :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pSkipExact (RTS.convert tag_count :: RTS.UInt 64)
       (RTS.pEnter "ICC._TagEntry" _TagEntry)
 
_Main :: RTS.Parser ()
 
_Main =
  do RTS.pEnter "ICC._ProfileHeader" _ProfileHeader
     RTS.pEnter "ICC._TagTable" _TagTable
 
_XYNumber :: RTS.Parser ()
 
_XYNumber =
  do RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_PositionNumber :: RTS.Parser ()
 
_PositionNumber =
  do RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_ASCII7 :: RTS.Parser ()
 
_ASCII7 =
  do RTS.pSkipMany (RTS.<||)
       (do (_313 :: RTS.UInt 8) <-
             RTS.uint8
               HS.<$> RTS.pMatch1 "136:14--136:24"
                        (RTS.bcRange (RTS.lit 1 :: RTS.UInt 8) (RTS.lit 255 :: RTS.UInt 8))
           RTS.pIsJust_ "136:14--136:35" "Value does not fit in target type"
             (RTS.convertMaybe _313 :: HS.Maybe (RTS.UInt 7)))
     (RTS.<||)
       (RTS.pSkipAtLeast (RTS.<||) (RTS.lit 1 :: RTS.UInt 64)
          (HS.const ()
             HS.<$> RTS.pMatch1 "137:17--137:24" (RTS.bcSingle (RTS.uint8 0))))
       (RTS.pError RTS.FromUser "137:30--137:59"
          (Vector.vecToString (Vector.vecFromRep "Non 0 string terminator")))
 
_Response16Number :: RTS.Parser ()
 
_Response16Number =
  do RTS.pEnter "ICC._BE16" _BE16
     HS.const ()
       HS.<$> RTS.pMatch "142:3--142:13"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     RTS.pEnter "ICC._BE32" _BE32
 
_Chunk :: RTS.UInt 64 -> RTS.Parser ()
 
_Chunk (sz :: RTS.UInt 64) =
  do (s :: RTS.Input) <- RTS.pPeek
     HS.void
       (RTS.pIsJust_ "500:8--500:14" "Not enough bytes"
          (RTS.limitLen sz s))
     RTS.pEnter "ICC._Goto" (_Goto sz)
 
_Lut8Type :: RTS.Parser ()
 
_Lut8Type =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mft1"))
     (number_of_input_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "352:30--352:34"
     (i :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_input_channels :: RTS.UInt 64)
     (number_of_output_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "354:31--354:35"
     (o :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_output_channels :: RTS.UInt 64)
     (number_of_clut_grid_points :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "356:32--356:36"
     (g :: RTS.UInt 64) <-
       RTS.pIsJust "357:8--357:41" "Value does not fit in target type"
         (RTS.convertMaybe number_of_clut_grid_points
            :: HS.Maybe (RTS.UInt 64))
     HS.const ()
       HS.<$> RTS.pMatch1 "358:3--358:13" (RTS.bcSingle (RTS.uint8 0))
     RTS.pSkipExact (RTS.lit 9 :: RTS.UInt 64)
       (RTS.pEnter "ICC._BE32" _BE32)
     RTS.pEnter "ICC._Chunk"
       (_Chunk (RTS.mul (RTS.lit 256 :: RTS.UInt 64) i))
     RTS.pEnter "ICC._Chunk"
       (_Chunk (RTS.mul (exp @(RTS.UInt 64) @(RTS.UInt 64) g i) o))
     RTS.pEnter "ICC._Chunk"
       (_Chunk (RTS.mul (RTS.lit 256 :: RTS.UInt 64) o))
 
_Lut16Type :: RTS.Parser ()
 
_Lut16Type =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mft2"))
     (number_of_input_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "367:30--367:34"
     (i :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_input_channels :: RTS.UInt 64)
     (number_of_output_channels :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "369:31--369:35"
     (o :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_output_channels :: RTS.UInt 64)
     (number_of_clut_grid_points :: RTS.UInt 8) <-
       RTS.uint8 HS.<$> RTS.pByte "371:32--371:36"
     (g :: RTS.UInt 64) <-
       RTS.pIsJust "372:8--372:41" "Value does not fit in target type"
         (RTS.convertMaybe number_of_clut_grid_points
            :: HS.Maybe (RTS.UInt 64))
     HS.const ()
       HS.<$> RTS.pMatch1 "373:3--373:13" (RTS.bcSingle (RTS.uint8 0))
     RTS.pSkipExact (RTS.lit 9 :: RTS.UInt 64)
       (RTS.pEnter "ICC._BE32" _BE32)
     (number_of_input_table_entries :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (n :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_input_table_entries :: RTS.UInt 64)
     (number_of_output_table_entries :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (m :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_output_table_entries :: RTS.UInt 64)
     RTS.pEnter "ICC._Chunk"
       (_Chunk (RTS.mul (RTS.mul (RTS.lit 256 :: RTS.UInt 64) n) i))
     RTS.pEnter "ICC._Chunk"
       (_Chunk
          (RTS.mul
             (RTS.mul (RTS.lit 2 :: RTS.UInt 64)
                (exp @(RTS.UInt 64) @(RTS.UInt 64) g i))
             o))
     RTS.pEnter "ICC._Chunk"
       (_Chunk (RTS.mul (RTS.mul (RTS.lit 2 :: RTS.UInt 64) m) o))
 
_LutAToBType :: RTS.Parser ()
 
_LutAToBType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mAB "))
     HS.const () HS.<$> RTS.pByte "386:31--386:35"
     HS.const () HS.<$> RTS.pByte "387:31--387:35"
     HS.const ()
       HS.<$> RTS.pMatch "388:3--388:13"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_LutBToAType :: RTS.Parser ()
 
_LutBToAType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "mBA "))
     HS.const () HS.<$> RTS.pByte "400:31--400:35"
     HS.const () HS.<$> RTS.pByte "401:31--401:35"
     HS.const ()
       HS.<$> RTS.pMatch "402:3--402:13"
                (Vector.fromList
                   [RTS.lit 2 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_Lut_8_16_AB_BA :: RTS.Parser ()
 
_Lut_8_16_AB_BA =
  (RTS.<||)
    (RTS.pEnter "lut8" (RTS.pEnter "ICC._Lut8Type" _Lut8Type))
    ((RTS.<||)
       (RTS.pEnter "lut16" (RTS.pEnter "ICC._Lut16Type" _Lut16Type))
       ((RTS.<||)
          (RTS.pEnter "lutAB" (RTS.pEnter "ICC._LutAToBType" _LutAToBType))
          (RTS.pEnter "lutBA" (RTS.pEnter "ICC._LutBToAType" _LutBToAType))))
 
_Lut_8_16_AB :: RTS.Parser ()
 
_Lut_8_16_AB =
  (RTS.<||)
    (RTS.pEnter "lut8" (RTS.pEnter "ICC._Lut8Type" _Lut8Type))
    ((RTS.<||)
       (RTS.pEnter "lut16" (RTS.pEnter "ICC._Lut16Type" _Lut16Type))
       (RTS.pEnter "lutAB" (RTS.pEnter "ICC._LutAToBType" _LutAToBType)))
 
_Lut_8_16_BA :: RTS.Parser ()
 
_Lut_8_16_BA =
  (RTS.<||)
    (RTS.pEnter "lut8" (RTS.pEnter "ICC._Lut8Type" _Lut8Type))
    ((RTS.<||)
       (RTS.pEnter "lut16" (RTS.pEnter "ICC._Lut16Type" _Lut16Type))
       (RTS.pEnter "lutBA" (RTS.pEnter "ICC._LutBToAType" _LutBToAType)))
 
_CurveType :: RTS.Parser ()
 
_CurveType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "curv"))
     (n :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pSkipExact (RTS.convert n :: RTS.UInt 64)
       (RTS.pEnter "ICC._BE16" _BE16)
 
_ParametricCurveType :: RTS.Parser ()
 
_ParametricCurveType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "para"))
     RTS.pEnter "ICC._BE16" _BE16
     HS.const ()
       HS.<$> RTS.pMatch "327:3--327:13"
                (Vector.fromList
                   [RTS.lit 0 :: RTS.UInt 8, RTS.lit 0 :: RTS.UInt 8])
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "ICC._BE32" _BE32)
 
_SomeCurve :: RTS.Parser ()
 
_SomeCurve =
  (RTS.<||)
    (RTS.pEnter "curve" (RTS.pEnter "ICC._CurveType" _CurveType))
    (RTS.pEnter "parametric_curve"
       (RTS.pEnter "ICC._ParametricCurveType" _ParametricCurveType))
 
_DateTimeType :: RTS.Parser ()
 
_DateTimeType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "dtim"))
     RTS.pEnter "ICC._DateTimeNumber" _DateTimeNumber
 
_Only :: forall b. RTS.DDL b => RTS.Parser () -> RTS.Parser ()
 
_Only (_P :: RTS.Parser ()) =
  do _P
     RTS.pEnd "535:24--535:26"
 
_TextType :: RTS.Parser ()
 
_TextType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "text"))
     RTS.pEnter "ICC._Only"
       (_Only @(Vector.Vector (RTS.UInt 7))
          (RTS.pEnter "ICC._ASCII7" _ASCII7))
 
_SignatureType :: RTS.Parser ()
 
_SignatureType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "sig "))
     RTS.pSkipExact (RTS.lit 4 :: RTS.UInt 64)
       (HS.const () HS.<$> RTS.pByte "270:47--270:51")
 
_Remote :: forall c. RTS.DDL c => RTS.Parser () -> RTS.Parser ()
 
_Remote (_P :: RTS.Parser ()) =
  do (s :: RTS.Input) <- RTS.pPeek
     _P
     RTS.pSetInput s
 
_ChunkRelativeTo ::
      RTS.Input -> (RTS.UInt 64 -> (RTS.UInt 64 -> RTS.Parser ()))
 
_ChunkRelativeTo (s :: RTS.Input) (off :: RTS.UInt 64)
  (sz :: RTS.UInt 64) =
  do RTS.pEnter "ICC._GotoRel" (_GotoRel s off)
     RTS.pEnter "ICC._Chunk" (_Chunk sz)
 
_UnicodeRecord :: RTS.Input -> RTS.Parser ()
 
_UnicodeRecord (s :: RTS.Input) =
  do RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
     (size :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (offset :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pEnter "ICC._Remote"
       (_Remote @RTS.Input
          (RTS.pEnter "ICC._ChunkRelativeTo"
             (_ChunkRelativeTo s (RTS.convert offset :: RTS.UInt 64)
                (RTS.convert size :: RTS.UInt 64))))
 
_MultiLocalizedUnicodeType :: RTS.Parser ()
 
_MultiLocalizedUnicodeType =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "mluc"))
     (record_number :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (record_size :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pEnter "ICC._Guard"
       (_Guard (record_size HS.== (RTS.lit 12 :: RTS.UInt 32)))
     RTS.pSkipExact (RTS.convert record_number :: RTS.UInt 64)
       (RTS.pEnter "ICC._UnicodeRecord" (_UnicodeRecord s))
 
_S15Fixed16ArrayType :: RTS.Parser ()
 
_S15Fixed16ArrayType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "sf32"))
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "ICC._BE32" _BE32)
 
_ColorantOrderType :: RTS.Parser ()
 
_ColorantOrderType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "clro"))
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pSkipMany (RTS.<||)
       (HS.const () HS.<$> RTS.pByte "304:8--304:12")
 
_ParseChunk ::
  forall e.
    RTS.DDL e => RTS.UInt 64 -> (RTS.Parser () -> RTS.Parser ())
 
_ParseChunk (sz :: RTS.UInt 64) (_P :: RTS.Parser ()) =
  do (s :: RTS.Input) <- RTS.pPeek
     (s1 :: RTS.Input) <-
       RTS.pIsJust "520:9--520:15" "Not enough bytes" (RTS.limitLen sz s)
     RTS.pSetInput s1
     _P
     (s2 :: RTS.Input) <-
       RTS.pIsJust "523:9--523:15" "Not enough bytes" (RTS.advanceBy sz s)
     RTS.pSetInput s2
 
_Colorant :: RTS.Parser ()
 
_Colorant =
  do RTS.pEnter "ICC._ParseChunk"
       (_ParseChunk @(Vector.Vector (RTS.UInt 7))
          (RTS.lit 32 :: RTS.UInt 64)
          (RTS.pEnter "ICC._Only"
             (_Only @(Vector.Vector (RTS.UInt 7))
                (RTS.pEnter "ICC._ASCII7" _ASCII7))))
     RTS.pSkipExact (RTS.lit 3 :: RTS.UInt 64)
       (RTS.pEnter "ICC._BE16" _BE16)
 
_ColorantTableType :: RTS.Parser ()
 
_ColorantTableType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "clrt"))
     (count_of_colorant :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pSkipExact (RTS.convert count_of_colorant :: RTS.UInt 64)
       (RTS.pEnter "ICC._Colorant" _Colorant)
 
_ResponseCurve :: RTS.UInt 64 -> RTS.Parser ()
 
_ResponseCurve (n :: RTS.UInt 64) =
  do RTS.pEnter "ICC._BE32" _BE32
     (counts :: Vector.Vector (RTS.UInt 32)) <-
       Vector.replicateM n (RTS.pEnter "ICC.BE32" pBE32)
     RTS.pSkipExact n (RTS.pEnter "ICC._XYNumber" _XYNumber)
     HS.void
       (RTS.loopMapM
          (\(qi :: RTS.UInt 32) ->
             Vector.replicateM (RTS.convert qi :: RTS.UInt 64)
               (RTS.pEnter "ICC.Response16Number" pResponse16Number))
          counts
          :: RTS.Parser (Vector.Vector (Vector.Vector Response16Number)))
     HS.pure ()
 
_ResponseCurveSet16Type :: RTS.Parser ()
 
_ResponseCurveSet16Type =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "rcs2"))
     (number_of_channels :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     (count :: RTS.UInt 16) <- RTS.pEnter "ICC.BE16" pBE16
     RTS.pSkipExact (RTS.convert count :: RTS.UInt 64)
       (do (off :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
           RTS.pEnter "ICC._Remote"
             (_Remote @ResponseCurve
                (do RTS.pEnter "ICC._GotoRel"
                      (_GotoRel s (RTS.convert off :: RTS.UInt 64))
                    RTS.pEnter "ICC._ResponseCurve"
                      (_ResponseCurve (RTS.convert number_of_channels :: RTS.UInt 64)))))
 
_MultiProcessElementsType :: RTS.Parser ()
 
_MultiProcessElementsType =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "mpet"))
     RTS.pEnter "ICC._BE16" _BE16
     RTS.pEnter "ICC._BE16" _BE16
     (number_of_processing_elements :: RTS.UInt 32) <-
       RTS.pEnter "ICC.BE32" pBE32
     (n :: RTS.UInt 64) <-
       HS.pure (RTS.convert number_of_processing_elements :: RTS.UInt 64)
     RTS.pEnter "ICC._Guard"
       (_Guard ((RTS.lit 0 :: RTS.UInt 64) HS.< n))
     (els :: Vector.Vector PositionNumber) <-
       Vector.replicateM n
         (RTS.pEnter "ICC.PositionNumber" pPositionNumber)
     HS.void
       (RTS.loopMapM
          (\(e :: PositionNumber) ->
             RTS.pEnter "ICC.ChunkRelativeTo"
               (pChunkRelativeTo s
                  (RTS.convert (HS.getField @"offset" e) :: RTS.UInt 64)
                  (RTS.convert (HS.getField @"size" e) :: RTS.UInt 64)))
          els
          :: RTS.Parser (Vector.Vector RTS.Input))
     HS.pure ()
 
_XYZType :: RTS.Parser ()
 
_XYZType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "XYZ "))
     RTS.pSkipMany (RTS.<||) (RTS.pEnter "ICC._XYZNumber" _XYZNumber)
 
_MeasurementType :: RTS.Parser ()
 
_MeasurementType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "meas"))
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._XYZNumber" _XYZNumber
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
     RTS.pEnter "ICC._BE32" _BE32
 
_ColorName :: RTS.UInt 64 -> RTS.Parser ()
 
_ColorName (m :: RTS.UInt 64) =
  do RTS.pEnter "ICC._ParseChunk"
       (_ParseChunk @(Vector.Vector (RTS.UInt 7))
          (RTS.lit 32 :: RTS.UInt 64)
          (RTS.pEnter "ICC._ASCII7" _ASCII7))
     RTS.pSkipExact (RTS.lit 3 :: RTS.UInt 64)
       (RTS.pEnter "ICC._BE16" _BE16)
     RTS.pSkipExact m (RTS.pEnter "ICC._BE16" _BE16)
 
_NamedColor2Type :: RTS.Parser ()
 
_NamedColor2Type =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "ncl2"))
     RTS.pEnter "ICC._BE32" _BE32
     (count :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     (number_of_coords :: RTS.UInt 32) <- RTS.pEnter "ICC.BE32" pBE32
     RTS.pEnter "ICC._ParseChunk"
       (_ParseChunk @(Vector.Vector (RTS.UInt 7))
          (RTS.lit 32 :: RTS.UInt 64)
          (RTS.pEnter "ICC._Only"
             (_Only @(Vector.Vector (RTS.UInt 7))
                (RTS.pEnter "ICC._ASCII7" _ASCII7))))
     RTS.pEnter "ICC._ParseChunk"
       (_ParseChunk @(Vector.Vector (RTS.UInt 7))
          (RTS.lit 32 :: RTS.UInt 64)
          (RTS.pEnter "ICC._Only"
             (_Only @(Vector.Vector (RTS.UInt 7))
                (RTS.pEnter "ICC._ASCII7" _ASCII7))))
     RTS.pSkipExact (RTS.convert count :: RTS.UInt 64)
       (RTS.pEnter "ICC._ColorName"
          (_ColorName (RTS.convert number_of_coords :: RTS.UInt 64)))
 
_ProfileSequenceDescType :: RTS.Parser ()
 
_ProfileSequenceDescType =
  RTS.pEnter "ICC._StartTag" (_StartTag (Vector.vecFromRep "pseq"))
 
_ViewConditionsType :: RTS.Parser ()
 
_ViewConditionsType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "view"))
     RTS.pEnter "ICC._XYZNumber" _XYZNumber
     RTS.pEnter "ICC._XYZNumber" _XYZNumber
     RTS.pEnter "ICC._BE32" _BE32
 
_Tag :: Vector.Vector (RTS.UInt 8) -> RTS.Parser ()
 
_Tag (sig :: Vector.Vector (RTS.UInt 8)) =
  (RTS.<||)
    ((RTS.<||)
       (RTS.pEnter "AToB0"
          (do RTS.pEnter "ICC._Guard"
                (_Guard (sig HS.== Vector.vecFromRep "A2B0"))
              RTS.pErrorMode RTS.Abort
                (RTS.pEnter "ICC._Lut_8_16_AB" _Lut_8_16_AB)))
       ((RTS.<||)
          (RTS.pEnter "AToB1"
             (do RTS.pEnter "ICC._Guard"
                   (_Guard (sig HS.== Vector.vecFromRep "A2B1"))
                 RTS.pErrorMode RTS.Abort
                   (RTS.pEnter "ICC._Lut_8_16_AB" _Lut_8_16_AB)))
          ((RTS.<||)
             (RTS.pEnter "AToB2"
                (do RTS.pEnter "ICC._Guard"
                      (_Guard (sig HS.== Vector.vecFromRep "A2B2"))
                    RTS.pErrorMode RTS.Abort
                      (RTS.pEnter "ICC._Lut_8_16_AB" _Lut_8_16_AB)))
             ((RTS.<||)
                (RTS.pEnter "blueMatrixColumn"
                   (do RTS.pEnter "ICC._Guard"
                         (_Guard (sig HS.== Vector.vecFromRep "bXYZ"))
                       RTS.pErrorMode RTS.Abort (RTS.pEnter "ICC._XYZType" _XYZType)))
                ((RTS.<||)
                   (RTS.pEnter "blueTRC"
                      (do RTS.pEnter "ICC._Guard"
                            (_Guard (sig HS.== Vector.vecFromRep "bTRC"))
                          RTS.pErrorMode RTS.Abort (RTS.pEnter "ICC._SomeCurve" _SomeCurve)))
                   ((RTS.<||)
                      (RTS.pEnter "BToA0"
                         (do RTS.pEnter "ICC._Guard"
                               (_Guard (sig HS.== Vector.vecFromRep "B2A0"))
                             RTS.pErrorMode RTS.Abort
                               (RTS.pEnter "ICC._Lut_8_16_BA" _Lut_8_16_BA)))
                      ((RTS.<||)
                         (RTS.pEnter "BToA1"
                            (do RTS.pEnter "ICC._Guard"
                                  (_Guard (sig HS.== Vector.vecFromRep "B2A1"))
                                RTS.pErrorMode RTS.Abort
                                  (RTS.pEnter "ICC._Lut_8_16_BA" _Lut_8_16_BA)))
                         ((RTS.<||)
                            (RTS.pEnter "BToA2"
                               (do RTS.pEnter "ICC._Guard"
                                     (_Guard (sig HS.== Vector.vecFromRep "B2A2"))
                                   RTS.pErrorMode RTS.Abort
                                     (RTS.pEnter "ICC._Lut_8_16_BA" _Lut_8_16_BA)))
                            ((RTS.<||)
                               (RTS.pEnter "BToD0"
                                  (do RTS.pEnter "ICC._Guard"
                                        (_Guard (sig HS.== Vector.vecFromRep "B2D0"))
                                      RTS.pErrorMode RTS.Abort
                                        (RTS.pEnter "ICC._MultiProcessElementsType"
                                           _MultiProcessElementsType)))
                               ((RTS.<||)
                                  (RTS.pEnter "BToD1"
                                     (do RTS.pEnter "ICC._Guard"
                                           (_Guard (sig HS.== Vector.vecFromRep "B2D1"))
                                         RTS.pErrorMode RTS.Abort
                                           (RTS.pEnter "ICC._MultiProcessElementsType"
                                              _MultiProcessElementsType)))
                                  ((RTS.<||)
                                     (RTS.pEnter "BToD2"
                                        (do RTS.pEnter "ICC._Guard"
                                              (_Guard (sig HS.== Vector.vecFromRep "B2D2"))
                                            RTS.pErrorMode RTS.Abort
                                              (RTS.pEnter "ICC._MultiProcessElementsType"
                                                 _MultiProcessElementsType)))
                                     ((RTS.<||)
                                        (RTS.pEnter "BToD3"
                                           (do RTS.pEnter "ICC._Guard"
                                                 (_Guard (sig HS.== Vector.vecFromRep "B2D3"))
                                               RTS.pErrorMode RTS.Abort
                                                 (RTS.pEnter "ICC._MultiProcessElementsType"
                                                    _MultiProcessElementsType)))
                                        ((RTS.<||)
                                           (RTS.pEnter "calibrationDateTime"
                                              (do RTS.pEnter "ICC._Guard"
                                                    (_Guard (sig HS.== Vector.vecFromRep "calt"))
                                                  RTS.pErrorMode RTS.Abort
                                                    (RTS.pEnter "ICC._DateTimeType" _DateTimeType)))
                                           ((RTS.<||)
                                              (RTS.pEnter "charTarget"
                                                 (do RTS.pEnter "ICC._Guard"
                                                       (_Guard (sig HS.== Vector.vecFromRep "targ"))
                                                     RTS.pErrorMode RTS.Abort
                                                       (RTS.pEnter "ICC._TextType" _TextType)))
                                              ((RTS.<||)
                                                 (RTS.pEnter "chromaticAdaptation"
                                                    (do RTS.pEnter "ICC._Guard"
                                                          (_Guard
                                                             (sig HS.== Vector.vecFromRep "chad"))
                                                        RTS.pErrorMode RTS.Abort
                                                          (RTS.pEnter "ICC._S15Fixed16ArrayType"
                                                             _S15Fixed16ArrayType)))
                                                 ((RTS.<||)
                                                    (RTS.pEnter "colorantOrder"
                                                       (do RTS.pEnter "ICC._Guard"
                                                             (_Guard
                                                                (sig
                                                                   HS.== Vector.vecFromRep "clro"))
                                                           RTS.pErrorMode RTS.Abort
                                                             (RTS.pEnter "ICC._ColorantOrderType"
                                                                _ColorantOrderType)))
                                                    ((RTS.<||)
                                                       (RTS.pEnter "colorantTable"
                                                          (do RTS.pEnter "ICC._Guard"
                                                                (_Guard
                                                                   (sig
                                                                      HS.== Vector.vecFromRep
                                                                              "clrt"))
                                                              RTS.pErrorMode RTS.Abort
                                                                (RTS.pEnter "ICC._ColorantTableType"
                                                                   _ColorantTableType)))
                                                       ((RTS.<||)
                                                          (RTS.pEnter "colorantTableOut"
                                                             (do RTS.pEnter "ICC._Guard"
                                                                   (_Guard
                                                                      (sig
                                                                         HS.== Vector.vecFromRep
                                                                                 "clot"))
                                                                 RTS.pErrorMode RTS.Abort
                                                                   (RTS.pEnter
                                                                      "ICC._ColorantTableType"
                                                                      _ColorantTableType)))
                                                          ((RTS.<||)
                                                             (RTS.pEnter
                                                                "colorimetricIntentImageState"
                                                                (do RTS.pEnter "ICC._Guard"
                                                                      (_Guard
                                                                         (sig
                                                                            HS.== Vector.vecFromRep
                                                                                    "ciis"))
                                                                    RTS.pErrorMode RTS.Abort
                                                                      (RTS.pEnter
                                                                         "ICC._SignatureType"
                                                                         _SignatureType)))
                                                             ((RTS.<||)
                                                                (RTS.pEnter "copyright"
                                                                   (do RTS.pEnter "ICC._Guard"
                                                                         (_Guard
                                                                            (sig
                                                                               HS.== Vector.vecFromRep
                                                                                       "cprt"))
                                                                       RTS.pErrorMode RTS.Abort
                                                                         (RTS.pEnter
                                                                            "ICC._MultiLocalizedUnicodeType"
                                                                            _MultiLocalizedUnicodeType)))
                                                                ((RTS.<||)
                                                                   (RTS.pEnter "deviceMfgDesc"
                                                                      (do RTS.pEnter "ICC._Guard"
                                                                            (_Guard
                                                                               (sig
                                                                                  HS.== Vector.vecFromRep
                                                                                          "dmnd"))
                                                                          RTS.pErrorMode RTS.Abort
                                                                            (RTS.pEnter
                                                                               "ICC._MultiLocalizedUnicodeType"
                                                                               _MultiLocalizedUnicodeType)))
                                                                   ((RTS.<||)
                                                                      (RTS.pEnter "deviceModelDesc"
                                                                         (do RTS.pEnter "ICC._Guard"
                                                                               (_Guard
                                                                                  (sig
                                                                                     HS.== Vector.vecFromRep
                                                                                             "dmdd"))
                                                                             RTS.pErrorMode
                                                                               RTS.Abort
                                                                               (RTS.pEnter
                                                                                  "ICC._MultiLocalizedUnicodeType"
                                                                                  _MultiLocalizedUnicodeType)))
                                                                      ((RTS.<||)
                                                                         (RTS.pEnter "DToB0"
                                                                            (do RTS.pEnter
                                                                                  "ICC._Guard"
                                                                                  (_Guard
                                                                                     (sig
                                                                                        HS.== Vector.vecFromRep
                                                                                                "D2B0"))
                                                                                RTS.pErrorMode
                                                                                  RTS.Abort
                                                                                  (RTS.pEnter
                                                                                     "ICC._MultiProcessElementsType"
                                                                                     _MultiProcessElementsType)))
                                                                         ((RTS.<||)
                                                                            (RTS.pEnter "DToB1"
                                                                               (do RTS.pEnter
                                                                                     "ICC._Guard"
                                                                                     (_Guard
                                                                                        (sig
                                                                                           HS.== Vector.vecFromRep
                                                                                                   "D2B1"))
                                                                                   RTS.pErrorMode
                                                                                     RTS.Abort
                                                                                     (RTS.pEnter
                                                                                        "ICC._MultiProcessElementsType"
                                                                                        _MultiProcessElementsType)))
                                                                            ((RTS.<||)
                                                                               (RTS.pEnter "DToB2"
                                                                                  (do RTS.pEnter
                                                                                        "ICC._Guard"
                                                                                        (_Guard
                                                                                           (sig
                                                                                              HS.== Vector.vecFromRep
                                                                                                      "D2B2"))
                                                                                      RTS.pErrorMode
                                                                                        RTS.Abort
                                                                                        (RTS.pEnter
                                                                                           "ICC._MultiProcessElementsType"
                                                                                           _MultiProcessElementsType)))
                                                                               ((RTS.<||)
                                                                                  (RTS.pEnter
                                                                                     "DToB3"
                                                                                     (do RTS.pEnter
                                                                                           "ICC._Guard"
                                                                                           (_Guard
                                                                                              (sig
                                                                                                 HS.== Vector.vecFromRep
                                                                                                         "D2B3"))
                                                                                         RTS.pErrorMode
                                                                                           RTS.Abort
                                                                                           (RTS.pEnter
                                                                                              "ICC._MultiProcessElementsType"
                                                                                              _MultiProcessElementsType)))
                                                                                  ((RTS.<||)
                                                                                     (RTS.pEnter
                                                                                        "gamut"
                                                                                        (do RTS.pEnter
                                                                                              "ICC._Guard"
                                                                                              (_Guard
                                                                                                 (sig
                                                                                                    HS.== Vector.vecFromRep
                                                                                                            "gamt"))
                                                                                            RTS.pErrorMode
                                                                                              RTS.Abort
                                                                                              (RTS.pEnter
                                                                                                 "ICC._Lut_8_16_BA"
                                                                                                 _Lut_8_16_BA)))
                                                                                     ((RTS.<||)
                                                                                        (RTS.pEnter
                                                                                           "grayTRC"
                                                                                           (do RTS.pEnter
                                                                                                 "ICC._Guard"
                                                                                                 (_Guard
                                                                                                    (sig
                                                                                                       HS.== Vector.vecFromRep
                                                                                                               "kTRC"))
                                                                                               RTS.pErrorMode
                                                                                                 RTS.Abort
                                                                                                 (RTS.pEnter
                                                                                                    "ICC._SomeCurve"
                                                                                                    _SomeCurve)))
                                                                                        ((RTS.<||)
                                                                                           (RTS.pEnter
                                                                                              "greenMatrixColumn"
                                                                                              (do RTS.pEnter
                                                                                                    "ICC._Guard"
                                                                                                    (_Guard
                                                                                                       (sig
                                                                                                          HS.== Vector.vecFromRep
                                                                                                                  "gXYZ"))
                                                                                                  RTS.pErrorMode
                                                                                                    RTS.Abort
                                                                                                    (RTS.pEnter
                                                                                                       "ICC._XYZType"
                                                                                                       _XYZType)))
                                                                                           ((RTS.<||)
                                                                                              (RTS.pEnter
                                                                                                 "greenTRC"
                                                                                                 (do RTS.pEnter
                                                                                                       "ICC._Guard"
                                                                                                       (_Guard
                                                                                                          (sig
                                                                                                             HS.== Vector.vecFromRep
                                                                                                                     "gTRC"))
                                                                                                     RTS.pErrorMode
                                                                                                       RTS.Abort
                                                                                                       (RTS.pEnter
                                                                                                          "ICC._SomeCurve"
                                                                                                          _SomeCurve)))
                                                                                              ((RTS.<||)
                                                                                                 (RTS.pEnter
                                                                                                    "luminance"
                                                                                                    (do RTS.pEnter
                                                                                                          "ICC._Guard"
                                                                                                          (_Guard
                                                                                                             (sig
                                                                                                                HS.== Vector.vecFromRep
                                                                                                                        "lumi"))
                                                                                                        RTS.pErrorMode
                                                                                                          RTS.Abort
                                                                                                          (RTS.pEnter
                                                                                                             "ICC._XYZType"
                                                                                                             _XYZType)))
                                                                                                 ((RTS.<||)
                                                                                                    (RTS.pEnter
                                                                                                       "measurement"
                                                                                                       (do RTS.pEnter
                                                                                                             "ICC._Guard"
                                                                                                             (_Guard
                                                                                                                (sig
                                                                                                                   HS.== Vector.vecFromRep
                                                                                                                           "meas"))
                                                                                                           RTS.pErrorMode
                                                                                                             RTS.Abort
                                                                                                             (RTS.pEnter
                                                                                                                "ICC._MeasurementType"
                                                                                                                _MeasurementType)))
                                                                                                    ((RTS.<||)
                                                                                                       (RTS.pEnter
                                                                                                          "mediaWhitePoint"
                                                                                                          (do RTS.pEnter
                                                                                                                "ICC._Guard"
                                                                                                                (_Guard
                                                                                                                   (sig
                                                                                                                      HS.== Vector.vecFromRep
                                                                                                                              "wtpt"))
                                                                                                              RTS.pErrorMode
                                                                                                                RTS.Abort
                                                                                                                (RTS.pEnter
                                                                                                                   "ICC._XYZType"
                                                                                                                   _XYZType)))
                                                                                                       ((RTS.<||)
                                                                                                          (RTS.pEnter
                                                                                                             "namedColor2"
                                                                                                             (do RTS.pEnter
                                                                                                                   "ICC._Guard"
                                                                                                                   (_Guard
                                                                                                                      (sig
                                                                                                                         HS.== Vector.vecFromRep
                                                                                                                                 "ncl2"))
                                                                                                                 RTS.pErrorMode
                                                                                                                   RTS.Abort
                                                                                                                   (RTS.pEnter
                                                                                                                      "ICC._NamedColor2Type"
                                                                                                                      _NamedColor2Type)))
                                                                                                          ((RTS.<||)
                                                                                                             (RTS.pEnter
                                                                                                                "outputResponse"
                                                                                                                (do RTS.pEnter
                                                                                                                      "ICC._Guard"
                                                                                                                      (_Guard
                                                                                                                         (sig
                                                                                                                            HS.== Vector.vecFromRep
                                                                                                                                    "resp"))
                                                                                                                    RTS.pErrorMode
                                                                                                                      RTS.Abort
                                                                                                                      (RTS.pEnter
                                                                                                                         "ICC._ResponseCurveSet16Type"
                                                                                                                         _ResponseCurveSet16Type)))
                                                                                                             ((RTS.<||)
                                                                                                                (RTS.pEnter
                                                                                                                   "perceptualRenderingIntentGamut"
                                                                                                                   (do RTS.pEnter
                                                                                                                         "ICC._Guard"
                                                                                                                         (_Guard
                                                                                                                            (sig
                                                                                                                               HS.== Vector.vecFromRep
                                                                                                                                       "rig0"))
                                                                                                                       RTS.pErrorMode
                                                                                                                         RTS.Abort
                                                                                                                         (RTS.pEnter
                                                                                                                            "ICC._SignatureType"
                                                                                                                            _SignatureType)))
                                                                                                                ((RTS.<||)
                                                                                                                   (RTS.pEnter
                                                                                                                      "preview0"
                                                                                                                      (do RTS.pEnter
                                                                                                                            "ICC._Guard"
                                                                                                                            (_Guard
                                                                                                                               (sig
                                                                                                                                  HS.== Vector.vecFromRep
                                                                                                                                          "pre0"))
                                                                                                                          RTS.pErrorMode
                                                                                                                            RTS.Abort
                                                                                                                            (RTS.pEnter
                                                                                                                               "ICC._Lut_8_16_AB_BA"
                                                                                                                               _Lut_8_16_AB_BA)))
                                                                                                                   ((RTS.<||)
                                                                                                                      (RTS.pEnter
                                                                                                                         "preview1"
                                                                                                                         (do RTS.pEnter
                                                                                                                               "ICC._Guard"
                                                                                                                               (_Guard
                                                                                                                                  (sig
                                                                                                                                     HS.== Vector.vecFromRep
                                                                                                                                             "pre1"))
                                                                                                                             RTS.pErrorMode
                                                                                                                               RTS.Abort
                                                                                                                               (RTS.pEnter
                                                                                                                                  "ICC._Lut_8_16_BA"
                                                                                                                                  _Lut_8_16_BA)))
                                                                                                                      ((RTS.<||)
                                                                                                                         (RTS.pEnter
                                                                                                                            "preview2"
                                                                                                                            (do RTS.pEnter
                                                                                                                                  "ICC._Guard"
                                                                                                                                  (_Guard
                                                                                                                                     (sig
                                                                                                                                        HS.== Vector.vecFromRep
                                                                                                                                                "pre2"))
                                                                                                                                RTS.pErrorMode
                                                                                                                                  RTS.Abort
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "ICC._Lut_8_16_BA"
                                                                                                                                     _Lut_8_16_BA)))
                                                                                                                         ((RTS.<||)
                                                                                                                            (RTS.pEnter
                                                                                                                               "profileDescription"
                                                                                                                               (do RTS.pEnter
                                                                                                                                     "ICC._Guard"
                                                                                                                                     (_Guard
                                                                                                                                        (sig
                                                                                                                                           HS.== Vector.vecFromRep
                                                                                                                                                   "desc"))
                                                                                                                                   RTS.pErrorMode
                                                                                                                                     RTS.Abort
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "ICC._MultiLocalizedUnicodeType"
                                                                                                                                        _MultiLocalizedUnicodeType)))
                                                                                                                            ((RTS.<||)
                                                                                                                               (RTS.pEnter
                                                                                                                                  "profileSequenceDesc"
                                                                                                                                  (do RTS.pEnter
                                                                                                                                        "ICC._Guard"
                                                                                                                                        (_Guard
                                                                                                                                           (sig
                                                                                                                                              HS.== Vector.vecFromRep
                                                                                                                                                      "pseq"))
                                                                                                                                      RTS.pErrorMode
                                                                                                                                        RTS.Abort
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "ICC._ProfileSequenceDescType"
                                                                                                                                           _ProfileSequenceDescType)))
                                                                                                                               ((RTS.<||)
                                                                                                                                  (RTS.pEnter
                                                                                                                                     "profileSequenceIdentifier"
                                                                                                                                     (do RTS.pEnter
                                                                                                                                           "ICC._Guard"
                                                                                                                                           (_Guard
                                                                                                                                              (sig
                                                                                                                                                 HS.== Vector.vecFromRep
                                                                                                                                                         "psid"))
                                                                                                                                         RTS.pErrorMode
                                                                                                                                           RTS.Abort
                                                                                                                                           (HS.pure
                                                                                                                                              ())))
                                                                                                                                  ((RTS.<||)
                                                                                                                                     (RTS.pEnter
                                                                                                                                        "redMatrixColumn"
                                                                                                                                        (do RTS.pEnter
                                                                                                                                              "ICC._Guard"
                                                                                                                                              (_Guard
                                                                                                                                                 (sig
                                                                                                                                                    HS.== Vector.vecFromRep
                                                                                                                                                            "rXYZ"))
                                                                                                                                            RTS.pErrorMode
                                                                                                                                              RTS.Abort
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "ICC._XYZType"
                                                                                                                                                 _XYZType)))
                                                                                                                                     ((RTS.<||)
                                                                                                                                        (RTS.pEnter
                                                                                                                                           "redTRC"
                                                                                                                                           (do RTS.pEnter
                                                                                                                                                 "ICC._Guard"
                                                                                                                                                 (_Guard
                                                                                                                                                    (sig
                                                                                                                                                       HS.== Vector.vecFromRep
                                                                                                                                                               "rTRC"))
                                                                                                                                               RTS.pErrorMode
                                                                                                                                                 RTS.Abort
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "ICC._SomeCurve"
                                                                                                                                                    _SomeCurve)))
                                                                                                                                        ((RTS.<||)
                                                                                                                                           (RTS.pEnter
                                                                                                                                              "saturationRenderingIntentGamut"
                                                                                                                                              (do RTS.pEnter
                                                                                                                                                    "ICC._Guard"
                                                                                                                                                    (_Guard
                                                                                                                                                       (sig
                                                                                                                                                          HS.== Vector.vecFromRep
                                                                                                                                                                  "rig2"))
                                                                                                                                                  RTS.pErrorMode
                                                                                                                                                    RTS.Abort
                                                                                                                                                    (RTS.pEnter
                                                                                                                                                       "ICC._SignatureType"
                                                                                                                                                       _SignatureType)))
                                                                                                                                           ((RTS.<||)
                                                                                                                                              (RTS.pEnter
                                                                                                                                                 "technology"
                                                                                                                                                 (do RTS.pEnter
                                                                                                                                                       "ICC._Guard"
                                                                                                                                                       (_Guard
                                                                                                                                                          (sig
                                                                                                                                                             HS.== Vector.vecFromRep
                                                                                                                                                                     "tech"))
                                                                                                                                                     RTS.pErrorMode
                                                                                                                                                       RTS.Abort
                                                                                                                                                       (RTS.pEnter
                                                                                                                                                          "ICC._SignatureType"
                                                                                                                                                          _SignatureType)))
                                                                                                                                              ((RTS.<||)
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "viewCondDesc"
                                                                                                                                                    (do RTS.pEnter
                                                                                                                                                          "ICC._Guard"
                                                                                                                                                          (_Guard
                                                                                                                                                             (sig
                                                                                                                                                                HS.== Vector.vecFromRep
                                                                                                                                                                        "vued"))
                                                                                                                                                        RTS.pErrorMode
                                                                                                                                                          RTS.Abort
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "ICC._MultiLocalizedUnicodeType"
                                                                                                                                                             _MultiLocalizedUnicodeType)))
                                                                                                                                                 (RTS.pEnter
                                                                                                                                                    "viewConditions"
                                                                                                                                                    (do RTS.pEnter
                                                                                                                                                          "ICC._Guard"
                                                                                                                                                          (_Guard
                                                                                                                                                             (sig
                                                                                                                                                                HS.== Vector.vecFromRep
                                                                                                                                                                        "view"))
                                                                                                                                                        RTS.pErrorMode
                                                                                                                                                          RTS.Abort
                                                                                                                                                          (RTS.pEnter
                                                                                                                                                             "ICC._ViewConditionsType"
                                                                                                                                                             _ViewConditionsType))))))))))))))))))))))))))))))))))))))))))))))))))
    (RTS.pError RTS.FromUser "228:6--228:46"
       (Vector.vecToString
          (Vector.concat
             (Vector.fromList [Vector.vecFromRep "Unregonized tag: ", sig]))))
 
_ParseTag :: TagEntry -> RTS.Parser ()
 
_ParseTag (t :: TagEntry) =
  do RTS.pEnter "ICC._Goto"
       (_Goto
          (RTS.convert (HS.getField @"offset_to_data_element" t)
             :: RTS.UInt 64))
     RTS.pEnter "ICC._ParseChunk"
       (_ParseChunk @Tag
          (RTS.convert (HS.getField @"size_of_data_element" t)
             :: RTS.UInt 64)
          (RTS.pEnter "ICC._Tag" (_Tag (HS.getField @"tag_signature" t))))
 
_ChromaticityType :: RTS.Parser ()
 
_ChromaticityType =
  do RTS.pEnter "ICC._StartTag"
       (_StartTag (Vector.vecFromRep "chrm"))
     (number_of_device_channels :: RTS.UInt 16) <-
       RTS.pEnter "ICC.BE16" pBE16
     RTS.pEnter "ICC._BE16" _BE16
     RTS.pSkipExact
       (RTS.convert number_of_device_channels :: RTS.UInt 64)
       (RTS.pEnter "ICC._XYNumber" _XYNumber)
 
_ValidateArray ::
  forall g.
    RTS.DDL g =>
      Vector.Vector (RTS.UInt 8) -> (RTS.Parser () -> RTS.Parser ())
 
_ValidateArray (arr :: Vector.Vector (RTS.UInt 8))
  (_P :: RTS.Parser ()) =
  do (s :: RTS.Input) <- RTS.pPeek
     RTS.pSetInput (RTS.arrayStream (Vector.vecFromRep "array") arr)
     _P
     RTS.pEnd "531:3--531:5"
     RTS.pSetInput s