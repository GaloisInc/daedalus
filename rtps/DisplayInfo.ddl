-- Automatically generated - do not edit
import Stdlib

--enum eImageType {
--  @value(-10)
--  JPEG,
--  @value(4)
--  GIF,
--  @value(190)
--  PNG,
--  MPEG4
--};
def Enum_DisplayInfoData_eImageType = {
  @PadTo 4;
  @val = Int32;
  ( (val != -10) && (val != 4) && (val != 190) && (val != 191) ) is false;
  ^ val
}

--enum eTransitionType {
--  NO_TRANSITION,
--  FADE,
--  WIPE_LEFT,
--  WIPE_RIGHT,
--  WIPE_UP,
--  WIPE_DOWN
--};
def Enum_DisplayInfoData_eTransitionType = {
  @PadTo 4;
  @val = Int32;
  ( (val != 0) && (val != 1) && (val != 2) && (val != 3) && (val != 4) && (val != 5) ) is false;
  ^ val
}

--enum eEffectType {
--  NO_EFFECT,
--  BLACK_AND_WHITE,
--  SEPIA,
--  VIVID,
--  SOFTEN
--};
def Enum_DisplayInfoData_eEffectType = {
  @PadTo 4;
  @val = Int32;
  ( (val != 0) && (val != 1) && (val != 2) && (val != 3) && (val != 4) ) is false;
  ^ val
}

--struct DisplayInfo {
--  @key
--  int32 key_id;
--  float background_color_r;
--  float background_color_g;
--  float background_color_b;
--  DisplayInfoData::eImageType image_type;
--  float display_duration;
--  DisplayInfoData::eTransitionType transition_type;
--  DisplayInfoData::eEffectType effect_type;
--};
def DisplayInfoData_DisplayInfo = {
  @PadTo 4;
  key_id = Int32;
  @PadTo 4;
  background_color_r = Float;
  @PadTo 4;
  background_color_g = Float;
  @PadTo 4;
  background_color_b = Float;
  image_type = Enum_DisplayInfoData_eImageType;
  @PadTo 4;
  display_duration = Float;
  transition_type = Enum_DisplayInfoData_eTransitionType;
  effect_type = Enum_DisplayInfoData_eEffectType;
}

-- Instantiable, top-level includes CDR encapsulation signature
def DisplayInfoData_DisplayInfo_Top = {
    Match [0x0, 0x1, 0x0, 0x0];
    $$ = Many DisplayInfoData_DisplayInfo;
}




