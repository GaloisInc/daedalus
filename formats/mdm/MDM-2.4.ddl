-- MORA Data Message parser
--
-- This parser is implemented in accordance with the details by Chapter
-- 6 ("MORA DATA MESSAGING") and related chapters of the Modular Open
-- RF Architecture Draft Specification, version 2.4, published March 1,
-- 2021. Where appropriate, the parser will make references to chapters
-- and sections in the aforementioned document.

def Main = MDM

def MDM =
  block
    header = MDM_Header
    -- body = MDM_Body

-- See: 6.1.2.1, MDM Header
def MDM_Header =
  block
    -- See 6.1.2.1.1, MDM Preamble
    -- "MDM"
    $[0x4d, 0x44, 0x4d]

    -- Ack octet, multiple meanings.
    --
    -- See 6.1.2.1.2: MDM Acknowledge
    -- See 6.2.3: MDM Acknowledgement Message (Type 2) Specification
    let raw_ack_byte = UInt8

    -- See 6.1.2.1.3: Version
    version = MDM_Version

    -- See 6.1.2.1.4: MDM Sequence number
    sequence_number = UInt16
    
    -- See 6.1.2.1.5: ML2B interface ID
    ml2b_interface_id = ML2B_Interface_id

    -- See 6.1.2.1.6: MDM type
    type = MDM_Type raw_ack_byte

-- See 6.1.2.1.3: Version, major followed by minor; e.g. 2.4 yields
-- major version 2, minor version 4.
def MDM_Version =
  block
    major = UInt8
    minor = UInt8

-- See 6.1.2.1.5: ML2B interface ID
def ML2B_Interface_id =
  block
    origination_id = UInt16
    destination_id = UInt16

-- Parse the message type followed by the number of messages field. This
-- returns only the type since the number of messages is always implied
-- by the type anyway.
--
-- See:
-- * 6.1.2.1.6: MDM type
-- * 6.1.2.1.7: Number of messages
def MDM_Type (raw_ack_byte: uint 8) =
  First
    -- Parse the message type octet followed by the number of messages
    -- octet, which has a fixed value corresponding to each message
    -- type.
    Type_VRT                  = @Match [0x1, 0x1]
    Type_Time_Of_Day          = @Match [0x3, 0x1]
    Type_Signal_Port_User_ID  = @Match [0x4, 0x1]
    Type_Health_Status        = @Match [0x5, 0x1]
    Type_Command              = @Match [0x6, 0x1]
    Type_Switch_Group_User_ID = @Match [0x7, 0x1]

    Type_Acknowledgement = block
      @Match [0x2, 0x0]
      -- See 6.3.2: MDM Acknowledgement Message (Type 2) Specification
      --
      -- In the Acknowledge messge type, the "ack" octet parsed in the
      -- message header is treated as a response octet which takes one
      -- of a number of meaningful values.
      --
      -- The specification states that all other possible values are
      -- Reserved for Future Use; if they are encountered here, they
      -- result in a parse failure.
      response = raw_ack_byte as? MDM_Ack_Response_Value

-- See 6.3.2: MDM Acknowledgement Message (Type 2) Specification, Table
-- 74
bitdata MDM_Ack_Response_Value where
  -- Acknowledged (A)
  Resp_A = 0x01
  -- Invalid Message (IM)
  Resp_IM = 0x02
  -- Invalid Function (IF)
  Resp_IF = 0x04
  -- Invalid Range (IR)
  Resp_IR = 0x08
  -- Resource Unavailable (RU)
  Resp_RU = 0x10
  -- Unexpected Sequence Number (US)
  Resp_US = 0x20
  -- Unauthorized User (UU)
  Resp_UU = 0x40

-- See 6.1.2.2, MDM Body
def MDM_Body (ty: MDM_Type) =
  case ty of
    Type_VRT ->
      Fail "VRT messages not yet supported"

    -- See 6.3.2: MDM Acknowledgement Message (Type 2) Specification
    -- Acknowledgement messages have no body.
    Type_Acknowledgement info ->
      ^ {| Body_Acknowledgement = { } |}

    -- See 6.3.3: MDM Time of Day Message (Type 3) Specification
    Type_Time_Of_Day ->
      {| Body_TimeOfDay = UInt32 |}

    -- See 6.3.4: MDM Signal Port User ID Message (Type 4) Specification
    -- Type_SignalPortUserID ->

    -- See 6.3.5: MDM Health Message (Type 5) Specification
    Type_Health_Status ->
      block
        let f1 = UInt32 as? Health_Status_Field_1
        let f2 = UInt32 as? Health_Status_Field_2
        ^ {| Body_HealthStatus = { status_field_1 = f1, status_field_2 = f2 } |}

    -- See 6.3.6: MDM Command Message (Type 6) Specification
    -- Type_Command ->

    -- See 6.3.7: MDM Switch Group User ID Message (Type 7) Specification
    -- Type_SwitchGroupUserID ->

-- See 6.3.5: MDM Health Message (Type 5) Specification, Health Status
-- Field #1 Format
bitdata Health_Status_Field_1 where
  Field1 = { alert_type: Alert_Type,
             port_id: uint 7,
             operational_parameter: Operational_Parameter }

-- See 6.3.5: MDM Health Message (Type 5) Specification, Health Status
-- Field #1 Format
bitdata Operational_Parameter where
  OP_NotUsed          = 0x0: uint 23
  OP_Temperature      = 0x1: uint 23
  OP_Power_Supply     = 0x2: uint 23
  OP_Local_Oscillator = 0x4: uint 23
  OP_RF_Path          = 0x8: uint 23
  OP_Memory           = 0x10: uint 23
  OP_Buffer           = 0x20: uint 23
  OP_Interface_Stack  = 0x40: uint 23
  OP_Cooling          = 0x80: uint 23
  OP_Time             = 0x100: uint 23
  OP_Navigation       = 0x200: uint 23
  OP_Position         = 0x400: uint 23

-- See 6.3.5: MDM Health Message (Type 5) Specification, Health Status
-- Field #1 Format
bitdata Alert_Type where
  AT_Normal   = 0x0: uint 2
  AT_Warning  = 0x1: uint 2
  AT_Failure  = 0x2: uint 2
  AT_Reserved = 0x3: uint 2

-- See 6.3.5: MDM Health Message (Type 5) Specification, Health Status
-- Field #2 Format
bitdata Health_Status_Field_2 where
  Field2 = { operational_state: uint 16,
             parameter_condition: Parameter_Condition }

-- See 6.3.5: MDM Health Message (Type 5) Specification, Health Status
-- Field #2 Format
bitdata Parameter_Condition where
  PC_NotUsed        = 0x0: uint 16
  PC_Value_Low      = 0x1: uint 16
  PC_Value_High     = 0x2: uint 16
  PC_Value_Abnormal = 0x4: uint 16
  PC_No_Value       = 0x8: uint 16
  PC_Unlocked       = 0x10: uint 16
  PC_Overflow       = 0x20: uint 16
  PC_Offline        = 0x40: uint 16
  PC_Internal       = 0x80: uint 16
  PC_External       = 0x100: uint 16
  PC_Seized         = 0x200: uint 16
  PC_Reset_Required = 0x400: uint 16
  PC_Normal         = 0x800: uint 16

def MDM_Body_u =
  union
    Body_Acknowledgement: {}
    Body_Time_Of_Day: uint 32
    -- Body_Signal_Port_User_ID:
    Body_Health_Status: Body_Health_Status_s
    -- Body_Command:
    -- Body_Switch_Group_User_ID:

-- See 6.3.5: MDM Health Message (Type 5) Specification
def Body_Health_Status_s =
  struct
    status_field_1: Health_Status_Field_1
    status_field_2: Health_Status_Field_2

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

def UInt16: uint 16 =
  block
    let b0 = UInt8
    let b1 = UInt8
    ^ (b0 # b1)

def UInt32: uint 32 =
  block
    let b0 = UInt8
    let b1 = UInt8
    let b2 = UInt8
    let b3 = UInt8
    ^ (b0 # b1 # b2 # b3)
