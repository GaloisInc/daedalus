-- import the message-level parser
import Stdlib

import DataMessage

import FileData
import DisplayInfo

-- DBG:
-- AppParser: parse application payloads
def AppParser0 (qos: [Parameter]) = {
    -- get the type name from the parameter list
    @tyNameMaybe = for (tyNmMaybe = nothing; p in qos) (
          just (p.val is typeNameVal) <| tyNmMaybe
    );

    -- run the parser for the typename
    First
      hasTyName = {
        @tyName = tyNameMaybe is just;
        First
          fileDataMessage = {
            Guard (tyName == "FileData::FileInfo");
            FileData_FileInfo_Top;
            }
          displayInfoMessage = {
            Guard (tyName == "DisplayInfoData::DisplayInfo");
            DisplayInfoData_DisplayInfo_Top;
            }
          notSubscribed = {
            Guard (tyName != "FileData::FileInfo" &&
                   tyName != "DisplayInfoData::DisplayInfo");
            }
        }
      noTyName = tyNameMaybe is nothing;
}

def AppParser x = ^{}

-- entry point: 
def Main = Message AppParser
