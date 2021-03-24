-- import the message-level parser
import Stdlib

import DataMessage

import FileData
import DisplayInfo

-- AppParser: parse application payloads
def AppParser (qos: [Parameter]) = {
    -- get the type name from the parameter list
    @tyNameMaybe = for (tyNmMaybe = nothing; p in qos) (
      Choose1 {
        { @nm = p.val is typeNameVal;
          ^just nm;
        };
        ^tyNmMaybe;
      }
    );

    -- run the parser for the typename
    Choose1 {
      hasTyName = {
        @tyName = tyNameMaybe is just;
        Choose1 {
          fileDataMessage = {
            Guard (tyName == "FileData::FileInfo");
            FileData_FileInfo_Top;
          };
          displayInfoMessage = {
            Guard (tyName == "DisplayInfoData::DisplayInfo");
            DisplayInfoData_DisplayInfo_Top;
          };
          notSubscribed = {
            Guard (tyName != "FileData::FileInfo" &&
                   tyName != "DisplayInfoData::DisplayInfo");
          };
        };
      };
      noTyName = tyNameMaybe is nothing;
    };
}

-- entry point: 
-- DBG:
def Main = Message AppParser
-- def Main = Message 
