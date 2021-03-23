-- import the message-level parser
import Stdlib

import DataMessage

import FileData
import DisplayInfo

-- AppParser: parse application payloads
def AppParser (maybeQos: QosParams) = {
  qos = maybeQos is hasQos;
  -- get the type name from the parameter list
  @tyNameMaybe = for (tyNmMaybe = nothing; p in qos) (
    Choose1 {
      { @nm = p is typeNameVal;
        ^just nm;
      };
      ^tyNmMaybe;
    }
  );
  @tyName = tyNameMaybe is just;

  -- run the parser for the typename
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
    }
  }
}

-- entry point: 
def Main = Message AppParser
