-- Automatically generated - do not edit
import Stdlib
import JFIF

def WithArrStream arr P = {
    @strm = GetStream;
    SetStream (arrayStream arr);
    $$ = P;
    SetStream strm;
}    
    

--const int32 FILEDATA_MAX_FILE_SIZE = 20;
def const_FileData_FILEDATA_MAX_FILE_SIZE = 2100000

--typedef sequence<octet, FileData::FILEDATA_MAX_FILE_SIZE> FileSequence;
def FileData_FileSequence = Sequence UInt8 const_FileData_FILEDATA_MAX_FILE_SIZE

--struct FileInfo {
--  @key
--  int32 file_id;
--  FileData::FileSequence data;
--};
def FileData_FileInfo = {
  -- @PadTo 4;
  file_id = Int32;
  @data = FileData_FileSequence;
  jfif  = WithArrStream data JFIF
}

-- Instantiable, top-level includes CDR encapsulation signature
def FileData_FileInfo_Top = {
    -- Match [0x0, 0x1, 0x0, 0x0];
     -- Many
     FileData_FileInfo;
}

def Main = FileData_FileInfo_Top 



 
