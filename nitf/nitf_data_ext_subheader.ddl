import nitf_lib

def DE = Match "DE"

def DataExtHeader = {
  DE ;
  desid = Many 25 BCSA ;
  desver = PosNumber 2 ;

  common = CommonSubheader ;

  desoflw = Choose {
    present = Choose {
      oflwUDHD = @(PadWSpaces 6 (Match "UDHD")) ;
      oflwUDID = @(PadWSpaces 6 (Match "UDID")) ;
      oflwXHD = @(PadWSpaces 6 (Match "XHD")) ;
      oflwIXSHD = @(PadWSpaces 6 (Match "IXSHD")) ;
      oflwSXSHD = @(PadWSpaces 6 (Match "SXSHD")) ;
      oflwTXSHD = @(PadWSpaces 6 (Match "TXSHD")) ;
    } ;
    nooflw = ^{} ;
  } ;

  dsitem = Choose {
    present = {
      desoflw is present ;
      UnsignedNum 3
    } ;
    omitted = desoflw is nooflw ;
  } ;

  desshl = IsNum 4 0 ;
}
