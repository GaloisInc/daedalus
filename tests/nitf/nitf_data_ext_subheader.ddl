import nitf_lib

def DE = "DE"

def DataExtHeader = {
  DE ;
  desid = Many 25 BCSA ;
  desver = PosNumber 2 ;

  common = CommonSubheader ;

  desoflw = Choose {
    present = Choose {
      oflwUDHD = @(PadWSpaces 6 "UDHD") ;
      oflwUDID = @(PadWSpaces 6 "UDID") ;
      oflwXHD = @(PadWSpaces 6 "XHD") ;
      oflwIXSHD = @(PadWSpaces 6 "IXSHD") ;
      oflwSXSHD = @(PadWSpaces 6 "SXSHD") ;
      oflwTXSHD = @(PadWSpaces 6 "TXSHD") ;
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
