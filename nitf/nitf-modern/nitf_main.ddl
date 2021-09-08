import nitf_lib
import nitf_header
import nitf_img_subheader
import nitf_graphic_subheader
import nitf_text_subheader
import nitf_data_ext_subheader
import nitf_res_ext_subheader

def CheckDateTime fdt dt = @{
   @ep = Epoch ;
   @lep = LiftDate ep ;
   -- check that the image was created after Epoch
   PartialOrdDate lep dt.partDate ;

   -- check that the image was created before today
   @today = Today ;
   @ltoday = LiftDate today ;
   PartialOrdDate dt.partDate ltoday ;

   -- check that the data was created before the file
   PartialOrdDateTime dt fdt ;
}

def Main = {
    h = Header;
    @ldt = LiftDateTime h.fdt ;

    -- parse each image segment:
    img_segments =
      map (imglens in h.li) {
        imgHeader = ISHeader ;
        CheckDateTime ldt imgHeader.idatim ;

        -- parse the bytes in the data segment
        -- imgData = Many imglens.li Byte ;
      };

    -- parse each graphic segment:
    graph_segments =
      map (gls in h.graphlens) {
        graphHeader = GraphicHeader ;

        -- parse the bytes in the data segment
        graphData = Many (gls.seglen as! uint 64) Byte ;
      } ;

    -- parse each text segment:
    txt_segments =
      map (textls in h.textlens) {
        txtHeader = TextHeader ;

        CheckDateTime ldt txtHeader.txtdt ;

        -- parse the bytes in the data segment
        txtData = Many (textls.lt as! uint 64) Byte ;
      };

    -- parse each data-extension segment:
    dataExt_segments =
      map (dataextls in h.dataextlens) {
        dataExtHeader = DataExtHeader ;

        -- parse the bytes in the data extension data segment
        dataExtData = Many (dataextls.ld as! uint 64) Byte ;
      } ;

    -- parse each reserved-extension segment:
    reservedExt_segments =
      map (resextls in h.resextlens) {
        resExtHeader = ResExtHeader ;

        -- parse the bytes in the reserved extension data segment
        resExtData = Many (resextls.lre as! uint 64) Byte ;
      } ;

    -- TEST: enable to fail JITC Neg Format cases
    -- END;
}
