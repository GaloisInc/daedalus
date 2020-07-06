' A VB Script to CSV export all sheets in a Calc document
' From: https://www.briankoponen.com/libreoffice-export-sheets-csv/

Sub ExportToCsv
    document = ThisComponent

    ' Use the global string tools library to generate a path to save each CSV
    GlobalScope.BasicLibraries.loadLibrary("Tools")
    FileDirectory = Tools.Strings.DirectoryNameoutofPath(document.getURL(), "/")

    ' Work out number of sheets for looping over them later.
    Sheets = document.Sheets
    NumSheets = Sheets.Count - 1

    ' Set up a propval object to store the filter properties
    Dim Propval(1) as New com.sun.star.beans.PropertyValue
    Propval(0).Name = "FilterName"
    Propval(0).Value = "Text - txt - csv (StarCalc)"
    Propval(1).Name = "FilterOptions"
    Propval(1).Value ="59,34,0,1,1"   'ASCII  59 = ;  34 = "

    For I = 0 to NumSheets
        ' For each sheet, assemble a filename and save using the filter
        document.getCurrentController.setActiveSheet(Sheets(I))
        Filename = FileDirectory + "/" + Sheets(I).Name + ".csv"
        FileURL = convertToURL(Filename)
        document.StoreToURL(FileURL, Propval())
    Next I

End Sub
