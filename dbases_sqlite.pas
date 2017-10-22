unit dbases_sqlite;

{$mode objfpc}

interface

uses
  Classes, SysUtils, db, sqldb, fpcsvexport, sqlite3conn, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBGrids, sqlite3dyn,
  clipbrd, LazUTF8, LazUTF8Classes;

type

  { TfrmSQLiteDBases }

  TfrmSQLiteDBases = class(TForm)
    CSVExporter1: TCSVExporter;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    lblConnectionStatus: TLabel;
    SQLite3Connection1: TSQLite3Connection;
    sqlFILES: TSQLQuery;
    sqlCOPY: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CreateDatabase(DBaseName : string);
    procedure WriteFILESValuesToDatabase(Filename, Filepath, HashValue, FileSize : string);
    procedure WriteCOPYValuesToDatabase(Col1, Col2, Col3, Col4, Col5 : string);
    procedure EmptyDBTable(TableName : string; DBGrid : TDBGrid);
    procedure UpdateGridFILES(Sender: TObject);
    procedure UpdateGridCOPYTAB(Sender: TObject);
    procedure SaveDBToCSV(DBGrid : TDBGrid; Filename : string);
    procedure SaveFILESTabToHTML(DBGrid : TDBGrid; Filename : string);
    procedure SaveCOPYWindowToHTML(DBGrid : TDBGrid; Filename : string);
    procedure DatasetToClipBoard(DBGrid : TDBGrid);
    procedure ShowDuplicates(DBGrid : TDBGrid);
    procedure DeleteDuplicates(DBGrid : TDBGrid);
    procedure SortByFileName(DBGrid : TDBGrid);
    procedure SortByFilePath(DBGrid : TDBGrid);
    procedure SortByHash(DBGrid : TDBGrid);
    procedure ShowAll(DBGrid : TDBGrid);
    procedure ShowAllCOPYGRID(DBGrid : TDBGrid);
    procedure CopyFileNameOfSelectedCell(DBGrid : TDBGrid);
    procedure CopyFilePathOfSelectedCell(DBGrid : TDBGrid);
    procedure CopyHashOfSelectedCell(DBGrid : TDBGrid);
    procedure CopySelectedRowFILESTAB(DBGrid : TDBGrid);
    procedure CopySelectedRowCOPYTAB(DBGrid : TDBGrid);
    procedure SortBySourceFilename(DBGrid : TDBGrid);
    procedure SortByDestinationFilename(DBGrid : TDBGrid);
    procedure SortBySourceHash(DBGrid : TDBGrid);
    procedure SortByDestinationHash(DBGrid : TDBGrid);
    function CountGridRows(DBGrid : TDBGrid) : integer;

  private
    { private declarations }
  public
    { public declarations }
  const
    // More information on the use of these values is below.
    // They need not be set as constants in your application. They can be any valid value
    application_id = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0 .. 4294967295)
    user_version = 23400001;     // must be a 32-bit Signed Integer (LongInt -2147483648 .. 2147483647)
  end;

var
  frmSQLiteDBases: TfrmSQLiteDBases;

implementation

{$R *.lfm}

{ TfrmSQLiteDBases }

  uses
    Unit2, uDisplayGrid;


procedure TfrmSQLiteDBases.FormCreate(Sender: TObject);
begin
  // SQLiteDefaultLibrary is from the sqlite3dyn unit, new with FPC3.0
     {$IFDEF Windows}
       // Ensure we're using the local sqlite3.dll
       SQLiteDefaultLibrary := 'sqlite3.dll';
     {$ENDIF}
       {$IFDEF Darwin}
         SQLiteDefaultLibrary := 'sqlite3.o.so';
       {$else}
         {$IFDEF UNIX and !$ifdef Darwin}
           SQLiteDefaultLibrary := 'sqlite3.o.so';
         {$ENDIF}
     {$ENDIF}

     if FileExists(SQLiteDefaultLibrary) then
     begin
       // Set the filename of the sqlite database
       SQLite3Connection1.DatabaseName := 'QuickHashDB.sqlite';
       // Create the database
       CreateDatabase(SQLite3Connection1.DatabaseName);
       if SQLIte3Connection1.Connected then lblConnectionStatus.Caption:= 'SQLite3 Database connection active';
     end
       else ShowMessage('Cannot create SQLite database. Ensure an SQLite.dll or equivalent file exists');
end;

// Create a fresh SQLite database for each instance of the program
procedure TfrmSQLiteDBases.CreateDatabase(DBaseName : string);
begin
  SQLite3Connection1.Close; // Ensure the connection is closed when we start
  //SQLite3Connection1.Password := txtPass.Text;
  try
    // Since we're making this database for the first time,
    // check whether the file already exists
    if FileExists(SQLite3Connection1.DatabaseName) then
    begin
      DeleteFile(SQLite3Connection1.DatabaseName);
    end;

      // Make a new database and add the tables
      try
        SQLite3Connection1.Open;
        SQLTransaction1.Active := true;

        // Periodically sort the database out to ensure it stays in tip top shape
        // during heavy usage
        SQLite3Connection1.ExecuteDirect('PRAGMA auto_vacuum = FULL;');

        // Per the SQLite Documentation (edited for clarity):
        // The pragma user_version is used to set or get the value of the user-version.
        // The user-version is a big-endian 32-bit signed integer stored in the database header at offset 60.
        // The user-version is not used internally by SQLite. It may be used by applications for any purpose.
        // http://www.sqlite.org/pragma.html#pragma_schema_version
        SQLite3Connection1.ExecuteDirect('PRAGMA user_version = ' + IntToStr(user_version) + ';');

        // Per the SQLite Documentation:
        // The application_id PRAGMA is used to query or set the 32-bit unsigned big-endian
        // "Application ID" integer located at offset 68 into the database header.
        // Applications that use SQLite as their application file-format should set the
        // Application ID integer to a unique integer so that utilities such as file(1) can
        // determine the specific file type rather than just reporting "SQLite3 Database".
        // A list of assigned application IDs can be seen by consulting the magic.txt file
        // in the SQLite source repository.
        // http://www.sqlite.org/pragma.html#pragma_application_id
        SQLite3Connection1.ExecuteDirect('PRAGMA application_id = ' + IntToStr(application_id) + ';');

        // Here we're setting up a table named "TBL_FILES" in the new database
        // Note AUTOINCREMENT is NOT used! If it is, it causes problems with RowIDs etc after multiple selections
        // Besides, SQLite advice is not to use it unless entirely necessary (http://sqlite.org/autoinc.html)
        SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_FILES"('+
                    ' "id" Integer NOT NULL PRIMARY KEY,'+
                    ' "FileName" Char(128) NOT NULL,'+
                    ' "FilePath" Char(128) NOT NULL,'+
                    ' "HashValue" Char(128) NOT NULL,'+
                    ' "FileSize" Char(128) NOT NULL);');
        // Creating an index based upon id in the TBL_FILES Table
        SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "FILES_id_idx" ON "TBL_FILES"( "id" );');

        // Here we're setting up a table named "TBL_COPY" in the new database
        SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_COPY"('+
                    ' "id" Integer NOT NULL PRIMARY KEY,'+
                    ' "SourceFilename" Char(128) NOT NULL,'+
                    ' "SourceHash" Char(128) NOT NULL,'+
                    ' "DestinationFilename" Char(128) NOT NULL,'+
                    ' "DestinationHash" Char(128) NOT NULL,'+
                    ' "DateAttributes" Char(128) NOT NULL);');
        // Creating an index based upon id in the TBL_COPY Table
        SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "COPIED_FILES_id_idx" ON "TBL_COPY"( "id" );');

        // Here we're setting up a table named "TBL_COMPAREFOLDERSA" in the new database
        // for the FolderA of Compare Two Directories
        SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_COMPAREFOLDERSA"('+
                    ' "id" Integer NOT NULL PRIMARY KEY,'+
                    ' "SrcFileAndPath" Char(128) NOT NULL,'+
                    ' "SrcHash" Char(128) NOT NULL);');
        // Creating an index based upon id in the COMPAREFOLDERSA Table
        SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "FOLDERA_FILES_id_idx" ON "TBL_COMPAREFOLDERSA"( "id" );');

        // Here we're setting up a table named "TBL_COMPAREFOLDERSB" in the new database
        // for the FolderB of Compare Two Directories
        SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_COMPAREFOLDERSB"('+
                    ' "id" Integer NOT NULL PRIMARY KEY,'+
                    ' "SrcFileAndPath" Char(128) NOT NULL,'+
                    ' "SrcHash" Char(128) NOT NULL);');
        // Creating an index based upon id in the COMPAREFOLDERSB Table
        SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "FOLDERB_FILES_id_idx" ON "TBL_COMPAREFOLDERSB"( "id" );');

        // Now write to the new database
        SQLTransaction1.CommitRetaining;
      except
        ShowMessage('Unable to create a new SQLite Database');
      end;
    except
      ShowMessage('Unable to check if database file exists');
    end;
end;

// I've spent what seems like half my life working out how to copy the entire selected
// row of a DBGrid component without success!! So I resorted to childhood logic.
// Anyone who knows of a better way, let me know!
procedure TfrmSQLiteDBases.CopySelectedRowFILESTAB(DBGrid : TDBGrid);
var
  FileNameCell, FilePathCell, FileHashCell, AllRowCells : string;
begin
  // Get the data from the filename cell that the user has selected
  FileNameCell := DBGrid.DataSource.DataSet.Fields[1].Value;
  // Get the data from the filepath cell that the user has selected
  FilePathCell := DBGrid.DataSource.DataSet.Fields[2].Value;
  // Get the data from the filehash cell that the user has selected
  FileHashCell := DBGrid.DataSource.DataSet.Fields[3].Value;
  // and just add them all together :-)
  AllRowCells := FileNameCell + ',' + FilePathCell + ',' + FileHashCell;
  Clipboard.AsText := AllRowCells;
end;

procedure TfrmSQLiteDBases.CopySelectedRowCOPYTAB(DBGrid : TDBGrid);
var
  AllRowCells, SourceFileNameCell, SourceHash,
    DestinationFilenameCell, DestinationHash, DateAttr : string;
begin
  // Get the data from the source filename cell that the user has selected
  SourceFileNameCell := DBGrid.DataSource.DataSet.Fields[1].Value;
  // Get the source file hash cell that the user has selected
  SourceHash := DBGrid.DataSource.DataSet.Fields[2].Value;
  // Get the destination filename
  DestinationFilenameCell := DBGrid.DataSource.DataSet.Fields[3].Value;
  // Get the destination hash
  DestinationHash  := DBGrid.DataSource.DataSet.Fields[4].Value;
  // Get the date attributes
  DateAttr         := DBGrid.DataSource.DataSet.Fields[5].Value;
  // and just add them all together :-)
  AllRowCells := SourceFileNameCell + ',' + SourceHash  + ',' + DestinationFilenameCell + ',' + DestinationHash + ',' + DateAttr;
  Clipboard.AsText := AllRowCells;
end;

// Counts rows of current DBGrid. Returns positive integer if successfull and
// returns active display to top row
function TfrmSQLiteDBases.CountGridRows(DBGrid : TDBGrid) : integer;
var
  NoOfRows : integer;
begin
  NoOfRows := -1;
  while not DBGrid.DataSource.DataSet.EOF do
  begin
    inc(NoOfRows, 1);
    DBGrid.DataSource.DataSet.Next;
  end;
  // Got to top of grid.
  DBGrid.DataSource.DataSet.First;
  // Return count
  result := NoOfRows;
end;
// Saves the grid in FILES tab to HTML. If small volume of records, uses a stringlist.
// If big volume, uses file stream.
procedure TfrmSQLiteDBases.SaveFILESTabToHTML(DBGrid : TDBGrid; Filename : string);
var
   strTitle, FileNameCell, FilePathCell, FileHashCell, AllRowCells : string;
  i, NoOfRowsInGrid : integer;
  sl                : TStringList;
  fs                : TFileStreamUTF8;

  const
    strHTMLHeader      = '<HTML>'  ;
    strTITLEHeader     = '<TITLE>QuickHash HTML Output' ;
    strBODYHeader      = '<BODY>'  ;
    strTABLEHeader     = '<TABLE>' ;
    strTABLEROWStart   = '<TR>'    ;
    strTABLEDATAStart  = '<TD>'    ;
    strTABLEDataEnd    = '</TD>'   ;
    strTABLEROWEnd     = '</TR>'   ;
    strTABLEFooter     = '</TABLE>';
    strBODYFooter      = '</BODY>' ;
    strTITLEFooter     = '</TITLE>';
    strHTMLFooter      = '</HTML>' ;

begin
  NoOfRowsInGrid := -1;
  // If database volume not too big, use memory and stringlists. Otherwise, use file writes
  if DBGrid.Name = 'RecursiveDisplayGrid1' then
    begin
      NoOfRowsInGrid := CountGridRows(DBGrid);// Count the rows first. If not too many, use memory. Otherwise, use filestreams
      if (NoOfRowsInGrid < 10000) and (NoOfRowsInGrid > -1) then
      try
        MainForm.StatusBar2.Caption:= ' Saving grid to ' + Filename + '...please wait';
        Application.ProcessMessages;
        // Write the grid to a stringlist
        sl := TStringList.Create;
        sl.add('<HTML>');
        sl.add('<TITLE>QuickHash HTML Output</TITLE>');
        sl.add('<BODY>');
        sl.add('<p>HTML Output generated ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', Now) + ' using ' + MainForm.Caption + '</p>');
        sl.add('<TABLE>');
        while not DBGrid.DataSource.DataSet.EOF do
        begin
          for i := 0 to DBGrid.DataSource.DataSet.FieldCount -1 do
          begin
            sl.add('<tr>');
            // Get the data from the filename cell that the user has selected
            FileNameCell := DBGrid.DataSource.DataSet.Fields[1].Value;
            sl.add('<td>'+FileNameCell+'</td>');
            // Get the data from the filepath cell that the user has selected
            FilePathCell := DBGrid.DataSource.DataSet.Fields[2].Value;
            sl.add('<td>'+FilePathCell+'</td>');
            // Get the data from the filehash cell that the user has selected
            FileHashCell := DBGrid.DataSource.DataSet.Fields[3].Value;
            sl.add('<td>'+FileHashCell+'</td>');
            sl.add('</tr>');
            DBGrid.DataSource.DataSet.Next;
          end;
        end;
        sl.add('</TABLE>');
        sl.add('</BODY> ');
        sl.add('</HTML> ');
        sl.SaveToFile(Filename);
      finally
        sl.free;
        MainForm.StatusBar2.Caption:= ' Data saved to HTML file ' + Filename + '...OK';
        Application.ProcessMessages;
      end
      else // Use filestream method because there's more than 10K rows. Too many to add HTML tags and store in memory
        try
        if not FileExists(filename) then
          begin
            fs := TFileStreamUTF8.Create(Filename, fmCreate);
          end
        else fs := TFileStreamUTF8.Create(Filename, fmOpenReadWrite);

        MainForm.StatusBar2.Caption:= ' Saving grid to ' + Filename + '...please wait';
        strTitle := '<p>HTML Output generated ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', Now) + ' using ' + MainForm.Caption + '</p>';
        Application.ProcessMessages;

        fs.Write(strHTMLHeader[1], Length(strHTMLHeader));
        fs.Write(#13#10, 2);
        fs.Write(strTITLEHeader[1], Length(strTITLEHeader));
        fs.Write(strTITLEFooter[1], Length(strTITLEFooter));
        fs.Write(#13#10, 2);
        fs.Write(strBODYHeader[1], Length(strBODYHeader));
        fs.Write(strTitle[1], Length(strTitle));
        fs.Write(#13#10, 2);
        fs.Write(strTABLEHeader[1], Length(strTABLEHeader));

        { strTABLEROWStart   = '<TR>'      = 4 bytes
          strTABLEDATAStart  = '<TD>'      = 4 bytes
          strTABLEDataEnd    = '</TD>'     = 5 bytes
          strTABLEROWEnd     = '</TR>'     = 5 bytes
          strTABLEFooter     = '</TABLE>'  = 8 bytes
          strBODYFooter      = '</BODY>'   = 7 bytes
          strTITLEFooter     = '</TITLE>'  = 8 bytes
          strHTMLFooter      = '</HTML>'   = 7 bytes}
        while not DBGrid.DataSource.DataSet.EOF do
        begin
          for i := 0 to DBGrid.DataSource.DataSet.FieldCount -1 do
          begin
            // Start new row
            fs.Write(strTABLEROWStart[1], 4);
            // Get the data from the filename cell that the user has selected
            FileNameCell := DBGrid.DataSource.DataSet.Fields[1].Value;
            // Write filename to new row
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(FileNameCell[1], Length(FileNameCell));
            fs.Write(strTABLEDataEnd[1], 5);

            // Get the data from the filepath cell that the user has selected
            FilePathCell := DBGrid.DataSource.DataSet.Fields[2].Value;
            // Write filepath to new row
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(FilePathCell[1], Length(FilePathCell));
            fs.Write(strTABLEDATAEnd[1], 5);

            // Get the data from the filehash cell that the user has selected
            FileHashCell := DBGrid.DataSource.DataSet.Fields[3].Value;
            // Write hash to new row
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(FileHashCell[1], Length(Trim(FileHashCell)));
            fs.Write(strTABLEDATAEnd[1], 5);
            // End the row
            fs.Write(strTABLEROWEnd[1], 5);
            fs.Write(#13#10, 2);
            DBGrid.DataSource.DataSet.Next;
            // TODO : Why does the last entry get repeated 5 times in the HTML output??
          end;
        end;
        fs.Write(strTABLEFooter, 8);
        fs.Write(#13#10, 2);
        fs.writeansistring(IntToStr(NoOfRowsInGrid) + ' grid entries saved.');
        fs.Write(strBODYFooter, 7);
        fs.Write(#13#10, 2);
        fs.Write(strHTMLFooter, 7);
        fs.Write(#13#10, 2);
        finally
          fs.free;
          MainForm.StatusBar2.Caption:= ' Data saved to HTML file ' + Filename + '...OK';
          Application.ProcessMessages;
        end;
    end
  else
    if DBGrid.Name = 'frmDisplayGrid1' then
    begin
      // Same as above but use the 5 columns from COPY grid instead of the 3 of FILES
    end;
end;

// Deletes a DB table from the SQLite DB
procedure TfrmSQLiteDBases.EmptyDBTable(TableName : string; DBGrid : TDBGrid);
var
  DynamicSQLQuery: TSQLQuery;
begin
  DynamicSQLQuery := TSQLQuery.Create(nil);
  try
    try
      DynamicSQLQuery.DataBase := sqlFILES.Database;
      DynamicSQLQuery.Transaction := sqlFILES.Transaction;
      DynamicSQLQuery.SQL.Text := 'DELETE FROM ' + TableName;
      if SQLite3Connection1.Connected then
      begin
        SQLTransaction1.Active := True;
        DynamicSQLQuery.ExecSQL;
        SQLTransaction1.CommitRetaining; // Retain transaction is important here
      end;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
  finally
    DynamicSQLQuery.Free;
  end;
end;

// SaveDBToCSV exports the DBGrid (DBGridName) to a CSV file (filename) for the user
// Based on example in FPC\3.0.2\source\packages\fcl-db\tests\testdbexport.pas
// Requires the lazdbexport package be installed in Lazarus IDE
procedure TfrmSQLiteDBases.SaveDBToCSV(DBGrid : TDBGrid; Filename : string);
var
  Exporter : TCSVExporter;
  ExportSettings: TCSVFormatSettings;
begin
  Exporter := TCSVExporter.Create(nil);
  ExportSettings := TCSVFormatSettings.Create(true);
  Exporter.FormatSettings := ExportSettings;
  Exporter.Dataset := DBGrid.DataSource.DataSet;
  Exporter.FileName := FileName;
  if Exporter.Execute > 0 then
    begin
      ShowMessage('CSV saved as ' + Filename);
    end
  else Showmessage('Could not save to CSV file ' + Filename);
end;

// Copies a DBGrid content to a temp text file then reads it into clipboard
procedure TfrmSQLiteDBases.DatasetToClipBoard(DBGrid : TDBGrid);
var
  DeletedOK : boolean;
  vStringList : TStringList;
  Exporter : TCSVExporter;
  ExportSettings: TCSVFormatSettings;
  FileName : string;
begin
    Filename := 'QH_TmpFile.tmp';
    DeletedOK := false;
    try
      Exporter := TCSVExporter.Create(nil);
      try
        ExportSettings := TCSVFormatSettings.Create(true);
        Exporter.FormatSettings := ExportSettings;
        Exporter.Dataset := DBGrid.DataSource.DataSet;
        Exporter.FileName := FileName;
        // if the temp outfile is written successfully with DBGrid content, load it to clipboard
        if Exporter.Execute > 0 then
          try
            // we can free it now the file is written OK. If we dont free now, we
            // cant use LoadFromFile next
            if assigned(exporter) then freeandnil(exporter);
            // Now load the text file into clipboard
            vStringList := TStringList.Create;
            vStringList.LoadFromFile('QH_TmpFile.tmp');
            // Write file to clipboard
            Clipboard.AsText := vStringList.Text;
          finally
            DeletedOK := DeleteFile(Filename);
            if DeletedOK = false then Showmessage('Could not delete temporary file QH_TmpFile.tmp');
            if assigned(vStringList) then freeandnil(vStringList);
            ShowMessage('Grid content now in clipboard.');
          end;
      finally
        // Nothing to do
      end;
    finally
      // Nothing to do
    end;
end;

// ShowDuplicates lists entries with duplicate hash values from the FILES tab,
// by searching hash column for matches and then displays all rows fully
// for which duplicate hashes were found
procedure TfrmSQLiteDBases.ShowDuplicates(DBGrid : TDBGrid);
// Sourced from https://stackoverflow.com/questions/46345862/sql-how-to-return-all-column-fields-for-one-column-containing-duplicates
begin
  try
  DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
  TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                        'FROM TBL_FILES WHERE HashValue IN ' +
                        '(SELECT HashValue FROM TBL_FILES ' +
                        'GROUP BY HashValue HAVING COUNT(*) > 1) ORDER BY hashvalue';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  MainForm.RecursiveDisplayGrid1.Options:= MainForm.RecursiveDisplayGrid1.Options + [dgAutoSizeColumns];
  DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// DeleteDuplicates remove duplicate files as found in the 'FILES' tab
procedure TfrmSQLiteDBases.DeleteDuplicates(DBGrid : TDBGrid);
var
  FileName, FilePath, NameAndPath, FileHash : string;
  i, FileDeletedCount : integer;
  FilesDeletedOK : boolean;
  slDuplicates, slDuplicatesDeleted : TStringList;
begin
  FilesDeletedOK := false;
  FileDeletedCount := 0;
  try
  slDuplicates := TStringList.Create;
  slDuplicates.Sorted := true;

  slDuplicatesDeleted := TStringList.Create;
  slDuplicatesDeleted.Sorted := true;

  while not DBGrid.DataSource.DataSet.EOF do
    begin
      for i := 0 to DBGrid.DataSource.DataSet.FieldCount -1 do
      begin
        FileName := DBGrid.DataSource.DataSet.Fields[1].Value;
        FilePath := DBGrid.DataSource.DataSet.Fields[2].Value;
        FileHash := DBGrid.DataSource.DataSet.Fields[3].Value;
        NameAndPath := FilePath+FileName;
        // Now, add the hash value, but only if it's not already in the stringlist
        // If the currently examined hashvalue IS in the list, then it must be a duplicate
        // and can therefore be deleted
        if slDuplicates.IndexOf(FileHash) > -1 then
          begin
            FilesDeletedOK := DeleteFile(NameAndPath); // it's a duplicate
            if FilesDeletedOK = true then
            begin
              inc(FileDeletedCount, 1);
              slDuplicatesDeleted.Add(NameAndPath + ',' + FileHash + ', was deleted OK');
            end;
            // reset deletion flag
            FilesDeletedOK := false;
          end
          else slDuplicates.add(FileHash);
        // Go to next record
        DBGrid.DataSource.DataSet.Next;
      end;
    end;
    // Allow user the choice to save results of the duplicate file deletions
    try
      if MessageDlg(IntToStr(FileDeletedCount) + ' duplicate files deleted. Save details to text file?', mtConfirmation,
        [mbCancel, mbNo, mbYes],0) = mrYes then
        begin
          MainForm.FilesDBGrid_SaveCSVDialog.Title := 'Save deleted file record as...';
          MainForm.FilesDBGrid_SaveCSVDialog.InitialDir := GetCurrentDir;
          MainForm.FilesDBGrid_SaveCSVDialog.Filter := 'Comma Sep|*.csv';
          MainForm.FilesDBGrid_SaveCSVDialog.DefaultExt := 'csv';
          if MainForm.FilesDBGrid_SaveCSVDialog.Execute then
            begin
               slDuplicatesDeleted.SaveToFile(MainForm.FilesDBGrid_SaveCSVDialog.Filename);
            end;
        end;
    except
      // do nothing
    end;
  finally
    slDuplicates.free;
  end;
end;


// *** Start of FILES tab related database routines ***

// Write computed values from the FILES tab to the database table TBL_FILES
procedure TfrmSQLiteDBases.WriteFILESValuesToDatabase(Filename, Filepath, HashValue, FileSize : string);
begin
  try
    sqlFILES.Close;
    // Insert the values into the database. We're using ParamByName which prevents SQL Injection
    // http://wiki.freepascal.org/Working_With_TSQLQuery#Parameters_in_TSQLQuery.SQL
    sqlFILES.SQL.Text := 'INSERT into TBL_FILES (Filename, FilePath, HashValue, FileSize) values (:Filename,:FilePath,:HashValue,:FileSize)';
    SQLTransaction1.Active := True;
    sqlFILES.Params.ParamByName('Filename').AsString := Filename;
    sqlFILES.Params.ParamByName('FilePath').AsString := FilePath;
    sqlFILES.Params.ParamByName('HashValue').AsString := hashvalue;
    sqlFILES.Params.ParamByName('FileSize').AsString := FileSize;
    sqlFILES.ExecSQL;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab to sort entries by filename alphabetically
procedure TfrmSQLiteDBases.SortByFileName(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                          'FROM TBL_FILES ORDER BY FileName';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    MainForm.RecursiveDisplayGrid1.Options:= MainForm.RecursiveDisplayGrid1.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;


// Used by FILES tab for sorting entries by file path alphabetically
procedure TfrmSQLiteDBases.SortByFilePath(DBGrid : TDBGrid);
begin
 try
   DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                        'FROM TBL_FILES ORDER BY FilePath';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   MainForm.RecursiveDisplayGrid1.Options:= MainForm.RecursiveDisplayGrid1.Options + [dgAutoSizeColumns];
   DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;


// Used by the FILES tab display grid to sort by hash
procedure TfrmSQLiteDBases.SortByHash(DBGrid : TDBGrid);
begin
 try
   DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                        'FROM TBL_FILES ORDER BY HashValue';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   MainForm.RecursiveDisplayGrid1.Options:= MainForm.RecursiveDisplayGrid1.Options + [dgAutoSizeColumns];
   DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to list all again
procedure TfrmSQLiteDBases.ShowAll(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT * FROM TBL_FILES';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    MainForm.RecursiveDisplayGrid1.Options:= MainForm.RecursiveDisplayGrid1.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to copy the content of Column 1 (filename) to clipboard
procedure TfrmSQLiteDBases.CopyFilenameOfSelectedCell(DBGrid : TDBGrid);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  if not (DBGrid.DataSource.DataSet.Fields[1].Value = NULL) then
  begin
    CellOfInterest := DBGrid.DataSource.DataSet.Fields[1].Value;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// // Used by the FILES tab display grid to copy the content of Column 2 (file path) to clipboard
procedure TfrmSQLiteDBases.CopyFilePathOfSelectedCell(DBGrid : TDBGrid);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  if not (DBGrid.DataSource.DataSet.Fields[2].Value = NULL) then
  begin
    CellOfInterest := DBGrid.DataSource.DataSet.Fields[2].Value;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// // Used by the FILES tab display grid to copy the content of Column 3 (Hash Value) to clipboard
procedure TfrmSQLiteDBases.CopyHashOfSelectedCell(DBGrid : TDBGrid);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  if not (DBGrid.DataSource.DataSet.Fields[3].Value = NULL) then
  begin
    CellOfInterest := DBGrid.DataSource.DataSet.Fields[3].Value;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// *** Start of COPY tab related functions ***

// Write computed values from the COPY tab to the database table TBL_COPY
procedure TfrmSQLiteDBases.WriteCOPYValuesToDatabase(Col1, Col2, Col3, Col4, Col5 : string);
{Col1 : Source Filename
 Col2 : Source Hash
 Col3 : Destination Filename
 Col4 : Destination Hash
 Col5 : DateAttributes;}
begin
  try
    // Insert the values into the database. We're using ParamByName which prevents SQL Injection
    // http://wiki.freepascal.org/Working_With_TSQLQuery#Parameters_in_TSQLQuery.SQL
    sqlCOPY.Close;
    sqlCOPY.SQL.Text := 'INSERT into TBL_COPY (SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes) values (:SourceFilename,:SourceHash,:DestinationFilename,:DestinationHash,:DateAttributes)';
    SQLTransaction1.Active := True;
    sqlCOPY.Params.ParamByName('SourceFilename').AsString := Col1;
    sqlCOPY.Params.ParamByName('SourceHash').AsString := Col2;
    sqlCOPY.Params.ParamByName('DestinationFilename').AsString := Col3;
    sqlCOPY.Params.ParamByName('DestinationHash').AsString := Col4;
    sqlCOPY.Params.ParamByName('DateAttributes').AsString := Col5;
    sqlCOPY.ExecSQL;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY tab display grid, to sort by source filename...Col 1
procedure TfrmSQLiteDBases.SortBySourceFilename(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY SourceFilename';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options:= frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COPY tab display grid to sort by destination filename...Col 3
procedure TfrmSQLiteDBases.SortByDestinationFilename(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                        'FROM TBL_COPY ORDER BY DestinationFilename';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options:= frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COPY tab display grid to sort by source hash, Col 2
procedure TfrmSQLiteDBases.SortBySourceHash(DBGrid : TDBGrid);
begin
 try
   DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY SourceHash';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options:= frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options + [dgAutoSizeColumns];
   DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY tab display grid to sort by destination hash...Col 4
procedure TfrmSQLiteDBases.SortByDestinationHash(DBGrid : TDBGrid);
begin
 try
   DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY DestinationHash';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options:= frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options + [dgAutoSizeColumns];
   DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY grid to show all items
procedure TfrmSQLiteDBases.ShowAllCOPYGRID(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT * FROM TBL_COPY';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options:= frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Saves the grid in COPY window to HTML. If small volume of records, uses a stringlist.
// If big volume, uses file stream.
procedure TfrmSQLiteDBases.SaveCOPYWindowToHTML(DBGrid : TDBGrid; Filename : string);
var
   strTitle, SourceFilename, DestinationFileName, DateAttributes, SourceFileHash, DestinationFileHash : string;
  i, NoOfRowsInGrid : integer;
  sl                : TStringList;
  fs                : TFileStreamUTF8;

  const
    strHTMLHeader      = '<HTML>'  ;
    strTITLEHeader     = '<TITLE>QuickHash HTML Output' ;
    strBODYHeader      = '<BODY>'  ;
    strTABLEHeader     = '<table>' ;
    strTABLEROWStart   = '<TR>'    ;
    strTABLEDATAStart  = '<TD>'    ;
    strTABLEDataEnd    = '</TD>'   ;
    strTABLEROWEnd     = '</TR>'   ;
    strTABLEFooter     = '</TABLE>';
    strBODYFooter      = '</BODY>' ;
    strTITLEFooter     = '</TITLE>';
    strHTMLFooter      = '</HTML>' ;

begin
  NoOfRowsInGrid := -1;
  // If database volume not too big, use memory and stringlists. Otherwise, use file writes
  NoOfRowsInGrid := CountGridRows(DBGrid);// Count the rows first. If not too many, use memory. Otherwise, use filestreams
  if (NoOfRowsInGrid < 10000) and (NoOfRowsInGrid > -1) then
  try
    MainForm.StatusBar2.Caption:= ' Saving grid to ' + Filename + '...please wait';
    Application.ProcessMessages;
    // Write the grid to a stringlist
    sl := TStringList.Create;
    sl.add('<HTML>');
    sl.add('<TITLE>QuickHash HTML Output</TITLE>');
    sl.add('<BODY>');
    sl.add('<p>HTML Output generated ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', Now) + ' using ' + MainForm.Caption + '</p>');
    sl.add('<table border=1>');
    while not DBGrid.DataSource.DataSet.EOF do
      begin
        for i := 0 to DBGrid.DataSource.DataSet.FieldCount -1 do
        begin
          sl.add('<tr>');
          // Get the data from the source filename cell
          SourceFilename := DBGrid.DataSource.DataSet.Fields[1].Value;
          sl.add('<td>'+SourceFilename+'</td>');
          // Get the data from the source file hash cell
          SourceFileHash := DBGrid.DataSource.DataSet.Fields[2].Value;
          sl.add('<td>'+SourceFileHash+'</td>');
          // Get the data from the destination name
          DestinationFilename := DBGrid.DataSource.DataSet.Fields[3].Value;
          sl.add('<td>'+DestinationFilename+'</td>');
          // Get the data from the source file hash cell
          DestinationFileHash := DBGrid.DataSource.DataSet.Fields[4].Value;
          sl.add('<td>'+DestinationFileHash+'</td>');
          // Get the data from the source file hash cell
          DateAttributes := DBGrid.DataSource.DataSet.Fields[5].Value;
          sl.add('<td>'+DateAttributes+'</td>');
          sl.add('</tr>');
          DBGrid.DataSource.DataSet.Next;
        end;
      end;
    sl.add('</TABLE>');
    sl.add('</BODY> ');
    sl.add('</HTML> ');
    sl.SaveToFile(Filename);
  finally
    sl.free;
    MainForm.StatusBar2.Caption:= ' Data saved to HTML file ' + Filename + '...OK';
    Application.ProcessMessages;
  end
  else // Use filestream method because there's more than 10K rows. Too many to add HTML tags and store in memory
    try
    if not FileExists(filename) then
      begin
        fs := TFileStreamUTF8.Create(Filename, fmCreate);
      end
    else fs := TFileStreamUTF8.Create(Filename, fmOpenReadWrite);

    MainForm.StatusBar2.Caption:= ' Saving grid to ' + Filename + '...please wait';
    strTitle := '<p>HTML Output generated ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', Now) + ' using ' + MainForm.Caption + '</p>';
    Application.ProcessMessages;

    fs.Write(strHTMLHeader[1], Length(strHTMLHeader));
    fs.Write(#13#10, 2);
    fs.Write(strTITLEHeader[1], Length(strTITLEHeader));
    fs.Write(strTITLEFooter[1], Length(strTITLEFooter));
    fs.Write(#13#10, 2);
    fs.Write(strBODYHeader[1], Length(strBODYHeader));
    fs.Write(strTitle[1], Length(strTitle));
    fs.Write(#13#10, 2);
    fs.Write('<table border=1>', 16);

    { strTABLEROWStart   = '<TR>'      = 4 bytes
      strTABLEDATAStart  = '<TD>'      = 4 bytes
      strTABLEDataEnd    = '</TD>'     = 5 bytes
      strTABLEROWEnd     = '</TR>'     = 5 bytes
      strTABLEFooter     = '</TABLE>'  = 8 bytes
      strBODYFooter      = '</BODY>'   = 7 bytes
      strTITLEFooter     = '</TITLE>'  = 8 bytes
      strHTMLFooter      = '</HTML>'   = 7 bytes}
    while not DBGrid.DataSource.DataSet.EOF do
    begin
      for i := 0 to DBGrid.DataSource.DataSet.FieldCount -1 do
      begin
        // Start new row
        fs.Write(strTABLEROWStart[1], 4);
        // Get the source filename cell
        SourceFilename := DBGrid.DataSource.DataSet.Fields[1].Value;
        // Write source filename to new row
        fs.Write(strTABLEDATAStart[1], 4);
        fs.Write(SourceFilename[1], Length(SourceFilename));
        fs.Write(strTABLEDataEnd[1], 5);

        // Get the source hash value
        SourceFileHash := DBGrid.DataSource.DataSet.Fields[2].Value;
        // Write the source hash value
        fs.Write(strTABLEDATAStart[1], 4);
        fs.Write(SourceFileHash[1], Length(SourceFileHash));
        fs.Write(strTABLEDATAEnd[1], 5);

        // Get the destination filename
        DestinationFileName := DBGrid.DataSource.DataSet.Fields[3].Value;
        // Write the destination hash
        fs.Write(strTABLEDATAStart[1], 4) ;
        fs.Write(DestinationFileName[1], Length(Trim(DestinationFileName)));
        fs.Write(strTABLEDATAEnd[1], 5);

        // Get the destination hash
        DestinationFileHash := DBGrid.DataSource.DataSet.Fields[3].Value;
        // Write the destination hash
        fs.Write(strTABLEDATAStart[1], 4) ;
        fs.Write(DestinationFileHash[1], Length(Trim(DestinationFileHash)));
        fs.Write(strTABLEDATAEnd[1], 5);

        // Get the destination filename
        DateAttributes := DBGrid.DataSource.DataSet.Fields[3].Value;
        // Write the destination hash
        fs.Write(strTABLEDATAStart[1], 4) ;
        fs.Write(DateAttributes[1], Length(Trim(DateAttributes)));
        fs.Write(strTABLEDATAEnd[1], 5);

        // End the row
        fs.Write(strTABLEROWEnd[1], 5);
        fs.Write(#13#10, 2);
        DBGrid.DataSource.DataSet.Next;
      end;
    end;
    fs.Write(strTABLEFooter, 8);
    fs.Write(#13#10, 2);
    fs.writeansistring(IntToStr(NoOfRowsInGrid) + ' grid entries saved.');
    fs.Write(strBODYFooter, 7);
    fs.Write(#13#10, 2);
    fs.Write(strHTMLFooter, 7);
    fs.Write(#13#10, 2);
    finally
      fs.free;
      MainForm.StatusBar2.Caption:= ' Data saved to HTML file ' + Filename + '...OK';
      Application.ProcessMessages;
    end;
end;

// There is an UpdateGridXXX routine for each tab where a DBGrid is used.
// Each one is to populate the grid with the data from the query.

// FILES tab update grid routine
procedure TfrmSQLiteDBases.UpdateGridFILES(Sender: TObject);
  begin
    try
    sqlFILES.Close;
    sqlFILES.SQL.Text := 'SELECT * FROM TBL_FILES';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    sqlFILES.Open;
    MainForm.RecursiveDisplayGrid1.Options:= MainForm.RecursiveDisplayGrid1.Options + [dgAutoSizeColumns];
    except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// COPY tab update grid routine
procedure TfrmSQLiteDBases.UpdateGridCOPYTAB(Sender: TObject);
  begin
    try
    sqlCOPY.Close;
    sqlCOPY.SQL.Text := 'SELECT * FROM TBL_COPY';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    sqlCOPY.Open;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options:= frmDisplayGrid1.RecursiveDisplayGrid_COPY.Options + [dgAutoSizeColumns];
    except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

procedure TfrmSQLiteDBases.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLite3Connection1.Close;
  SQLite3Connection1.Free;
end;

{  procedure frmSQLiteDBases.btnCountRowsClick(Sender: TObject);
  begin

    //SQLite3Connection1.Close; // Ensure the connection is closed when we start

    //SQLite3Connection1.Password := txtPass.Text; // The current password

    // Try to perform query
    try
      SQLite3Connection1.Connected := True;

      // Set SQL text to count all rows from the TBL_FILES table
      sqlFILES.SQL.Clear;
      sqlFILES.SQL.Text := 'Select Count(*) from TBL_FILES';
      sqlFILES.Open;

      // Allow the DBGrid to view the results of our query
      DataSource1.DataSet := sqlFILES;
      DBGrid1.DataSource := DataSource1;
      DBGrid1.AutoFillColumns := true;

    except
      ShowMessage('Unable to query the database');
    end;
  end;
}

// Counts the rows of a given database table
// Not needed except for direct table interaction. The "RowCounter" function
// superseeds this and counts the actively displayed DBGrid.
{
function TfrmSQLiteDBases.GetTableRowCount(TableName : string; DBGrid : TDBGrid) : integer;
begin
  result := 0;
  try
    sqlFILES.SQL.Text := 'SELECT Count(*) FROM ' + TableName;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    sqlFILES.Open;
    result := sqlFILES.Fields[0].AsInteger
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;
}

initialization

end.

