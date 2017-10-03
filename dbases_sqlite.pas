unit dbases_sqlite;

{$mode objfpc}

interface

uses
  Classes, SysUtils, db, sqldb, fpcsvexport, sqlite3conn, FileUtil, LResources,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBGrids, sqlite3dyn,
  clipbrd;

type

  { TfrmSQLiteDBases }

  TfrmSQLiteDBases = class(TForm)
    CSVExporter1: TCSVExporter;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    lblConnectionStatus: TLabel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLQuery2: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CreateDatabase(DBaseName : string);
    procedure WriteFILESValuesToDatabase(Filename, Filepath, HashValue, FileSize : string);
    procedure WriteCOPYValuesToDatabase(Col1, Col2, Col3, Col4, Col5 : string);
    procedure EmptyDBTable(TableName : string; DBGridName : TDBGrid);
    procedure UpdateGridFILES(Sender: TObject);
    procedure UpdateGridCOPYTAB(Sender: TObject);
    procedure SaveDBToCSV(DBGridName : TDBGrid; Filename : string);
    procedure DatasetToClipBoard(const aDataset:TDBGrid);
    procedure ShowDuplicates(DataSource : TDBGrid);
    procedure SortByFileName(DataSource : TDBGrid);
    procedure SortByFilePath(DataSource : TDBGrid);
    procedure SortByHash(DataSource : TDBGrid);
    procedure ShowAll(DataSource : TDBGrid);
    procedure ShowAllCOPYGRID(DataSource : TDBGrid);
    procedure CopyFileNameOfSelectedCell(DBData : TDBGrid);
    procedure CopyFilePathOfSelectedCell(DBData : TDBGrid);
    procedure CopyHashOfSelectedCell(DBData : TDBGrid);
    procedure CopySelectedRow(DBData : TDBGrid);
    procedure SortBySourceFilename(DataSource : TDBGrid);
    procedure SortByDestinationFilename(DataSource : TDBGrid);
    procedure SortBySourceHash(DataSource : TDBGrid);
    procedure SortByDestinationHash(DataSource : TDBGrid);
    function GetTableRowCount(TableName : string; DataSource : TDBGrid) : integer; // If there's more than 2 billion entries, then someone is getting their mileage out of Quickhash!!!
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
procedure TfrmSQLiteDBases.CopySelectedRow(DBData : TDBGrid);
var
  FileNameCell, FilePathCell, FileHashCell, AllRowCells : string;
begin
  // Get the data from the filename cell that the user has selected
  FileNameCell := DBData.DataSource.DataSet.Fields[1].Value;
  // Get the data from the filepath cell that the user has selected
  FilePathCell := DBData.DataSource.DataSet.Fields[2].Value;
  // Get the data from the filehash cell that the user has selected
  FileHashCell := DBData.DataSource.DataSet.Fields[3].Value;
  // and just add them all together :-)
  AllRowCells := FileNameCell + ',' + FilePathCell + ',' + FileHashCell;
  Clipboard.AsText := AllRowCells;
end;

// Deletes a DB table from the SQLite DB
procedure TfrmSQLiteDBases.EmptyDBTable(TableName : string; DBGridName : TDBGrid);
var
  DynamicSQLQuery: TSQLQuery;
begin
  DynamicSQLQuery := TSQLQuery.Create(nil);
  try
    try
      DynamicSQLQuery.DataBase := SQLQuery1.Database;
      DynamicSQLQuery.Transaction := SQLQuery1.Transaction;
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
procedure TfrmSQLiteDBases.SaveDBToCSV(DBGridName : TDBGrid; Filename : string);
var
  Exporter : TCSVExporter;
  ExportSettings: TCSVFormatSettings;
begin
  Exporter := TCSVExporter.Create(nil);
  ExportSettings := TCSVFormatSettings.Create(true);
  Exporter.FormatSettings := ExportSettings;
  Exporter.Dataset := DBGridName.DataSource.DataSet;
  Exporter.FileName := FileName;
  if Exporter.Execute > 0 then
    begin
      ShowMessage('CSV saved as ' + Filename);
    end
  else Showmessage('Could not save to CSV file ' + Filename);
end;

// Copies a DBGrid content to a temp text file then reads it into clipboard
procedure TfrmSQLiteDBases.DatasetToClipBoard(const aDataset:TDBGrid);
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
        Exporter.Dataset := aDataset.DataSource.DataSet;
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

// Counts the rows of a given database table
function TfrmSQLiteDBases.GetTableRowCount(TableName : string; DataSource : TDBGrid) : integer;
begin
  result := 0;
  try
    SQLQuery1.SQL.Text := 'SELECT Count(*) FROM ' + TableName;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    SQLQuery1.Open;
    result := SQLQuery1.Fields[0].AsInteger
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// ShowDuplicates lists entries with duplicate hash values from the FILES tab,
// by searching hash column for matches and then displays all rows fully
// for which duplicate hashes were found
procedure TfrmSQLiteDBases.ShowDuplicates(DataSource : TDBGrid);
// Sourced from https://stackoverflow.com/questions/46345862/sql-how-to-return-all-column-fields-for-one-column-containing-duplicates
begin
  try
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                        'FROM TBL_FILES WHERE HashValue IN ' +
                        '(SELECT HashValue FROM TBL_FILES ' +
                        'GROUP BY HashValue HAVING COUNT(*) > 1) ORDER BY hashvalue';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Open;

  // Allow the DBGrid to view the results of our query
  DataSource1.DataSet := SQLQuery1;
  MainForm.RecursiveDisplayGrid1.DataSource := DataSource1;
  MainForm.RecursiveDisplayGrid1.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;


// *** Start of FILES tab related database routines ***

// Write computed values from the FILES tab to the database table TBL_FILES
procedure TfrmSQLiteDBases.WriteFILESValuesToDatabase(Filename, Filepath, HashValue, FileSize : string);
begin
  try
    SQLQuery1.Close;
    // Insert the values into the database. We're using ParamByName which prevents SQL Injection
    // http://wiki.freepascal.org/Working_With_TSQLQuery#Parameters_in_TSQLQuery.SQL
    SQLQuery1.SQL.Text := 'INSERT into TBL_FILES (Filename, FilePath, HashValue, FileSize) values (:Filename,:FilePath,:HashValue,:FileSize)';
    SQLTransaction1.Active := True;
    SQLQuery1.Params.ParamByName('Filename').AsString := Filename;
    SQLQuery1.Params.ParamByName('FilePath').AsString := FilePath;
    SQLQuery1.Params.ParamByName('HashValue').AsString := hashvalue;
    SQLQuery1.Params.ParamByName('FileSize').AsString := FileSize;
    SQLQuery1.ExecSQL;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab to sort entries by filename alphabetically
procedure TfrmSQLiteDBases.SortByFileName(DataSource : TDBGrid);
begin
  try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                          'FROM TBL_FILES ORDER BY FileName';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    SQLQuery1.Open;

    // Allow the DBGrid to view the results of our query
    DataSource1.DataSet := SQLQuery1;
    MainForm.RecursiveDisplayGrid1.DataSource := DataSource1;
    MainForm.RecursiveDisplayGrid1.AutoFillColumns := true;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;


// Used by FILES tab for sorting entries by file path alphabetically
procedure TfrmSQLiteDBases.SortByFilePath(DataSource : TDBGrid);
begin
 try
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                        'FROM TBL_FILES ORDER BY FilePath';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Open;

  // Allow the DBGrid to view the results of our query
  DataSource1.DataSet := SQLQuery1;
  MainForm.RecursiveDisplayGrid1.DataSource := DataSource1;
  MainForm.RecursiveDisplayGrid1.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;


// Used by the FILES tab display grid to sort by hash
procedure TfrmSQLiteDBases.SortByHash(DataSource : TDBGrid);
begin
 try
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize ' +
                        'FROM TBL_FILES ORDER BY HashValue';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Open;

  // Allow the DBGrid to view the results of our query
  DataSource1.DataSet := SQLQuery1;
  MainForm.RecursiveDisplayGrid1.DataSource := DataSource1;
  MainForm.RecursiveDisplayGrid1.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to list all again
procedure TfrmSQLiteDBases.ShowAll(DataSource : TDBGrid);
begin
  try
  SQLQuery1.Close;
  SQLQuery1.SQL.Text := 'SELECT * FROM TBL_FILES';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery1.Open;

  // Allow the DBGrid to view the results of our query
  DataSource1.DataSet := SQLQuery1;
  MainForm.RecursiveDisplayGrid1.DataSource := DataSource1;
  MainForm.RecursiveDisplayGrid1.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to copy the content of Column 1 (filename) to clipboard
procedure TfrmSQLiteDBases.CopyFilenameOfSelectedCell(DBData : TDBGrid);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  if not (DBData.DataSource.DataSet.Fields[1].Value = NULL) then
  begin
    CellOfInterest := DBData.DataSource.DataSet.Fields[1].Value;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// // Used by the FILES tab display grid to copy the content of Column 2 (file path) to clipboard
procedure TfrmSQLiteDBases.CopyFilePathOfSelectedCell(DBData : TDBGrid);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  if not (DBData.DataSource.DataSet.Fields[2].Value = NULL) then
  begin
    CellOfInterest := DBData.DataSource.DataSet.Fields[2].Value;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// // Used by the FILES tab display grid to copy the content of Column 3 (Hash Value) to clipboard
procedure TfrmSQLiteDBases.CopyHashOfSelectedCell(DBData : TDBGrid);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  if not (DBData.DataSource.DataSet.Fields[3].Value = NULL) then
  begin
    CellOfInterest := DBData.DataSource.DataSet.Fields[3].Value;
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
    SQLQuery2.Close;
    SQLQuery2.SQL.Text := 'INSERT into TBL_COPY (SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes) values (:SourceFilename,:SourceHash,:DestinationFilename,:DestinationHash,:DateAttributes)';
    SQLTransaction1.Active := True;
    SQLQuery2.Params.ParamByName('SourceFilename').AsString := Col1;
    SQLQuery2.Params.ParamByName('SourceHash').AsString := Col2;
    SQLQuery2.Params.ParamByName('DestinationFilename').AsString := Col3;
    SQLQuery2.Params.ParamByName('DestinationHash').AsString := Col4;
    SQLQuery2.Params.ParamByName('DateAttributes').AsString := Col5;
    SQLQuery2.ExecSQL;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY tab display grid, to sort by source filename...Col 1
procedure TfrmSQLiteDBases.SortBySourceFilename(DataSource : TDBGrid);
begin
  try
    SQLQuery2.Close;
    SQLQuery2.SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY SourceFilename';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    SQLQuery2.Open;

    // Allow the DBGrid to view the results of our query
    DataSource2.DataSet := SQLQuery2;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.DataSource := DataSource2;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.AutoFillColumns := true;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COPY tab display grid to sort by destination filename...Col 3
procedure TfrmSQLiteDBases.SortByDestinationFilename(DataSource : TDBGrid);
begin
  try
    SQLQuery2.Close;
    SQLQuery2.SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY DestinationFilename';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    SQLQuery2.Open;

    // Allow the DBGrid to view the results of our query
    DataSource2.DataSet := SQLQuery2;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.DataSource := DataSource2;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.AutoFillColumns := true;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COPY tab display grid to sort by source hash, Col 2
procedure TfrmSQLiteDBases.SortBySourceHash(DataSource : TDBGrid);
begin
 try
  SQLQuery2.Close;
  SQLQuery2.SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY SourceHash';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery2.Open;

  // Allow the DBGrid to view the results of our query
  DataSource2.DataSet := SQLQuery2;
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.DataSource := DataSource2;
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY tab display grid to sort by destination hash...Col 4
procedure TfrmSQLiteDBases.SortByDestinationHash(DataSource : TDBGrid);
begin
 try
  SQLQuery2.Close;
  SQLQuery2.SQL.Text := 'SELECT Id, SourceFilename, SourceHash, DestinationFilename, DestinationHash, DateAttributes ' +
                          'FROM TBL_COPY ORDER BY DestinationHash';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery2.Open;

  // Allow the DBGrid to view the results of our query
  DataSource2.DataSet := SQLQuery2;
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.DataSource := DataSource2;
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY grid to show all items
procedure TfrmSQLiteDBases.ShowAllCOPYGRID(DataSource : TDBGrid);
begin
  try
  SQLQuery2.Close;
  SQLQuery2.SQL.Text := 'SELECT * FROM TBL_COPY';
  SQLite3Connection1.Connected := True;
  SQLTransaction1.Active := True;
  SQLQuery2.Open;

  // Allow the DBGrid to view the results of our query
  DataSource2.DataSet := SQLQuery2;
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.DataSource := DataSource2;
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.AutoFillColumns := true;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// There is an UpdateGridXXX routine for each tab where a DBGrid is used.
// Each one is to populate the grid with the data from the query.

// FILES tab update grid routine
procedure TfrmSQLiteDBases.UpdateGridFILES(Sender: TObject);
  begin
    try
    SQLQuery1.Close;
    SQLQuery1.SQL.Text := 'SELECT * FROM TBL_FILES';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    SQLQuery1.Open;

    // Allow the DBGrid to view the results of our query
    DataSource1.DataSet := SQLQuery1;
    MainForm.RecursiveDisplayGrid1.DataSource := DataSource1;
    MainForm.RecursiveDisplayGrid1.AutoFillColumns := true;
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
    SQLQuery2.Close;
    SQLQuery2.SQL.Text := 'SELECT * FROM TBL_COPY';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    SQLQuery2.Open;

    // Allow the DBGrid to view the results of our query
    DataSource2.DataSet := SQLQuery2;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.DataSource := DataSource2;
    frmDisplayGrid1.RecursiveDisplayGrid_COPY.AutoFillColumns := true;
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
      SQLQuery1.SQL.Clear;
      SQLQuery1.SQL.Text := 'Select Count(*) from TBL_FILES';
      SQLQuery1.Open;

      // Allow the DBGrid to view the results of our query
      DataSource1.DataSet := SQLQuery1;
      DBGrid1.DataSource := DataSource1;
      DBGrid1.AutoFillColumns := true;

    except
      ShowMessage('Unable to query the database');
    end;
  end;
}

initialization

end.

