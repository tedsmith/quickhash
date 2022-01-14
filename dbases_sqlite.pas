unit dbases_sqlite; // New to v3.0.0 of QuickHash

// Rule of thumb - For INSERT, UPDATE, DELETE, use SQLQuery.ExecSQL, for SELECT use SQLQuery.Open:

{$mode objfpc}{$H+} // {$H+} ensures all strings are of unlimited size, and set as ansistring

interface

uses
{$ifdef Linux}
  dl,
{$endif}
{$ifdef Darwin}
  dl,
{$endif}
  Classes, SysUtils, db, sqldb, sqldblib, fpcsvexport, sqlite3conn, FileUtil,
  LResources, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, DBGrids,
  sqlite3dyn, clipbrd, DbCtrls, LazUTF8, LazUTF8Classes;

type

  { TfrmSQLiteDBases }

  TfrmSQLiteDBases = class(TForm)
    CSVExporter1               : TCSVExporter; // We use this for users who want to clipboard the results. Works fine if not too many values.
    DataSource1                : TDataSource;
    DataSource2                : TDataSource;
    DataSource3                : TDataSource;
    SQLDBLibraryLoaderLinux    : TSQLDBLibraryLoader;
    SQLDBLibraryLoaderOSX      : TSQLDBLibraryLoader;
    SQLDBLibraryLoaderWindows  : TSQLDBLibraryLoader;
    SQLite3Connection1         : TSQLite3Connection;
    sqlFILES                   : TSQLQuery;
    sqlCOPY                    : TSQLQuery;
    sqlCOMPARETWOFOLDERS       : TSQLQuery;
    SQLTransaction1            : TSQLTransaction;
    lblConnectionStatus        : TLabel;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure CreateDatabase(DBaseName : string);
    procedure WriteFILESValuesToDatabase(Filename, Filepath, HashValue, FileSize : string; KnownHash : boolean);
    procedure WriteCOPYValuesToDatabase(Col1, Col2, Col3, Col4, Col5 : string);
    procedure PrepareData_COMPARE_TWO_FOLDERS;
    procedure Write_COMPARE_TWO_FOLDERS(FilePath, FilePathName, FileHash : string);
    procedure EmptyDBTable(TableName : string; DBGrid : TDBGrid);
    procedure EmptyDBTableCOPY(TableName : string; DBGrid : TDBGrid);
    procedure EmptyDBTableC2F(TableName : string; DBGrid : TDBGrid);
    procedure UpdateGridFILES(Sender: TObject);
    procedure UpdateGridCOPYTAB(Sender: TObject);
    procedure UpdateGridCOMPARETWOFOLDERSTAB(Sender: TObject);
    procedure SaveFILESDBToCSV(DBGrid : TDBGrid; Filename : string);
    procedure SaveCopyDBToCSV(DBGrid : TDBGrid; Filename : string);
    procedure SaveC2FDBToCSV(DBGrid : TDBGrid; Filename : string);
    procedure SaveFILESTabToHTML(DBGrid : TDBGrid; Filename : string);
    procedure SaveCOPYWindowToHTML(DBGrid : TDBGrid; Filename : string);
    procedure SaveC2FWindowToHTML(DBGrid : TDBGrid; Filename : string);
    procedure DatasetToClipBoard(DBGrid : TDBGrid);
    procedure DatasetToClipBoardFILES(DBGrid : TDBGrid);
    procedure DatasetToClipBoardCOPYTAB(DBGrid : TDBGrid);
    procedure ShowDuplicates(DBGrid : TDBGrid);
    procedure DeleteDuplicates(DBGrid : TDBGrid);
    procedure SortByID(DBGrid : TDBGrid);
    procedure SortByFileName(DBGrid : TDBGrid);
    procedure SortByFilePath(DBGrid : TDBGrid);
    procedure SortByHash(DBGrid : TDBGrid);
    procedure SortByHashList(DBGrid : TDBGrid);
    procedure FilterOutHashListNO(DBGrid : TDBGrid);
    procedure FilterOutHashListYES(DBGrid : TDBGrid);
    procedure ShowAll(DBGrid : TDBGrid);
    procedure ShowAllCOPYGRID(DBGrid : TDBGrid);
    procedure ShowAllC2FGRID(DBGrid : TDBGrid);
    procedure CopyFileNameOfSelectedCell(DBGrid : TDBGrid);
    procedure CopyFilePathOfSelectedCell(DBGrid : TDBGrid);
    procedure CopyHashOfSelectedCell(DBGrid : TDBGrid);
    procedure CopyAllHashesFILESTAB(DBGrid : TDBGrid; UseFileFlag : Boolean);
    procedure CopySelectedRowFILESTAB(DBGrid : TDBGrid);
    procedure CopySelectedRowCOPYTAB(DBGrid : TDBGrid);
    procedure CopySelectedRowC2FTAB(DBGrid : TDBGrid);
    procedure CopySelectedRowsC2FTAB(DBGrid : TDBGrid);
    procedure SortBySourceFilename(DBGrid : TDBGrid);
    procedure SortByDestinationFilename(DBGrid : TDBGrid);
    procedure SortBySourceHash(DBGrid : TDBGrid);
    procedure SortByDestinationHash(DBGrid : TDBGrid);
    procedure ShowMismatchesC2F(DBGrid : TDBGrid);
    procedure ShowDuplicatesC2FTAB(DBGrid : TDBGrid);
    procedure Copy_C2F_DuplicatesList(DBGrid : TDBGrid);
    procedure ShowDiffHashes(DBGrid : TDBGrid);
    procedure ShowMatchingHashes(DBGrid : TDBGrid);
    procedure ShowMissingFilesFolderA(DBGrid : TDBGrid);
    procedure ShowMissingFilesFolderB(DBGrid : TDBGrid);
    procedure ShowMissingFromFolderAAndFolderB(DBGrid : TDBGrid);
    function DBVersionLookup() : string;
    function CountGridRows(AGrid: TDBGrid; ATableName: string): Integer;

  private
    { private declarations }
  public
    DBName           : string; // New to v3.3.0. Used by PreserveDB button to enable the user to save the DB.
    ChosenDelimiter  : string; // New to v3.3.0 to enable use of preferred delim char
    FFilePathA       : String; // new to v3.3.0 for Compare Two Folders tab
    FFilePathB       : String; // new to v3.3.0 for Compare Two Folders tab
    FC2Fquery        : Boolean;// New to v3.3.0 to enable and control results of "Show Duplicates" right click option
    { public declarations }
  const
    // More information on the use of these values is below.
    // They need not be set as constants and can be any valid value
    application_id = 1189021115; // must be a 32-bit Unsigned Integer (Longword 0 .. 4294967295)  https://www.sqlite.org/pragma.html#pragma_application_id
    user_version   = 23400001;   // must be a 32-bit Signed Integer (LongInt -2147483648 .. 2147483647)  https://www.sqlite.org/pragma.html#pragma_user_version
  end;

var
  frmSQLiteDBases: TfrmSQLiteDBases;

implementation

{$R *.lfm}

{ TfrmSQLiteDBases }

  uses
    Unit2, uDisplayGrid, udisplaygrid3;

// On creation we check for SQLite capability and load as we find it.
// If it cant be found, QH will run with some tabs, but not those that need SQLIte backend
procedure TfrmSQLiteDBases.FormCreate(Sender: TObject);
const
  {$ifdef CPU32}
  LIB_FOLDER : ansistring = 'libs\x86';
  {$else ifdef CPU64}
  LIB_FOLDER : ansistring = 'libs\x64';
  {$endif}
//  LIB_FOLDER : ansistring = 'libs';
var
  guid                  : TGuid;
  SQLiteLibraryPath     : string = Default(string);
  strFileNameRandomiser : string = Default(string);
  SafePlaceForDB        : string = Default(string);
  {$ifdef Linux}
    LibHandle : Pointer;
    Pdlinfo : Pdl_info;
    PtrSQLiteLibraryPath : PChar;
  {$endif}
  {$ifdef darwin}
     LibHandle : THandle = Default(THandle);
  {$endif}
begin
  // Set the delimiter to comma by default.
  ChosenDelimiter := ',';

  // Initiate calls to SQLite libraries for WINDOWS
  {$ifdef windows}
    SQLDBLibraryLoaderWindows.ConnectionType := 'SQLite3';
  // Load the right DLL for the architecture of Windows in use
  {$ifdef CPU32}
    SQLiteLibraryPath := ExtractFilePath(Application.ExeName)+IncludeTrailingPathDelimiter(LIB_FOLDER)+'sqlite3-win32.dll';
  {$else ifdef CPU64}
    SQLiteLibraryPath := ExtractFilePath(Application.ExeName)+IncludeTrailingPathDelimiter(LIB_FOLDER)+'sqlite3-win64.dll';
  {$endif}

  // Check the DLL exists and load it
  if FileExists(SQLiteLibraryPath) then
  begin
  SQLDBLibraryLoaderWindows.LibraryName := SQLiteLibraryPath;
  SQLDBLibraryLoaderWindows.Enabled := true;
  SQLDBLibraryLoaderWindows.LoadLibrary;   // We dont need to use LoadLibraryEx here as SQLDBLibraryLoader seems to load the supplied SQlite.dll as intended

  if CreateGUID(guid) = 0 then
  begin
   strFileNameRandomiser := GUIDToString(guid);
  end
  else
   begin
     strFileNameRandomiser := FormatDateTime('YYYY-MM-DD_HH-MM-SS.ZZZ', Now);
   end;
  // write the SQLite database file to system temp
  SafePlaceForDB := GetTempDir;
  if ForceDirectories(SafePlaceForDB) then
  begin
   SQLite3Connection1.DatabaseName := SafePlaceForDB + 'QuickHashDB_' + strFileNameRandomiser + '.sqlite';
   // Create the database
   CreateDatabase(SQLite3Connection1.DatabaseName);
   if SQLIte3Connection1.Connected then
   begin
     lblConnectionStatus.Caption:= 'SQLite3 Database connection active';
     DBName := SQLite3Connection1.DatabaseName;  // We call DBName from Unit2, that is why it is declared here
   end;
  end
  else
   begin
     Showmessage('Could not create folder ' + SafePlaceForDB + ' for ' + SQLite3Connection1.DatabaseName);
   end;
  end
  else
  begin
    ShowMessage('Cannot create SQLite database. Probably SQLite libraries are not on your system.');
    MainForm.TabSheet3.Enabled := false; // disable FileS tab, because it needs SQLite
    MainForm.TabSheet4.Enabled := false; // disable Copy tab, because it needs SQLite
  end;
  {$endif} // End of Windows compiler directive

  // Initiate calls to standard built in SQLite libraries for LINUX
  {$ifdef linux}
  SQLDBLibraryLoaderLinux.ConnectionType := 'SQLite3';
  SQLiteLibraryPath := '';
  LibHandle := dlopen('libsqlite3.so.0', RTLD_LAZY);
  if LibHandle <> nil then
  begin
    Pdlinfo := LibHandle;
    PtrSQLiteLibraryPath := Pdlinfo^.dli_fbase;
    SQLiteLibraryPath := String(PtrSQLiteLibraryPath);
    PtrSQLiteLibraryPath := nil;
    dlclose(LibHandle);
  end;

  if FileExists(SQLiteLibraryPath) then
  begin
    SQLDBLibraryLoaderLinux.LibraryName := SQLiteLibraryPath;
    SQLDBLibraryLoaderLinux.Enabled := true;
    SQLDBLibraryLoaderLinux.LoadLibrary;
    if CreateGUID(guid) = 0 then
    begin
     strFileNameRandomiser := GUIDToString(guid);
    end
    else
     begin
       strFileNameRandomiser := FormatDateTime('YYYY-MM-DD_HH-MM-SS.ZZZ', Now);
     end;
    // write the SQLite database file to system temp
    SafePlaceForDB := GetTempDir;
    if ForceDirectories(SafePlaceForDB) then
    begin
     SQLite3Connection1.DatabaseName := SafePlaceForDB + 'QuickHashDB_' + strFileNameRandomiser + '.sqlite';
     // Create the database
     CreateDatabase(SQLite3Connection1.DatabaseName);
     if SQLIte3Connection1.Connected then
     begin
       lblConnectionStatus.Caption:= 'SQLite3 Database connection active';
       DBName := SQLite3Connection1.DatabaseName;  // We call DBName from Unit2, that is why it is declared here
     end;
    end
    else
     begin
       Showmessage('Could not create folder ' + SafePlaceForDB + ' for ' + SQLite3Connection1.DatabaseName);
     end;
  end
  else
  begin
    ShowMessage('Cannot create SQLite database. Probably SQLite libraries are not on your system.');
    MainForm.TabSheet3.Enabled := false; // disable FileS tab, because it needs SQLite
    MainForm.TabSheet4.Enabled := false; // disable Copy tab, because it needs SQLite
  end;
  {$endif}   // End of Linux compiler directive

  // Initiate calls to standard built in SQLite libraries for APPLE OSX
  {$ifdef darwin}
  // Thanks to OSX being a total and utter pain, moving goal posts with every release of OSX,
  // and since BigSur has removed libraries, more Skullduggery is required for
  // that platform. Thanks Apple, from me.
  SQLDBLibraryLoaderOSX.ConnectionType := 'SQLite3';
  SQLiteLibraryPath := '';

  // First check the SQLite lib can be loaded by calling the new dynamic cache of Big Sur
  LibHandle := loadLibrary(PChar('libsqlite3.dylib'));

  // check whether loading was possible and successful but then just unload it
  // to allow the TSQLDBLibraryLoader to load it, later
  if LibHandle <> 0 then
  begin
    // Nothing is needed here anymore
  end
  else ShowMessage('Cannot load SQLite libraries for backend use.' + SysErrorMessage(GetLastOSError));

  // unload library and pass control to TSQLDBLibraryLoader
  if LibHandle <> NilHandle then
  begin
    unloadLibrary(LibHandle);
    SQLDBLibraryLoaderOSX.LibraryName := 'libsqlite3.dylib';
    SQLDBLibraryLoaderOSX.Enabled := true;
    SQLDBLibraryLoaderOSX.LoadLibrary;

    // Generate a unique name for the DB
    if CreateGUID(guid) = 0 then
    begin
      strFileNameRandomiser := GUIDToString(guid);
    end
    else
      begin
        strFileNameRandomiser := FormatDateTime('YYYY-MM-DD_HH-MM-SS.ZZZ', Now);
      end;

    // write the SQLite database file to system temp
    SafePlaceForDB := GetTempDir;
    if ForceDirectories(SafePlaceForDB) then
    begin
      SQLite3Connection1.DatabaseName := SafePlaceForDB + 'QuickHashDB_' + strFileNameRandomiser + '.sqlite';
      // Create the database
      CreateDatabase(SQLite3Connection1.DatabaseName);
      if SQLIte3Connection1.Connected then
      begin
        lblConnectionStatus.Caption:= 'SQLite3 Database connection active';
        DBName := SQLite3Connection1.DatabaseName;  // We call DBName from Unit2, that is why it is declared here
      end;
    end
  end;

  LibHandle := NilHandle;

  // Method used prior to v3.3.0, for info
  { SQLDBLibraryLoaderOSX.ConnectionType := 'SQLite3';
  SQLiteLibraryPath := '';
  LibHandle := dlopen('libsqlite3.dylib', RTLD_LAZY);
  if LibHandle <> nil then
  begin
    Pdlinfo := LibHandle;
    PtrSQLiteLibraryPath := Pdlinfo^.dli_fbase;
    SQLiteLibraryPath := String(PtrSQLiteLibraryPath);
    PtrSQLiteLibraryPath := nil;
    dlclose(LibHandle);
  end;}
  {$endif} // End of Apple OSX compiler directive
end;

// Create a fresh SQLite database for each instance of the program
procedure TfrmSQLiteDBases.CreateDatabase(DBaseName : string);
begin
  SQLite3Connection1.Close; // Ensure the connection is closed when we start
  try
    // Since we're making this database for the first time,
    // check whether the file already exists and remove it if it does
    if FileExists(SQLite3Connection1.DatabaseName) then
    begin
      DeleteFile(SQLite3Connection1.DatabaseName);
    end;
    // Make a new, clean, database and add the tables
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

      // Here we're setting up a table named "TBL_FILES" in the new database for FileS tab
      // Note AUTOINCREMENT is NOT used! If it is, it causes problems with RowIDs etc after multiple selections
      // Besides, SQLite advice is not to use it unless entirely necessary (http://sqlite.org/autoinc.html)
      // VARCHAR is set as 32767 to ensure max length of NFTS based filename and paths can be utilised
      SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_FILES"('           +
                                       ' "No" Integer NOT NULL PRIMARY KEY,' +
                                       ' "FileName" VARCHAR(4096) NOT NULL,' +
                                       ' "FilePath" VARCHAR(4096) NOT NULL,' +
                                       ' "HashValue" VARCHAR NOT NULL,'      +
                                       ' "FileSize" VARCHAR NULL,'           +
                                       ' "KnownHashFlag" VARCHAR NULL);');
      // Creating an index based upon id in the TBL_FILES Table
      SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "FILES_id_idx" ON "TBL_FILES"( "No" );');

      // Here we're setting up a table named "TBL_COPY" in the new database for Copy tab
      // VARCHAR is set as 32767 to ensure max length of NFTS based filename and paths can be utilised
      SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_COPY"('                      +
                                       ' "No" Integer NOT NULL PRIMARY KEY,'           +
                                       ' "SourceFilename" VARCHAR(4096) NOT NULL,'     +
                                       ' "SourceHash" VARCHAR NULL,'                   +
                                       ' "DestinationFilename" VARCHAR(4096) NOT NULL,'+
                                       ' "DestinationHash" VARCHAR NULL,'              +
                                       ' "DateAttributes" VARCHAR NULL);');
      // Creating an index based upon id in the TBL_COPY Table
      SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "COPIED_FILES_id_idx" ON "TBL_COPY"( "No" );');

      // New to v3.2.0 to enable a display grid for the comparison of two folders
      // Here we're setting up a table named "TBL_COMPARE_TWO_FOLDERS" in the new database for Comapre Two Folders tab
      // VARCHAR is set as 4096 to ensure max length of NFTS based filename and paths can be utilised
      SQLite3Connection1.ExecuteDirect('CREATE TABLE "TBL_COMPARE_TWO_FOLDERS"('+
                                       ' "No" Integer NOT NULL PRIMARY KEY,'+
                                       ' "FilePath" VARCHAR(4096) NULL,'    +
                                       ' "FileName" VARCHAR(4096) NULL,'    +
                                       ' "FileHash" VARCHAR NULL);');

      // Creating an index based upon id in the TBL_COMPARE_TWO_FOLDERS Table
      // Pre v3.3.0 was : SQLite3Connection1.ExecuteDirect('CREATE UNIQUE INDEX "COMPARE_TWO_FOLDERS_id_idx" ON "TBL_COMPARE_TWO_FOLDERS"( "No" );'); //DS (original) - no need because it is primary key
      // The following is new to v3.3.0 following extensive improvement with community help:
      SQLite3Connection1.ExecuteDirect('CREATE INDEX COMPARE_TWO_FOLDERS_path_name_hash_idx '+
                                       '  ON TBL_COMPARE_TWO_FOLDERS (FilePath, FileName, FileHash)');
      SQLite3Connection1.ExecuteDirect('CREATE INDEX COMPARE_TWO_FOLDERS_path_hash_name_idx '+
                                       '  ON TBL_COMPARE_TWO_FOLDERS (FilePath, FileHash, FileName)');

      SQLite3Connection1.ExecuteDirect('CREATE TABLE TBL_COMPARE_TWO_FOLDERS_MATCH ( '+
                                       'No Integer NOT NULL PRIMARY KEY, '+
                                       'FileName VARCHAR NULL, '+
                                       'FilePathA VARCHAR NULL, '+
                                       'FileHashA VARCHAR NULL, '+
                                       'FilePathB VARCHAR NULL, '+
                                       'FileHashB VARCHAR NULL) ');

      SQLite3Connection1.ExecuteDirect('CREATE TABLE TBL_COMPARE_TWO_FOLDERS_DUPLICATES ( '+
                                       'No Integer NOT NULL PRIMARY KEY, '+
                                       'FilePath VARCHAR NULL, '+
                                       'FileName VARCHAR NULL, '+
                                       'FileHash VARCHAR NULL) ');
      // Now write to the new database
      SQLTransaction1.CommitRetaining;
    except
      ShowMessage('SQLite detected but unable to create a new SQLite Database');
    end;
  except
    ShowMessage('SQLite detected but could not check if a database file exists');
  end;
end;

// Shows the user what version of SQLite is in use. Useful for bug reporting etc
function TfrmSQLiteDBases.DBVersionLookup() : string;
 var
  DynamicSQLQuery: TSQLQuery;
begin
  result := '';
  DynamicSQLQuery := TSQLQuery.Create(nil);
  try
    try
      DynamicSQLQuery.DataBase := SQLTransaction1.DataBase;
      DynamicSQLQuery.SQL.Add('SELECT sqlite_version()');
      DynamicSQLQuery.Open;
      result := ('SQLite version : ' + DynamicSQLQuery.FieldByName('sqlite_version()').AsString);
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

// Copy selected row to clipboard from FILES grid
procedure TfrmSQLiteDBases.CopySelectedRowFILESTAB(DBGrid : TDBGrid);
var
  FileNameCell : string = Default(string);
  FilePathCell : string = Default(string);
  FileHashCell : string = Default(string);
  AllRowCells  : string = Default(string);
begin
  ChosenDelimiter := MainForm.ChosenDelimiter;
  FileNameCell    := DBGrid.DataSource.DataSet.Fields[1].AsString;
  FilePathCell    := DBGrid.DataSource.DataSet.Fields[2].AsString;
  FileHashCell    := DBGrid.DataSource.DataSet.Fields[3].AsString;
  AllRowCells     := FileNameCell+ChosenDelimiter+FilePathCell+ChosenDelimiter+FileHashCell;
  Clipboard.AsText:= AllRowCells;
end;

// Copy selected row to clipboard from COPY grid
procedure TfrmSQLiteDBases.CopySelectedRowCOPYTAB(DBGrid : TDBGrid);
var
  AllRowCells             : string = Default(string);
  SourceFileNameCell      : string = Default(string);
  SourceHash              : string = Default(string);
  DestinationFilenameCell : string = Default(string);
  DestinationHash         : string = Default(string);
  DateAttr                : string = Default(string);
begin
  ChosenDelimiter         := MainForm.ChosenDelimiter;
  SourceFileNameCell      := DBGrid.DataSource.DataSet.Fields[1].AsString;
  SourceHash              := DBGrid.DataSource.DataSet.Fields[2].AsString;
  DestinationFilenameCell := DBGrid.DataSource.DataSet.Fields[3].AsString;
  DestinationHash         := DBGrid.DataSource.DataSet.Fields[4].AsString;
  DateAttr                := DBGrid.DataSource.DataSet.Fields[5].AsString;
  // and just add them all together :-)
  AllRowCells := SourceFileNameCell+ChosenDelimiter+SourceHash +ChosenDelimiter+DestinationFilenameCell+ChosenDelimiter+DestinationHash+ChosenDelimiter+DateAttr;
  Clipboard.AsText := AllRowCells;
end;

// Copy selected row to clipboard from COMPARE TWO FOLDERS grid
procedure TfrmSQLiteDBases.CopySelectedRowC2FTAB(DBGrid : TDBGrid);
var
  AllRowCells : string = Default(string);
  strID       : string = Default(string);
  FileNameCell: string = Default(string);
  FolderPathA : string = Default(string);
  FileHashA   : string = Default(string);
  FolderPathB : string = Default(string);
  FileHashB   : string = Default(string);
  // For filtered view
  FolderPath  : string = Default(string);
  FileName    : string = Default(string);
  FileHash    : string = Default(string);
begin
  ChosenDelimiter := MainForm.ChosenDelimiter;

  // If user has enabled "Show Duplicates" filter, copy the adjusted layout
  If FC2Fquery = false  then
  begin
  strID          := DBGrid.DataSource.DataSet.Fields[0].AsString;
  FolderPath     := DBGrid.DataSource.DataSet.Fields[1].AsString;
  FileName       := DBGrid.DataSource.DataSet.Fields[2].AsString;
  FileHash       := DBGrid.DataSource.DataSet.Fields[3].AsString;

  AllRowCells := strID       +ChosenDelimiter+
                 FolderPath  +ChosenDelimiter+
                 FileName    +ChosenDelimiter+
                 FileHash    +ChosenDelimiter;

  Clipboard.AsText := AllRowCells;
  end
  else   // If user has not enabled "Show Duplicates" filter, copy the standard layout
  begin
  strID           := DBGrid.DataSource.DataSet.Fields[0].AsString;
  FileNameCell    := DBGrid.DataSource.DataSet.Fields[1].AsString;
  FolderPathA     := DBGrid.DataSource.DataSet.Fields[2].AsString;
  FileHashA       := DBGrid.DataSource.DataSet.Fields[3].AsString;
  FolderPathB     := DBGrid.DataSource.DataSet.Fields[4].AsString;
  FileHashB       := DBGrid.DataSource.DataSet.Fields[5].AsString;

  AllRowCells     := strID        +ChosenDelimiter+
                     FileNameCell +ChosenDelimiter+
                     FolderPathA  +ChosenDelimiter+
                     FileHashA    +ChosenDelimiter+
                     FolderPathB  +ChosenDelimiter+
                     FileHashB;

  Clipboard.AsText := AllRowCells;
  end;
end;

// Copies multiple selected rows (plural) to clipboard of the "Compare Two Folders" results grid
// For some odd reason it copies it in the reverse order to shown in grid. Not sure why yet.
// New to v3.3.0
procedure TfrmSQLiteDBases.CopySelectedRowsC2FTAB(DBGrid : TDBGrid);
var
  AllRowCells  : string = Default(string);
  strID        : string = Default(string);
  FileNameCell : string = Default(string);
  FolderPathA  : string = Default(string);
  FileHashA    : string = Default(string);
  FolderPathB  : string = Default(string);
  FileHashB    : string = Default(string);
  i : Integer;

  FolderPath   : string = Default(string);
  FileName     : string = Default(string);
  FileHash     : string = Default(string);
begin
  ChosenDelimiter := MainForm.ChosenDelimiter;
  for i := 0 to DBGrid.SelectedRows.Count -1 do
  with DBGrid.DataSource.DataSet do
  begin
  // If user has enabled "Show Duplicates" filter, copy the adjusted layout
  If FC2Fquery = false  then
  begin
    GotoBookmark(Pointer(DBGrid.SelectedRows.Items[i]));
    strID        := DBGrid.DataSource.DataSet.Fields[0].AsString;
    FolderPath   := DBGrid.DataSource.DataSet.Fields[1].AsString;
    FileName     := DBGrid.DataSource.DataSet.Fields[2].AsString;
    FileHash     := DBGrid.DataSource.DataSet.Fields[3].AsString;


    AllRowCells  := AllRowCells + strID +ChosenDelimiter+
                    FolderPath  +ChosenDelimiter+
                    FileName    +ChosenDelimiter+
                    FileHash    +LineEnding;

    Clipboard.AsText := AllRowCells;
  end
  else   // If user has not enabled "Show Duplicates" filter, copy the standard layout
  begin
    GotoBookmark(Pointer(DBGrid.SelectedRows.Items[i]));
    strID           := DBGrid.DataSource.DataSet.Fields[0].AsString;
    FileNameCell    := DBGrid.DataSource.DataSet.Fields[1].AsString;
    FolderPathA     := DBGrid.DataSource.DataSet.Fields[2].AsString;
    FileHashA       := DBGrid.DataSource.DataSet.Fields[3].AsString;
    FolderPathB     := DBGrid.DataSource.DataSet.Fields[4].AsString;
    FileHashB       := DBGrid.DataSource.DataSet.Fields[5].AsString;

    AllRowCells     := AllRowCells  + strID +ChosenDelimiter+
                       FileNameCell +ChosenDelimiter+
                       FolderPathA  +ChosenDelimiter+
                       FileHashA    +ChosenDelimiter+
                       FolderPathB  +ChosenDelimiter+
                       FileHashB    +LineEnding;
  end;
  Clipboard.AsText := AllRowCells;         // Clipboard.AsText:=sCSV;
  end;
end;

// Counts rows of the calling display tab using dedicated SQLQuery instead of
// DBGrid.DataSet.RecordCount, which only gives the count shown on screen.
// Returns positive integer if successfull
function TfrmSQLiteDBases.CountGridRows(AGrid: TDBGrid; ATableName: string): Integer;
begin
  result := -1;
  with TSQLQuery.Create(nil) do
  try
    Database := TSQLQuery(AGrid.DataSource.DataSet).Database;
    SQL.Text := ('SELECT COUNT(*) FROM '+ATableName);
    Open;
    Result := Fields[0].AsInteger;
    Close;
  finally
    Free;
  end;

  {pre v3.3.0 method
  result := -1;
  DBGrid.DataSource.Dataset.DisableControls;
  DBGrid.DataSource.DataSet.First;
  while not DBGrid.DataSource.DataSet.EOF do
  begin
    inc(NoOfRows, 1);
    DBGrid.DataSource.DataSet.Next;
  end;
  DBGrid.DataSource.Dataset.EnableControls;
  // Go to top of grid.
  DBGrid.DataSource.DataSet.First;
  // Return count
  If NoOfRows > -1 then result := NoOfRows;  }
end;

// Saves the grid in FILES tab to HTML. If small volume of records, uses a stringlist.
// If big volume, uses file stream.
procedure TfrmSQLiteDBases.SaveFILESTabToHTML(DBGrid : TDBGrid; Filename : string);
var
  strTitle       : string = Default(string);
  FileIDCell     : string = Default(string);
  FileNameCell   : string = Default(string);
  FilePathCell   : string = Default(string);
  FileHashCell   : string = Default(string);
  FileSizeCell   : string = Default(string);
  KnownHashCell  : string = Default(string);
  NoOfRowsInGrid : integer = Default(integer);
  sl             : TStringList;
  fs             : TFileStreamUTF8;
  ExportSQLQuery : TSQLQuery;
const
  strHTMLHeader      = '<HTML>'  ;
  strTITLEHeader     = '<TITLE>QuickHash HTML Output' ;
  strBODYHeader      = '<BODY>'  ;
  strTABLEHeader     = '<TABLE>' ;
  strTABLEROWStart   = '<TR>'    ;
  strTABLEDATAStart  = '<TD>'    ;
  strTABLEDataEnd    = '</TD>'   ;
  strTABLEROWEnd     = '</TR>'   ;
  strTableRow1       = strTABLEROWStart+'<TD><B>ID</B></TD><TD><B>Filename</B></TD><TD><B>Filepath</B></TD><TD><B>Filehash</B></TD><TD><B>Filesize</B></TD>';
  strTABLEFooter     = '</TABLE>';
  strBODYFooter      = '</BODY>' ;
  strTITLEFooter     = '</TITLE>';
  strHTMLFooter      = '</HTML>' ;

begin
  // If database volume not too big, use memory and stringlists. Otherwise, use file writes
  if DBGrid.Name = 'DBGrid_FILES' then
    begin
      NoOfRowsInGrid := CountGridRows(DBGrid, 'TBL_FILES');// Count the rows first. If not too many, use memory. Otherwise, use filestreams
      if (NoOfRowsInGrid < 20000) and (NoOfRowsInGrid > -1) then
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
        if MainForm.cbLoadHashList.Checked then
        begin
          sl.add(strTableRow1 + '<TD>Known Hash Flag</TD>' + strTABLEROWEnd)
        end else sl.add(strTableRow1 + strTABLEROWEnd);

        try
          DBGrid.DataSource.DataSet.DisableControls;
          DBGrid.DataSource.DataSet.First;
          while not DBGrid.DataSource.DataSet.EOF do
            begin
              sl.add('<tr>');
              // Get the data from the ID cell
              FileIDCell := DBGrid.DataSource.DataSet.Fields[0].AsString;
              sl.add('<td>'+FileIDCell+'</td>');
              // Get the data from the filename cell
              FileNameCell := DBGrid.DataSource.DataSet.Fields[1].AsString;
              sl.add('<td>'+FileNameCell+'</td>');
              // Get the data from the filepath cell
              FilePathCell := DBGrid.DataSource.DataSet.Fields[2].AsString;
              sl.add('<td>'+FilePathCell+'</td>');
              // Get the data from the filehash cell
              FileHashCell := DBGrid.DataSource.DataSet.Fields[3].AsString;
              sl.add('<td>'+FileHashCell+'</td>');
              // Get the data from the filesize cell
              FileSizeCell := DBGrid.DataSource.DataSet.Fields[4].AsString;
              sl.add('<td>'+FileSizeCell+'</td>');
              // Get the data from the Known Hash Cell, if required
              if MainForm.cbLoadHashList.Checked then
              begin
                KnownHashCell := DBGrid.DataSource.DataSet.Fields[5].AsString;
                sl.add('<td>'+KnownHashCell+'</td>');
              end;
              sl.add('</tr>');
              DBGrid.DataSource.DataSet.Next;
            end;
          sl.add('</TABLE>');
          sl.add('</BODY> ');
          sl.add('</HTML> ');
        finally
          DBGrid.DataSource.DataSet.EnableControls;
        end;
      finally
        sl.SaveToFile(Filename);
        sl.free;
        MainForm.StatusBar2.Caption:= ' Data saved to HTML file ' + Filename + '...OK';
        ShowMessage(MainForm.StatusBar2.Caption);
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
          fs.Write(strTableRow1[1], Length(strTableRow1));

          { strTABLEROWStart   = '<TR>'      = 4 bytes
            strTABLEDATAStart  = '<TD>'      = 4 bytes
            strTABLEDataEnd    = '</TD>'     = 5 bytes
            strTABLEROWEnd     = '</TR>'     = 5 bytes
            strTABLEFooter     = '</TABLE>'  = 8 bytes
            strBODYFooter      = '</BODY>'   = 7 bytes
            strTITLEFooter     = '</TITLE>'  = 8 bytes
            strHTMLFooter      = '</HTML>'   = 7 bytes}

          // v3.3.0 highlighted that the exporting of large volumes of data in the
          // DBGrid, even as a filestream, was causing QH to crash dueo the DBGrid controls.
          // So for v3.3.1 onwards, we create temporary dedicated pure SQLQueries to handle this instead.
          try
            ExportSQLQuery := TSQLQuery.Create(nil);
            try
              ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_FILES';  // Get all the data from Files tab table
              ExportSQLQuery.Database := SQLite3Connection1;
              ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
              ExportSQLQuery.Open;
              ExportSQLQuery.First;
              while not ExportSQLQuery.EOF do
              begin
                fs.Write(strTABLEROWStart[1], 4);

                // Get the data from the ID cell
                FileIDCell := ExportSQLQuery.FieldByName('No').AsString;
                // Write filename to new row
                fs.Write(strTABLEDATAStart[1], 4);
                fs.Write(FileIDCell[1], Length(FileIDCell));
                fs.Write(strTABLEDataEnd[1], 5);

                // Get the data from the filename cell
                FileNameCell := ExportSQLQuery.FieldByName('FileName').AsString;
                // Write filename to new row
                fs.Write(strTABLEDATAStart[1], 4);
                fs.Write(FileNameCell[1], Length(FileNameCell));
                fs.Write(strTABLEDataEnd[1], 5);

                // Get the data from the filepath cell
                FilePathCell := ExportSQLQuery.FieldByName('FilePath').AsString;
                // Write filepath to new row
                fs.Write(strTABLEDATAStart[1], 4);
                fs.Write(FilePathCell[1], Length(FilePathCell));
                fs.Write(strTABLEDATAEnd[1], 5);

                // Get the data from the filehash cell
                FileHashCell := ExportSQLQuery.FieldByName('HashValue').AsString;
                // Write hash to new row
                fs.Write(strTABLEDATAStart[1], 4) ;
                fs.Write(FileHashCell[1], Length(Trim(FileHashCell)));
                fs.Write(strTABLEDATAEnd[1], 5);

                // Get the data from the filesize cell
                FileSizeCell := ExportSQLQuery.FieldByName('FileSize').AsString;
                // Write hash to new row
                fs.Write(strTABLEDATAStart[1], 4) ;
                fs.Write(FileSizeCell[1], Length(Trim(FileSizeCell)));
                fs.Write(strTABLEDATAEnd[1], 5);

                //Get the data from the KnownHashFlag cell, if it has been selected for use by the user
                if MainForm.cbLoadHashList.Checked then
                begin
                  KnownHashCell := ExportSQLQuery.FieldByName('KnownHashFlag').AsString;
                  // Write hash to new row
                  fs.Write(strTABLEDATAStart[1], 4) ;
                  fs.Write(KnownHashCell[1], Length(Trim(KnownHashCell)));
                  fs.Write(strTABLEDATAEnd[1], 5);
                end;

                // End the row
                fs.Write(strTABLEROWEnd[1], 5);
                fs.Write(#13#10, 2);
                // Repeat for the next row
                ExportSQLQuery.next;
              end;
            finally
              // Nothing to free here
            end;
            fs.Write(strTABLEFooter, 8);
            fs.Write(#13#10, 2);
            fs.writeansistring(IntToStr(NoOfRowsInGrid) + ' grid entries saved.');
            fs.Write(strBODYFooter, 7);
            fs.Write(#13#10, 2);
            fs.Write(strHTMLFooter, 7);
            fs.Write(#13#10, 2);
          finally
            ExportSQLQuery.free; // Free the temp SQL Query
          end;
      finally
        fs.free;                 // Free the filestream
        MainForm.StatusBar2.Caption:= ' Data saved to HTML file ' + Filename + '...OK';
        Application.ProcessMessages;
        ShowMessage(MainForm.StatusBar2.Caption);
      end;
    end
  else
    if DBGrid.Name = 'frmDisplayGrid1' then
    begin
      // See SaveCOPYWindowToHTML
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

// Deletes a DB table from the COPY DB
procedure TfrmSQLiteDBases.EmptyDBTableCOPY(TableName : string; DBGrid : TDBGrid);
var
  DynamicSQLQuery: TSQLQuery;
begin
  DynamicSQLQuery := TSQLQuery.Create(nil);
  try
    try
      DynamicSQLQuery.DataBase := sqlCOPY.Database;
      DynamicSQLQuery.Transaction := sqlCOPY.Transaction;
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

// Empties table of Compare Two Folders
procedure TfrmSQLiteDBases.EmptyDBTableC2F(TableName : string; DBGrid : TDBGrid);
var
  DynamicSQLQuery: TSQLQuery;
begin
  DynamicSQLQuery := TSQLQuery.Create(nil);
  try
    try
      DynamicSQLQuery.DataBase := sqlCOMPARETWOFOLDERS.Database;
      DynamicSQLQuery.Transaction := sqlCOMPARETWOFOLDERS.Transaction;
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
// using a filestream and dedicated TSQLQuery due to DBGrid poor perfomance for large data.
// Note if the user has filtered the display, such as show all duplicates, the whole
// tabel of data will still be dumped to text for sorting etc in Excel or similar.
procedure TfrmSQLiteDBases.SaveFILESDBToCSV(DBGrid : TDBGrid; Filename : string);
var
  linetowrite   : ansistring = Default(ansistring);
  FileIDCell    : string = Default(string);
  FileNameCell  : string = Default(string);
  FilePathCell  : string = Default(string);
  FileHashCell  : string = Default(string);
  FileSizeCell  : string = Default(string);
  KnownHashCell : string = Default(string);
  CSVFileToWrite: TFilestreamUTF8;
  ExportSQLQuery: TSQLQuery;
  n             : integer = Default(integer);
  KnownHashFlagIsSet : boolean = Default(boolean);
begin
  Mainform.StatusBar2.SimpleText := 'Writing data to file...please wait';
  Application.ProcessMessages;

  ChosenDelimiter := MainForm.ChosenDelimiter;

  try
    // Create a filestream for the output CSV.
    CSVFileToWrite := TFileStreamUTF8.Create(Filename, fmCreate);
    if CSVFileToWrite.Handle > 0 then
      begin
        linetowrite := ('No' + ChosenDelimiter +
                        'Filename'  + ChosenDelimiter +
                        'FilePath'  + ChosenDelimiter +
                        'HashValue' + ChosenDelimiter +
                        'FileSize'  + ChosenDelimiter +
                        'KnownHashFlag' + LineEnding);

        n := Length(LineToWrite);
        CSVFileToWrite.Write(linetowrite[1], n);

        // Write all columns, but dont try to include the Known Hash result if not computed to start with
        // This boolean check should be quicker instead of checking for every row whether the field is empty or not
        if MainForm.cbLoadHashList.checked then KnownHashFlagIsSet := true
          else KnownHashFlagIsSet := false;

        try
          ExportSQLQuery := TSQLQuery.Create(nil);
          try
            ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_FILES';  // Get all the data from Files tab table
            ExportSQLQuery.Database := SQLite3Connection1;
            ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
            ExportSQLQuery.Open;
            ExportSQLQuery.First;
            while not ExportSQLQuery.EOF do
            begin
              // Include all columns, inc hash flag, but exclude the row count (not needed for a CSV output).
              // Get the data from the ID cell
              FileIDCell := ExportSQLQuery.FieldByName('No').AsString;
              // Get the data from the filename cell
              FileNameCell := ExportSQLQuery.FieldByName('FileName').AsString;
              // Get the data from the filepath cell
              FilePathCell := ExportSQLQuery.FieldByName('FilePath').AsString;
              // Get the data from the filehash cell
              FileHashCell := ExportSQLQuery.FieldByName('HashValue').AsString;
              // Get the data from the filesize cell
              FileSizeCell := ExportSQLQuery.FieldByName('FileSize').AsString;

              //Get the data from the KnownHashFlag cell, if it has been selected for use by the user
              if KnownHashFlagIsSet then
              begin
                KnownHashCell := ExportSQLQuery.FieldByName('KnownHashFlag').AsString;
              end;

              linetowrite := (FileIDCell    + ChosenDelimiter +
                              FileNameCell  + ChosenDelimiter +
                              FilePathCell  + ChosenDelimiter +
                              FileHashCell  + ChosenDelimiter +
                              FileSizeCell  + ChosenDelimiter +
                              KnownHashCell + LineEnding);

              // Write row to file
              n := 0;
              n := Length(linetowrite);
              CSVFileToWrite.Write(linetowrite[1], n);

              // Repeat for the next row
              ExportSQLQuery.next;
            end;
          finally
            // Nothing to free here
          end;  // End of ExportSQLQuery.Open;
        finally
          ExportSQLQuery.free; // Free the temp SQL Query
        end; // End of SQLQuery.Create
      end; // End of CSVFileToWrite.Handle check
    finally
      CSVFileToWrite.Free;
      Mainform.StatusBar2.SimpleText := 'DONE';
      ShowMessage('Grid data now in ' + Filename);
    end;
end;

// exports the DBGrid (DBGridName) to a CSV file (filename) from the COPY tab
// using a filestream and dedicated TSQLQuery due to DBGrid poor perfomance for large data.
// Note if the user has filtered the display, such as show all duplicates, the whole
// table of data will still be dumped to text for sorting etc in Excel or similar.
procedure TfrmSQLiteDBases.SaveCopyDBToCSV(DBGrid : TDBGrid; Filename : string);
var
  linetowrite         : ansistring = Default(ansistring);
  FileIDCell          : string = Default(string);
  SourceFilename      : string = Default(string);
  SourceFileHash      : string = Default(string);
  DestinationFileName : string = Default(string);
  DestinationFileHash : string = Default(string);
  DateAttributes      : string = Default(string);
  n                   : integer = Default(integer);
  CSVFileToWrite      : TFilestreamUTF8;
  ExportSQLQuery      : TSQLQuery;

begin
  Mainform.StatusBar2.SimpleText := 'Writing data to file...please wait';
  Application.ProcessMessages;
  ChosenDelimiter := MainForm.ChosenDelimiter;

  try
  // Create a filestream for the output CSV.
  CSVFileToWrite := TFileStreamUTF8.Create(Filename, fmCreate);
  // Add header
  linetowrite := 'No'                       + ChosenDelimiter +
                'Source Filename'          + ChosenDelimiter +
                'Source Hash'              + ChosenDelimiter +
                'Destination Filename'     + ChosenDelimiter +
                'Destination Hash'         + ChosenDelimiter +
                'Original Date Attributes' + LineEnding;

  n := Length(linetowrite);
  CSVFileToWrite.Write(linetowrite[1], n);

    try
    ExportSQLQuery := TSQLQuery.Create(nil);
      try
        ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_COPY';  // Get all the data from Files tab table
        ExportSQLQuery.Database := SQLite3Connection1;
        ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
        ExportSQLQuery.Open;
        ExportSQLQuery.First;
        while not ExportSQLQuery.EOF do
        begin
          FileIDCell          := ExportSQLQuery.FieldByName('No').AsString;
          SourceFilename      := ExportSQLQuery.FieldByName('SourceFilename').AsString;
          SourceFileHash      := ExportSQLQuery.FieldByName('SourceHash').AsString;
          DestinationFileName := ExportSQLQuery.FieldByName('DestinationFileName').AsString;
          DestinationFileHash := ExportSQLQuery.FieldByName('DestinationHash').AsString;
          DateAttributes      := ExportSQLQuery.FieldByName('DateAttributes').AsString;

          linetowrite := (FileIDCell          + ChosenDelimiter +
                          SourceFilename      + ChosenDelimiter +
                          SourceFileHash      + ChosenDelimiter +
                          DestinationFileName + ChosenDelimiter +
                          DestinationFileHash + ChosenDelimiter +
                          DateAttributes      + LineEnding);
          // Write row to file
          n := 0;
          n := Length(linetowrite);
          CSVFileToWrite.Write(linetowrite[1], n);

          // Repeat for the next row
          ExportSQLQuery.next;
        end;
      finally
        // Nothing to free here
      end;  // End of ExportSQLQuery.Open;
    finally
      ExportSQLQuery.free; // Free the temp SQL Query
    end; // End of SQLQuery.Create
  finally
   CSVFileToWrite.Free;
   Mainform.StatusBar2.SimpleText := 'DONE';
   ShowMessage('Grid data now in ' + Filename);
  end;
end;

procedure TfrmSQLiteDBases.SaveC2FDBToCSV(DBGrid : TDBGrid; Filename : string);
var
  linetowrite : ansistring;
  // For when FC2Fquery = false
  strID       : string = Default(string);
  FilePath    : string = Default(string);
  strFileName : string = Default(string);
  FileHash    : string = Default(string);
  // For when FC2Fquery = true
  FilePathA   : string = Default(string);
  FileHashA   : string = Default(string);
  FilePathB   : string = Default(string);
  FileHashB   : string = Default(string);

  n : integer = Default(integer);
  CSVFileToWrite : TFilestreamUTF8;
  ExportSQLQuery : TSQLQuery;
begin
  Mainform.StatusBar2.SimpleText := 'Writing results to file...please wait';
  Application.ProcessMessages;
  linetowrite := '';
  ChosenDelimiter := MainForm.ChosenDelimiter;
  try
    CSVFileToWrite := TFileStreamUTF8.Create(Filename, fmCreate);
    if CSVFileToWrite.Handle > 0 then
    begin
      // Add headers and adjust column headings depending on which filter is active
      If FC2Fquery = false  then
      begin
        linetowrite := 'No'        +ChosenDelimiter+
                       'Filepath'  +ChosenDelimiter+
                       'FileName'  +ChosenDelimiter+
                       'FileHash'  +LineEnding;
        n := Length(linetowrite);
        CSVFileToWrite.Write(linetowrite[1], n);
      end
      else
      begin
        linetowrite := 'No'        +ChosenDelimiter+
                       'Filename'  +ChosenDelimiter+
                       'FilePathA' +ChosenDelimiter+
                       'FileHashA' +ChosenDelimiter+
                       'FilePathB' +ChosenDelimiter+
                       'FileHashB' + LineEnding;
        n := Length(linetowrite);
        CSVFileToWrite.Write(linetowrite[1], n);
      end;

      // Add content of grid  depending on which filter is active
      If FC2Fquery = false  then
      begin
        try
          ExportSQLQuery := TSQLQuery.Create(nil);
          try
            ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_COMPARE_TWO_FOLDERS';  // Get all the data from Compare Two Folders tab table
            ExportSQLQuery.Database := SQLite3Connection1;
            ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
            ExportSQLQuery.Open;
            ExportSQLQuery.First;
            while not ExportSQLQuery.EOF do
            begin
              strID       := ExportSQLQuery.FieldByName('No').AsString;
              Filepath    := ExportSQLQuery.FieldByName('FilePath').AsString;
              strFileName := ExportSQLQuery.FieldByName('FileName').AsString;
              FileHash    := ExportSQLQuery.FieldByName('FileHash').AsString;

              linetowrite := (strID       + ChosenDelimiter +
                              FilePath    + ChosenDelimiter +
                              strFileName + ChosenDelimiter +
                              FileHash    + LineEnding);

              n := 0;
              n := Length(linetowrite);
              CSVFileToWrite.Write(linetowrite[1], n);
              // Repeat for the next row
              ExportSQLQuery.next;
            end;
          finally
            // Nothing to free here
          end;
        finally
          ExportSQLQuery.free; // Free the temp SQL Query
        end;
      end
      else // FC2Fquery is TRUE
      try
      ExportSQLQuery := TSQLQuery.Create(nil);
        try
          ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_COMPARE_TWO_FOLDERS_MATCH';  // Get all the data from Compare Two Folders tab table
          ExportSQLQuery.Database := SQLite3Connection1;
          ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
          ExportSQLQuery.Open;
          ExportSQLQuery.First;
          while not ExportSQLQuery.EOF do
          begin
            strID       := ExportSQLQuery.FieldByName('No').AsString;
            strFileName := ExportSQLQuery.FieldByName('FileName').AsString;
            FilePathA   := ExportSQLQuery.FieldByName('FilePathA').AsString;
            FileHashA   := ExportSQLQuery.FieldByName('FileHashA').AsString;
            FilePathB   := ExportSQLQuery.FieldByName('FilePathB').AsString;
            FileHashB   := ExportSQLQuery.FieldByName('FileHashB').AsString;

            linetowrite := (strID       + ChosenDelimiter +
                            strFileName + ChosenDelimiter +
                            FilePathA   + ChosenDelimiter +
                            FileHashA   + ChosenDelimiter +
                            FilePathB   + ChosenDelimiter +
                            FileHashB   + LineEnding);

            n := 0;
            n := Length(linetowrite);
            CSVFileToWrite.Write(linetowrite[1], n);
            // Repeat for the next row
            ExportSQLQuery.next;
          end;
        finally
          // Nothing to free here
        end;
      finally
        ExportSQLQuery.free; // Free the temp SQL Query
      end;
    end;
  finally
    CSVFileToWrite.Free;
    Mainform.StatusBar2.SimpleText := 'DONE';
    ShowMessage('Grid data now in ' + Filename);
  end;
end;

// Used by "Compare Two Folders" to clipboard the different view when a users right clicks
// and asks to list duplicates. As the column layout is different, this is its own clipboard event
procedure TfrmSQLiteDBases.Copy_C2F_DuplicatesList(DBGrid : TDBGrid);
var
  CSVClipboardList : TStringListUTF8;
  RowCount : integer = Default(integer);
begin
  Mainform.StatusBar2.SimpleText := 'Counting rows and writing to clipboard if possible...please wait';
  Application.ProcessMessages;
  ChosenDelimiter := MainForm.ChosenDelimiter;
    try
      CSVClipboardList := TStringListUTF8.Create;
      // Add the grid headers
      CSVClipboardList.Add('No'      + ChosenDelimiter + 'Filepath'  + ChosenDelimiter +
                           'FileName'+ ChosenDelimiter + 'HashValue');
      DBGrid.DataSource.Dataset.DisableControls;
      try
        // Add the grid content
        DBGrid.DataSource.Dataset.First;
        while not DBGrid.DataSource.Dataset.EoF do
        begin
          CSVClipboardList.Add((DBGrid.DataSource.DataSet.Fields[0].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[1].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[2].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[3].Text) +LineEnding);

          DBGrid.DataSource.Dataset.Next;
        end;
      finally
        DBGrid.DataSource.Dataset.EnableControls;
      end;
    finally
      Clipboard.AsText := CSVClipboardList.Text;
      CSVClipboardList.Free;
    end;
    Mainform.StatusBar2.SimpleText := 'DONE';
    ShowMessage('Grid data now in clipboard ');
end;

// Used to clipboard the entire FILES display grid
procedure TfrmSQLiteDBases.DatasetToClipBoardFILES(DBGrid : TDBGrid);
var
  CSVClipboardList : TStringListUTF8;
  RowCount : integer = Default(integer);
  KnownHashFlagIsSet : boolean = Default(boolean);
begin
  Mainform.StatusBar2.SimpleText := 'Copying data to clipboard...please wait';
  Application.ProcessMessages;

  if MainForm.C2FDelimiterComboBox.text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.C2FDelimiterComboBox.text;              // The C2F tab delimiter
  end
  else if MainForm.FileSDelimiterComboBox.Text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.FileSDelimiterComboBox.Text;            // The FileS tab delimiter
  end
  else if MainForm.CopyDelimiterComboBox.Text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.CopyDelimiterComboBox.Text;             // The Copy tab delimiter
  end
  else if MainForm.TextLBLDelimiterComboBox.Text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.TextLBLDelimiterComboBox.Text;          // The Text tab delimiter
  end;

  if ChosenDelimiter = 'Set Delimiter' then                             // If still default, use comma
  begin
    ChosenDelimiter := ',';
  end
    else  // Tab is non-printable, so requires conversion to #9, and may have already been converted
          // as ChosenDelimiter is global
    if (ChosenDelimiter = 'Tab') or (ChosenDelimiter = #9) then         // If Tab use #9
    begin
      ChosenDelimiter := #9;
    end
      else  // It is tricky to show a space character in the list so I chose to write "Space" so that needs converting to #32
      if (ChosenDelimiter = 'Space') or (ChosenDelimiter = #32) then    // If space use #32
      begin
        ChosenDelimiter := #32;
      end;

  RowCount := CountGridRows(DBGrid, 'TBL_FILES');
  if RowCount < 20000 then
  begin
    try
      CSVClipboardList := TStringListUTF8.Create;
      CSVClipboardList.Add('Filename'  + ChosenDelimiter + 'FilePath' + ChosenDelimiter +
                        'HashValue' + ChosenDelimiter + 'FileSize' + ChosenDelimiter +
                        'KnownHashFlag');

      if MainForm.cbLoadHashList.checked then KnownHashFlagIsSet := true
          else KnownHashFlagIsSet := false;

      DBGrid.DataSource.Dataset.DisableControls;
      try
        DBGrid.DataSource.Dataset.First;
        while not DBGrid.DataSource.Dataset.EoF do
        begin
          if KnownHashFlagIsSet then
          begin
            // Include all columns, inc hash flag, but exclude the row count (not needed for a CSV output).
            CSVClipboardList.Add(DBGrid.DataSource.DataSet.Fields[1].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[2].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[3].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[4].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[5].Text);
          end
          else
            begin
            // Include all columns, exc hash flag, but exclude the row count (not needed for a CSV output).
            CSVClipboardList.Add(DBGrid.DataSource.DataSet.Fields[1].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[2].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[3].Text + ChosenDelimiter+
                           DBGrid.DataSource.DataSet.Fields[4].Text);
            end;
          DBGrid.DataSource.Dataset.Next;
        end;
      finally
        DBGrid.DataSource.Dataset.EnableControls;
      end;
    finally
      Clipboard.AsText := CSVClipboardList.Text;
      CSVClipboardList.Free;
    end;
    Mainform.StatusBar2.SimpleText := 'DONE';
    ShowMessage('Grid data now in clipboard');
  end
  else
  begin
    ShowMessage('Row count exceeds 20K. Please use "Save to CSV file" instead');
    Mainform.StatusBar2.SimpleText := 'Cliboarding effort aborted due to volume of data';
  end;
end;

// Copies the DBGrid content to clipboard
procedure TfrmSQLiteDBases.DatasetToClipBoard(DBGrid : TDBGrid);
var
  CSVClipboardList : TStringListUTF8;
  RowCount : integer = Default(integer);
  tblName : string = Default(string);
begin
  Mainform.StatusBar2.SimpleText := 'Counting rows and writing to clipboard if possible...please wait';
  Application.ProcessMessages;

  // I need to work out a better way to do this but ChosenDelimiter is global
  // and set by the OnSelectionChange event of several drop downs. So at the point
  // a copy to clipboard is triggered, check if any have been changed away from
  // default of "Set Delimiter", which results in a comma being used.
  if MainForm.C2FDelimiterComboBox.text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.C2FDelimiterComboBox.text;              // The C2F tab delimiter
  end
  else if MainForm.FileSDelimiterComboBox.Text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.FileSDelimiterComboBox.Text;            // The FileS tab delimiter
  end
  else if MainForm.CopyDelimiterComboBox.Text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.CopyDelimiterComboBox.Text;             // The Copy tab delimiter
  end
  else if MainForm.TextLBLDelimiterComboBox.Text <> 'Set Delimiter' then
  begin
    ChosenDelimiter := MainForm.TextLBLDelimiterComboBox.Text;          // The Text tab delimiter
  end;

  if ChosenDelimiter = 'Set Delimiter' then                             // If still default, use comma
  begin
    ChosenDelimiter := ',';
  end
    else  // Tab is non-printable, so requires conversion to #9, and may have already been converted
          // as ChosenDelimiter is global
    if (ChosenDelimiter = 'Tab') or (ChosenDelimiter = #9) then         // If Tab use #9
    begin
      ChosenDelimiter := #9;
    end
      else  // It is tricky to show a space character in the list so I chose to write "Space" so that needs converting to #32
      if (ChosenDelimiter = 'Space') or (ChosenDelimiter = #32) then    // If space use #32
      begin
        ChosenDelimiter := #32;
      end;

  if DBGrid.Name = 'DBGrid_FILES'     then tblName := 'TBL_FILES';
  if DBGrid.Name = 'DBGrid_C2F'       then tblName := 'TBL_COMPARE_TWO_FOLDERS';
  if DBGrid.Name = 'DBGrid_COPY'      then tblName := 'TBL_COPY';

  RowCount := CountGridRows(DBGrid, tblName);
  if RowCount < 20000 then
  begin
    try
      CSVClipboardList := TStringListUTF8.Create;
      // Add the grid headers. If "Show Duplicates" was just active, adjust header layout
      If FC2Fquery = false  then
      begin
      CSVClipboardList.Add('No'      + ChosenDelimiter + 'Filepath'  + ChosenDelimiter +
                           'FileName'+ ChosenDelimiter + 'HashValue' + ChosenDelimiter);
      end
      else
      begin
      // Add the grid headers as normal
      CSVClipboardList.Add('No'      + ChosenDelimiter + 'Filename'  + ChosenDelimiter +
                           'FolderAPath'+ ChosenDelimiter + 'FolderAHashValue' + ChosenDelimiter +
                           'FolderBPath'+ ChosenDelimiter + 'FolderBHashValue');
      end;
      DBGrid.DataSource.Dataset.DisableControls;
      try
        // Add the grid content itself, and adjust depending on filter last activated
        DBGrid.DataSource.Dataset.First;
        while not DBGrid.DataSource.Dataset.EoF do
        begin
          If FC2Fquery = false  then
          begin
            CSVClipboardList.Add((DBGrid.DataSource.DataSet.Fields[0].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[1].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[2].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[3].Text) +ChosenDelimiter);
          end
          else
          begin
          CSVClipboardList.Add((DBGrid.DataSource.DataSet.Fields[0].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[1].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[2].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[3].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[4].Text) +ChosenDelimiter+
                         (DBGrid.DataSource.DataSet.Fields[5].Text));
          end;
          DBGrid.DataSource.Dataset.Next;
        end;
      finally
        DBGrid.DataSource.Dataset.EnableControls;
      end;
    finally
      Clipboard.AsText := CSVClipboardList.Text;
      CSVClipboardList.Free;
    end;
    Mainform.StatusBar2.SimpleText := 'DONE';
    ShowMessage('Grid data now in clipboard ');
  end
  else
  begin
    Mainform.StatusBar2.SimpleText := 'Row count exceeded 20K. Use "Save to CSV file" instead';
    Showmessage('Row count exceeds 20K. Please use "Save to CSV file" instead');
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
  MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
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
  FileName    : string = Default(string);
  FilePath    : string = Default(string);
  NameAndPath : string = Default(string);
  FileHash    : string = Default(string);

  i, FileDeletedCount : integer;
  FilesDeletedOK      : boolean = Default(boolean);
  slDuplicates, slDuplicatesDeleted : TStringList;
begin
  FileDeletedCount := 0;
  ChosenDelimiter := MainForm.ChosenDelimiter;
  try
  slDuplicates := TStringList.Create;
  slDuplicates.Sorted := true;

  slDuplicatesDeleted := TStringList.Create;
  slDuplicatesDeleted.Sorted := true;
  DBGrid.DataSource.DataSet.DisableControls;
  DBGrid.DataSource.Dataset.First;
  while not DBGrid.DataSource.DataSet.EOF do
    begin
      for i := 0 to DBGrid.DataSource.DataSet.FieldCount -1 do
      begin
        FileName := DBGrid.DataSource.DataSet.Fields[1].AsString;
        FilePath := DBGrid.DataSource.DataSet.Fields[2].AsString;
        FileHash := DBGrid.DataSource.DataSet.Fields[3].AsString;
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
              slDuplicatesDeleted.Add(NameAndPath+ChosenDelimiter+FileHash + ', was deleted OK');
            end;
            // reset deletion flag
            FilesDeletedOK := false;
          end
          else slDuplicates.add(FileHash);
        // Go to next record
        DBGrid.DataSource.DataSet.Next;
      end;
    end;
    DBGrid.DataSource.DataSet.EnableControls;

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
procedure TfrmSQLiteDBases.WriteFILESValuesToDatabase(Filename, Filepath, HashValue, FileSize : string; KnownHash : boolean);
var
  KnownHashFlag : string = Default(string);
begin
  try
    sqlFILES.Close;
    // Insert the values into the database. We're using ParamByName which prevents SQL Injection
    // http://wiki.freepascal.org/Working_With_TSQLQuery#Parameters_in_TSQLQuery.SQL

    if MainForm.cbLoadHashList.Checked then
      begin
        if KnownHash = false then
          begin
            KnownHashFlag := 'No';
            sqlFILES.SQL.Text := 'INSERT into TBL_FILES (Filename, FilePath, HashValue, FileSize, KnownHashFlag) values (:Filename,:FilePath,:HashValue,:FileSize,:KnownHashFlag)';
          end
        else
        begin
          KnownHashFlag := 'Yes';
          sqlFILES.SQL.Text := 'INSERT into TBL_FILES (Filename, FilePath, HashValue, FileSize, KnownHashFlag) values (:Filename,:FilePath,:HashValue,:FileSize,:KnownHashFlag)';
        end;
      end
    else sqlFILES.SQL.Text := 'INSERT into TBL_FILES (Filename, FilePath, HashValue, FileSize) values (:Filename,:FilePath,:HashValue,:FileSize)';

    SQLTransaction1.Active := True;
    sqlFILES.Params.ParamByName('Filename').AsString := Filename;
    sqlFILES.Params.ParamByName('FilePath').AsString := FilePath;
    sqlFILES.Params.ParamByName('HashValue').AsString := hashvalue;
    sqlFILES.Params.ParamByName('FileSize').AsString := FileSize;
    if MainForm.cbLoadHashList.Checked then
      begin
        sqlFILES.Params.ParamByName('KnownHashFlag').AsString := KnownHashFlag;
      end;
    sqlFILES.ExecSQL;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab to sort entries by ID in order
procedure TfrmSQLiteDBases.SortByID(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                     'FROM TBL_FILES ORDER BY Id';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
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
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                     'FROM TBL_FILES ORDER BY FileName';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
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
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                    'FROM TBL_FILES ORDER BY FilePath';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
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
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                    'FROM TBL_FILES ORDER BY HashValue';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
   DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to sort by the Yes\No values of Known Hash import
procedure TfrmSQLiteDBases.SortByHashList(DBGrid : TDBGrid);
begin
 try
   DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
   TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                    'FROM TBL_FILES ORDER BY KnownHashFlag';
   SQLite3Connection1.Connected := True;
   SQLTransaction1.Active := True;
   MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
   DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to filter out values unknown to imported hash list
// i.e. Filter out all the rows that are No in Known to hash import
procedure TfrmSQLiteDBases.FilterOutHashListNO(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close;
    // This SQL query may not scale to large data sets as it uses the LIKE word
    // But it should be OK for many thousands of rows, but perhas not millions.
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text :=  'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                      'FROM TBL_FILES WHERE KnownHashFlag LIKE ''No''';

    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the FILES tab display grid to filter out values known to imported hash list
// i.e. Filter out all the rows that are Yes in Known to hash import
procedure TfrmSQLiteDBases.FilterOutHashListYES(DBGrid : TDBGrid);
begin
  try
    DBGrid.DataSource.Dataset.Close;
    // This SQL query may not scale to large data sets as it uses the LIKE word
    // But it should be OK for many thousands of rows, but perhas not millions.
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text :=  'SELECT Id, Filename, FilePath, HashValue, FileSize, KnownHashFlag ' +
                                                      'FROM TBL_FILES WHERE KnownHashFlag LIKE ''Yes''';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
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
    MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
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
  CellOfInterest : string = Default(string);
begin
  if not (DBGrid.DataSource.DataSet.Fields[1].AsString = NULL) then
  begin
    CellOfInterest := DBGrid.DataSource.DataSet.Fields[1].AsString;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// // Used by the FILES tab display grid to copy the content of Column 2 (file path) to clipboard
procedure TfrmSQLiteDBases.CopyFilePathOfSelectedCell(DBGrid : TDBGrid);
var
  CellOfInterest : string = Default(string);
begin
  if not (DBGrid.DataSource.DataSet.Fields[2].AsString = NULL) then
  begin
    CellOfInterest := DBGrid.DataSource.DataSet.Fields[2].AsString;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// Used by the FILES tab display grid to copy the content of Column 3 (Hash Value) to clipboard
procedure TfrmSQLiteDBases.CopyHashOfSelectedCell(DBGrid : TDBGrid);
var
  CellOfInterest : string = Default(string);
begin
  if not (DBGrid.DataSource.DataSet.Fields[3].AsString = NULL) then
  begin
    CellOfInterest := DBGrid.DataSource.DataSet.Fields[3].AsString;
    Clipboard.AsText := CellOfInterest;
  end;
end;

// Used by the FILES tab display grid to copy all the hash values of Column 3 to clipboard
// Useful to create hashlists without adding the entire grid content
procedure TfrmSQLiteDBases.CopyAllHashesFILESTAB(DBGrid : TDBGrid; UseFileFlag : Boolean);
var
  slFileHashes        : TStringList;
  tempfile            : TFileStream;
  n                   : integer;
  ChosenHashAlg       : string = Default(string);
  Header              : string = Default(string);
  linetowrite         : string = Default(string);
  FileForCopiedHashes : string = Default(string);
begin
  n := 0;
  case MainForm.AlgorithmChoiceRadioBox3.ItemIndex of
      0: begin
      ChosenHashAlg := 'MD5';
      end;
      1: begin
      ChosenHashAlg := 'SHA-1';
      end;
      2: begin
      ChosenHashAlg := 'SHA-3';
      end;
      3: begin
      ChosenHashAlg := 'SHA256';
      end;
      4: begin
      ChosenHashAlg := 'SHA512';
      end;
      5: begin
      ChosenHashAlg := 'xxHash';
      end;
      6: begin
      ChosenHashAlg := 'Blake2B';
      end;
  end;

  Header := ChosenHashAlg;

  // If hash value count too large for clipboard use, write to a file
  if UseFileFlag then
  begin
    if MainForm.SaveDialog8_SaveJustHashes.Execute then
    begin
      Mainform.StatusBar2.SimpleText := 'Writing hash values to file...please wait';
      Application.ProcessMessages;
      FileForCopiedHashes := MainForm.SaveDialog8_SaveJustHashes.FileName;

      try
        tempfile := TFileStream.Create(FileForCopiedHashes, fmCreate);
        // Give the list a header of the chosen hash algorithm
        linetowrite := Header + LineEnding;
        tempfile.Write(linetowrite[1], Length(linetowrite));
        // Now add all the hash strings
        DBGrid.DataSource.DataSet.DisableControls;
        DBGrid.DataSource.DataSet.First;
        while not DBGrid.DataSource.DataSet.EOF do
        begin
          linetowrite := (DBGrid.DataSource.DataSet.Fields[3].Text) + LineEnding;
          n := Length(linetowrite);
          try
            tempfile.Write(linetowrite[1], n);
          finally
            DBGrid.DataSource.DataSet.Next;
          end;
        end;
        DBGrid.DataSource.DataSet.EnableControls;
      finally
        tempfile.Free;
      end;
      Mainform.StatusBar2.SimpleText := 'DONE';
      ShowMessage('Hash column content now in ' + FileForCopiedHashes);
    end
    else ShowMessage('Unable to create a file to store the hashes. Check write permissions of location');
  end
  else // Hash value count should go into clipboard OK unless the host is shockingly low on memory
  begin
    Mainform.StatusBar2.SimpleText := 'Writing hash values to clipboard...please wait';
    try
      slFileHashes := TStringList.Create;
      slFileHashes.Add(Header); // Give the list a header of the chosen hash algorithm
      DBGrid.DataSource.DataSet.DisableControls;
      DBGrid.DataSource.DataSet.First;
      while not DBGrid.DataSource.DataSet.EOF do
      begin
        slFileHashes.Add(DBGrid.DataSource.DataSet.Fields[3].Text);
        DBGrid.DataSource.DataSet.Next;
      end;
      DBGrid.DataSource.DataSet.EnableControls;
      Clipboard.AsText := slFileHashes.Text;
    finally
      slFileHashes.Free;
      Mainform.StatusBar2.SimpleText := 'DONE. Hash column content now in clipboard.';
      ShowMessage('Hash column content now in clipboard.');
    end;
  end;
end;

// *** Start of COMPARE TWO FOLDERS tab related functions ***

// Used by the COMPARE TWO FOLDERS grid to insert data into the TBL_COMPARE_TWO_FOLDERS table
procedure TfrmSQLiteDBases.Write_COMPARE_TWO_FOLDERS(FilePath, FilePathName, FileHash : string);
begin
  try
    sqlCOMPARETWOFOLDERS.Close;
    sqlCOMPARETWOFOLDERS.SQL.Text := 'INSERT INTO TBL_COMPARE_TWO_FOLDERS (FilePath, FileName, FileHash) VALUES (:FilePath, :FileName, :FileHash)';
    SQLTransaction1.Active := True;
    sqlCOMPARETWOFOLDERS.Params.ParamByName('FilePath').AsString := FilePath;
    sqlCOMPARETWOFOLDERS.Params.ParamByName('FileName').AsString := Copy(FilePathName,Length(FilePath)+1,Length(FilePathName));
    sqlCOMPARETWOFOLDERS.Params.ParamByName('FileHash').AsString := FileHash;
    sqlCOMPARETWOFOLDERS.ExecSQL;

  except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
  end;
end;

// Used by the COMPARE TWO FOLDERS grid to display mis-matched files and hashes to the user via right click option
// New to v3.3.0
procedure TfrmSQLiteDBases.ShowMismatchesC2F(DBGrid : TDBGrid);
begin
  try
    If not FC2Fquery then ShowAllC2FGRID(DBGrid);
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    DBGrid.DataSource.DataSet.Filter:='FileHashA='''' or FileHashB='''' or FileHashA<>FileHashB';
    DBGrid.DataSource.DataSet.Filtered:=True;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COMPARE TWO FOLDERS grid to display duplicates from folders
// New to v3.3.0
procedure TfrmSQLiteDBases.ShowDuplicatesC2FTAB(DBGrid : TDBGrid);
begin
  try
    FC2Fquery:=False;
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.DataSet.Filter:='';
    DBGrid.DataSource.Dataset.Close;
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'select * from TBL_COMPARE_TWO_FOLDERS_DUPLICATES';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;


// Used by the COMPARE TWO FOLDERS grid to display files with differing hashes
// New to v3.3.0
procedure TfrmSQLiteDBases.ShowDiffHashes(DBGrid : TDBGrid);
begin
  try
    If not FC2Fquery then ShowAllC2FGRID(DBGrid);
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    DBGrid.DataSource.DataSet.Filter:='FileHashA<>FileHashB and FileHashA<>'''' and FileHashB<>''''';
    DBGrid.DataSource.DataSet.Filtered:=True;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COMPARE TWO FOLDERS grid to display only matching hashes (also if both files are empty)
// New to v3.3.0
procedure TfrmSQLiteDBases.ShowMatchingHashes(DBGrid : TDBGrid);
begin
  try
    If not FC2Fquery then ShowAllC2FGRID(DBGrid);
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    DBGrid.DataSource.DataSet.Filter:='FileHashA=FileHashB';
    DBGrid.DataSource.DataSet.Filtered:=True;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COMPARE TWO FOLDERS grid to show files that are missing from folder A
// New to v3.3.0
procedure TfrmSQLiteDBases.ShowMissingFilesFolderA(DBGrid : TDBGrid);
begin
  try
    If not FC2Fquery then ShowAllC2FGRID(DBGrid);
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    DBGrid.DataSource.DataSet.Filter:='FileHashA=''''';
    DBGrid.DataSource.DataSet.Filtered:=True;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COMPARE TWO FOLDERS grid to show files that are missing from folder B
// New to v3.3.0
procedure TfrmSQLiteDBases.ShowMissingFilesFolderB(DBGrid : TDBGrid);
begin
  try
    If not FC2Fquery then ShowAllC2FGRID(DBGrid);
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    DBGrid.DataSource.DataSet.Filter:='FileHashB=''''';
    DBGrid.DataSource.DataSet.Filtered:=True;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;

// Used by the COMPARE TWO FOLDERS grid to show files that are missing either from folder A or from folder B
procedure TfrmSQLiteDBases.ShowMissingFromFolderAAndFolderB(DBGrid : TDBGrid);
begin
  try
    If not FC2Fquery then ShowAllC2FGRID(DBGrid);
    DBGrid.DataSource.DataSet.Filtered:=False; // Remove any filters
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlFILES but the query connected to the grid
    DBGrid.DataSource.DataSet.Filter:='FileHashA='''' or FileHashB=''''';
    DBGrid.DataSource.DataSet.Filtered:=True;
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
    except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
    end;
end;
// Used by the COMPARE TWO FOLDERS grid to show all items
procedure TfrmSQLiteDBases.ShowAllC2FGRID(DBGrid : TDBGrid);
begin
  try
    FC2Fquery:=True;
    DBGrid.DataSource.DataSet.Filtered:=False;
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlCOMPARETWOFOLDERS but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text:='select * from TBL_COMPARE_TWO_FOLDERS_MATCH';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
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
    sqlCOPY.Params.ParamByName('SourceFilename').AsString      := Col1;
    sqlCOPY.Params.ParamByName('SourceHash').AsString          := Col2;
    sqlCOPY.Params.ParamByName('DestinationFilename').AsString := Col3;
    sqlCOPY.Params.ParamByName('DestinationHash').AsString     := Col4;
    sqlCOPY.Params.ParamByName('DateAttributes').AsString      := Col5;
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
    frmDisplayGrid1.DBGrid_COPY.Options:= frmDisplayGrid1.DBGrid_COPY.Options + [dgAutoSizeColumns];
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
    frmDisplayGrid1.DBGrid_COPY.Options:= frmDisplayGrid1.DBGrid_COPY.Options + [dgAutoSizeColumns];
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
   frmDisplayGrid1.DBGrid_COPY.Options:= frmDisplayGrid1.DBGrid_COPY.Options + [dgAutoSizeColumns];
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
   frmDisplayGrid1.DBGrid_COPY.Options:= frmDisplayGrid1.DBGrid_COPY.Options + [dgAutoSizeColumns];
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
    DBGrid.DataSource.Dataset.Close; // <--- we don't use sqlCOPY but the query connected to the grid
    TSQLQuery(DBGrid.DataSource.Dataset).SQL.Text := 'SELECT * FROM TBL_COPY';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    frmDisplayGrid1.DBGrid_COPY.Options:= frmDisplayGrid1.DBGrid_COPY.Options + [dgAutoSizeColumns];
    DBGrid.DataSource.Dataset.Open;
  except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// Used by the COPY display grid to clipboard data from the grid
procedure TfrmSQLiteDBases.DatasetToClipBoardCOPYTAB(DBGrid : TDBGrid);
var
  CSVClipboardList : TStringListUTF8;
  RowCount : integer = Default(integer);
begin
  Mainform.StatusBar2.SimpleText := 'Counting rows and writing to clipboard if possible...please wait';
  Application.ProcessMessages;
  ChosenDelimiter := MainForm.ChosenDelimiter;
  RowCount := CountGridRows(DBGrid, 'TBL_COPY');
  if RowCount < 20000 then
  begin
    try
      CSVClipboardList := TStringListUTF8.Create;
      // Add the grid headers
      CSVClipboardList.Add('No'                   + ChosenDelimiter +
                           'Source Filename'      + ChosenDelimiter +
                           'Source FileHash'      + ChosenDelimiter +
                           'Destination Filename' + ChosenDelimiter +
                           'Destination Hash'     + ChosenDelimiter +
                           'Original Date Attributes');
      DBGrid.DataSource.Dataset.DisableControls;
      try
        // Add the grid content
        DBGrid.DataSource.Dataset.First;
        while not DBGrid.DataSource.Dataset.EoF do
        begin
          CSVClipboardList.Add((DBGrid.DataSource.DataSet.Fields[0].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[1].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[2].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[3].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[4].Text) +ChosenDelimiter+
                               (DBGrid.DataSource.DataSet.Fields[5].Text));
          DBGrid.DataSource.Dataset.Next;
        end;
      finally
        DBGrid.DataSource.Dataset.EnableControls;
      end;
    finally
      Clipboard.AsText := CSVClipboardList.Text;
    end;
    Mainform.StatusBar2.SimpleText := 'DONE';
    ShowMessage('Grid data now in clipboard ');
  end
  else
  begin
    Mainform.StatusBar2.SimpleText := 'Row count exceeded 20K. Use "Save to CSV file" instead';
    Showmessage('Row count exceeds 20K. Please use "Save to CSV file" instead');
  end;
end;

// Saves the grid in COPY window to HTML. If small volume of records, uses a stringlist.
// If big volume, uses file stream.
procedure TfrmSQLiteDBases.SaveCOPYWindowToHTML(DBGrid : TDBGrid; Filename : string);
var
  strTitle             : string = Default(string);
  strTableHeader       : string = Default(string);
  FileIDCell           : string = Default(string);
  SourceFilename       : string = Default(string);
  DestinationFileName  : string = Default(string);
  DateAttributes       : string = Default(string);
  SourceFileHash       : string = Default(string);
  DestinationFileHash  : string = Default(string);
  NoOfRowsInGrid       : integer = Default(integer);
  sl                   : TStringList;
  fs                   : TFileStreamUTF8;
  ExportSQLQuery       : TSQLQuery;

  const
    strHTMLHeader      = '<HTML>'  ;
    strTITLEHeader     = '<TITLE>QuickHash HTML Output' ;
    strBODYHeader      = '<BODY>'  ;
    strTABLEROWStart   = '<TR>'    ;
    strTABLEDATAStart  = '<TD>'    ;
    strID              = '<td>ID</td>';
    strSrcFilenameHead = '<td>Source Filename</td>';
    strSrcHashHead     = '<td>Source Hash</td>';
    strDestFilenameHead= '<td>Destination Filename</td>';
    strDestHashHead    = '<td>Destination Hash</td>';
    strDateAttr        = '<td>Original Date Attributes</td>';
    strTABLEDataEnd    = '</TD>'   ;
    strTABLEROWEnd     = '</TR>'   ;
    strTABLEFooter     = '</TABLE>';
    strBODYFooter      = '</BODY>' ;
    strTITLEFooter     = '</TITLE>';
    strHTMLFooter      = '</HTML>' ;

begin
  // If database volume not too big, use memory and stringlists. Otherwise, use file writes
  NoOfRowsInGrid := CountGridRows(DBGrid, 'TBL_COPY');// Count the rows first. If not too many, use memory. Otherwise, use filestreams
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
    sl.add('<tr><td>Source Filename</td><td>Source Hash</td><td>Destination Filename</td><td>Destination Hash</td><td>Original Date Attributes</td></tr>');

    try
      DBGrid.DataSource.DataSet.DisableControls;
      DBGrid.DataSource.DataSet.First;
      while not DBGrid.DataSource.DataSet.EOF do
        begin
          sl.add('<tr>');
          // Get the data from the source filename cell
          SourceFilename := DBGrid.DataSource.DataSet.Fields[1].AsString;
          sl.add('<td>'+SourceFilename+'</td>');
          // Get the data from the source file hash cell
          SourceFileHash := DBGrid.DataSource.DataSet.Fields[2].AsString;
          sl.add('<td>'+SourceFileHash+'</td>');
          // Get the data from the destination name
          DestinationFilename := DBGrid.DataSource.DataSet.Fields[3].AsString;
          sl.add('<td>'+DestinationFilename+'</td>');
          // Get the data from the source file hash cell
          DestinationFileHash := DBGrid.DataSource.DataSet.Fields[4].AsString;
          sl.add('<td>'+DestinationFileHash+'</td>');
          // Get the data from the source file hash cell
          DateAttributes := DBGrid.DataSource.DataSet.Fields[5].AsString;
          sl.add('<td>'+DateAttributes+'</td>');
          sl.add('</tr>');
          DBGrid.DataSource.DataSet.Next;
        end;
    finally
      DBGrid.DataSource.DataSet.EnableControls;
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

    // Create HTML Header Data
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

    // Add a header row to the HTML
    strTableHeader := '<tr>'+ strID + strSrcFilenameHead + strSrcHashHead + strDestFilenameHead + strDestHashHead + strDateAttr + '</tr>';
    fs.Write(strTableHeader[1], Length(strTableHeader));

    // Now write the main table content, row by row

    { strTABLEROWStart   = '<TR>'      = 4 bytes
      strTABLEDATAStart  = '<TD>'      = 4 bytes
      strTABLEDataEnd    = '</TD>'     = 5 bytes
      strTABLEROWEnd     = '</TR>'     = 5 bytes
      strTABLEFooter     = '</TABLE>'  = 8 bytes
      strBODYFooter      = '</BODY>'   = 7 bytes
      strTITLEFooter     = '</TITLE>'  = 8 bytes
      strHTMLFooter      = '</HTML>'   = 7 bytes}

      try
        ExportSQLQuery := TSQLQuery.Create(nil);
        try
          ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_COPY';  // Get all the data from Copy tab table
          ExportSQLQuery.Database := SQLite3Connection1;
          ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
          ExportSQLQuery.Open;
          ExportSQLQuery.First;
          while not ExportSQLQuery.EOF do
          begin
            fs.Write(strTABLEROWStart[1], 4);

            // Get the data from the ID cell
            FileIDCell := ExportSQLQuery.FieldByName('No').AsString;
            // Write filename to new row
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(FileIDCell[1], Length(FileIDCell));
            fs.Write(strTABLEDataEnd[1], 5);

            SourceFilename := ExportSQLQuery.FieldByName('SourceFilename').AsString;
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(SourceFilename[1], Length(SourceFilename));
            fs.Write(strTABLEDataEnd[1], 5);

            SourceFileHash := ExportSQLQuery.FieldByName('SourceHash').AsString;
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(SourceFileHash[1], Length(SourceFileHash));
            fs.Write(strTABLEDATAEnd[1], 5);

            DestinationFileName := ExportSQLQuery.FieldByName('DestinationFileName').AsString;
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(DestinationFileName[1], Length(Trim(DestinationFileName)));
            fs.Write(strTABLEDATAEnd[1], 5);

            DestinationFileHash := ExportSQLQuery.FieldByName('DestinationHash').AsString;
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(DestinationFileHash[1], Length(Trim(DestinationFileHash)));
            fs.Write(strTABLEDATAEnd[1], 5);

            DateAttributes := ExportSQLQuery.FieldByName('DateAttributes').AsString;
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(DateAttributes[1], Length(Trim(DateAttributes)));
            fs.Write(strTABLEDATAEnd[1], 5);

            // End the row
            fs.Write(strTABLEROWEnd[1], 5);
            fs.Write(#13#10, 2);
            // Repeat for the next row
            ExportSQLQuery.next;
          end;
        finally
          // Nothing to free here
        end;
        fs.Write(strTABLEFooter, 8);
        fs.Write(#13#10, 2);
        fs.writeansistring(IntToStr(NoOfRowsInGrid) + ' grid entries saved.');
        fs.Write(strBODYFooter, 7);
        fs.Write(#13#10, 2);
        fs.Write(strHTMLFooter, 7);
        fs.Write(#13#10, 2);
      finally
        ExportSQLQuery.free; // Free the temp SQL Query
      end;
    finally
      fs.free;
      Showmessage('Data saved to HTML file ' + Filename + '...OK');
      Application.ProcessMessages;
    end;
end;

// Saves the "Compare Two Folders" grid to HTML. If small volume of records, uses a stringlist.
// If big volume, uses file stream.
procedure TfrmSQLiteDBases.SaveC2FWindowToHTML(DBGrid : TDBGrid; Filename : string);
var
  strTitle          : string = Default(string);
  HeaderRow         : string = Default(string);
  strID             : string = Default(string);
  strFilename       : string = Default(string);
  FilepathA         : string = Default(string);
  FileHashA         : string = Default(string);
  FilePathB         : string = Default(string);
  FileHashB         : string = Default(string);
  NoOfRowsInGrid    : integer = Default(integer);
  sl                : TStringList;
  fs                : TFileStreamUTF8;
  ExportSQLQuery    : TSQLQuery;
const
  strHTMLHeader     = '<HTML>'  ;
  strTITLEHeader    = '<TITLE>QuickHash HTML Output' ;
  strBODYHeader     = '<BODY>'  ;
  strTABLEROWStart  = '<TR>'    ;
  strTABLEDATAStart = '<TD>'    ;
  strTABLEDataEnd   = '</TD>'   ;
  strTABLEROWEnd    = '</TR>'   ;
  strTABLEFooter    = '</TABLE>';
  strBODYFooter     = '</BODY>' ;
  strTITLEFooter    = '</TITLE>';
  strHTMLFooter     = '</HTML>' ;

begin
  // If database volume not too big, use memory and stringlists. Otherwise, use file writes
  NoOfRowsInGrid := 0;
  NoOfRowsInGrid := CountGridRows(DBGrid, 'TBL_COMPARE_TWO_FOLDERS_MATCH');// Count the rows first. If not too many, use memory. Otherwise, use filestreams
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
    sl.add('<tr><td>ID</td><td>Filename</td><td>FilepathA</td><td>FileHashA</td><td>FilePathB</td><td>FileHashB</td></tr>');

    try
    DBGrid.DataSource.DataSet.DisableControls;
    DBGrid.DataSource.DataSet.First;
    while not DBGrid.DataSource.DataSet.EOF do
      begin
          sl.add('<tr>');
          strID := DBGrid.DataSource.DataSet.Fields[0].AsString;
          sl.add('<td>'+strID+'</td>');
          strFileName := DBGrid.DataSource.DataSet.Fields[1].AsString;
          sl.add('<td>'+strFileName+'</td>');
          FilepathA := DBGrid.DataSource.DataSet.Fields[2].AsString;
          sl.add('<td>'+FilepathA+'</td>');
          FileHashA := DBGrid.DataSource.DataSet.Fields[3].AsString;
          sl.add('<td>'+FileHashA+'</td>');
          FilepathB := DBGrid.DataSource.DataSet.Fields[4].AsString;
          sl.add('<td>'+FilepathB+'</td>');
          FileHashB := DBGrid.DataSource.DataSet.Fields[5].AsString;
          sl.add('<td>'+FileHashB+'</td>');
          sl.add('</tr>');
          DBGrid.DataSource.DataSet.Next;
        end;
    sl.add('</TABLE>');
    sl.add('</BODY> ');
    sl.add('</HTML> ');
    finally
      DBGrid.DataSource.DataSet.EnableControls;
    end;

  finally
    sl.SaveToFile(Filename);
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
    HeaderRow := '';
    HeaderRow := ('<tr><td>ID</td><td>Filename</td><td>FilepathA</td><td>FileHashA</td><td>FilePathB</td><td>FileHashB</td></tr>');
    fs.Write(HeaderRow[1], Length(HeaderRow));

    { strTABLEROWStart   = '<TR>'      = 4 bytes
      strTABLEDATAStart  = '<TD>'      = 4 bytes
      strTABLEDataEnd    = '</TD>'     = 5 bytes
      strTABLEROWEnd     = '</TR>'     = 5 bytes
      strTABLEFooter     = '</TABLE>'  = 8 bytes
      strBODYFooter      = '</BODY>'   = 7 bytes
      strTITLEFooter     = '</TITLE>'  = 8 bytes
      strHTMLFooter      = '</HTML>'   = 7 bytes}

       try
        ExportSQLQuery := TSQLQuery.Create(nil);
        try
          ExportSQLQuery.SQL.Text := 'SELECT * FROM TBL_COMPARE_TWO_FOLDERS_MATCH';  // Get all the data from Compare Two Folders tab table
          ExportSQLQuery.Database := SQLite3Connection1;
          ExportSQLQuery.UniDirectional := True; //<- this is the important part for memory handling reasons but cant be used in an active DBGrid - only for SQLQueries
          ExportSQLQuery.Open;
          ExportSQLQuery.First;
          while not ExportSQLQuery.EOF do
          begin
            fs.Write(strTABLEROWStart[1], 4);

            strID := ExportSQLQuery.FieldByName('No').AsString;
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(strID[1], Length(strID));
            fs.Write(strTABLEDataEnd[1], 5);

            strFileName := ExportSQLQuery.FieldByName('FileName').AsString;
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(strFileName[1], Length(strFileName));
            fs.Write(strTABLEDataEnd[1], 5);

            FilepathA := ExportSQLQuery.FieldByName('FilePathA').AsString;
            fs.Write(strTABLEDATAStart[1], 4);
            fs.Write(FilepathA[1], Length(FilepathA));
            fs.Write(strTABLEDATAEnd[1], 5);

            FileHashA := ExportSQLQuery.FieldByName('FileHashA').AsString;
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(FileHashA[1], Length(Trim(FileHashA)));
            fs.Write(strTABLEDATAEnd[1], 5);

            FilepathB := ExportSQLQuery.FieldByName('FilePathB').AsString;
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(FilepathB[1], Length(Trim(FilepathB)));
            fs.Write(strTABLEDATAEnd[1], 5);

            FileHashB := ExportSQLQuery.FieldByName('FileHashB').AsString;
            fs.Write(strTABLEDATAStart[1], 4) ;
            fs.Write(FileHashB[1], Length(Trim(FileHashB)));
            fs.Write(strTABLEDATAEnd[1], 5);

            fs.Write(strTABLEROWEnd[1], 5);
            fs.Write(#13#10, 2);
            // Repeat for the next row
            ExportSQLQuery.next;
          end;
        finally
          // Nothing to free here
        end;
        fs.Write(strTABLEFooter, 8);
        fs.Write(#13#10, 2);
        fs.writeansistring(IntToStr(NoOfRowsInGrid) + ' grid entries saved.');
        fs.Write(strBODYFooter, 7);
        fs.Write(#13#10, 2);
        fs.Write(strHTMLFooter, 7);
        fs.Write(#13#10, 2);
        finally
          ExportSQLQuery.free; // Free the temp SQL Query
        end;
    finally
      fs.free;
      Showmessage('Data saved to HTML file ' + Filename + '...OK');
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
    MainForm.DBGrid_FILES.Options:= MainForm.DBGrid_FILES.Options + [dgAutoSizeColumns];
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
    frmDisplayGrid1.DBGrid_COPY.Options:= frmDisplayGrid1.DBGrid_COPY.Options + [dgAutoSizeColumns];
    except
    on E: EDatabaseError do
    begin
      MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
    end;
  end;
end;

// New to v3.3.0 thanks to community support. Enables a much more varied option for filtering results
// But this takes a long time with tens of thousands of rows. Need a better way!
procedure TfrmSQLiteDBases.PrepareData_COMPARE_TWO_FOLDERS; // prepares matches and duplicates with row id
begin
  try
    sqlCOMPARETWOFOLDERS.Close;
    sqlCOMPARETWOFOLDERS.SQL.Text :=
      'INSERT into TBL_COMPARE_TWO_FOLDERS_MATCH (FileName,FilePathA,FileHashA,FilePathB,FileHashB) ' +
      'SELECT a.FileName,a.FilePath as FilePathA, a.FileHash as FileHashA, '+
      ' b.FilePath as FilePathB, b.FileHash as FileHashB '+
      'FROM TBL_COMPARE_TWO_FOLDERS a '+
      '  left join TBL_COMPARE_TWO_FOLDERS b on a.FileName=b.FileName and b.FilePath=:FilePathB '+
      'WHERE a.FilePath=:FilePathA '+
      'union '+
      'SELECT c.FileName,d.FilePath as FilePathA, d.FileHash as FileHashA, '+
      ' c.FilePath as FilePathB, c.FileHash as FileHashB '+
      'FROM TBL_COMPARE_TWO_FOLDERS c '+
      '  left join TBL_COMPARE_TWO_FOLDERS d on c.FileName=d.FileName and d.FilePath=:FilePathA '+
      'WHERE c.FilePath=:FilePathB';
    SQLTransaction1.Active := True;
    sqlCOMPARETWOFOLDERS.ParamByName('FilePathA').AsString:=FFilePathA;
    sqlCOMPARETWOFOLDERS.ParamByName('FilePathB').AsString:=FFilePathB;
    sqlCOMPARETWOFOLDERS.ExecSQL;
    sqlCOMPARETWOFOLDERS.SQL.Text :=
      'INSERT into TBL_COMPARE_TWO_FOLDERS_DUPLICATES (FilePath,FileName,FileHash) ' +
      'SELECT distinct a.FilePath, a.FileName, a.FileHash ' +
      'FROM tbl_compare_two_folders a, ' +
      '     tbl_compare_two_folders b ' +
      'WHERE a.FileHash = b.FileHash ' +
      '  and a.id <> b.id ';

    SQLTransaction1.Active := True;
    sqlCOMPARETWOFOLDERS.ExecSQL;
  except
      on E: EDatabaseError do
      begin
        MessageDlg('Error','A database error has occurred. Technical error message: ' + E.Message,mtError,[mbOK],0);
      end;
  end;
end;

// COMPARE TWO FOLDERS tab update grid routine
procedure TfrmSQLiteDBases.UpdateGridCOMPARETWOFOLDERSTAB(Sender: TObject);
  begin
    try
    FC2Fquery:=True;
    sqlCOMPARETWOFOLDERS.Close;
    sqlCOMPARETWOFOLDERS.SQL.Text:='select * from TBL_COMPARE_TWO_FOLDERS_MATCH';
    SQLite3Connection1.Connected := True;
    SQLTransaction1.Active := True;
    sqlCOMPARETWOFOLDERS.Open;
    frmDisplayGrid3.DBGrid_C2F.Options := frmDisplayGrid3.DBGrid_C2F.Options + [dgAutoSizeColumns];
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

initialization

end.

