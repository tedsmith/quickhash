unit uEnvironmentChecker;

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  FileUtil, sha1, dbases_sqlite;

type

  { TfrmEnvironmentCheck }

  TfrmEnvironmentCheck = class(TForm)
    btnCheckNow: TButton;
    memEnvList: TMemo;
    procedure btnCheckNowClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    function DLLScan(expectedPath : rawbytestring) : Boolean;
  private

  public

  end;

var
  frmEnvironmentCheck: TfrmEnvironmentCheck;

implementation

{ TfrmEnvironmentCheck }

function TfrmEnvironmentCheck.DLLScan(expectedPath : rawbytestring) : Boolean;
  {libewf-x64.dll		5D33227712DA76316613DCD88B2749916DBA5ACA
  libewf-x86.dll		915E3F26E170A062312A8CD73462AE6ECA6EF7BA
  libgcc_s_dw2-1.dll		201924954A5A593C4CA24EE0FE799A764B41598D
  libwinpthread-1.dll		34E84ED8F69F05FCAD212B02C2B064A5C7377904
  sqlite3-win32.dll		0B25A2BA06DD5B8FE2A5F33C5C8442D4C12A2B70
  sqlite3-win64.dll		2092405B3755C71E12E2F6EE4D193B321999CB62
  zlib1.dll (32-bit copy)	B1D1FECBB568EDCF712232738BA3805B47BC6036
  zlib1.dll (64-bit copy)	A10687C37DEB2CE5422140B541A64AC15534250F}
var
  sl                : TStringList;
  i                 : integer = Default(integer);
  ExpectedFileCount : integer = Default(integer);
  ComputedHash      : string = Default(string);
  DLLName           : string = Default(string);
begin
  result := false;
  ExpectedFileCount := 5;
  {$ifdef CPU32}
  memEnvList.Text:= 'Number of DLLs expected : '       + IntToStr(ExpectedFileCount) + '. ' + LineEnding +
                    'Expected DLLs and SHA-1 hashes: ' + LineEnding +
                    'libewf-x86.dll'		+ #9#9 + '915E3F26E170A062312A8CD73462AE6ECA6EF7BA' + Lineending+
                    'libgcc_s_dw2-1.dll'	+ #9#9 + '201924954A5A593C4CA24EE0FE799A764B41598D' + Lineending+
                    'libwinpthread-1.dll'	+ #9#9 + '34E84ED8F69F05FCAD212B02C2B064A5C7377904' + Lineending+
                    'sqlite3-win32.dll'		+ #9#9 + '0B25A2BA06DD5B8FE2A5F33C5C8442D4C12A2B70' + Lineending+
                    'zlib1.dll (x86 copy)'	+ #9#9 + 'B1D1FECBB568EDCF712232738BA3805B47BC6036' + Lineending+
                    '==================================================================='           + Lineending;
  {$else ifdef CPU64}
  memEnvList.Text:= 'Number of DLLs expected : '        + IntToStr(ExpectedFileCount) + '. ' + LineEnding +
                    'Expected DLLs and SHA-1 hashes: '  + LineEnding +
                    'libewf-x64.dll'             + #9#9 + '5D33227712DA76316613DCD88B2749916DBA5ACA' + Lineending +
                    'libgcc_s_dw2-1.dll'	 + #9#9 + '201924954A5A593C4CA24EE0FE799A764B41598D' + Lineending +
                    'libwinpthread-1.dll'        + #9#9 + '34E84ED8F69F05FCAD212B02C2B064A5C7377904' + Lineending +
                    'sqlite3-win64.dll'		 + #9#9 + '2092405B3755C71E12E2F6EE4D193B321999CB62' + Lineending +
                    'zlib1.dll (x64 copy)'	 + #9#9 + 'A10687C37DEB2CE5422140B541A64AC15534250F' + Lineending +
                    '========================================================================='      + Lineending;
  {$endif}
  try
    memEnvList.Lines.Add('Found DLLs and computed SHA-1 hashes:');
    sl := TStringList.Create;
    FindAllFiles(sl, expectedPath, '*.dll', true, faAnyFile);
    if sl.count = 5 then
    begin
    for i := 0 to sl.count -1 do
      begin
        DLLName := ExtractFileName(sl.strings[i]);
        ComputedHash :=  Uppercase(SHA1Print(SHA1File(sl.strings[i])));
        memEnvList.Lines.Add(DLLName + #9#9 + ComputedHash);
      end;
    end
    else memEnvList.Lines.Add(IntToStr(ExpectedFileCount) + ' DLLs were expected but only '
                              + IntToStr(sl.count) + ' were found.');
  finally
    memEnvList.Lines.Add('Number of DLLs found : ' + IntToStr(sl.count));
    memEnvList.Lines.Add('========================================================================='  + Lineending);
    sl.Free;
    result := true;
  end;
end;

procedure TfrmEnvironmentCheck.FormCreate(Sender: TObject);
begin
  memEnvList.Clear;
end;

procedure TfrmEnvironmentCheck.btnCheckNowClick(Sender: TObject);
var
  {$ifdef Windows}
    {$ifdef CPU32}
    LIB_FOLDER : ansistring = 'libs\x86';
    {$else ifdef CPU64}
    LIB_FOLDER : ansistring = 'libs\x64';
    {$endif}
  {$endif}

  ExpectedDLLPath : rawbytestring;
  AreDLLsPresent  : Boolean = Default(Boolean);
  strActiveDB     : string = Default(string);
  strDBVersion    : string = Default(string);
begin
  memEnvList.Lines.Add('Checking environment. Please wait...');
  {$ifdef Windows}
  // Check DLLs for Windows installs
  ExpectedDLLPath := ExtractFilePath(Application.ExeName)+IncludeTrailingPathDelimiter(LIB_FOLDER);
  AreDLLsPresent := DLLScan(ExpectedDLLPath);
  if AreDLLsPresent = false then memEnvList.Lines.Add('At least 1 DLL is missing. QuickHash may not function fully.');
  {$endif}
  // Show SQLite Name
  strActiveDB := dbases_sqlite.frmSQLiteDBases.DBName;
  if Length(strActiveDB) > 0 then memEnvList.Lines.Add('SQLite Database Name : ' + strActiveDB)
    else memEnvList.Lines.Add('SQLite Database name : Unknown');

  // Show SQLite version
  strDBVersion := frmSQLiteDBases.DBVersionLookup;
  if Length(strDBVersion) > 0 then
    memEnvList.Lines.Add(strDBVersion) else
      memEnvList.Lines.Add('SQlite version could not be determined');

  Application.ProcessMessages;
  frmEnvironmentCheck.Visible:= true;
end;

initialization
  {$I uenvironmentchecker.lrs}

end.

