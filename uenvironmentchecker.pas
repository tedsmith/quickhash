unit uEnvironmentChecker;  // New to v3.3.0

{$mode objfpc}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  windows, FileUtil, sha1, dbases_sqlite;

type
  MEMORYSTATUSEX = record
     dwLength     : DWORD;
     dwMemoryLoad : DWORD;
     ullTotalPhys : uint64;
     ullAvailPhys : uint64;
     ullTotalPageFile : uint64;
     ullAvailPageFile : uint64;
     ullTotalVirtual  : uint64;
     ullAvailVirtual  : uint64;
     ullAvailExtendedVirtual : uint64;
  end;
  { TfrmEnvironmentCheck }

  TfrmEnvironmentCheck = class(TForm)
    btnCheckNow: TButton;
    memEnvList: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure btnCheckNowClick(Sender: TObject);
    function DLLScan(expectedPath : rawbytestring) : Boolean;  // To scan Windows DLLs
    function FormatByteSize(const bytes: QWord): string;
    function GetSystemMem: string;                             // Returns installed RAM (as viewed by your OS) in GB, with 2 decimals }
  private

  public

  end;

var
  frmEnvironmentCheck: TfrmEnvironmentCheck;

implementation

{ TfrmEnvironmentCheck }

{$IFDEF WINDOWS}
// Populate interface with quick view to RAM status
  function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): BOOL; stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';

// http://stackoverflow.com/questions/7859978/get-total-and-available-memory-when-4-gb-installed
// Returns installed RAM (as viewed by your OS) in Gb\Tb} as string. Empty string on failure.
function TfrmEnvironmentCheck.GetSystemMem: string;
VAR
  MS_Ex : MemoryStatusEx;
  strTotalPhysMem, strTotalPhysAvail : string;
begin
 result := '';
 FillChar(MS_Ex{%H-}, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength := SizeOf(MemoryStatusEx);
 if GlobalMemoryStatusEx(MS_Ex) then
   begin
     strTotalPhysMem := FormatByteSize(MS_Ex.ullTotalPhys);
     strTotalPhysAvail := FormatByteSize(MS_Ex.ullAvailPhys);
     Result:= strTotalPhysMem + ' total, ' + #10#13 +
              strTotalPhysAvail + ' available' + #10#13;
   end
 else Result := 'No Data';
end;
{$ENDIF}

function TfrmEnvironmentCheck.FormatByteSize(const bytes: QWord): string;
var
  B: byte;
  KB: word;
  MB: QWord;
  GB: QWord;
  TB: QWord;
begin
  B  := 1;         //byte
  KB := 1024 * B;  //kilobyte
  MB := 1024 * KB; //megabyte
  GB := 1024 * MB; //gigabyte
  TB := 1024 * GB; //terabyte

  if bytes > TB then
    result := FormatFloat('#.## TiB', bytes / TB)
  else
    if bytes > GB then
      result := FormatFloat('#.## GiB', bytes / GB)
    else
      if bytes > MB then
        result := FormatFloat('#.## MiB', bytes / MB)
      else
        if bytes > KB then
          result := FormatFloat('#.## KiB', bytes / KB)
        else
          if bytes > B then
          result := FormatFloat('#.## bytes', bytes)
        else
          if bytes = 0 then
          result := '0 bytes';
end;
function TfrmEnvironmentCheck.DLLScan(expectedPath : rawbytestring) : Boolean;
  {// On Windows :
  libewf-x64.dll		5D33227712DA76316613DCD88B2749916DBA5ACA
  libewf-x86.dll		915E3F26E170A062312A8CD73462AE6ECA6EF7BA
  libgcc_s_dw2-1.dll		201924954A5A593C4CA24EE0FE799A764B41598D
  libwinpthread-1.dll		34E84ED8F69F05FCAD212B02C2B064A5C7377904
  sqlite3-win32.dll		0B25A2BA06DD5B8FE2A5F33C5C8442D4C12A2B70
  sqlite3-win64.dll		2092405B3755C71E12E2F6EE4D193B321999CB62
  zlib1.dll (32-bit copy)	B1D1FECBB568EDCF712232738BA3805B47BC6036
  zlib1.dll (64-bit copy)	A10687C37DEB2CE5422140B541A64AC15534250F
  // On Linux :
  libewf-Linux-x64.so           2376C9092754ABF401CFA1D17C00801DAAB4D143
  }
var
  sl                : TStringList;
  i                 : integer = Default(integer);
  ExpectedFileCount : integer = Default(integer);
  ComputedHash      : string = Default(string);
  LibraryName       : string = Default(string);
begin
  result := false;
  ExpectedFileCount := 5;
  {$ifdef Windows}
    {$ifdef CPU32}
    memEnvList.Text:= 'Number of DLLs expected : '       + IntToStr(ExpectedFileCount) + '. ' + LineEnding +
                      'Expected DLLs and SHA-1 hashes: ' + LineEnding +
                      'libewf-x86.dll'		+ #9#9 + '915E3F26E170A062312A8CD73462AE6ECA6EF7BA' + Lineending+
                      'libgcc_s_dw2-1.dll'	+ #9#9 + '201924954A5A593C4CA24EE0FE799A764B41598D' + Lineending+
                      'libwinpthread-1.dll'	+ #9#9 + '34E84ED8F69F05FCAD212B02C2B064A5C7377904' + Lineending+
                      'sqlite3-win32.dll'       + #9#9 + '0B25A2BA06DD5B8FE2A5F33C5C8442D4C12A2B70' + Lineending+
                      'zlib1.dll (x86 copy)'	+ #9#9 + 'B1D1FECBB568EDCF712232738BA3805B47BC6036' + Lineending+
                      '==================================================================='           + Lineending;
    {$else ifdef CPU64}
    memEnvList.Text:= 'Number of DLLs expected : '        + IntToStr(ExpectedFileCount) + '. ' + LineEnding +
                      'Expected DLLs and SHA-1 hashes: '  + LineEnding +
                      'libewf-x64.dll'             + #9#9 + '5D33227712DA76316613DCD88B2749916DBA5ACA' + Lineending +
                      'libgcc_s_dw2-1.dll'	   + #9#9 + '201924954A5A593C4CA24EE0FE799A764B41598D' + Lineending +
                      'libwinpthread-1.dll'        + #9#9 + '34E84ED8F69F05FCAD212B02C2B064A5C7377904' + Lineending +
                      'sqlite3-win64.dll'	   + #9#9 + '2092405B3755C71E12E2F6EE4D193B321999CB62' + Lineending +
                      'zlib1.dll (x64 copy)'	   + #9#9 + 'A10687C37DEB2CE5422140B541A64AC15534250F' + Lineending +
                      '========================================================================='      + Lineending;
    {$endif}
  {$endif}

  {$ifdef Linux}
  memEnvList.Text:= 'Number of SO files expected : 1'                       + LineEnding +
  'Expected SO files and SHA-1 hashes: '                                    + LineEnding +
  'libewf-Linux-x64.so' + #9#9 + '2376C9092754ABF401CFA1D17C00801DAAB4D143' + Lineending;
  {$endif}
  try
    memEnvList.Lines.Add('Found DLLs and computed SHA-1 hashes:');
    sl := TStringList.Create;
    {$ifdef Windows}
    FindAllFiles(sl, expectedPath, '*.dll', true, faAnyFile);
    if sl.count = 5 then
    {$endif}
    {$ifdef Linux}
    FindAllFiles(sl, expectedPath, '*.so', true, faAnyFile);
    if sl.count = 1 then
    {$endif}

    {$ifdef Darwin}
    // do nothing but keep the compiler happy . This function isnt even called on OSX
    // but compiling on OSX will require rewriting the code below with the following if statement
    if sl.count = 1 then
    {$endif}
    begin
    for i := 0 to sl.count -1 do
      begin
        LibraryName  := ExtractFileName(sl.strings[i]);
        ComputedHash :=  Uppercase(SHA1Print(SHA1File(sl.strings[i])));
        memEnvList.Lines.Add(LibraryName + #9#9 + ComputedHash);
      end;
    end
    else memEnvList.Lines.Add(IntToStr(ExpectedFileCount) + ' library files were expected but '
                              + IntToStr(sl.count) + ' were found.');
  finally
    memEnvList.Lines.Add('Number of libraries found : ' + IntToStr(sl.count));
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
  {$ifdef Linux}
    {$ifdef CPU32}
    LIB_FOLDER : ansistring = 'libs/x86';
    {$else ifdef CPU64}
    LIB_FOLDER : ansistring = 'libs/x64';
    {$endif}
  {$endif}

  ExpectedDLLPath : rawbytestring;
  AreDLLsPresent  : Boolean = Default(Boolean);
  strActiveDB     : string = Default(string);
  strDBVersion    : string = Default(string);
  strRAMData      : string = Default(string);
begin
  memEnvList.Lines.Add('Checking environment. Please wait...');
  {$ifdef Windows}
    // Check DLLs for Windows installs and other Windows based checks
    ExpectedDLLPath := ExtractFilePath(Application.ExeName)+IncludeTrailingPathDelimiter(LIB_FOLDER);
    AreDLLsPresent  := DLLScan(ExpectedDLLPath);
    if AreDLLsPresent = false then
    begin
      memEnvList.Lines.Add('At least 1 DLL is missing. QuickHash may not function fully.');
    end;
    strRAMData := GetSystemMem;
    if Length(strRAMData) > 0 then
    begin
      memEnvList.Lines.Add('RAM Data : '+ strRAMData);
    end;
  {$endif}
  {$ifdef Darwin}
     memEnvList.Lines.Add('Libraries are mostly part of OSX natively since Big Sur except libewf which is not available on OSX with Quickhash yet. So skipping library check.');
  {$endif}
  {$ifdef Linux}
    ExpectedDLLPath := ExtractFilePath(Application.ExeName)+IncludeTrailingPathDelimiter(LIB_FOLDER);
    AreDLLsPresent := DLLScan(ExpectedDLLPath);
    if AreDLLsPresent = false then memEnvList.Lines.Add('At least 1 SO file is missing. QuickHash may not function fully.');
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

