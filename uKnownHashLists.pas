// New to Jan 4th 2018 beta of v3.0.0 - provided for capability of users importing a known list of hashes
// from a text file and checking if they appear when the user selects a folder
// in the FileS tab.
// In future, I will add a duplication option for the user,
// i.e. add only unique hashes or all hashes.

unit uKnownHashLists;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, contnrs;

var
  HL1 : TFPHashList; // global hash list to store imported known hashes of files (HL1)

procedure CreateMemResidentHashLists();
procedure ImportHashList(Filename      : string);
procedure Free;

function IsHashInTheKnownList(hashval  : string) : boolean;
function CountHashesInKnownList()      : integer;

implementation

procedure CreateMemResidentHashLists();
begin
  try
   HL1 := TFPHashList.Create;
  except
   ShowMessage('Could not allocate memory for hash lists');
  end;
end;

// Read in existing hashes from hash file and add to HL1, inserting unique values only
procedure ImportHashList(Filename : string);
var
 InFile     : textfile;
 SourceData : string;
 LineCounter, NoOfDuplicates : integer;
begin
  LineCounter := 0;
  NoOfDuplicates := 0;
  AssignFile(InFile, Filename);
   try
     reset(InFile);
     while not EOF(InFile) do
     begin
       readln(InFile, SourceData);
       inc(LineCounter, 1);
       // Add the hash value if not already in the list
       if HL1.FindIndexOf(SourceData) < 0 then
       begin
         HL1.Add(SourceData, @SourceData);
       end;
     end;
   finally
     CloseFile(InFile);
     NoOfDuplicates := LineCounter - HL1.Count;
     ShowMessage(IntToStr(HL1.Count) +      ' unique hashes imported into memory.' + #13#10 +
                 IntToStr(LineCounter) +    ' lines read from input file.'         + #13#10 +
                 IntToStr(NoOfDuplicates) + ' duplicates detected and ignored.'    + #13#10 +
                                            'Now select a folder to hash...');
   end;
 end;

// Is the current hash value existing in the imported hash list? If it is, returns true, false otherwise
function IsHashInTheKnownList(hashval : string) : boolean;
begin
  if HL1.FindIndexOf(hashval) < 0 then // If it's there, the result will be greater than -1
  begin
    result := false;
  end
  else result := true;
end;

// Returns the count of newly imported UNIQUE hash values into the known hash list
// -1 otherwise
function CountHashesInKnownList() : integer;
begin
  result := HL1.Count;
end;

// Close hash lists HL1 in case the user chooses another hashlist.
procedure Free;
begin
  try
    HL1.Free;
  except
    ShowMessage('The hash list of existing files stored in memory could not be freed.');
  end;
end;

end.
