// New to v3.0.0 - provided for capability of users importing a known list of hashes
// from a text file and checking if they appear when the user selects a folder
// in the FileS tab
unit uKnownHashLists;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, contnrs;

var
  HL1, HL2 : TFPHashList;   // global hash lists of known files (HL1) and added hashes (HL2)

procedure CreateMemResidentHashLists();
procedure ImportHashList(Filename : string);
procedure AddToNewHashList(hashval : string);
function CompareHashLists(): Boolean;
function ComputeWhatHashesAreMissing() : integer;
procedure Free;


implementation

Uses
  Unit2;

procedure CreateMemResidentHashLists();
begin
   try
   HL1 := TFPHashList.Create;
   HL2 := TFPHashList.Create;
   except
     ShowMessage('Could not allocate memory for hash lists');
   end;
end;

// Read in existing hashes from hash file and add to HL1
procedure ImportHashList(Filename : string);
var
 InFile     : textfile;
 SourceData : string;
begin
   AssignFile(InFile, Filename);
   try
     reset(InFile);
     while not EOF(InFile) do
     begin
       readln(InFile, SourceData);
       HL1.Add(SourceData, @SourceData);
       // To show hash itself, use : ShowMessage(HashListA.NameOfIndex(i));
     end;
   finally
     CloseFile(InFile);
     ShowMessage('Hash list imported into memory OK. Now select folder to examine');
   end;
 end;

// Read in a newly computed hash and add to HL2
procedure AddToNewHashList(hashval : string);
begin
  HL2.Add(HashVal, @HashVal);
end;

// Compare HL1 agains HL2. Returns true if matching, false otherwise
function CompareHashLists(): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (HL1.Count <> HL2.Count) then
    Exit;
  for i := 0 to HL1.Count-1 do
    if (HL2.FindIndexOf(HL1.NameOfIndex(i)) < 0) then
      Exit;
  Result := True;
end;

// Compute what hashes are missing from HL2 that were found in HL1
function ComputeWhatHashesAreMissing() : integer;
var
  i, j : integer;
  sl : TStringList;
begin
  i := -1;
  j := -1;
  sl := TStringList.create;
  sl.Sorted:=true;

  if HL1.Count > HL2.Count then
  for i := 0 to HL1.Count-1 do
    begin
      if (HL2.FindIndexOf(HL1.NameOfIndex(i)) < 0) then
      begin
        sl.Add(HL1.NameOfIndex(i));
      end;
    end
  else
    begin
    if HL2.Count > HL1.Count then
    for j := 0 to HL2.Count-1 do
      if (HL1.FindIndexOf(HL2.NameOfIndex(j)) < 0) then
      begin
        sl.Add(HL2.NameOfIndex(j));
      end;
    end;

    // Save results of the hash lookup, saving those hashes not found
    try
      MainForm.sdHashListLookupResults.Title:= 'Save hash lookup results as...';
      MainForm.sdHashListLookupResults.InitialDir := GetCurrentDir;
      MainForm.sdHashListLookupResults.Filter := 'Text|*.txt';
      MainForm.sdHashListLookupResults.DefaultExt := 'txt';
      if MainForm.sdHashListLookupResults.Execute then
      begin
        SL.SaveToFile(MainForm.sdHashListLookupResults.FileName);
      end;
    finally
      SL.Free;
    end;

    if i > -1 then result := i
      else if j > -1 then result := j
        else result := -1;
end;

// Close hadh lists HL1 and HL2
procedure Free;
begin
  try
  HL1.Free;
    try
      HL2.Free;
      except
        ShowMessage('The hash list of newly computed files stored in memory could not be freed.');
      end;
  except
    ShowMessage('The hash list of existing files stored in memory could not be freed.');
  end;
end;

end.
