// New to Jan 4th 2018 beta of v3.0.0 - provided for capability of users importing a known list of hashes
// from a text file and checking if they appear when the user selects a folder
// in the FileS tab.

{   Quick Hash GUI - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
                  and generating hash values for them.

    Copyright (C) 2011-2018  Ted Smith www.quickhash-gui.org

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 2 of the License, or
    any later version. This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You can read a copy of the GNU General Public License at
    http://www.gnu.org/licenses/>. Also, http://www.gnu.org/copyleft/gpl.html

    Use of the name 'QuickHash GUI' must refer to this utility
    only and must not be re-used in another tool if based upon this code.
    The code is Copyright of Ted Smith 2011 - 2018 (www.quickhash-gui.org)
}

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
