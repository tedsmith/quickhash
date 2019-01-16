unit FindAllFilesEnhanced;
// An enhanced version of the FPC FindAllFiles routine that will detect hidden files
// and hidden files inside hidden directories. Credit to users EngKin and Bart from
// the forums for quickly developing this unit

{
   Quick Hash GUI - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
                     and generating hash values for them.

   Copyright (C) 2011-2019  Ted Smith www.quickhash-gui.org

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
{$mode objfpc}{$H+} // {$H+} ensures strings are of unlimited size

interface
uses
  LazUTF8Classes, Classes;

function FindAllFilesEx(const SearchPath: string; SearchMask: string;
    SearchSubDirs: boolean; IncludeHiddenDirs: boolean): TStringListUTF8;

implementation
uses
  FileUtil,
  SysUtils;  //for faHidden

type
  { TListFileSearcher }

  TListFileSearcher = class(TFileSearcher)
  private
    FList: TStrings;
  protected
    procedure DoFileFound; override;
  public
    constructor Create(AList: TStrings);
  end;

{ TListFileSearcher }

procedure TListFileSearcher.DoFileFound;
begin
  FList.Add(FileName);
end;

constructor TListFileSearcher.Create(AList: TStrings);
begin
  inherited Create;
  FList := AList;
end;

function FindAllFilesEx(const SearchPath: string; SearchMask: string;
  SearchSubDirs: boolean; IncludeHiddenDirs: boolean): TStringListUTF8;
var
  Searcher: TListFileSearcher;
begin
  Result   := TStringListUTF8.Create;
  Searcher := TListFileSearcher.Create(Result);
  Searcher.DirectoryAttribute := Searcher.DirectoryAttribute or faHidden;
  try
    Searcher.Search(SearchPath, SearchMask, SearchSubDirs);
  finally
    Searcher.Free;
  end;
end;

end.

