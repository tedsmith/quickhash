{
   Quick Hash GUI - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
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

    QuickHash was first registered on sourceforge on 29th May 2011 and was later
    migrated to the domain www.quickhash-gui.org in December 2016.
    Read more about it's development history online at :
    https://quickhash-gui.org/about-quickhash-gui/

}
program quickhash;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazdbexport, Unit2, FindAllFilesEnhanced, diskmodule, uDisplayGrid,
  diskspecification, uProgress, frmAboutUnit, zvdatetimectrls, dbases_sqlite,
  uKnownHashLists;

{$R *.res}

begin
  Application.Title:='QuickHash';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmDisplayGrid1, frmDisplayGrid1);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmSQLiteDBases, frmSQLiteDBases);
  {$IFDEF Windows}
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmTechSpecs, frmTechSpecs);
  {$ENDIF}
  {$IFDEF Darwin}
   // nothing
  {$else}
  {$IFDEF UNIX and !$ifdef Darwin}
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmTechSpecs, frmTechSpecs);
  {$ENDIF}
  {$ENDIF}
  Application.Run;
end.


