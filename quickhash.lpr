{   Quick Hash - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
    and generating hash values for them.

    Copyright (C) 2011-2019  Ted Smith www.quickhash-gui.org

    The use of the word 'quick' refers to the ease in which the software operates
    in both Linux, Apple Mac and Windows (very few options to worry about, no
    syntax to remember etc) though tests suggest that in most cases the hash
    values are generated as quick or quicker than most mainstream tools.

    The user should be aware of other data hashing tools and use them to cross-check
    findings for critical data :
    md5sum, sha1sum, sha256sum and sha512sum (for Linux),
    FTK Imager, X-Ways Forensics, WinHex, EnCase, FTK (Windows) and many more

    Benchmark tests are welcomed.

    Contributions from members at the Lazarus forums, Stackoverflow and other
    StackExchnage groups are welcomed and acknowledged. Contributions from
    DaReal Shinji are also welcomed and acknowledged, particularly helping with
    Debian package creation and ideas

    NOTE: Date and time values, as computed in recursive directory hashing, are not
    daylight saving time adjusted. Source file date and time values are recorded.

    Open-Source license:

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

    HashLib4Pascal and xxHash64 libraries are both licensed under the MIT License
    https://opensource.org/licenses/MIT

    HashLib4Pascal : https://github.com/Xor-el/HashLib4Pascal and developed by
                     Github user Xor-el (Ugochukwu Stanley). Use of the
                     library is welcomed and acknowledged and very much appreciated,
                     as is the help that was offered by the developer of said library

    xxHash64       : https://github.com/Cyan4973/xxHash and http://cyan4973.github.io/xxHash/
                     Github user Cyan4973. Use of the library is also welcomed and acknowledged
                     and very much appreciated

    QuickHash is created using the Freepascal Compiler and Lazarus-IDE
    http://www.lazarus-ide.org/ developed by Sourceforge users :
    mgaertner,
    mhess,
    user4martin,
    vlx,
    vsnijders

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
  //FindAllFilesEnhanced, uKnownHashLists,
  Forms, lazdbexport, Unit2, diskmodule, uDisplayGrid,
  diskspecification, uProgress, frmAboutUnit, zvdatetimectrls, dbases_sqlite;

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


