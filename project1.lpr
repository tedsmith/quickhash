program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit2, sha1Customised, diskmoduleunit1, FindAllFilesEnhanced,
uDisplayGrid;

//{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

{$R *.res}

begin
  Application.Title:='Quick Hash';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  {$IFDEF Windows}
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  {$ENDIF Windows} // Disk hash form, needed only for Windows
  Application.CreateForm(TfrmDisplayGrid1, frmDisplayGrid1);
  Application.Run;
end.

