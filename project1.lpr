program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit2, sha1Customised, FindAllFilesEnhanced,
uDisplayGrid, diskmodule, diskspecification, uProgress;

//{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

{$R *.res}

begin
  Application.Title:='Quick Hash';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  {$IFDEF Windows}
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmTechSpecs, frmTechSpecs);
  {$ENDIF}
  {$IFDEF Darwin}
  // not availabkle for Mac
  {$else}
  {$IFDEF UNIX and !$ifdef Darwin} // because Apple had to 'borrow' Unix for their OS!
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmTechSpecs, frmTechSpecs);
  {$ENDIF}
  {$ENDIF}

  Application.CreateForm(TfrmDisplayGrid1, frmDisplayGrid1);
  Application.Run;
end.


