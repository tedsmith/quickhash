program quickhash;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazdbexport, Unit2, FindAllFilesEnhanced, diskmodule, uDisplayGrid,
  diskspecification, uProgress, frmAboutUnit, zvdatetimectrls, dbases_sqlite;

//{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

{$R *.res}

begin
  Application.Title:='QuickHash';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TfrmSQLiteDBases, frmSQLiteDBases);
  Application.CreateForm(TfrmAbout, frmAbout);
  Application.CreateForm(TfrmDisplayGrid1, frmDisplayGrid1);
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
  Application.Run;
end.


