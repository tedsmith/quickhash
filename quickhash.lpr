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

//{$IFDEF WINDOWS}{$R project1.rc}{$ENDIF}

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
  {$IFDEF UNIX and !$ifdef Darwin} // because Apple had to 'borrow' Unix for their OS!
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.CreateForm(TfrmTechSpecs, frmTechSpecs);
  {$ENDIF}
  {$ENDIF}
  Application.Run;
end.


