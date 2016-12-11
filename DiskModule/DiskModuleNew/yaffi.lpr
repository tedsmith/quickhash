program diskhashingmodule;
// Based on my disk imager, YAFFI https://github.com/tedsmith/yaffi.
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, diskmodule,
  diskspecification, uProgress, uGPT, sha1Customised, md5customised;

{$R *.res}

begin
  Application.Title:='Quick Hash Disk Hashing Module';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmDiskHashingModule, frmDiskHashingModule);
  Application.CreateForm(TfrmTechSpecs, frmTechSpecs);
  Application.CreateForm(TfrmProgress, frmProgress);
  Application.Run;

end.

