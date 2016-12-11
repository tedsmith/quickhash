unit uProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type

  { TfrmProgress }

  TfrmProgress = class(TForm)
    GroupBox1: TGroupBox;
    lblPercent: TLabel;
    lblResult: TLabel;
    lblStatus: TLabel;
    lblTotalBytesSource: TLabel;
    lblTotalBytesRead: TLabel;
    ProgressBar1: TProgressBar;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmProgress: TfrmProgress;

implementation

uses
  diskmodule;

{$R *.lfm}

{ TfrmProgress }


procedure TfrmProgress.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  lblStatus.Caption:= 'Aborting...';
  frmDiskHashingModule.Stop:= true;
end;

end.

