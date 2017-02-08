unit uProgress;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls;

type

  { TfrmProgress }

  TfrmProgress = class(TForm)
    btnAbortHashing: TButton;
    GroupBox1: TGroupBox;
    lblProgressTimeTaken: TLabel;
    lblProgressEndedAt: TLabel;
    lblProgressStartTime: TLabel;
    lblPercent: TLabel;
    lblResult: TLabel;
    lblStatus: TLabel;
    lblTotalBytesSource: TLabel;
    lblTotalBytesRead: TLabel;
    ProgressBar1: TProgressBar;
    procedure btnAbortHashingClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
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

procedure TfrmProgress.FormCreate(Sender: TObject);
begin
  // Reset labels if run again before exiting QuickHash
  lblProgressStartTime.Caption := 'Started at : ';
  lblProgressEndedAt.Caption   := 'Ended at: ';
  lblProgressTimeTaken.Caption := 'Time taken: ';
end;

procedure TfrmProgress.btnAbortHashingClick(Sender: TObject);
begin
  diskmodule.Stop := true; // Stops any further buffer reads
  frmDiskHashingModule.ledtComputedHashA.Text := 'Aborted';
  frmDiskHashingModule.ledtComputedHashB.Text := 'Aborted';
  frmDiskHashingModule.ledtComputedHashC.Text := 'Aborted';
  frmDiskHashingModule.ledtComputedHashD.Text := 'Aborted';
  frmProgress.Close;
end;

procedure TfrmProgress.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  lblStatus.Caption:= 'Aborting...';
  // Reset labels if run again before exiting QuickHash
  lblProgressStartTime.Caption := 'Started at : ';
  lblProgressEndedAt.Caption   := 'Ended at: ';
  lblProgressTimeTaken.Caption := 'Time taken: ';
end;

end.

