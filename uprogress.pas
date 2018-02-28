unit uProgress;
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
}

{$mode objfpc}{$H+} // {$H+} ensures strings are of unlimited size

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

