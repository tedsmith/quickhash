unit frmAboutUnit;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    Button1: TButton;
    memAbout: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmAbout: TfrmAbout;

implementation

{ TfrmAbout }

procedure TfrmAbout.Button1Click(Sender: TObject);
begin
  frmAbout.Close;
end;

initialization
  {$I frmaboutunit.lrs}

end.

