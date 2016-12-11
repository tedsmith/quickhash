unit diskspecification;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TfrmTechSpecs }

  TfrmTechSpecs = class(TForm)
    btnSaveTechnicalSpecs: TButton;
    Memo1: TMemo;
    sdTechSpecsFile: TSaveDialog;
    procedure btnSaveTechnicalSpecsClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmTechSpecs: TfrmTechSpecs;

implementation

{$R *.lfm}

{ TfrmTechSpecs }

procedure TfrmTechSpecs.btnSaveTechnicalSpecsClick(Sender: TObject);
begin
 sdTechSpecsFile.Execute;
 Memo1.Lines.SaveToFile(sdTechSpecsFile.FileName);
end;

end.

