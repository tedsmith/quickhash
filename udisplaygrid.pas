unit uDisplayGrid;
{ A new unit added in QuickHash 2.6.3 to better enable the seperate display of
  results when copying files from Source to Destination folders. Allows the main
  form to contain treeview explorer style charts for source and destination.
}
{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Menus;

type

  { TfrmDisplayGrid1 }

  TfrmDisplayGrid1 = class(TForm)
    btnClipboardResults2: TButton;
    CopyAndHashGrid: TStringGrid;
    frmDisplayGridPopupMenu: TPopupMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    frmDisplayGridSaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnClipboardResults2Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDisplayGrid1: TfrmDisplayGrid1;

implementation

{ TfrmDisplayGrid1 }

procedure TfrmDisplayGrid1.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  CopyAndHashGrid.Clear;
end;

procedure TfrmDisplayGrid1.btnClipboardResults2Click(Sender: TObject);
begin
  try
    frmDisplayGrid1.CopyAndHashGrid.CopyToClipboard(false);
  finally
    ShowMessage('Grid content now in clipboard...Paste (Ctrl+V) into spreadsheet or text editor')
  end
end;

// Copy selected row to clipboard
procedure TfrmDisplayGrid1.MenuItem1Click(Sender: TObject);
begin
  CopyAndHashGrid.CopyToClipboard(true);
end;

// Copy the whole grid to clipboard
procedure TfrmDisplayGrid1.MenuItem2Click(Sender: TObject);
begin
    CopyAndHashGrid.CopyToClipboard(false);
end;

// Copy whole grid to TSV file
procedure TfrmDisplayGrid1.MenuItem3Click(Sender: TObject);
begin
  if frmDisplayGridSaveDialog1.Execute then
    begin
      CopyAndHashGrid.SaveToCSVFile(frmDisplayGridSaveDialog1.FileName);
    end;
end;


initialization
  {$I udisplaygrid.lrs}

end.

