unit uDisplayGrid;
{ A new unit added in QuickHash 2.6.3 to better enable the seperate display of
  results when copying files from Source to Destination folders. Allows the main
  form to contain treeview explorer style charts for source and destination.

  SQLite added with v3.0.0 and routines re-written accordingly
}
{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Menus, DBGrids, DbCtrls;

type

  { TfrmDisplayGrid1 }

  TfrmDisplayGrid1 = class(TForm)
    btnClipboardResults2: TButton;
    DBNavigator1: TDBNavigator;
    MenuItem_CopyToClipboard: TMenuItem;
    MenuItem_SaveDBToCSV: TMenuItem;
    MenuItem_ShowAllCOPYGRID: TMenuItem;
    MenuItem_SortByDestinationNameCOPYGRID: TMenuItem;
    MenuItem_SortBySourceNameCOPYGRID: TMenuItem;
    MenuItem_SortBySourceHashCOPYGRID: TMenuItem;
    MenuItem_SortByDestinationHashCOPYGRID: TMenuItem;
    RecursiveDisplayGrid_COPY: TDBGrid;
    frmDisplayGridPopupMenu: TPopupMenu;
    frmDisplayGridSaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnClipboardResults2Click(Sender: TObject);
    procedure MenuItem_CopyToClipboardClick(Sender: TObject);
    procedure MenuItem_SaveDBToCSVClick(Sender: TObject);
    procedure MenuItem_ShowAllCOPYGRIDClick(Sender: TObject);
    procedure MenuItem_SortByDestinationHashCOPYGRIDClick(Sender: TObject);
    procedure MenuItem_SortByDestinationNameCOPYGRIDClick(Sender: TObject);
    procedure MenuItem_SortBySourceHashCOPYGRIDClick(Sender: TObject);
    procedure MenuItem_SortBySourceNameCOPYGRIDClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmDisplayGrid1: TfrmDisplayGrid1;

implementation

{$R *.lfm}

uses
// New as of v3.0.0
dbases_sqlite;

{ TfrmDisplayGrid1 }

procedure TfrmDisplayGrid1.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  RecursiveDisplayGrid_COPY.Clear
end;

procedure TfrmDisplayGrid1.btnClipboardResults2Click(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(frmDisplayGrid1.RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_CopyToClipboardClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_SaveDBToCSVClick(Sender: TObject);
begin
  frmDisplayGridSaveDialog1.Title := 'Save results as...';
  frmDisplayGridSaveDialog1.InitialDir := GetCurrentDir;
  frmDisplayGridSaveDialog1.Filter := 'Comma Sep|*.csv';
  frmDisplayGridSaveDialog1.DefaultExt := 'csv';

  if frmDisplayGridSaveDialog1.Execute then
  begin
    frmSQLiteDBases.SaveDBToCSV(RecursiveDisplayGrid_COPY, frmDisplayGridSaveDialog1.FileName);
  end;
end;

procedure TfrmDisplayGrid1.MenuItem_ShowAllCOPYGRIDClick(Sender: TObject);
begin
  RecursiveDisplayGrid_COPY.Clear;
  frmSQLiteDBases.ShowAllCOPYGRID(RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_SortByDestinationHashCOPYGRIDClick(Sender: TObject);
begin
  RecursiveDisplayGrid_COPY.Clear;
  frmSQLiteDBases.SortByDestinationHash(RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_SortBySourceHashCOPYGRIDClick(Sender: TObject);
begin
  RecursiveDisplayGrid_COPY.Clear;
  frmSQLiteDBases.SortBySourceHash(RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_SortBySourceNameCOPYGRIDClick(Sender: TObject);
begin
  RecursiveDisplayGrid_COPY.Clear;
  frmSQLiteDBases.SortBySourceFilename(RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_SortByDestinationNameCOPYGRIDClick(Sender: TObject);
begin
  RecursiveDisplayGrid_COPY.Clear;
  frmSQLiteDBases.SortByDestinationFilename(RecursiveDisplayGrid_COPY);
end;


initialization

end.

