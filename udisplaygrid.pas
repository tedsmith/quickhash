unit uDisplayGrid;
{ A new unit added in QuickHash 2.6.3 to better enable the seperate display of
  results when copying files from Source to Destination folders. Allows the main
  form to contain treeview explorer style charts for source and destination.

  SQLite added with v3.0.0 and routines re-written accordingly


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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  Grids, StdCtrls, Menus, DBGrids, DbCtrls;

type

  { TfrmDisplayGrid1 }

  TfrmDisplayGrid1 = class(TForm)
    btnClipboardResultsCOPYTAB: TButton;
    CopyTabDBNavigator: TDBNavigator;
    MenuItem_SaveDBToCSV: TMenuItem;
    MenuItem_CopySelectedRowCOPYGRID: TMenuItem;
    MenuItem_CopyToClipboard: TMenuItem;
    MenuItem_SaveDBToHTML: TMenuItem;
    MenuItem_ShowAllCOPYGRID: TMenuItem;
    MenuItem_SortByDestinationNameCOPYGRID: TMenuItem;
    MenuItem_SortBySourceNameCOPYGRID: TMenuItem;
    MenuItem_SortBySourceHashCOPYGRID: TMenuItem;
    MenuItem_SortByDestinationHashCOPYGRID: TMenuItem;
    RecursiveDisplayGrid_COPY: TDBGrid;
    frmDisplayGridPopupMenu: TPopupMenu;
    frmDisplayGridSaveDialog1: TSaveDialog;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure btnClipboardResultsCOPYTABClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MenuItem_SaveDBToCSVClick(Sender: TObject);
    procedure MenuItem_CopySelectedRowCOPYGRIDClick(Sender: TObject);
    procedure MenuItem_CopyToClipboardClick(Sender: TObject);
    procedure MenuItem_SaveDBToHTMLClick(Sender: TObject);
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

procedure TfrmDisplayGrid1.btnClipboardResultsCOPYTABClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(frmDisplayGrid1.RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.FormCreate(Sender: TObject);
begin

end;

procedure TfrmDisplayGrid1.MenuItem_CopySelectedRowCOPYGRIDClick(Sender: TObject);
begin
  frmSQLiteDBases.CopySelectedRowCOPYTAB(RecursiveDisplayGrid_COPY);
end;

procedure TfrmDisplayGrid1.MenuItem_CopyToClipboardClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(RecursiveDisplayGrid_COPY);
end;

// Save the COPY Window results pane to HTML File
procedure TfrmDisplayGrid1.MenuItem_SaveDBToHTMLClick(Sender: TObject);
var
  ExportFilename : string;
begin
  ExportFilename := '';
  frmDisplayGridSaveDialog1.Title := 'Save results as...';
  frmDisplayGridSaveDialog1.InitialDir := GetCurrentDir;
  frmDisplayGridSaveDialog1.Filter := 'Web Page|*.html';
  frmDisplayGridSaveDialog1.DefaultExt := 'HTML';

  if frmDisplayGridSaveDialog1.Execute then
  begin
    ExportFilename := frmDisplayGridSaveDialog1.FileName;
    frmSQLiteDBases.SaveCOPYWindowToHTML(RecursiveDisplayGrid_COPY, ExportFilename);
  end;
end;

 // Save the COPY Window results pane to CSV File
procedure TfrmDisplayGrid1.MenuItem_SaveDBToCSVClick(Sender: TObject);
var
  ExportFilename : string;
begin
  ExportFilename := '';
  frmDisplayGridSaveDialog1.Title := 'Save results as...';
  frmDisplayGridSaveDialog1.InitialDir := GetCurrentDir;
  frmDisplayGridSaveDialog1.Filter := 'Comma Sep|*.csv';
  frmDisplayGridSaveDialog1.DefaultExt := 'csv';

  if frmDisplayGridSaveDialog1.Execute then
  begin
    ExportFilename := frmDisplayGridSaveDialog1.FileName;
    frmSQLiteDBases.SaveDBToCSV(RecursiveDisplayGrid_COPY, ExportFilename);
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

