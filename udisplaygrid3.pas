unit udisplaygrid3;
// Added for v3.2.0 to enable a grid like view of the results of the "Compare Two Folders"
// similar ot hat seen for the "Copy" tab.

{$mode objfpc}{$H+} // {$H+} ensures strings are of unlimited size

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, DBGrids,
  Menus, DBCtrls, StdCtrls, dbases_sqlite;
type
   // New as of v3.2.0 . Added to allow the display of "Compare Two Folders" results for users.
  { TfrmDisplayGrid3 }

  TfrmDisplayGrid3 = class(TForm)
    btnC2FClipboard: TButton;
    DBGrid_C2F: TDBGrid;
    DBNavigator_C2F: TDBNavigator;
    MenuItem_C2F_ShowDuplicates: TMenuItem;
    MenuItem_C2F_MissingFromFolderAAndFolderB: TMenuItem;
    MenuItem_C2F_MissingFolderAFiles: TMenuItem;
    MenuItem_C2F_MissingFolderBFiles: TMenuItem;
    MenuItem_C2F_ShowMatchingHashes: TMenuItem;
    MenuItem_C2F_ShowDiffHashes: TMenuItem;
    MenuItem_C2F_SelectedRowsC2FGRID: TMenuItem;
    MenuItem_C2F_ShowMismatches: TMenuItem;
    MenuItem_C2FToHTML: TMenuItem;
    MenuItem_C2F_SaveResultsCSV: TMenuItem;
    MenuItem_CopySelectedRowC2FGRID: TMenuItem;
    MenuItem_ClipboardAllRows: TMenuItem;
    MenuItem_ShowAll: TMenuItem;
    PopupMenu_C2FGrid: TPopupMenu;
    frmDisplayGrid3SaveDialog: TSaveDialog;
    procedure btnC2FClipboardClick(Sender: TObject);
    procedure MenuItem_C2F_MissingFolderAFilesClick(Sender: TObject);
    procedure MenuItem_C2F_MissingFolderBFilesClick(Sender: TObject);
    procedure MenuItem_C2F_MissingFromFolderAAndFolderBClick(Sender: TObject);
    procedure MenuItem_C2F_ShowDiffHashesClick(Sender: TObject);
    procedure MenuItem_C2F_SelectedRowsC2FGRIDClick(Sender: TObject);
    procedure MenuItem_C2F_ShowDuplicatesClick(Sender: TObject);
    procedure MenuItem_C2F_ShowMatchingHashesClick(Sender: TObject);
    procedure MenuItem_C2F_ShowMismatchesClick(Sender: TObject);
    procedure MenuItem_C2FToHTMLClick(Sender: TObject);
    procedure MenuItem_C2F_SaveResultsCSVClick(Sender: TObject);
    procedure MenuItem_ClipboardAllRowsClick(Sender: TObject);
    procedure MenuItem_CopySelectedRowC2FGRIDClick(Sender: TObject);
    procedure MenuItem_ShowAllClick(Sender: TObject);
  private

  public

  end;

var
  frmDisplayGrid3: TfrmDisplayGrid3;

implementation

{$R *.lfm}

{ TfrmDisplayGrid3 }

// Shows the entire "Compare Two Folders" DB grid, in case the user has filtered it
// and wants to see it again
procedure TfrmDisplayGrid3.MenuItem_ShowAllClick(Sender: TObject);
begin
  DBGrid_C2F.Clear;
  frmSQLiteDBases.ShowAllC2FGRID(DBGrid_C2F);
end;

// Copies the entire "Compare Two Folders" DB grid to clipboard via pop up menu option
procedure TfrmDisplayGrid3.MenuItem_ClipboardAllRowsClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(DBGrid_C2F);
end;

// Copies the entire "Compare Two Folders" DB grid to clipboard if user presses button
procedure TfrmDisplayGrid3.btnC2FClipboardClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(DBGrid_C2F);
end;

// Used by "Compare Two Folders" DB grid to show files that are missing from folder A
procedure TfrmDisplayGrid3.MenuItem_C2F_MissingFolderAFilesClick(Sender: TObject
  );
begin
  frmSQLiteDBases.ShowMissingFilesFolderA(DBGrid_C2F);
end;

// Used by "Compare Two Folders" DB grid to show files that are missing from folder B
procedure TfrmDisplayGrid3.MenuItem_C2F_MissingFolderBFilesClick(Sender: TObject
  );
begin
  frmSQLiteDBases.ShowMissingFilesFolderB(DBGrid_C2F);
end;
// Used by "Compare Two Folders" DB grid to show files that are missing from folder A or B
procedure TfrmDisplayGrid3.MenuItem_C2F_MissingFromFolderAAndFolderBClick(
  Sender: TObject);
begin
  frmSQLiteDBases.ShowMissingFromFolderAAndFolderB(DBGrid_C2F);
end;

// Used by "Compare Two Folders" DB grid to show files that have differing hashes
procedure TfrmDisplayGrid3.MenuItem_C2F_ShowDiffHashesClick(Sender: TObject);
begin
  frmSQLiteDBases.ShowDiffHashes(DBGrid_C2F);
end;

// Save the "Compare Two Folders" Window results pane to HTML File
procedure TfrmDisplayGrid3.MenuItem_C2FToHTMLClick(Sender: TObject);
var
  ExportFilename : string;
begin
  if frmSQLiteDBases.FC2Fquery = true then  // If "Show Duplicates" is active, do not allow HTML output
  begin
    ExportFilename := '';
    frmDisplayGrid3SaveDialog.Title := 'Save results as...';
    frmDisplayGrid3SaveDialog.InitialDir := GetCurrentDir;
    frmDisplayGrid3SaveDialog.Filter := 'Web Page|*.html';
    frmDisplayGrid3SaveDialog.DefaultExt := 'HTML';

    if frmDisplayGrid3SaveDialog.Execute then
    begin
      ExportFilename := frmDisplayGrid3SaveDialog.FileName;
      frmSQLiteDBases.SaveC2FWindowToHTML(DBGrid_C2F, ExportFilename);
    end;
  end
  else Showmessage('HTML output not possible with "Show Duplicates" active. Restore default view or use "Save to CSV" instead');
end;

// Show mis-matched values, where a hash was found in one folder but not the other
procedure TfrmDisplayGrid3.MenuItem_C2F_ShowMismatchesClick(Sender: TObject);
begin
  frmSQLiteDBases.ShowMismatchesC2F(DBGrid_C2F);
end;
// Save the "Compare Two Folders" results pane to CSV File
procedure TfrmDisplayGrid3.MenuItem_C2F_SaveResultsCSVClick(Sender: TObject);
var
  ExportFilename : string;
begin
  ExportFilename := '';
  frmDisplayGrid3SaveDialog.Title := 'Save results as...';
  frmDisplayGrid3SaveDialog.InitialDir := GetCurrentDir;
  frmDisplayGrid3SaveDialog.Filter := 'Comma Sep|*.csv';
  frmDisplayGrid3SaveDialog.DefaultExt := 'csv';

  if frmDisplayGrid3SaveDialog.Execute then
  begin
    ExportFilename := frmDisplayGrid3SaveDialog.FileName;
    frmSQLiteDBases.SaveC2FDBToCSV(DBGrid_C2F, ExportFilename);
  end;
end;

// Copies the selected row of "Compare Two Folders" DB grid to clipboard via pop up menu option
procedure TfrmDisplayGrid3.MenuItem_CopySelectedRowC2FGRIDClick(Sender: TObject
  );
begin
  frmSQLiteDBases.CopySelectedRowC2FTAB(DBGrid_C2F);
end;

// Copies the selected rowS (plural) of "Compare Two Folders" DB grid to clipboard via pop up menu option
procedure TfrmDisplayGrid3.MenuItem_C2F_SelectedRowsC2FGRIDClick(Sender: TObject);
begin
  frmSQLiteDBases.CopySelectedRowsC2FTAB(DBGrid_C2F);
end;

procedure TfrmDisplayGrid3.MenuItem_C2F_ShowDuplicatesClick(Sender: TObject);
begin
  frmSQLiteDBases.ShowDuplicatesC2FTAB(DBGrid_C2F);
end;

// Shows hashes that match in "Compare Two Folders" DB grid
procedure TfrmDisplayGrid3.MenuItem_C2F_ShowMatchingHashesClick(Sender: TObject
  );
begin
  frmSQLiteDBases.ShowMatchingHashes(DBGrid_C2F);
end;

initialization


end.

