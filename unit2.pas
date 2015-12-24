{
    Quick Hash - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
    and generating hash values for them.

    The use of the word 'quick' refers to the ease in which the software operates
    in both Linux, Apple Mac and Windows (very few options
    to worry about, no syntax to remember etc) though tests suggest that in most
    cases the hash values are generated as quick or quicker than most mainstream
    tools, such as FTK Imager (Windows), 'EnCase' (Windows), md5sum, sha1sum,
    sha256sum and sha512sum (Linux).

    Benchmark tests are welcomed to test on across various platforms and architectures.

    Contributions from members at the Lazarus forums, Stackoverflow and other
    StackExchnage groups are welcomed and acknowledged. In particular, user Engkin
    (http://forum.lazarus.freepascal.org/index.php?action=profile;u=52702) who
    helped with the speeds of the SHA1 and MD5 implementations ENORMOUSLY, David Heferman of the
    Stackoverflow forums who helped me with the methodology of hashing in buffers
    and Taazz who pointed out where I'd made some daft mistakes. Thanks guys!

    Copyright (C) 2011-2015  Ted Smith https://sourceforge.net/users/tedtechnology

    NOTE: Date and time values, as computed in recursive directory hashing, are not
    daylight saving time adjusted. Source file date and time values are recorded.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version. You are not granted permission to create
    another disk or file hashing tool based on this code and call it 'QuickHash'.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You can read a copy of the GNU General Public License at
    http://www.gnu.org/licenses/>. Also, http://www.gnu.org/copyleft/gpl.html

}

unit Unit2; // Unit 1 was superseeded and related to pre v2.0.0

{$mode objfpc}{$H+} // {$H+} ensures strings are of unlimited size

interface

uses
  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}

    Classes, SysUtils, Strutils, FileUtil, LResources, Forms, Controls,
    Graphics, Dialogs, StdCtrls, Menus, ComCtrls, Grids, ExtCtrls, sysconst,
    LazUTF8Classes, lclintf, ShellCtrls, uDisplayGrid,

  FindAllFilesEnhanced, // an enhanced version of FindAllFiles, to ensure hidden files are found, if needed

  // we have to use a customised MD5 library to process Unicode on Windows and
  // to run a customised MD5Transform function
  md5customised,
  // we use a customised SHA1 library to process Unicode on Windows and to run
  // a customised SHA1Transform function
  sha1customised,

  // The DCPCrypt library, for some of the hashing algorithms used in certain
  // circumstances. SHA256 and SHA512 are not part of FPC:
    DCPsha512, DCPsha256, DCPsha1, DCPmd5,

  // Remaining Uses clauses for specific OS's
  {$IFDEF Windows}
    Windows,
    // For Windows, this is a specific disk hashing tab for QuickHash. Not needed for Linux
    DiskModuleUnit1;
  {$ENDIF}
  {$IFDEF Darwin}
    MacOSAll;
  {$else}
    {$IFDEF UNIX and !$ifdef Darwin} // because Apple had to 'borrow' Unix for their OS!
      UNIX;
    {$ENDIF}
  {$ENDIF}

type

  { TMainForm }

   TMainForm = class(TForm)
    AlgorithmChoiceRadioBox3: TRadioGroup;
    AlgorithmChoiceRadioBox4: TRadioGroup;
    AlgorithmChoiceRadioBox1: TRadioGroup;
    AlgorithmChoiceRadioBox6: TRadioGroup;
    AlgorithmChoiceRadioBox5: TRadioGroup;
    btnCompare: TButton;
    btnCompareTwoFiles: TButton;
    btnCompareTwoFilesSaveAs: TButton;
    btnDirA: TButton;
    btnDirB: TButton;
    btnFileACompare: TButton;
    btnFileBCompare: TButton;
    btnHashFile: TButton;
    btnRecursiveDirectoryHashing: TButton;
    btnClipboardResults: TButton;
    btnCallDiskHasherModule: TButton;
    btnClearTextArea: TButton;
    btnCopyToClipboardA: TButton;
    btnCopyToClipboardB: TButton;
    btnSaveComparisons: TButton;
    btnStopScan1: TButton;
    btnStopScan2: TButton;
    btnLBL: TButton;
    btnFLBL: TButton;
    Button8CopyAndHash: TButton;
    FileTypeMaskCheckBox2: TCheckBox;
    chkUNCMode: TCheckBox;
    chkHiddenFiles: TCheckBox;
    chkCopyHidden: TCheckBox;
    CheckBoxListOfDirsAndFilesOnly: TCheckBox;
    CheckBoxListOfDirsOnly: TCheckBox;
    chkFlagDuplicates: TCheckBox;
    chkNoRecursiveCopy: TCheckBox;
    chkNoPathReconstruction: TCheckBox;
    chkRecursiveDirOverride: TCheckBox;
    CopyFilesHashingGroupBox: TGroupBox;
    DirectoryHashingGroupBox: TGroupBox;
    DirSelectedField: TEdit;
    Edit2SourcePath: TEdit;
    Edit3DestinationPath: TEdit;
    FileHashingGroupBox: TGroupBox;
    edtFileNameToBeHashed: TEdit;
    FileMaskField: TEdit;
    FileMaskField2: TEdit;
    FileTypeMaskCheckBox1: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    gbDirectoryComparisons: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    lbleExpectedHash: TLabeledEdit;
    lbleExpectedHashText: TLabeledEdit;
    lblURLBanner: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblDirAName: TLabel;
    lblDirBName: TLabel;
    lblFileAHash: TLabel;
    lblFileAName: TLabel;
    lblFileBHash: TLabel;
    lblFileBName: TLabel;
    lblFilesCopiedPercentage: TLabel;
    lblDataCopiedSoFar: TLabel;
    lblHashMatchResult: TLabel;
    lblNoOfFilesToExamine2: TLabel;
    lblNoOfFilesToExamine: TLabel;
    lblPercentageComplete: TLabel;
    lblTotalBytesExamined: TLabel;
    lblFilesExamined: TLabel;
    lblNoFilesInDir: TLabel;
    lblDragAndDropNudge: TLabel;
    lblDiskHashingRunAsAdminWarning: TLabel;
    lblTimeTakenB: TLabel;
    lblTimeFinishedB: TLabel;
    lblTimeStartB: TLabel;
    lblTimeFinishedA: TLabel;
    lblTimeTakenA: TLabel;
    lblTimeStartA: TLabel;
    lblStatusB: TLabel;
    lblStatusA: TLabel;
    lblHashMatchB: TLabel;
    lblHashMatchA: TLabel;
    lblFileCountDiffA: TLabel;
    lblFileCountDiffB: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    lblTimeTaken6C: TLabel;
    lblTimeTaken5C: TLabel;
    lblTimeTaken6A: TLabel;
    lblTimeTaken6B: TLabel;
    lblTimeTaken5B: TLabel;
    lblTimeTaken5A: TLabel;
    lblTimeTaken4: TLabel;
    lblTimeTaken3: TLabel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    lblTimeTaken1: TLabel;
    lblTimeTaken2: TLabel;
    AlgorithmChoiceRadioBox2: TRadioGroup;
    lblTotalFileCountA: TLabel;
    lblTotalFileCountB: TLabel;
    lblTotalFileCountNumberA: TLabel;
    lblTotalFileCountNumberB: TLabel;
    memFileHashField: TMemo;
    FLBLDialog: TOpenDialog;
    SaveDialog5: TSaveDialog;
    SaveDialog6: TSaveDialog;
    SaveDialog7: TSaveDialog;
    SelectDirectoryDialog4: TSelectDirectoryDialog;
    SelectDirectoryDialog5: TSelectDirectoryDialog;
    sgDirB: TStringGrid;
    DirListA: TShellTreeView;
    DirListB: TShellTreeView;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    StatusBar3: TStatusBar;
    StatusBar4: TStatusBar;
    StrHashValue: TMemo;
    memoHashText: TMemo;
    NoOfFilesExamined: TEdit;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1CopyAndHashOptions: TPanel;
    PercentageComplete: TLabel;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    SaveDialog4: TSaveDialog;
    SaveToCSVCheckBox1: TCheckBox;
    SaveToCSVCheckBox2: TCheckBox;
    SaveToHTMLCheckBox1: TCheckBox;
    SaveToHTMLCheckBox2: TCheckBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SelectDirectoryDialog2: TSelectDirectoryDialog;
    SelectDirectoryDialog3: TSelectDirectoryDialog;
    RecursiveDisplayGrid1: TStringGrid;
    sgDirA: TStringGrid;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TextHashingGroupBox: TGroupBox;
    procedure AlgorithmChoiceRadioBox2SelectionChanged(Sender: TObject);
    procedure AlgorithmChoiceRadioBox5SelectionChanged(Sender: TObject);
    procedure btnClipboardHashValueClick(Sender: TObject);
    procedure btnCompareTwoFilesClick(Sender: TObject);
    procedure btnCompareTwoFilesSaveAsClick(Sender: TObject);
    procedure btnCopyToClipboardAClick(Sender: TObject);
    procedure btnCopyToClipboardBClick(Sender: TObject);
    procedure btnDirAClick(Sender: TObject);
    procedure btnDirBClick(Sender: TObject);
    procedure btnFileACompareClick(Sender: TObject);
    procedure btnFileBCompareClick(Sender: TObject);
    //procedure btnHashTextClick(Sender: TObject);
    procedure btnHashFileClick(Sender: TObject);
    procedure btnLaunchDiskModuleClick(Sender: TObject);
    procedure btnLBLClick(Sender: TObject);
    procedure btnRecursiveDirectoryHashingClick(Sender: TObject);
    procedure btnSaveComparisonsClick(Sender: TObject);
    procedure btnStopScan1Click(Sender: TObject);
    procedure btnClipboardResultsClick(Sender: TObject);
    procedure btnStopScan2Click(Sender: TObject);
    procedure btnCallDiskHasherModuleClick(Sender: TObject);
    procedure btnCompareClick(Sender: TObject);
    procedure btnClearTextAreaClick(Sender: TObject);
    procedure btnFLBLClick(Sender: TObject);
    procedure Button8CopyAndHashClick(Sender: TObject);
    procedure CheckBoxListOfDirsAndFilesOnlyChange(Sender: TObject);
    procedure CheckBoxListOfDirsOnlyChange(Sender: TObject);
    procedure chkUNCModeChange(Sender: TObject);
    procedure DirListAClick(Sender: TObject);
    procedure DirListBClick(Sender: TObject);
    procedure Edit2SourcePathEnter(Sender: TObject);
    procedure Edit3DestinationPathEnter(Sender: TObject);
    procedure FileTypeMaskCheckBox1Change(Sender: TObject);
    procedure FileTypeMaskCheckBox2Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure HashFile(FileIterator: TFileIterator);
    procedure lblURLBannerClick(Sender: TObject);
    procedure ProcessDir(const SourceDirName: string);
    procedure MisMatchFileCountCompare(HashListA, HashListB, FileAndHashListA, FileAndHashListB : TStringList);
    procedure CompareTwoHashes(FileAHash, FileBHash : string);
    procedure HashText(Sender: TObject);
    procedure ClearText(Sender: TObject);
    procedure MisMatchHashCompare(HashListA, HashListB, FileAndHashListA, FileAndHashListB : TStringList);
    function  CalcTheHashString(strToBeHashed:ansistring):string;
    function  CalcTheHashFile(FileToBeHashed:string):string;
    function  FormatByteSize(const bytes: QWord): string;
    function  RemoveLongPathOverrideChars(strPath : string; LongPathOverrideVal : string) : string;
    procedure SaveOutputAsCSV(Filename : string; GridName : TStringGrid);
    procedure EmptyDisplayGrid(Grid : TStringGrid);
    function FileSizeWithLongPath(strFileName : string) : Int64;
    {$IFDEF Windows}
    function DateAttributesOfCurrentFile(var SourceDirectoryAndFileName:string):string;
    function FileTimeToDTime(FTime: TFileTime): TDateTime;
    function GetSystemMem: string;  { Returns installed RAM (as viewed by your OS) in GB, with 2 decimals }

    {$ENDIF}
    {$IFDEF LINUX}
    function DateAttributesOfCurrentFileLinux(var SourceDirectoryAndFileName:string):string;
    {$ENDIF}
    {$ifdef UNIX}
      {$ifdef Darwin}
        function DateAttributesOfCurrentFileLinux(var SourceDirectoryAndFileName:string):string;
      {$ENDIF}
    {$ENDIF}
    function CustomisedForceDirectoriesUTF8(const Dir: string; PreserveTime: Boolean): Boolean;
    procedure SHA1RadioButton3Change(Sender: TObject);
    procedure TabSheet3ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);

  private
    { private declarations }
  public
    { public declarations }
   FileCounter, NoOfFilesInDir2: integer; // Used jointly by Button3Click and Hashfile procedures
   TotalBytesRead : UInt64;
   StopScan1, StopScan2, SourceDirValid, DestDirValid : Boolean;
   SourceDir, DestDir : string; // For the joint copy and hash routines

    DirA, DirB : string;
   sValue1 : string; // Set by GetWin32_DiskDriveInfo then used by ListDisks OnClick event - Windows only

  const
  {$IFDEF WINDOWS}
    // For coping better with 260 MAX_PATH limits of Windows. Instead we invoke Unicode
    // variant of FindAllFiles by using '\\?\' and '\\?\UNC\' prefixes
    LongPathOverride : string = '\\?\';
    // A and B below are for the Directory Comparison tab only
    LongPathOverrideA : string = '\\?\';
    LongPathOverrideB : string = '\\?\';

   {$else}
    {$IFDEF Darwin}
    const
      LongPathOverride : string = '';     // MAX_PATH is 4096 is Linux & Mac, so not needed
      LongPathOverrideA : string = '';
      LongPathOverrideB : string = '';
      {$else}
        {$IFDEF UNIX and !$ifdef Darwin}
        const
          LongPathOverride : string = '';
          LongPathOverrideA : string = '';
          LongPathOverrideB : string = '';
        {$ENDIF}
   {$ENDIF}
   {$ENDIF}
   end;

var
  MainForm: TMainForm;

implementation

{ TMainForm }


procedure TMainForm.FormCreate(Sender: TObject);
var
  x, y, z : integer;
begin
  x := screen.Width;
  y := screen.Height;

  if x < MainForm.Width then
    begin
       MainForm.Width := x - 50;
       frmDisplayGrid1.Width := MainForm.Width - 20;
     end;

  if y < MainForm.Height then
    begin
       Mainform.Height := y - 50;
       frmDisplayGrid1.Width := MainForm.Width - 20;
    end;

  StopScan1 := false;
  StopScan2 := false;

  btnCopyToClipboardA.Enabled := false;
  btnCopyToClipboardB.Enabled := false;

  // In Lazarus versions  < 1.4.4, the 'FileSortType' property of ShellTreeViews
  // would cause the listing to be doubled if anything other than fstNone was chosen
  // So this will ensure I have sorting until that is fixed.
  // http://bugs.freepascal.org/view.php?id=0028565
  DirListA.AlphaSort;
  DirListB.AlphaSort;

  // Better to have some folder icons for each node of the tree but
  // I can't work out at the moment how to do it for ever folder.
  // The following only does the root nodes.

  // Place a folder icon for each node of the trees
 { z := 0;
  for z := 0 to DirListA.Items.Count -1 do
  begin
    DirListA.Items[z].ImageIndex := 0; // Add the folder icon for each DirListA node
    if DirListA.Items[z].HasChildren then
      for y := 0 to DirListA.Items[y].Count do
      begin
        DirListA.Items[y].Index:=;
      end;
  end;

  z := 0;
  for z := 0 to DirListB.Items.Count -1 do
  begin
    DirListB.Items[z].ImageIndex := 0; // Add the folder icon for each DirListB node
  end;  }

  {$IFDEF WINDOWS}
    Label8.Caption        := '';
    chkCopyHidden.Enabled := false;
    chkCopyHidden.ShowHint:= true;
    chkCopyHidden.Hint:= 'On Windows, QuickHash finds hidden files and folders by default';
    // Remove the advice about using the File tab for hashing files.
    Label6.Caption := '';
  {$ENDIF}

  {$IFDEF LINUX}

   // For Linux users, we don't want them trying to use the Windows Disk hashing module
   // created for Windows users.
   btnCallDiskHasherModule.Enabled := false;
   Label8.Caption         := 'LINUX USERS - Hash disks using "File" tab and navigate to /dev/sdX or /dev/sdXX as root';
   Tabsheet5.Enabled      := true;
   Tabsheet5.Visible      := true;
   chkCopyHidden.Enabled  := true;
   chkCopyHidden.ShowHint := true;
   chkCopyHidden.Hint     := 'In Linux, tick this to ensure hidden directories and hidden files in them are detected, if you want them';

   // UNC mode is for Windows only so disable in Linux
   chkUNCMode.Enabled        := false;
   chkUNCMode.Visible        := false;
   Edit2SourcePath.Text      := 'Source directory selection';
   Edit3DestinationPath.Text := 'Destination directory selection';
  {$Endif}

 {$ifdef UNIX}
    {$ifdef Darwin}
      // For Apple Mac users, we don't want them trying to use the Windows Disk hashing module
      // created for Windows users.
      btnCallDiskHasherModule.Enabled := false;
      Tabsheet5.Enabled      := true;
      Tabsheet5.Visible      := true;
      Label8.Caption         := 'Apple Mac Users - Hash disks using "File" tab and navigate to /dev/sdX or /dev/sdXX as root';
      chkCopyHidden.Enabled  := true;
      chkCopyHidden.ShowHint := true;
      chkCopyHidden.Hint     := 'In Apple Mac, tick this to ensure hidden directories and hidden files in them are detected, if you want them';

      // UNC mode is for Windows only so disable in Apple Mac
      chkUNCMode.Enabled        := false;
      chkUNCMode.Visible        := false;
      Edit2SourcePath.Text      := 'Source directory selection';
      Edit3DestinationPath.Text := 'Destination directory selection';
    {$ENDIF}
 {$ENDIF}

end;
// FormDropFiles is the same as btnHashFileClick, except it disables the OpenDialog
// element and computes the filename from the drag n drop variable and hashes the file.
procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);

var
  filename : ansistring;
  fileHashValue : ansistring;
  start, stop, elapsed : TDateTime;

begin
  StatusBar1.SimpleText := '';

  // First, clear the captions from any earlier file hashing actions
  lblTimeTaken1.Caption := '';
  lblTimeTaken2.Caption := '';
  Label1.Caption := '';
  memFileHashField.Clear;

   begin
    filename := FileNames[0];
    if DirectoryExists(filename) then
    begin
      ShowMessage('Drag and drop of folders is not supported in this tab.');
    end
    else
    if FileExistsUTF8(filename) then
     begin
       start := Now;
       lblTimeTaken1.Caption := 'Started at  : '+ TimeToStr(Start);
       Tabsheet2.Show;
       edtFileNameToBeHashed.Caption := (filename);
       label1.Caption := 'Hashing file... ';
       StatusBar1.SimpleText := ' HASHING FILE...PLEASE WAIT';
       Application.ProcessMessages;
       fileHashValue := CalcTheHashFile(Filename); // Custom function
       memFileHashField.Lines.Add(UpperCase(fileHashValue));
       label1.Caption := 'Complete.';
       StatusBar1.SimpleText := ' HASHING COMPLETE!';
       OpenDialog1.Close;

       stop := Now;
       elapsed := stop - start;
       lblTimeTaken2.Caption := 'Time taken : '+ TimeToStr(elapsed);
       Application.ProcessMessages;
     end
    else
      ShowMessage('An error occured opening the file. Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
   end;
end;


procedure TMainForm.HashText(Sender: TObject);
var
  strHashValueText : string;
begin
  if Length(memoHashText.Text) = 0 then
    begin
      StrHashValue.Caption := 'Awaiting input in text field...';
    end
    else
      begin
       strHashValueText := Trim(Uppercase(CalcTheHashString(memoHashText.Text)));
       StrHashValue.Caption := strHashValueText;

       if lbleExpectedHashText.Text <> '...' then
       begin
       if strHashValueText = Trim(Uppercase(lbleExpectedHashText.Text)) then
         begin
           Showmessage('Expected hash matches the generated text hash, OK');
         end
       else
         begin
           Showmessage('Expected hash DOES NOT match the generated text hash!');
         end;
      end;
    end;
end;

procedure TMainForm.btnHashFileClick(Sender: TObject);
var
  filename : string;
  fileHashValue : ansistring;
  start, stop, elapsed : TDateTime;

begin
  StatusBar1.SimpleText := '';
  if OpenDialog1.Execute then
    begin
      filename := OpenDialog1.Filename;
    end;
  // First, clear the captions from any earlier file hashing actions
  lblTimeTaken1.Caption := '';
  lblTimeTaken2.Caption := '';
  Label1.Caption := '';
  memFileHashField.Clear;

  if FileExistsUTF8(filename) then
  begin
    start := Now;
    lblTimeTaken1.Caption := 'Started at  : '+ FormatDateTime('dd/mm/yy hh:mm:ss', Start);

    edtFileNameToBeHashed.Caption := (filename);
    label1.Caption := 'Hashing file... ';
    StatusBar1.SimpleText := ' H A S H I N G  F I L E...P L E A S E  W A I T';
    Application.ProcessMessages;
    fileHashValue := CalcTheHashFile(Filename); // Custom function
    memFileHashField.Lines.Add(UpperCase(fileHashValue));
    label1.Caption := 'Complete.';
    StatusBar1.SimpleText := ' H A S H I N G  C OM P L E T E !';

    OpenDialog1.Close;

    stop := Now;
    elapsed := stop - start;

    // If the user has ane existing hash to check, compare it here
    if lbleExpectedHash.Text <> '...' then
    begin
      if Uppercase(fileHashValue) = Trim(Uppercase(lbleExpectedHash.Text)) then
        begin
          Showmessage('Expected hash matches the computed file hash, OK');
        end
    else
      begin
        Showmessage('Expected hash DOES NOT match the computed file hash!');
      end;
    end;

    lblTimeTaken2.Caption := 'Time taken : '+ TimeToStr(elapsed);
    Application.ProcessMessages;
  end
  else
    ShowMessage('An error occured opening the file. Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
  end;

procedure TMainForm.btnLaunchDiskModuleClick(Sender: TObject);
begin

end;

// Hash each line of text in the input area line-by-line and save to output CSV file
// Useful for users who need to generate hashes of lists of names or e-mail addresses
// Google Adsense, for example, requires this as a SHA256. So QuickHash will enable this
// https://support.google.com/adwords/answer/6276125?hl=en-GB
procedure TMainForm.btnLBLClick(Sender: TObject);
var
  slLBL : TStringList;
  i     : Longword;
begin
  try
    slLBL := TStringList.Create;
    for i := 0 to memoHashText.Lines.Count -1 do
      begin
        slLBL.Add(memoHashText.Lines[i] + ',' + Trim(CalcTheHashString(memoHashText.Lines[i])));
      end;
  finally
    SaveDialog7.Title := 'Save line-by-line results as...';
    SaveDialog7.InitialDir := GetCurrentDir;
    SaveDialog7.Filter := 'Comma Sep|*.csv';
    SaveDialog7.DefaultExt := 'csv';

    if SaveDialog7.Execute then
      begin
        slLBL.SaveToFile(SaveDialog7.FileName)
      end
    else ShowMessage('Unable to save file ' + SaveDialog7.FileName);
    slLBL.Free;
  end;
end;

// Load a text file and hash it line by line, outputting to the memo field
// Similar to the procedure for hashing the memo line-by-line in "Text" tab
procedure TMainForm.btnFLBLClick(Sender: TObject);
var
  slFLBLInput, slFLBLOutput : TStringList; // FLBL = File, Line by Line
  i : Longword;
begin
  i := 0;
  ShowMessage(GetSystemMem);
  if FLBLDialog.Execute then
    begin
      try
      slFLBLInput := TStringList.Create;
      slFLBLOutput:= TStringList.Create;
      slFLBLInput.Sorted:= false;
      slFLBLOutput.Sorted:= false;

      // Load the input file to memory
      slFLBLInput.LoadFromFile(FLBLDialog.FileName);

      // Write the input to a new stringlist, hash each line
      for i := 0 to slFLBLInput.Count -1 do
        begin
          slFLBLOutput.Add(slFLBLInput.Strings[i] + ',' + Trim(CalcTheHashString(slFLBLInput.Strings[i])));
        end;

      // If the putput is smaller than 30Mb, load it to the screen
      if Length(slFLBLOutput.Text) < 30000000 then
        begin
        for i := 0 to slFLBLOutput.Count -1 do
          begin
            memoHashText.Lines.Add(slFLBLOutput.Strings[i]);
          end;
        memoHashText.Clear;
        memoHashText.Perform(EM_SCROLLCARET, 0, i);
        end
      else
      // Otherwise, just save it and be done with it
        begin
          memoHashText.Clear;
          memoHashText.Lines.Add('Data set too large for display. Save the output file');
        end;
      finally
        SaveDialog7.Title      := 'Save files line-by-line results as...';
        SaveDialog7.InitialDir := GetCurrentDir;
        SaveDialog7.Filter     := 'Comma Sep|*.csv';
        SaveDialog7.DefaultExt := 'csv';
        SaveDialog7.FileName   := 'Output';
        if SaveDialog7.Execute then
          begin
            slFLBLOutput.SaveToFile(SaveDialog7.FileName)
          end
        else ShowMessage('Unable to save output file ' + SaveDialog7.FileName);
        slFLBLOutput.Free;
        slFLBLInput.Free;
      end;
    end
  else ShowMessage('Unable to open text file for line-by-line analysis');
end;

// http://stackoverflow.com/questions/7859978/get-total-and-available-memory-when-4-gb-installed
function TMainForm.GetSystemMem: string;  { Returns installed RAM (as viewed by your OS) in Gb\Tb}
VAR
  MS_Ex : MemoryStatus;
begin
 FillChar (MS_Ex, SizeOf(MemoryStatus), #0);
 MS_Ex.dwLength := SizeOf(MemoryStatus);
 GlobalMemoryStatus(MS_Ex);
 //Result:= FormatByteSize(MS_Ex.dwAvailVirtual);
 Result:= FormatByteSize(MS_Ex.dwTotalPhys);
end;

// Procedure SaveOutputAsCSV
// An object orientated way to save any given display grid to CSV to save me
// retyping the same code over and over!
procedure TMainForm.SaveOutputAsCSV(Filename : string; GridName : TStringGrid);
var
  slTmpLoadedFromOutput : TStringLIst;
  InsTextPos : integer;
begin
  // Here we save the grid to CSV, then load into memory, insert
  // the title line and version number of QuickHash, then save it
  // back to CSV. A bit round the houses but best I can think of for now
  InsTextPos := 0;
  try
    GridName.SaveToCSVFile(FileName);
    slTmpLoadedFromOutput := TStringLIst.Create;
    slTmpLoadedFromOutput.LoadFromFile(FileName);
    slTmpLoadedFromOutput.Insert(InsTextPos, MainForm.Caption + '. Log generated: ' + DateTimeToStr(Now));
  finally
    // Write back the original data with the newly inserted first line
    slTmpLoadedFromOutput.SaveToFile(FileName);
    slTmpLoadedFromOutput.Free;
  end;
end;

// Procedure btnRecursiveDirectoryHashingClick
// Finds the files in a directory and hashes them, recursively by default
procedure TMainForm.btnRecursiveDirectoryHashingClick(Sender: TObject);

var
  DirToHash, CSVLogFile, HTMLLogFile1, SearchMask : string;
  FS                                              : TFileSearcher;
  TotalFilesToExamine, slDuplicates               : TStringList;
  start, stop, elapsed                            : TDateTime;
  j, i, DuplicatesDeleted                         : integer;
  DeleteResult                                    : Boolean;

  begin
  FileCounter                   := 1;
  TotalBytesRead                := 0;
  DuplicatesDeleted             := 0;
  lblTimeTaken3.Caption         := '...';
  lblTimeTaken4.Caption         := '...';
  lblFilesExamined.Caption      := '...';
  lblPercentageComplete.Caption := '...';
  lblTotalBytesExamined.Caption := '...';

  slDuplicates := TStringList.Create;
  slDuplicates.Sorted := true;

  if SelectDirectoryDialog1.Execute then
    begin
      DirSelectedField.Caption := SelectDirectoryDialog1.FileName;
      DirToHash := SelectDirectoryDialog1.FileName;

      {$ifdef Windows}
      // If chosen path is a UNC path, we need to append the UNC prefix to the
      // Unicode 32K long API call of \\?\
      if (Pos('\\', DirToHash) > 0) then
      begin
        LongPathOverride := '\\?\UNC\';
        Delete(DirToHash, 1, 2); // Delete the \\ from the DirToHash path (otherwise it becomes '\\?\UNC\\\')
      end;
      {$endif}

      RecursiveDisplayGrid1.Visible := false;
      RecursiveDisplayGrid1.rowcount := 0;
      // Check selected dir exists. If it does, start the process.
      if DirPathExists(LongPathOverride+DirToHash) then
        begin
        // Now lets recursively count each file,
         start := Now;
         lblTimeTaken3.Caption := 'Started: '+ FormatDateTime('dd/mm/yy hh:mm:ss', Start);
         StatusBar2.SimpleText := ' C O U N T I N G  F I L E S...P L E A S E  W A I T   A   M O M E N T ...';
         Label5.Visible        := true;
         Application.ProcessMessages;

         // By default, the recursive dir hashing will hash all files of all sub-dirs
         // from the root of the chosen dir. If the box is ticked, the user just wants
         // to hash the files in the root of the chosen dir.

         if chkRecursiveDirOverride.Checked then   // User does NOT want recursive
           begin
             if chkHiddenFiles.Checked then        // ...but does want hidden files
               begin
                 TotalFilesToExamine := FindAllFilesEx(LongPathOverride+DirToHash, '*', False, True);
               end
             else                                  // User does not want hidden
               begin
                 TotalFilesToExamine := FindAllFiles(LongPathOverride+DirToHash, '*', False);
               end;
           end
         else
           begin                                  // User DOES want recursive
             if chkHiddenFiles.Checked then         // ...and he wants hidden
               begin
                 TotalFilesToExamine := FindAllFilesEx(LongPathOverride+DirToHash, '*', true, true);
               end
             else                                  // ...but not want hidden
               begin
                 TotalFilesToExamine := FindAllFiles(LongPathOverride+DirToHash, '*', true);
               end;
           end;
         lblNoFilesInDir.Caption := IntToStr(TotalFilesToExamine.count);
         NoOfFilesInDir2 := StrToInt(lblNoFilesInDir.Caption);  // A global var
         RecursiveDisplayGrid1.rowcount := TotalFilesToExamine.Count +1;
         Application.ProcessMessages;

         // Create and assign a File Searcher instance and dictate its behaviour.
         // Then hash each file accordingly.
         try
           FS := TFileSearcher.Create;

           // Set parameters for searching for hidden or non-hidden files and dirs
           // and (since v2.6.4) with a mask, or not
           if chkHiddenFiles.Checked then
             begin
               if FileTypeMaskCheckBox2.Checked then FS.MaskSeparator:= ';';
               FS.DirectoryAttribute := faAnyFile or faHidden;
               FS.FileAttribute := faAnyFile or faHidden;
               FS.OnFileFound := @HashFile;
             end
           else
             begin
               if FileTypeMaskCheckBox2.Checked then FS.MaskSeparator:= ';';
               FS.FileAttribute := faAnyFile;
               FS.OnFileFound := @HashFile;
             end;

           // Set parameters for searching recursivley or not, and (since v2.6.4)
           // with a mask, or not
           if chkRecursiveDirOverride.Checked then
             begin
               if FileTypeMaskCheckBox2.Checked then
                 SearchMask := FileMaskField2.Text
                 else SearchMask := '';
               FS.Search(LongPathOverride+DirToHash, SearchMask, False, False);
             end
           else
             begin
               if FileTypeMaskCheckBox2.Checked then
                 SearchMask := FileMaskField2.Text
                 else SearchMask := '';
               FS.Search(LongPathOverride+DirToHash, SearchMask, True, False);
             end;
         finally
           // Hashing complete. Now free resources
           FS.Free;
           TotalFilesToExamine.Free;
         end;

         {  Now that the data is all computed, display the grid in the GUI.
            We have this hidden during processing for speed purposes.
            In a test, 3K files took 3 minutes with the grid display refreshed for each file.
            With the grid hidden until this point though, the same files took just 12 seconds! }

         RecursiveDisplayGrid1.Visible := true;

         // Now traverse the grid for duplicate hash entries, if the user wishes to

         if chkFlagDuplicates.Checked then
           begin
           RecursiveDisplayGrid1.SortOrder := soAscending;
           RecursiveDisplayGrid1.SortColRow(true, 3, RecursiveDisplayGrid1.FixedRows, RecursiveDisplayGrid1.RowCount - 1);
            for i := 1 to RecursiveDisplayGrid1.RowCount -1 do
            begin
              if RecursiveDisplayGrid1.Cells[3, i] = RecursiveDisplayGrid1.Cells[3, i-1] then
                begin
                 RecursiveDisplayGrid1.Cells[5, i] := 'Yes, of file ' + RecursiveDisplayGrid1.Cells[1,i-1];
                 slDuplicates.Add(RecursiveDisplayGrid1.Cells[2,i-1] + RecursiveDisplayGrid1.Cells[1, i-1]);
                end;
            end;
            slDuplicates.Sort;
           end;

         // and conclude timings and update display
         stop := Now;
         elapsed := stop - start;
         lblTimeTaken4.Caption := 'Time taken : '+ TimeToStr(elapsed);
         StatusBar2.SimpleText := ' DONE! ';
         btnClipboardResults.Enabled := true;

        // Now output the complete StringGrid to a csv text file

        // FYI, RecursiveDisplayGrid1.Cols[X].savetofile('/home/ted/test.txt'); is good for columns
        // RecursiveDisplayGrid1.Rows[X].savetofile('/home/ted/test.txt'); is good for rows

         if SaveToCSVCheckBox1.Checked then
           begin
             SaveDialog1.Title := 'Save your CSV text log file as...';
             SaveDialog1.InitialDir := GetCurrentDir;
             SaveDialog1.Filter := 'Comma Sep|*.csv|Text file|*.txt';
             SaveDialog1.DefaultExt := 'csv';
             if SaveDialog1.Execute then
               begin
                SaveOutputAsCSV(SaveDialog1.FileName, RecursiveDisplayGrid1);
               end;
           end;

         // And\Or, output the complete StringGrid to a HTML file

         if SaveToHTMLCheckBox1.Checked then
           begin
           SaveDialog2.Title := 'Save your HTML log file as...';
           SaveDialog2.InitialDir := GetCurrentDir;
           SaveDialog2.Filter := 'HTML|*.html';
           SaveDialog2.DefaultExt := 'html';
           if SaveDialog2.Execute then
             begin
               i := 0;
               j := 0;
               HTMLLogFile1 := SaveDialog2.FileName;
               with TStringList.Create do
               try
                 Add('<html>');
                 Add('<title>QuickHash HTML Output</title>');
                 Add('<body>');
                 Add('<br />');
                 Add('<p><strong>' + MainForm.Caption + '. ' + 'Log Created: ' + DateTimeToStr(Now)+'</strong></p>');
                 Add('<p><strong>File and Hash listing for: ' + DirToHash + '</strong></p>');
                 Add('<table border=1>');
                 Add('<tr>');
                 Add('<td>' + 'ID');
                 Add('<td>' + 'File Name');
                 Add('<td>' + 'File Path');
                 Add('<td>' + 'Hash');
                 Add('<td>' + 'Size');
                 for i := 0 to RecursiveDisplayGrid1.RowCount-1 do
                   begin
                     Add('<tr>');
                     for j := 0 to RecursiveDisplayGrid1.ColCount-1 do
                       Add('<td>' + RecursiveDisplayGrid1.Cells[j,i] + '</td>');
                       add('</tr>');
                   end;
                 Add('</table>');
                 Add('</body>');
                 Add('</html>');
                 SaveToFile(HTMLLogFile1);
               finally
                 Free;
                 HTMLLogFile1 := '';
               end;
             end;
          end;
        end
        else
        begin
          ShowMessage('Invalid directory selected' + sLineBreak + 'You must select a directory. Error code : ' + SysErrorMessageUTF8(GetLastOSError));
        end;
    end;

  // Now see if the user wishes to delete any found duplicates
  if chkFlagDuplicates.Checked then
    begin
      if slDuplicates.Count > 0 then
        if MessageDlg(IntToStr(slDuplicates.Count) + ' duplicate files were found. Delete them now?', mtConfirmation,
          [mbCancel, mbNo, mbYes],0) = mrYes then
            begin
              for i := 0 to (slDuplicates.Count -1) do
                begin
                  StatusBar2.SimpleText:= 'Deleting duplicate file ' + slDuplicates.Strings[i];
                  StatusBar2.Refresh;
                  if SysUtils.DeleteFile(slDuplicates.Strings[i]) then
                    inc(DuplicatesDeleted);
                end;
              StatusBar2.SimpleText:= 'Finished deleting ' + IntToStr(DuplicatesDeleted) + ' duplicate files';
              StatusBar2.Refresh;
              ShowMessage(IntToStr(DuplicatesDeleted) + ' duplicate files deleted.');
            end;
      slDuplicates.Free;  // this needs to be freed, regardless of whether it contained any entries or not
    end; // end of duplicate deletion phase
end;

procedure TMainForm.btnSaveComparisonsClick(Sender: TObject);
var
  slHTMLOutput : TStringList;
  HTMLLogFile3 : string;
  i, j         : integer;
begin
  SaveDialog6.Title := 'Save Grid A as CSV log file as...';
  SaveDialog6.InitialDir := GetCurrentDir;
  SaveDialog6.Filter := 'Comma Sep|*.csv|Text file|*.txt';
  SaveDialog6.DefaultExt := 'csv';
  ShowMessage('You will now be prompted to save two seperate CSV files and one combined HTML file...');

  if SaveDialog6.Execute then
    begin
      SaveOutputAsCSV(SaveDialog6.FileName, sgDirA);
    end;

  SaveDialog6.Title := 'Save Grid B as CSV log file as...';
  if SaveDialog6.Execute then
    begin
      SaveOutputAsCSV(SaveDialog6.FileName, sgDirB);
    end;

  // HTML Output
  SaveDialog6.Title := 'Save Grids A and B as HTML log file as...';
  SaveDialog6.InitialDir := GetCurrentDir;
  SaveDialog6.Filter := 'HTML|*.html';
  SaveDialog6.DefaultExt := 'html';
  if SaveDialog6.Execute then
    begin
      HTMLLogFile3 := SaveDialog6.FileName;
      slHTMLOutput := TStringList.Create;
      try
       slHTMLOutput.Add('<html>');
       slHTMLOutput.Add('<title>QuickHash HTML Output</title>');
       slHTMLOutput.Add('<body>');
       slHTMLOutput.Add('<br />');
       slHTMLOutput.Add('<p><strong>' + MainForm.Caption + '. ' + 'Log Created: ' + DateTimeToStr(Now)+'</strong></p>');
       slHTMLOutput.Add('<p><strong>File and Hash Comparisons of ' + lblDirAName.Caption + ' and ' + lblDirBName.Caption + '</strong></p>');

       // Grid A content to HTML

       slHTMLOutput.Add('<p>Table A</p>');
       slHTMLOutput.Add('<table border=1>');
       slHTMLOutput.Add('<tr>');
       slHTMLOutput.Add('<td>' + 'ID');
       slHTMLOutput.Add('<td>' + 'File Path & Name');
       slHTMLOutput.Add('<td>' + 'Hash');

       i := 0;
       j := 0;
       for i := 0 to sgDirA.RowCount-1 do
         begin
           slHTMLOutput.Add('<tr>');
           for j := 0 to sgDirA.ColCount-1 do
             slHTMLOutput.Add('<td>' + sgDirA.Cells[j,i] + '</td>');
             slHTMLOutput.Add('</tr>');
         end;
       slHTMLOutput.Add('</table>');
       slHTMLOutput.Add('<p>Total Files : ' + IntToStr(sgDirA.Rowcount -1) + '</p>');

       // Grid B content to HTML

       slHTMLOutput.Add('<p>Table B</p>');
       slHTMLOutput.Add('<table border=1>');
       slHTMLOutput.Add('<tr>');
       slHTMLOutput.Add('<td>' + 'ID');
       slHTMLOutput.Add('<td>' + 'File Path & Name');
       slHTMLOutput.Add('<td>' + 'Hash');
       slHTMLOutput.Add('</tr>');

       i := 0;
       j := 0;
       for i := 0 to sgDirB.RowCount-1 do
         begin
           slHTMLOutput.Add('<tr>');
           for j := 0 to sgDirB.ColCount-1 do
             slHTMLOutput.Add('<td>' + sgDirB.Cells[j,i] + '</td>');
             slHTMLOutput.Add('</tr>');
         end;
       slHTMLOutput.Add('</table>');
       slHTMLOutput.Add('<p>Total Files : ' + IntToStr(sgDirB.Rowcount -1) + '</p>');
       slHTMLOutput.Add('<p>Result? : ' + lblHashMatchB.Caption + '</p>');
       slHTMLOutput.Add('</body>');
       slHTMLOutput.Add('</html>');
       slHTMLOutput.SaveToFile(HTMLLogFile3);
      finally
       slHTMLOutput.Free;
       HTMLLogFile3 := '';
      end;
    end; // End of Savedialog6.Execute for HTML
end;


procedure TMainForm.btnClipboardResultsClick(Sender: TObject);
begin
  try
    RecursiveDisplayGrid1.CopyToClipboard();
  finally
    ShowMessage('Grid content now in clipboard...Paste (Ctrl+V) into spreadsheet or text editor')
  end
end;

procedure TMainForm.btnStopScan1Click(Sender: TObject);
begin
  StopScan1 := TRUE;
  if StopScan1 = TRUE then
  begin
    Abort;
  end;
end;

procedure TMainForm.btnStopScan2Click(Sender: TObject);
begin
  StopScan2 := TRUE;
  if StopScan2 = TRUE then
  begin
    Abort;
  end;
end;


// Calls the 'frmDiskHashingModule' Form used for disk hashing in Windows.
// It also clears all labels from any previous runs of the form.
procedure TMainForm.btnCallDiskHasherModuleClick(Sender: TObject);
begin

{$ifdef Windows}
  DiskModuleUnit1.frmDiskHashingModule.lbledtStartAtTime.Text := 'HH:MM';
  DiskModuleUnit1.frmDiskHashingModule.ListBox1.Clear;
  DiskModuleUnit1.frmDiskHashingModule.lblSpeedB.Caption             := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblDiskNameB.Caption          := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblByteCapacityB.Caption      := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblBytesLeftToHashB.Caption   := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblStartTimeB.Caption         := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblEndTimeB.Caption           := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblSpeedB.Caption             := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblModelB.Caption             := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblMediaTypeB.Caption         := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblInterfaceB.Caption         := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblSectorsB.Caption           := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblTimeTakenB.Caption         := '...';
  DiskModuleUnit1.frmDiskHashingModule.lblManufacturerB.Caption      := '...';
  DiskModuleUnit1.frmDiskHashingModule.edtComputedHash.Text          := '...';
  DiskModuleUnit1.frmDiskHashingModule.Show;
  {$Endif}
end;

// RemoveLongPathOverrideChars : The long path override prefix will be either:
// '\\?\' or
// '\\?\UNC\'
// For display purposes in the grid, it needs to be removed. This function
// simply does that and returns the sanatised pathname without this API noise.
function TMainForm.RemoveLongPathOverrideChars(strPath : string; LongPathOverrideVal : string) : string;
begin
  result := '';
  if LongPathOverrideVal = '\\?\' then
  begin
    // Delete the UNC API prefix of '\\?\' from the display
    result := Copy(strPath, 5, (Length(strPath) - 3));
  end
  else if LongPathOverrideVal = '\\?\UNC\' then
  begin
    // Delete the UNC API prefix and restore the UNC path chars of '\\'
    result := '\' + Copy(strPath, 8, (Length(strPath) - 7));
  end
end;

 // btnCompareClick : Will compare the listings of two directories, inc hidden files
 // The user is not presented with a choice for hiddne files because a comparison
 // of directories must be an exacting process.
procedure TMainForm.btnCompareClick(Sender: TObject);

var
  FilePath, FileName, FullPathAndName, FileHashA, FileHashB,
    HashOfListA, HashOfListB, Mismatch, s, strTimeTaken, strTimeDifference : string;

  TotalFilesDirA, TotalFilesDirB,       // Stringlists just for the file names
    HashListA, HashListB,               // Stringlists just for the hashes of each file in each directory
    FileAndHashListA, FileAndHashListB, // Stringlists for the combined lists of both hashes with filenames
    MisMatchList : TStringList;

  i : integer;

  StartTime, EndTime, TimeTaken : TDateTime;

begin
  // Initialise vars and display captions, to ensure any previous runs are cleared
  i                                := 0;
  DirA                             := lblDirAName.Caption;
  DirB                             := lblDirBName.Caption;

  {$ifdef Windows}
  // Check if a UNC server path is given for either DirA or DirB.
  // If so, adjust LongPathOverride and append the UNC prefix to ensure that both
  // the 32K path length limit and the UNC rules are adhered to

  if (Pos('\\', lblDirAName.Caption) > 0) then
  begin
    LongPathOverrideA := '\\?\UNC\';
    Delete(DirA, 1, 2); // Delete the \\ from the DirA path
  end;

  if (Pos('\\', lblDirBName.Caption) > 0) then
  begin
    LongPathOverrideB := '\\?\UNC\';
    Delete(DirB, 1, 2); // Delete the \\ from the DirB path
  end;
  {$endif}

  StartTime                        := Now;
  sgDirA.Clean;
  sgDirB.Clean;
  lblTotalFileCountNumberA.Caption := '...';
  lblTotalFileCountNumberB.Caption := '...';
  lblFileCountDiffB.Caption        := '...';
  lblHashMatchB.Caption            := '...';
  lblTimeStartB.Caption            := FormatDateTime('dd/mm/yy hh:mm:ss', StartTime);
  lblTimeFinishedB.Caption         := 'Please wait...';
  lblTimeTakenB.Caption            := '...';
  lblTimeStartB.Refresh;
  lblTimeFinishedB.Refresh;

  try
    // First, list and hash the files in DirA
    lblStatusB.Caption      := 'Counting files in ' + DirA + ' ...please wait';
    TotalFilesDirA          := TStringList.Create;
    TotalFilesDirA.Sorted   := true;
    TotalFilesDirA          := FindAllFilesEx(LongPathOverrideA+DirA, '*', True, True);
    TotalFilesDirA.Sort;
    sgDirA.RowCount         := TotalFilesDirA.Count + 1;
    HashListA               := TStringList.Create;
    FileAndHashListA        := TStringList.Create;
    HashListA.Sorted        := true;
    FileAndHashListA.Sorted := true;

    lblStatusB.Caption      := 'Examining files in ' + DirA + ' ...please wait';
    Application.ProcessMessages;

    for i := 0 to TotalFilesDirA.Count -1 do
      begin
        FilePath            := ExtractFilePath(TotalFilesDirA.Strings[i]);
        FileName            := ExtractFileName(TotalFilesDirA.Strings[i]);
        FullPathAndName     := FilePath + FileName;
        FileHashA           := CalcTheHashFile(FullPathAndName);
        HashListA.Add(FileHashA);
        FileAndHashListA.Add(FullPathAndName + ':' + FileHashA + ':');
        // Populate display grid for DirA
        sgDirA.Cells[0, i+1] := IntToStr(i+1);
        {$IFDEF Windows}
          sgDirA.Cells[1, i+1] := RemoveLongPathOverrideChars(FullPathAndName, LongPathOverrideB);
        {$ENDIF}
        {$IFDEF Darwin}
           sgDirA.Cells[1, i+1] := FullPathAndName;
        {$else}
          {$IFDEF UNIX and !$ifdef Darwin} // because Apple had to 'borrow' Unix for their OS!
            sgDirA.Cells[1, i+1] := FullPathAndName;
          {$ENDIF}
        {$ENDIF}
        sgDirA.Cells[2, i+1] := UpperCase(FileHashA);
        sgDirA.Row           := i;
        sgDirA.col           := 1;
        end;

    HashListA.Sort;

    lblTotalFileCountNumberA.Caption := IntToStr(TotalFilesDirA.Count);

    Application.ProcessMessages;

    // Then, list and hash the files in DirB
    lblStatusB.Caption       := 'Counting and examining files in ' + DirB + ' ...please wait';
    TotalFilesDirB           := TStringList.Create;
    TotalFilesDirB.Sorted    := true;
    TotalFilesDirB           := FindAllFilesEx(LongPathOverrideB+DirB, '*', True, True);
    TotalFilesDirB.Sort;
    sgDirB.RowCount          := TotalFilesDirB.Count + 1;

    HashListB                := TStringList.Create;
    FileAndHashListB         := TStringList.Create;
    HashListB.Sorted         := true;
    FileAndHashListB.Sorted  := true;

    lblStatusB.Caption       := 'Counting and examining files in ' + DirB + ' ...please wait';
    lblStatusB.Refresh;
    for i := 0 to TotalFilesDirB.Count -1 do
        begin
          FilePath             := ExtractFilePath(TotalFilesDirB.Strings[i]);
          FileName             := ExtractFileName(TotalFilesDirB.Strings[i]);
          FullPathAndName      := FilePath + FileName;
          FileHashB            := CalcTheHashFile(FullPathAndName);
          HashListB.Add(FileHashB);
          FileAndHashListB.Add(FullPathAndName + ':' + FileHashB + ':');
          // Populate display grid for DirB
          sgDirB.Cells[0, i+1] := IntToStr(i+1);
           {$IFDEF Windows}
             sgDirB.Cells[1, i+1] := RemoveLongPathOverrideChars(FullPathAndName, LongPathOverrideB);
          {$ENDIF}
          {$IFDEF Darwin}
             sgDirB.Cells[1, i+1] := FullPathAndName;
          {$else}
            {$IFDEF UNIX and !$ifdef Darwin} // because Apple had to 'borrow' Unix for their OS!
            sgDirB.Cells[1, i+1] := FullPathAndName;
            {$ENDIF}
          {$ENDIF}

          sgDirB.Cells[2, i+1] := Uppercase(FileHashB);
          sgDirB.Row           := i;
          sgDirB.col           := 1;
        end;
    HashListB.Sort;
    FileAndHashListB.Sort;

    lblTotalFileCountNumberB.Caption := IntToStr(TotalFilesDirB.Count);
    lblStatusB.Caption := 'Comparing files in ' + DirA + ' against files in ' + DirB + ' ...please wait';
    lblStatusB.Refresh;
    Application.ProcessMessages;
    // Now work out where the differences are.
    // Start by establishing if the dirs are identical : same no of files + same hashes = matching dirs
    if TotalFilesDirB.Count > TotalFilesDirA.Count then
      begin
        lblFileCountDiffB.Caption := IntToStr(TotalFilesDirB.Count - TotalFilesDirA.Count);
      end
    else if TotalFilesDirA.Count > TotalFilesDirB.Count then
      begin
        lblFileCountDiffB.Caption := IntToStr(TotalFilesDirA.Count - TotalFilesDirB.Count);
      end
    else lblFileCountDiffB.Caption := '0';

    { If there is no difference between file count, then if all the files are
      actually the same files, the hash lists themselves will be identical if there
      were no errors or no file mistmatches.
      So instead of comparing each hash line by line, just hash the two hash lists and see if they match
      However, we don't know whether DirA or DirB is the one that might have most files in,
      so we do a count of each subtracted by the other
    }
    if ((TotalFilesDirB.Count - TotalFilesDirA.Count) = 0) or ((TotalFilesDirA.Count - TotalFilesDirB.Count) = 0) then
      begin
      // We compare the hashlists using the developers choice of hash alg, i.e. SHA1
      HashOfListA    := SHA1Print(SHA1String(HashListA.Text));
      HashOfListB    := SHA1Print(SHA1String(HashListB.Text));
      if HashOfListA = HashOfListB then
        begin
        lblStatusB.Caption := 'Finished examining files. ' + DirA + ' matches ' + DirB;
        lblStatusB.Refresh;
        lblHashMatchB.Caption:= 'MATCH!';
        end
      else
        begin
          // So the file counts match but the hash lists differ.
          lblStatusB.Caption    := DirA + ' does not match match ' + DirB;
          lblHashMatchB.Caption := 'MIS-MATCH! File count is the same, but hashes differ.';
          MisMatchHashCompare(HashListA, HashListB, FileAndHashListA, FileAndHashListB);
        end;
      end;

    // If both matched, the previous loop will have been executed.
    // If, however, one dir has a higher file count than the other, the following loop runs
    // Start of Mis-Match Loop:
    if (TotalFilesDirB.Count < TotalFilesDirA.Count) or (TotalFilesDirB.Count > TotalFilesDirA.Count) then
      begin
        lblHashMatchB.Caption:= 'MIS-MATCH! File counts are different.';
        FileAndHashListA.Sort;
        FileAndHashListB.Sort;
        MisMatchFileCountCompare(HashListA, HashListB, FileAndHashListA, FileAndHashListB);
      end; // End of mis-match loop
  finally
    // Only enable the copy to clipboard and save button if the grids have more
    // rows of data in them besides the header row.
    if sgDirA.RowCount > 1 then btnCopyToClipboardA.Enabled := true;
    if sgDirB.RowCount > 1 then btnCopyToClipboardB.Enabled := true;
    if (sgDirA.RowCount > 1) or (sgDirB.RowCount > 1) then
      btnSaveComparisons.Enabled  := true;
    Application.ProcessMessages;
  end;

  try
    HashListA.Free;
    TotalFilesDirA.Free;
    FileAndHashListA.Free;

    TotalFilesDirB.Free;
    FileAndHashListB.Free;
    HashListB.Free;
  finally
    // Compute timings and display them
    EndTime                  := Now;
    lblTimeFinishedB.Caption := FormatDateTime('dd/mm/yy hh:mm:ss', EndTime);
    TimeTaken                := EndTime - StartTime;
    strTimeTaken             := FormatDateTime('h" hrs, "n" min, "s" sec"', TimeTaken);
    lblTimeTakenB.Caption    := strTimeTaken;
    Application.ProcessMessages;
  end;
end;

// btnClearTextAreaClick : Clears the whole text field if the user requests to do so
procedure TMainForm.btnClearTextAreaClick(Sender: TObject);
begin
  memoHashText.Clear;
end;

// ClearText : Invoked OnEnter of the text field only if the standing text exists
procedure TMainForm.ClearText(Sender: TObject);
begin
  if memoHashText.Lines[0] = 'Type or paste text here - hash will update as you type' then memoHashText.Clear;
end;

// MisMatchCompare takes two hash lists generated from two directories, along with
// two other lists that include both the hashes and the filenames, and it compares
// one pair against the other and highlights the mis matches.
procedure TMainForm.MisMatchFileCountCompare(HashListA, HashListB, FileAndHashListA, FileAndHashListB : TStringList);
var
  i, indexA, indexB,  HashPosStart , FileNameAndPathPosStart, FileNameAndPathPosEnd : integer;
  MisMatchList : TStringList;
  MissingHash, ExtractedFileName : string;

begin
  i := 0;
  indexA := 0;
  indexB := 0;
  HashPosStart := 0;
  FileNameAndPathPosStart := 0;
  FileNameAndPathPosEnd := 0;

  try
    MismatchList := TStringList.Create;

    // Check the content of ListB against ListA

    lblStatusB.Caption := 'Checking files in ' + DirB + ' against those in ' + DirA;
    lblStatusB.Refresh;
    for i := 0 to HashListB.Count -1 do
     begin
       if not HashListA.Find(HashListB.Strings[i], indexA) then
         begin
           MissingHash := HashListB.Strings[i];
           HashPosStart := Pos(MissingHash, FileAndHashListB.Text);
           FileNameAndPathPosEnd := RPosEx(':', FileAndHashListB.Text, HashPosStart);
           FileNameAndPathPosStart := RPosEx(':', FileAndHashListB.Text, FileNameAndPathPosEnd -1);
           if (HashPosStart > 0) and (FileNameAndPathPosStart > 0) and (FileNameAndPathPosEnd > 0) then
             begin
               ExtractedFileName := Copy(FileAndHashListB.Text, FileNameAndPathPosStart -1, (FileNameAndPathPosEnd - FileNameAndPathPosStart) +1);
               MisMatchList.Add(ExtractedFileName + ' ' + MissingHash + ' is NOT in both directories');
             end;
         end;
     end;

    // Check the content of ListA against ListB

    lblStatusB.Caption := 'Checking files in ' + DirA + ' against those in ' + DirB;
    lblStatusB.Refresh;
    for i := 0 to HashListA.Count -1 do
     begin
       if not HashListB.Find(HashListA.Strings[i], indexA) then
         begin
           MissingHash := HashListA.Strings[i];
           HashPosStart := Pos(MissingHash, FileAndHashListA.Text);
           FileNameAndPathPosEnd := RPosEx(':', FileAndHashListA.Text, HashPosStart);
           FileNameAndPathPosStart := RPosEx(':', FileAndHashListA.Text, FileNameAndPathPosEnd -1);
           if (HashPosStart > 0) and (FileNameAndPathPosStart > 0) and (FileNameAndPathPosEnd > 0) then
             begin
               ExtractedFileName := Copy(FileAndHashListA.Text, FileNameAndPathPosStart -1, (FileNameAndPathPosEnd - FileNameAndPathPosStart) +1);
               MisMatchList.Add(ExtractedFileName + ' ' + MissingHash + ' is NOT in both directories');
             end;
         end;
     end;

    // Notify user of mis-matched files that are in one dir but not the other
    if (MisMatchList.Count > 0) then
     begin
       lblStatusB.Caption := 'There is a mis-match between the two directories.';
       ShowMessage(MisMatchList.Text)
     end
     else
     begin
       ShowMessageFmt('Dir A and Dir B contain %d identical files',[HashListB.Count]);
     end;
    finally // Finally for MisMatch
      if assigned (MisMatchList) then MismatchList.Free;
    end;
end;

// MisMatchHashCompare : When file counts match in both directories but hashes differ, this works out what files are different by hash
procedure TMainForm.MisMatchHashCompare(HashListA, HashListB, FileAndHashListA, FileAndHashListB : TStringList);
var
  i, indexA, indexB,  HashPosStart , FileNameAndPathPosStart, FileNameAndPathPosEnd : integer;
  MisMatchList : TStringList;
  MissingHash, ExtractedFileName : string;

begin
  i                        := 0;
  indexA                   := 0;
  indexB                   := 0;
  HashPosStart             := 0;
  FileNameAndPathPosStart  := 0;
  FileNameAndPathPosEnd    := 0;

  try
    MismatchList := TStringList.Create;

    // Check the content of ListB against ListA

    lblStatusB.Caption := 'Checking files in ' + DirB + ' against those in ' + DirA;
    lblStatusB.Refresh;
    for i := 0 to HashListB.Count -1 do
     begin
       if not HashListA.Find(HashListB.Strings[i], indexA) then
         begin
           MissingHash := HashListB.Strings[i];
           HashPosStart := Pos(MissingHash, FileAndHashListB.Text);
           FileNameAndPathPosEnd := RPosEx(':', FileAndHashListB.Text, HashPosStart);
           FileNameAndPathPosStart := RPosEx(':', FileAndHashListB.Text, FileNameAndPathPosEnd -1);
           if (HashPosStart > 0) and (FileNameAndPathPosStart > 0) and (FileNameAndPathPosEnd > 0) then
             begin
               ExtractedFileName := Copy(FileAndHashListB.Text, FileNameAndPathPosStart -1, (FileNameAndPathPosEnd - FileNameAndPathPosStart) +1);
               MisMatchList.Add(ExtractedFileName + ' ' + MissingHash + ' is a hash mismatch');
             end;
         end;
     end;

    // Check the content of ListA against ListB

    lblStatusB.Caption := 'Checking files in ' + DirA + ' against those in ' + DirB;
    lblStatusB.Refresh;
    for i := 0 to HashListA.Count -1 do
     begin
       if not HashListB.Find(HashListA.Strings[i], indexA) then
         begin
           MissingHash := HashListA.Strings[i];
           HashPosStart := Pos(MissingHash, FileAndHashListA.Text);
           FileNameAndPathPosEnd := RPosEx(':', FileAndHashListA.Text, HashPosStart);
           FileNameAndPathPosStart := RPosEx(':', FileAndHashListA.Text, FileNameAndPathPosEnd -1);
           if (HashPosStart > 0) and (FileNameAndPathPosStart > 0) and (FileNameAndPathPosEnd > 0) then
             begin
               ExtractedFileName := Copy(FileAndHashListA.Text, FileNameAndPathPosStart -1, (FileNameAndPathPosEnd - FileNameAndPathPosStart) +1);
               MisMatchList.Add(ExtractedFileName + ' ' + MissingHash + ' is a hash mismatch');
             end;
         end;
     end;

    // Notify user of mis-matched files that are in one dir but not the other
    if (MisMatchList.Count > 0) then
     begin
       lblStatusB.Caption := 'There is a hash mis-match between the two directories.';
       ShowMessage(MisMatchList.Text)
     end
     else
     begin
       ShowMessageFmt('Dir A and Dir B contain %d identical files',[HashListB.Count]);
     end;
    finally // Finally for MisMatch
      if assigned (MisMatchList) then MismatchList.Free;
    end;

end;

// EmptyDisplayGrid will quickly empty the display grid from previous runs
procedure TMainForm.EmptyDisplayGrid(Grid : TStringGrid);
var
  i : integer;
begin
  for i := 0 to Grid.ColCount - 1 do
    Grid.Cols[i].Clear;
end;

procedure TMainForm.Button8CopyAndHashClick(Sender: TObject);
begin
  frmDisplayGrid1.CopyAndHashGrid.Visible := false; // Hide the grid if it was left visible from an earlier run
  lblNoOfFilesToExamine.Caption    := '';
  lblNoOfFilesToExamine2.Caption   := '';
  lblFilesCopiedPercentage.Caption := '';
  lblDataCopiedSoFar.Caption       := '';
  lblTimeTaken6A.Caption           := '...';
  lblTimeTaken6B.Caption           := '...';
  lblTimeTaken6C.Caption           := '...';
  StatusBar3.Caption               := ('Counting files...please wait');
  Application.ProcessMessages;

  if chkUNCMode.Checked then
    begin
      SourceDir := Edit2SourcePath.Text;
      DestDir   := Edit3DestinationPath.Text;

      {$ifdef Windows}
      // If chosen path is a UNC path, we need to append the UNC prefix to the
      // Unicode 32K long API call of \\?\
      if (Pos('\\', SourceDir) > 0) then
      begin
        LongPathOverride := '\\?\UNC\';
        Delete(SourceDir, 1, 2); // Delete the \\ from the DirToHash path (otherwise it becomes '\\?\UNC\\\')
      end;
      {$endif}
      // Now process the copy and paste in UNC mode
      ProcessDir(SourceDir);
    end
  else
  begin
    // In case the user changes either the source or destination after already
    // running a job once, and so without necessarily clicking with the mouse,
    // get the source and destination paths again
    DirListAClick(Sender);
    DirListBClick(Sender);

    // Now process the selected source and destination in non-UNC mode
    ProcessDir(SourceDir);

    if SourceDirValid AND DestDirValid = FALSE then
      begin
        // Now disable the 'Go!' button again
        Button8CopyAndHash.Enabled := false;
      end;
  end;
  Application.ProcessMessages;
end;

procedure TMainForm.FileTypeMaskCheckBox1Change(Sender: TObject);
begin
  if FileMaskField.Visible then
    begin
    FileMaskField.Visible := false
    end
  else if FileMaskField.Visible = false then
    begin
    FileMaskField.Visible := true;
    end;
  {$IFDEF LINUX}
    if FileTypeMaskCheckBox1.Checked then
      ShowMessage('Remember *.JPG and *.jpg are different extension types in Linux!');
  {$ENDIF}

  {$ifdef UNIX}
    {$ifdef Darwin}
    if FileTypeMaskCheckBox1.Checked then
    ShowMessage('Remember *.JPG and *.jpg are different extension types in Apple Mac!');

    {$ENDIF}
 {$ENDIF}
end;

procedure TMainForm.FileTypeMaskCheckBox2Change(Sender: TObject);
begin
  if FileTypeMaskCheckBox2.Checked then FileMaskField2.Visible := true
  else FileMaskField2.Visible := false;
end;

procedure TMainForm.btnCompareTwoFilesClick(Sender: TObject);
var
  FileA, FileB, FileAHash, FileBHash : string;
begin
  FileA                      := '';
  FileB                      := '';
  FileAHash                  := '';
  FileBHash                  := '';
  lblHashMatchResult.Caption := '';
  lblFileAHash.Caption       := '';
  lblFileBHash.Caption       := '';

  FileA := Trim(lblFileAName.Caption);
  FileB := Trim(lblFileBName.Caption);

  if (FileExistsUTF8(FileA) = false) or (FileExistsUTF8(FileB) = false) then
  begin
    StatusBar4.SimpleText := 'BOTH FILES MUST BE SELECTED!';
    Application.ProcessMessages;
    Abort;
  end
  else
    begin
      // FileA
      StatusBar4.SimpleText := 'Computing hash of ' + FileA + '...';

      if FileExistsUTF8(FileA) then
      begin
        Application.ProcessMessages;
        FileAHash := Uppercase(CalcTheHashFile(FileA));
        lblFileAHash.Caption := FileAHash;
      end
      else ShowMessage('File A is invalid or cannot be accessed');

      //FileB
      StatusBar4.SimpleText := 'Computing hash of ' + FileB + '...';
      if FileExistsUTF8(FileB) then
      begin
        Application.ProcessMessages;
        FileBHash := Uppercase(CalcTheHashFile(FileB));
        lblFileBHash.Caption := FileBHash;
      end
      else ShowMessage('File B is invalid or cannot be accessed');

      // Compare FileA and FileB Hash values
      CompareTwoHashes(FileAHash, FileBHash);
      StatusBar4.SimpleText := 'Hash comparison complete.';
      btnCompareTwoFilesSaveAs.Enabled := true;
      Application.ProcessMessages;
    end;
end;

procedure TMainForm.btnCompareTwoFilesSaveAsClick(Sender: TObject);
var
  slCompareTwoFiles : TStringList;
begin
  slCompareTwoFiles := TStringList.Create;
  slCompareTwoFiles.Add('File A: ' + lblFileAName.Caption + ', ' + 'Hash: ' + lblFileAHash.Caption);
  slCompareTwoFiles.Add('File B: ' + lblFileBName.Caption + ', ' + 'Hash: ' + lblFileBHash.Caption);
  slCompareTwoFiles.Add('Result: ' + lblHashMatchResult.Caption);

  if SaveDialog5.Execute then
  begin
    SaveDialog5.InitialDir := GetCurrentDir;
    slCompareTwoFiles.SaveToFile(SaveDialog5.FileName);
  end;
  slCompareTwoFiles.Free;
end;

// Procedure CompareTwoHashes : Simply checks two hash strings and compares them
procedure TMainForm.CompareTwoHashes(FileAHash, FileBHash : string);
begin
  lblHashMatchResult.Caption := '';
  if FileAHash = FileBHash then
  begin
  lblHashMatchResult.Caption:= 'MATCH!';
  end
  else
  lblHashMatchResult.Caption:= 'MIS-MATCH!';
end;

procedure TMainForm.btnDirAClick(Sender: TObject);
begin
  SelectDirectoryDialog4.Execute;
  lblDirAName.Caption := SelectDirectoryDialog4.FileName;
end;

procedure TMainForm.btnDirBClick(Sender: TObject);
begin
  SelectDirectoryDialog5.Execute;
  lblDirBName.Caption := SelectDirectoryDialog5.FileName;
end;

procedure TMainForm.btnCopyToClipboardAClick(Sender: TObject);
begin
  sgDirA.CopyToClipboard(false);
  ShowMessage('Content of Grid A is in clipboard. Ctrl+V to paste it elsewhere');
end;

procedure TMainForm.btnCopyToClipboardBClick(Sender: TObject);
begin
  sgDirB.CopyToClipboard(false);
  ShowMessage('Content of Grid B is in clipboard. Ctrl+V to paste it elsewhere');
end;


// Used in "Compare Two Files" tab, to select File A
procedure TMainForm.btnFileACompareClick(Sender: TObject);
begin
  btnCompareTwoFilesSaveAs.Enabled := false;
  if OpenDialog1.Execute then
  begin
    lblFileAName.Caption := OpenDialog1.FileName;
  end;
end;
// Used in "Compare Two Files" tab, to select FileB
procedure TMainForm.btnFileBCompareClick(Sender: TObject);
begin
  btnCompareTwoFilesSaveAs.Enabled := false;
  if OpenDialog1.Execute then
  begin
    lblFileBName.Caption := OpenDialog1.FileName;
  end;
end;

procedure TMainForm.btnClipboardHashValueClick(Sender: TObject);
begin
  try
    memoHashText.CopyToClipboard;
  finally
   ShowMessage('Hash value is in clipboard');
  end;
end;

// For users hashing a single file, where they decide to switch the hash choice.
// Saves them re-adding the file again.
procedure TMainForm.AlgorithmChoiceRadioBox2SelectionChanged(Sender: TObject);
var
  HashValue : ansistring;
begin
  if edtFileNameToBeHashed.Text <> 'File being hashed...' then
    begin
      memFileHashField.Clear;
      StatusBar1.SimpleText := 'RECOMPUTING NEW HASH VALUE...Please wait.';
      Application.ProcessMessages;
      HashValue := CalcTheHashFile(edtFileNameToBeHashed.Text);
      memFileHashField.Lines.Add(Uppercase(HashValue));
      StatusBar1.SimpleText := 'RECOMPUTED NEW HASH VALUE.';
    end;
end;

procedure TMainForm.AlgorithmChoiceRadioBox5SelectionChanged(Sender: TObject);
var
  HashValueA, HashValueB : ansistring;
begin
  HashValueA := '';
  HashValueB := '';
  if FileExists(lblFileAName.Caption) and FileExists(lblFileBName.Caption) then
    begin
      StatusBar4.SimpleText := 'RECOMPUTING NEW HASH VALUES...Please wait.';
      Application.ProcessMessages;
      HashValueA := Uppercase(CalcTheHashFile(lblFileAName.Caption));
      lblFileAHash.Caption := HashValueA;
      Application.ProcessMessages;
      HashValueB := Uppercase(CalcTheHashFile(lblFileBName.Caption));
      lblFileBHash.Caption := HashValueB;
      StatusBar4.SimpleText := 'RECOMPUTED NEW HASH VALUES.';
      Application.ProcessMessages;
    end;
end;
// As strings are so quick to compute, I have used the DCPCrypt library for all
// of the 4 hashing algorithms for consistancy and simplicity. This differs though
// for file and disk hashing, where speed is more important - see CalcTheHashFile

function TMainForm.CalcTheHashString(strToBeHashed:ansistring):string;

  var
    TabRadioGroup1: TRadioGroup;
    varMD5Hash: TDCP_MD5;
    varSHA1Hash: TDCP_SHA1;
    varSHA256Hash: TDCP_SHA256;
    varSHA512Hash: TDCP_SHA512;

    DigestMD5:    array[0..31] of byte;  // MD5 produces a 128 bit digest (32 byte output)
    DigestSHA1:   array[0..31] of byte;  // SHA1 produces a 160 bit digest (32 byte output)
    DigestSHA256: array[0..31] of byte;  // SHA256 produces a 256 bit digest (32 byte output)
    DigestSHA512: array[0..63] of byte;  // SHA512 produces a 512 bit digest (64 byte output)

    i: integer;
    GeneratedHash: string;
    SourceData : ansistring;

  begin
    SourceData := '';
    GeneratedHash := '';
    SourceData := strToBeHashed;
    if Length(SourceData) > 0 then
      begin
        case PageControl1.TabIndex of
          0: TabRadioGroup1 := AlgorithmChoiceRadioBox1;  //RadioGroup on the 1st tab.
          1: TabRadioGroup1 := AlgorithmChoiceRadioBox2;  //RadioGroup on the 2nd tab.
          2: TabRadioGroup1 := AlgorithmChoiceRadioBox3;  //RadioGroup on the 3rd tab.
          3: TabRadioGroup1 := AlgorithmChoiceRadioBox4;  //RadioGroup on the 4th tab.
          4: TabRadioGroup1 := AlgorithmChoiceRadioBox6;  //RadioGroup on the 5th tab.
        end;

        case TabRadioGroup1.ItemIndex of
          0: begin
               varMD5Hash := TDCP_MD5.Create(nil);        // create the hash instance
               varMD5Hash.Init;                           // initialize it
               varMD5Hash.UpdateStr(SourceData);          // hash the string
               varMD5Hash.Final(DigestMD5);               // produce the digest
               varMD5Hash.Free;                           // Free the resource
               for i := 0 to 15 do                        // Generate 32 (16 hex values)character output
                 GeneratedHash := GeneratedHash + IntToHex(DigestMD5[i],2);
             end;
          1: begin
               varSHA1Hash := TDCP_SHA1.Create(nil);
               varSHA1Hash.Init;
               varSHA1Hash.UpdateStr(SourceData);
               varSHA1Hash.Final(DigestSHA1);
               varSHA1Hash.Free;
               for i := 0 to 19 do                        // 40 (20 hex values) character output
                GeneratedHash := GeneratedHash + IntToHex(DigestSHA1[i],2);
             end;
          2: begin
               varSHA256Hash := TDCP_SHA256.Create(nil);
               varSHA256Hash.Init;
               varSHA256Hash.UpdateStr(SourceData);
               varSHA256Hash.Final(DigestSHA256);
               varSHA256Hash.Free;
               for i := 0 to 31 do                        // 64 (32 hex values) character output
                GeneratedHash := GeneratedHash + IntToHex(DigestSHA256[i],2);
             end;
          3: begin
               varSHA512Hash := TDCP_SHA512.Create(nil);
               varSHA512Hash.Init;
               varSHA512Hash.UpdateStr(SourceData);
               varSHA512Hash.Final(DigestSHA512);
               varSHA512Hash.Free;
               for i := 0 to 63 do                        // 128 (64 hex values) character output
                GeneratedHash := GeneratedHash + IntToHex(DigestSHA512[i],2);
             end;
      end;
    end;
    result := GeneratedHash;  // return the resultant hash digest, if successfully computed
  end;

function TMainForm.CalcTheHashFile(FileToBeHashed:string):string;
  var
    {MD5 and SHA1 utilise the LCL functions, whereas SHA256 and SHA512 utilise
    the DCPCrypt library. MD5 and SHA1 from LCL seem to be much faster for large
    files and disks than the DCPCrypt ones, so DCPCrypt only used for SHA256\512
    on the grounds that there is no other LCL utilisation to choose from, yet.
    Also, FileStreams are used for SHA256/512.
    Streams are not necessary for MD5 and SHA1.}
    TabRadioGroup2: TRadioGroup;
    varSHA256Hash: TDCP_SHA256;
    varSHA512Hash: TDCP_SHA512;

    DigestSHA256: array[0..31] of byte;  // SHA256 produces a 256 bit digest (32 byte output)
    DigestSHA512: array[0..63] of byte;  // SHA512 produces a 512 bit digest (64 byte output)

    i : integer;
    SourceDataSHA256, SourceDataSHA512: TFileStreamUTF8;
    GeneratedHash: string;

  begin
    SourceDataSHA256 := nil;
    SourceDataSHA512 := nil;
    GeneratedHash    := '';

    case PageControl1.TabIndex of
      0: TabRadioGroup2 := AlgorithmChoiceRadioBox1;  //RadioGroup for Text.
      1: TabRadioGroup2 := AlgorithmChoiceRadioBox2;  //RadioGroup for File.
      2: TabRadioGroup2 := AlgorithmChoiceRadioBox3;  //RadioGroup for FileS.
      3: TabRadioGroup2 := AlgorithmChoiceRadioBox4;  //RadioGroup for Copy.
      4: TabRadioGroup2 := AlgorithmChoiceRadioBox5;  //RadioGroup for Compare Two Files.
      5: TabRadioGroup2 := AlgorithmChoiceRadioBox6;  //RadioGroup for Compare Direcories.
    end;

    case TabRadioGroup2.ItemIndex of
      0: begin
           if FileSize(FileToBeHashed) > 1048576 then    // if file > 1Mb
             begin
              GeneratedHash := MD5Print(MD5File(FileToBeHashed, 2097152));    //2Mb buffer
             end
           else
           if FileSize(FileToBeHashed) = 0 then
             begin
               {$ifdef UNIX}
               // On Linux, block devices like disks often report 0 byte size but need to be accessed still
               if Pos('/dev/', FileToBeHashed) > 0 then
                 GeneratedHash := MD5Print(MD5File(FileToBeHashed, 2097152));
               {$else ifdef Windows}
               GeneratedHash := 'Not computed, zero byte file';
               {$endif}
             end
           else
           begin
            GeneratedHash := MD5Print(MD5File(FileToBeHashed));            //1024 bytes buffer
           end;
         end;
      1: begin
           if FileSize(FileToBeHashed) > 1048576 then
             begin
               GeneratedHash := SHA1Print(SHA1File(FileToBeHashed, 2097152));  //2Mb buffer
             end
           else
           if FileSize(FileToBeHashed) = 0 then
             begin
               {$ifdef UNIX}
               // On Linux, block devices like disks often report 0 byte size but need to be accessed still
               if Pos('/dev/', FileToBeHashed) > 0 then
                 GeneratedHash := SHA1Print(SHA1File(FileToBeHashed, 2097152));
               {$else ifdef Windows}
               GeneratedHash := 'Not computed, zero byte file';
               {$endif}
             end
           else
             GeneratedHash := SHA1Print(SHA1File(FileToBeHashed))            //1024 bytes buffer
         end;
      2: begin
           // The LCL does not have a SHA256 implementation, so DCPCrypt used instead
           // Note the use of UTF8 FileStreams, to cope with Unicode on Windows
           SourceDataSHA256 := TFileStreamUTF8.Create(FileToBeHashed, fmOpenRead);
           if SourceDataSHA256 <> nil then
             begin
             i := 0;
             varSHA256Hash := TDCP_SHA256.Create(nil);
             varSHA256Hash.Init;
             varSHA256Hash.UpdateStream(SourceDataSHA256, SourceDataSHA256.Size);
             varSHA256Hash.Final(DigestSHA256);
             varSHA256Hash.Free;
             for i := 0 to 31 do                        // 64 character output
               GeneratedHash := GeneratedHash + IntToHex(DigestSHA256[i],2);
             end;  // End of SHA256 else if
           // If the file is a zero byte file, override the default zero size hash
           // with a "not computed" message, rather than a 'fake' hash.
           if SourceDataSHA256.Size = 0 then
             begin
             {$ifdef UNIX}
              // On Linux, block devices like disks often report 0 byte size but need to be accessed still
              if Pos('/dev/', SourceDataSHA256.FileName) > 0 then
                begin
                   i := 0;
                   varSHA256Hash := TDCP_SHA256.Create(nil);
                   varSHA256Hash.Init;
                   varSHA256Hash.UpdateStream(SourceDataSHA256, SourceDataSHA256.Size);
                   varSHA256Hash.Final(DigestSHA256);
                   varSHA256Hash.Free;
                   for i := 0 to 31 do                        // 64 character output
                     GeneratedHash := GeneratedHash + IntToHex(DigestSHA256[i],2);
                   end;
             {$else ifdef Windows}
             GeneratedHash := 'Not computed, zero byte file';
             {$endif}
             end;
         SourceDataSHA256.Free;
         end;
       3: begin
            // The LCL does not have a SHA512 implementation, so DCPCrypt used instead
            // Note the use of UTF8 FileStreams, to cope with Unicode on Windows
            SourceDataSHA512 := TFileStreamUTF8.Create(FileToBeHashed, fmOpenRead);
            if SourceDataSHA512 <> nil then
              begin
              i := 0;
              varSHA512Hash := TDCP_SHA512.Create(nil);
              varSHA512Hash.Init;
              varSHA512Hash.UpdateStream(SourceDataSHA512, SourceDataSHA512.Size);
              varSHA512Hash.Final(DigestSHA512);
              varSHA512Hash.Free;
              for i := 0 to 63 do                        // 128 character output
               GeneratedHash := GeneratedHash + IntToHex(DigestSHA512[i],2);
              end;
            // If the file is a zero byte file, override the default zero size hash
            // with a "not computed" message, rather than a 'fake' hash.
            if SourceDataSHA512.Size = 0 then
              begin
              {$ifdef UNIX}
              // On Linux, block devices like disks often report 0 byte size but need to be accessed still
              if Pos('/dev/', SourceDataSHA512.FileName) > 0 then
                begin
                   i := 0;
                   varSHA512Hash := TDCP_SHA512.Create(nil);
                   varSHA512Hash.Init;
                   varSHA512Hash.UpdateStream(SourceDataSHA512, SourceDataSHA512.Size);
                   varSHA512Hash.Final(DigestSHA512);
                   varSHA512Hash.Free;
                   for i := 0 to 31 do                        // 64 character output
                     GeneratedHash := GeneratedHash + IntToHex(DigestSHA512[i],2);
                   end;
               {$else ifdef Windows}
               GeneratedHash := 'Not computed, zero byte file';
               {$endif}
              end;
          SourceDataSHA512.Free;
          end;
    end;
  result := GeneratedHash;  // return the resultant hash digest, if successfully computed
  end;

procedure TMainForm.HashFile(FileIterator: TFileIterator);
var
  SizeOfFile : int64;
  NameOfFileToHashFull, PathOnly, NameOnly, PercentageProgress : string;
  fileHashValue : ansistring;
  SG : TStringGrid;

begin
  SG            := TStringGrid.Create(self);
  SizeOfFile    := 0;
  fileHashValue := '';

  if StopScan1 = FALSE then    // If Stop button clicked, cancel scan
    begin
    NameOfFileToHashFull := FileIterator.FileName;
    PathOnly   := FileIterator.Path;
    NameOnly   := ExtractFileName(FileIterator.FileName);
    SizeOfFile := FileIterator.FileInfo.Size;

    // This function is called by all three tabs seperately but I dont know how
    // to tell it to update the progress bar of its calling tab, so all three
    // updated for now.

    StatusBar1.SimpleText := 'Currently Hashing: ' + NameOfFileToHashFull;
    StatusBar2.SimpleText := 'Currently Hashing: ' + NameOfFileToHashFull;
    StatusBar3.SimpleText := 'Currently Hashing: ' + NameOfFileToHashFull;

    // Now generate the hash value using a custom function and convert the result to uppercase

    FileHashValue := UpperCase(CalcTheHashFile(NameOfFileToHashFull));

    // Now lets update the stringgrid and text fields

    // StringGrid Elements:
    // Col 0 is FileCounter. Col 1 is File Name. Col 2 is Hash. Col 3 is filesize as a string

    RecursiveDisplayGrid1.Cells[0,FileCounter] := IntToStr(FileCounter);
    RecursiveDisplayGrid1.Cells[1,FileCounter] := NameOnly;
    {$IFDEF Windows}
      RecursiveDisplayGrid1.Cells[2,FileCounter] := RemoveLongPathOverrideChars(PathOnly, LongPathOverride);
    {$ENDIF}
      {$IFDEF Darwin}
        RecursiveDisplayGrid1.Cells[2,FileCounter] := PathOnly;
      {$else}
        {$IFDEF UNIX and !$ifdef Darwin} // because Apple had to 'borrow' Unix for their OS!
           RecursiveDisplayGrid1.Cells[2,FileCounter] := PathOnly;
        {$ENDIF}
      {$ENDIF}
    RecursiveDisplayGrid1.Cells[3,FileCounter] := FileHashValue;
    RecursiveDisplayGrid1.Cells[4,FileCounter] := IntToStr(SizeOfFile) + ' bytes ' + '(' + FormatByteSize(SizeOfFile) + ')';

    // Dynamically scroll the list so the user always has the most recently hashed
    // file insight

    RecursiveDisplayGrid1.row := FileCounter;

    // Progress Status Elements:

    lblFilesExamined.Caption:= IntToStr(FileCounter);
    PercentageProgress := IntToStr((FileCounter * 100) DIV NoOfFilesInDir2);
    lblPercentageComplete.Caption := PercentageProgress + '%';
    TotalBytesRead := TotalBytesRead + SizeOfFile;
    lblTotalBytesExamined.Caption := FormatByteSize(TotalBytesRead);

    Application.ProcessMessages;
    FileCounter := FileCounter+1;
    end;

  StatusBar1.SimpleText := '';
  StatusBar2.SimpleText := '';
  StatusBar3.SimpleText := '';
  SG.Free;
end;

procedure TMainForm.lblURLBannerClick(Sender: TObject);
var
  QuickHashURL: string;
begin
  QuickHashURL := 'https://sourceforge.net/projects/quickhash/';
  OpenURL(QuickHashURL);
end;

procedure TMainForm.ProcessDir(const SourceDirName: string);

{$IFDEF WINDOWS}
type
  TRange = 'A'..'Z';   // For the drive lettering of Windows systems
{$ENDIF}
var
  i, NoOfFilesCopiedOK, j, k, HashMismtachCount,
    FileCopyErrors, ZeroByteFilesCounter, DupCount : integer;

  SizeOfFile2, TotalBytesRead2, NoFilesExamined, m: Int64;

  SubDirStructure, SourceFileHasHash, DestinationFileHasHash, FinalisedDestDir,
    FinalisedFileName, CopiedFilePathAndName, SourceDirectoryAndFileName,
    FormattedSystemDate, OutputDirDateFormatted,
    CrDateModDateAccDate, CurrentFile, CSVLogFile2, HTMLLogFile2,
    strNoOfFilesToExamine, SubDirStructureParent, strTimeDifference : string;

  SystemDate, StartTime, EndTime, TimeDifference : TDateTime;

  FilesFoundToCopy, DirectoriesFoundList, SLCopyErrors : TStringList;

  {$IFDEF WINDOWS}
  DriveLetter : char;  // For MS Windows drive letter irritances only
  {$ENDIF}

begin
  SubDirStructure         := '';
  FinalisedDestDir        := '';
  SourceFileHasHash       := '';
  DestinationFileHasHash  := '';
  CrDateModDateAccDate    := '';
  NoOfFilesCopiedOK       := 0;
  HashMismtachCount       := 0;
  FileCopyErrors          := 0;
  ZeroByteFilesCounter    := 0;
  SizeOfFile2             := 0;
  TotalBytesRead2         := 0;
  DupCount                := 0;
  i                       := 0;
  j                       := 0;
  k                       := 0;
  m                       := 0;

  SLCopyErrors := TStringList.Create;

  // Ensures the selected source directory is set as the directory to be searched
  // and then finds all the files and directories within, storing as a StringList.
  // Check with the user that he wants to proceed before starting the copy, compute
  // the systems date and time settings, display that, and also generate part of
  // the output directory based on time of execution

  // This is for the GUI output
  StartTime  := Now;
  // This is for the user, to alert him if it is incorrect
  SystemDate := Now();
  DateTimeToStr(SystemDate);

  // Date and time for the user, to be displayed later
  FormattedSystemDate := FormatDateTime('dd/mm/yy hh:mm:ss', SystemDate);

  // Date and time for the output directory, to be used later with other dir structures
  OutputDirDateFormatted := FormatDateTime('yy-mm-dd_hhmmss', SystemDate);

  SetCurrentDir(SourceDirName);

  {$IFDEF WINDOWS}
  // FindFilesEx will find hidden files in hidden directories, or hidden files
  // in unhidden directories.
  // On Linux, though, we need different behaviour - see IFDEF below
  if chkNoRecursiveCopy.Checked then          // Does not want recursive
    begin
      if FileTypeMaskCheckBox1.Checked then   // ...and does want a file mask
        begin
          FilesFoundToCopy := FindAllFilesEx(LongPathOverride+SourceDirName, FileMaskField.Text, False, True);
        end
      else                                    // but does not want a file mask
        begin
          FilesFoundToCopy := FindAllFilesEx(LongPathOverride+SourceDirName, '*', False, True);
        end;
    end;

  if not chkNoRecursiveCopy.Checked then     // Does want recursive
    begin
      if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
        begin
          FilesFoundToCopy := FindAllFilesEx(LongPathOverride+SourceDirName, FileMaskField.Text, True, True);
        end
      else                                    // but does not want a file mask
        begin
          FilesFoundToCopy := FindAllFilesEx(LongPathOverride+SourceDirName, '*', True, True);
        end;
    end;
  {$ENDIF}

  {$IFDEF LINUX}
  // Determine whether a file mask, recursive, or hidden files are included
  // On Linux, the "Hidden Files?" button is enabled to allow them to be
  // found and copied as needed
  if chkNoRecursiveCopy.Checked then
    begin
      if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
        if chkCopyHidden.Checked then         // ... but does want hidden files
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, False, True);
          end;

      if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
        if not chkCopyHidden.Checked then         // ... but does want hidden files
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, False, False);
          end;

       if chkCopyHidden.Checked then         // ... but does want hidden files
         if not FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
           begin
             FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', False, False);
           end;

       if not FileTypeMaskCheckBox1.Checked then
         if not chkCopyHidden.Checked then
           begin
             FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', False, False);
           end;
    end;

  if not chkNoRecursiveCopy.Checked then
    begin
      if chkCopyHidden.Checked then         // ... but does want hidden files
        if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, True, True);
          end;

      if chkCopyHidden.Checked then         // ... but does want hidden files
        if not FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', True, True);
          end;

      if not chkCopyHidden.Checked then         // ... but does want hidden files
        if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, True, False);
          end;

      if not chkCopyHidden.Checked then         // ... but does want hidden files
        if not FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', true, False);
          end;
     end;
  {$ENDIF}

  {$IFDEF UNIX}
    {$IFDEF Darwin}
    // Determine whether a file mask, recursive, or hidden files are included
    // On Apple, same as Linux, the "Hidden Files?" button is enabled to allow them to be
    // found and copied as needed
    if chkNoRecursiveCopy.Checked then
      begin
        if FileTypeMaskCheckBox1.Checked then
          if chkCopyHidden.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, False, True);
            end;

        if FileTypeMaskCheckBox1.Checked then
          if not chkCopyHidden.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, False, False);
            end;

         if chkCopyHidden.Checked then
           if not FileTypeMaskCheckBox1.Checked then
             begin
               FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', False, False);
             end;

         if not FileTypeMaskCheckBox1.Checked then
           if not chkCopyHidden.Checked then
             begin
               FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', False, False);
             end;
      end;

    if not chkNoRecursiveCopy.Checked then
      begin
        if chkCopyHidden.Checked then
          if FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, True, True);
            end;

        if chkCopyHidden.Checked then
          if not FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', True, True);
            end;

        if not chkCopyHidden.Checked then
          if FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMaskField.Text, True, False);
            end;

        if not chkCopyHidden.Checked then
          if not FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, '*', true, False);
            end;
       end;
    {$ENDIF}
  {$ENDIF}
  if MessageDlg('Proceed?', 'Source directory contains ' + IntToStr(FilesFoundToCopy.Count) + ' mask-matched files, inc sub-dirs. FYI, the host system date settings are : ' + FormattedSystemDate + '. Do you want to proceed?', mtConfirmation,
   [mbCancel, mbNo, mbYes],0) = mrYes then

    begin
    strNoOfFilesToExamine := IntToStr(FilesFoundToCopy.Count);
    lblTimeTaken6A.Caption := FormatDateTime('dd/mm/yy hh:mm:ss', SystemDate);
    Application.ProcessMessages;

    try
      // If the user just wants a list of the source dir, do that. Otherwise, do
      // the copying and hashing and everything after the else

      // 1st if : User wants to just generate a list of dirs & files. Date values added, too
      if CheckBoxListOfDirsAndFilesOnly.Checked then
        begin
        i := 0;
          for i := 0 to FilesFoundToCopy.Count -1 do
            begin
              CurrentFile := FilesFoundToCopy.Strings[i];
              {$IFDEF Windows}
              CrDateModDateAccDate := DateAttributesOfCurrentFile(CurrentFile);
              {$ENDIF}
              frmDisplayGrid1.CopyAndHashGrid.rowcount    := i + 1;
              frmDisplayGrid1.CopyAndHashGrid.Cells[0, i] := IntToStr(i);
              frmDisplayGrid1.CopyAndHashGrid.Cells[1, i] := FilesFoundToCopy.Strings[i];
              frmDisplayGrid1.CopyAndHashGrid.Cells[5, i] := CrDateModDateAccDate;
              frmDisplayGrid1.CopyAndHashGrid.row         := i;
              frmDisplayGrid1.CopyAndHashGrid.col         := 1;
            end;
          {$IFDEF Windows}
          ShowMessage('An attempt to compute file date attributes was also conducted. Scroll to the right if they are not visible.');
          {$endif}
          frmDisplayGrid1.btnClipboardResults2.Enabled := true;
        end
      else
      // 2nd if : User wants to just generate a list of directories
      if CheckBoxListOfDirsOnly.Checked then
        begin
        i := 0;
        DirectoriesFoundList := FindAllDirectories(SourceDir, true);
        if DirectoriesFoundList.Count = 0 then
          ShowMessage('No subdirectories found (though files may exist). No data to display.')
        else
          try
            for i := 0 to DirectoriesFoundList.Count -1 do
              begin
                frmDisplayGrid1.CopyAndHashGrid.rowcount    := i + 1;
                frmDisplayGrid1.CopyAndHashGrid.Cells[0, i] := IntToStr(i);
                frmDisplayGrid1.CopyAndHashGrid.Cells[1, i] := DirectoriesFoundList.Strings[i];
                frmDisplayGrid1.CopyAndHashGrid.Row         := i;
                frmDisplayGrid1.CopyAndHashGrid.col         := 1;
              end;
          finally
            frmDisplayGrid1.btnClipboardResults2.Enabled := true;
            DirectoriesFoundList.free;
          end;
        end
      else

      // Else: User wants to do a full copy and hash of all files, so lets begin

      for i := 0 to FilesFoundToCopy.Count -1 do
        begin
          if StopScan1 = FALSE then
            begin
            SourceFileHasHash := '';
            DestinationFileHasHash := '';

            {$IFDEF WINDOWS}
              // Get the file size using a stream, because the traditional FileSize
              // function can't injest the Long Path prefix of '\\?\' or '\\?\UNC\'
              m := FileSizeWithLongPath(FilesFoundToCopy.Strings[i]);
            {$else}
              {$IFDEF Darwin}
                // Not long path stupidity for Apple Mac
                m := FileSize(FilesFoundToCopy.Strings[i]);
              {$else}
                {$IFDEF UNIX and !$ifdef Darwin}
                  // Not long path stupidity for Linux, either
                  m := FileSize(FilesFoundToCopy.Strings[i]);
                {$ENDIF}
              {$ENDIF}
            {$ENDIF}

            if m >= 0 then
              begin
              StatusBar3.SimpleText := 'Currently hashing and copying: ' + RemoveLongPathOverrideChars(FilesFoundToCopy.Strings[i], LongPathOverride);
              Application.ProcessMessages;
              { Now we have some output directory jiggery pokery to deal with, that
                needs to accomodate both OS's. Firstly,
                In Linux   : /home/ted/SrcDir/ needs to become /home/ted/NewDestDir/home/ted/SrcDir
                In Windows : C:\SrcDir\SubDirA needs to become E:\NewDestDir\SrcDir\SubDirA

                In addition, we need to generate a datestamped parent directory for the output
                in case the user generates several seperate outputs to the same parent dir
              }
                // Firstly, compute the original filename and path, less trailing slash

                {$IFDEF WINDOWS}
                   SourceDirectoryAndFileName := ChompPathDelim(CleanAndExpandDirectory(RemoveLongPathOverrideChars(FilesFoundToCopy.Strings[i], LongPathOverride)));
                {$else}
                   {$IFDEF Darwin}
                     SourceDirectoryAndFileName := ChompPathDelim(CleanAndExpandDirectory(FilesFoundToCopy.Strings[i]));
                   {$else}
                     {$IFDEF UNIX and !$ifdef Darwin}
                       SourceDirectoryAndFileName := ChompPathDelim(CleanAndExpandDirectory(FilesFoundToCopy.Strings[i]));
                     {$ENDIF}
                   {$ENDIF}
                {$ENDIF}


                // Now reformulate the source sub-dir structure, from the selected dir downwards
                // but only if the user has not checked the box "Dont rebuild path?"
                // If he has, then just dump the files to the root of the destination dir
                if chkNoPathReconstruction.Checked = false then
                  begin
                    SubDirStructure := IncludeTrailingPathDelimiter(ExtractFileDir(SourceDirectoryAndFileName));
                    if chkUNCMode.Checked then
                      begin
                        Delete(SubDirStructure, 1, 1); // remove one of two \ from the \\ prefix to form the slash of the directory split
                      end;
                  end
                else
                 begin
                    SubDirStructure := IncludeTrailingPathDelimiter(DestDir);
                  end;

                // And also generate a timestamped parent directory for the output dir, named after the time of execution
                SubDirStructureParent := ChompPathDelim(IncludeTrailingPathDelimiter(DestDir) + IncludeTrailingPathDelimiter('QH_' + OutputDirDateFormatted));

              { Now concatenate the original sub directory to the destination directory
                and the datestamped parent directory to form the total path, inc filename
                Note : Only directories containing files will be recreated in destination.
                Empty dirs and files whose extension do match a chosen mask (if any)
                are skipped.
                If user wishes to dump files to root of destination, use destination dir name instead}

                if chkNoPathReconstruction.Checked = false then
                  begin
                    FinalisedDestDir := SubDirStructureParent + SubDirStructure;
                  end
                else
                  begin
                     FinalisedDestDir := SubDirStructureParent;
                  end;

              {$IFDEF Windows}
              { Due to the nonsensories of Windows drive lettering, we have to allow
                for driver lettering in the finalised destination path.
                This loop finds 'C:' in the middle of the concatanated path and
                return its position. It then deletes 'C:' of 'C:\' if found, or any
                other A-Z drive letter, leaving the '\' for the path
                So, C:\SrcDir\SubDirA becomes E:\NewDestDir\SrcDir\SubDirA instead of
                E:\NewDestDir\C:SrcDir\SubDirA. UNC paths are taken care of by ForceDirectories }

              for DriveLetter in TRange do
                begin
                  k := posex(DriveLetter+':', FinalisedDestDir, 4);
                  Delete(FinalisedDestDir, k, 2);
                end;

              {Now, again, only if Windows, obtain the Created, Modified and Last Accessed
              dates from the sourcefile by calling custom function 'DateAttributesOfCurrentFile'
              Linux does not have 'Created Dates' so this does not need to run on Linux platforms}

              CrDateModDateAccDate := DateAttributesOfCurrentFile(SourceDirectoryAndFileName);

              {$ENDIF}
                {$IFDEF LINUX}
                // Get the 'Last Modified' date, only, for Linux files
                CrDateModDateAccDate := DateAttributesOfCurrentFileLinux(SourceDirectoryAndFileName);
                {$ENDIF}
                  {$IFDEF UNIX}
                    {$IFDEF Darwin}
                      // Get the 'Last Modified' date, only, for Apple Mac files
                      CrDateModDateAccDate := DateAttributesOfCurrentFileLinux(SourceDirectoryAndFileName);
                    {$ENDIF}
                  {$ENDIF}
              // Determine the filename string of the file to be copied
              FinalisedFileName := ExtractFileName(FilesFoundToCopy.Strings[i]);

              // Before copying the file and creating storage areas, lets hash the source file
              SourceFileHasHash := Uppercase(CalcTheHashFile(SourceDirectoryAndFileName));

              // Now create the destination directory structure, if it is not yet created.

              if not DirectoryExistsUTF8(FinalisedDestDir) then
                begin
                  if not CustomisedForceDirectoriesUTF8(FinalisedDestDir, true) then
                    begin
                      ShowMessage(FinalisedDestDir+' cannot be created. Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
                    end;
                end;

              // Now copy the file to the newly formed or already existing destination dir
              // and hash it. Then check that source and destination hashes match.
              // Then total up how many copied and hashed OK, or not.
              // If the user chooses not to reconstruct source dir structure,
              // check for filename conflicts, create an incrementer to ensure uniqueness,
              // and rename to "name.ext_DuplicatedNameX". Otherwise, reconstruct source path

              if chkNoPathReconstruction.Checked = false then
                begin
                  CopiedFilePathAndName := IncludeTrailingPathDelimiter(FinalisedDestDir) + FinalisedFileName;
                end
                else
                  begin
                    if FileExists(IncludeTrailingPathDelimiter(FinalisedDestDir) + FinalisedFileName) then
                    begin
                      DupCount := DupCount + 1;
                      CopiedFilePathAndName := IncludeTrailingPathDelimiter(FinalisedDestDir) + FinalisedFileName + '_DuplicatedName' + IntToStr(DupCount);
                    end
                    else
                    CopiedFilePathAndName := IncludeTrailingPathDelimiter(FinalisedDestDir) + FinalisedFileName;
                  end;

              // Now copy the file, either to the reconstructed path or to the root

              if not FileUtil.CopyFile(SourceDirectoryAndFileName, CopiedFilePathAndName) then
                begin
                  ShowMessage('Failed to copy file : ' + SourceDirectoryAndFileName + ' Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
                  SLCopyErrors.Add('Failed to copy: ' + SourceDirectoryAndFileName + ' ' + SourceFileHasHash);
                  FileCopyErrors := FileCopyErrors + 1;
                end
              else
              DestinationFileHasHash := UpperCase(CalcTheHashFile(CopiedFilePathAndName));
              NoOfFilesCopiedOK := NoOfFilesCopiedOK + 1;

              // Check for hash errors
              if SourceFileHasHash <> DestinationFileHasHash then
                begin
                  HashMismtachCount := HashMismtachCount + 1;
                  SLCopyErrors.Add('Hash mismatch. Source file ' + SourceDirectoryAndFileName + ' ' + SourceFileHasHash + ' Hash of copied file: ' + CopiedFilePathAndName + ' ' + DestinationFileHasHash);
                end
              else if SourceFileHasHash = DestinationFileHasHash then
                begin
                // With the display grid, adding one to each value ensures the first row headings do not conceal the first file
                  frmDisplayGrid1.CopyAndHashGrid.rowcount      := i + 2; // Add a grid buffer count to allow for failed copies - avoids 'Index Out of Range' error
                  frmDisplayGrid1.CopyAndHashGrid.Cells[0, i+1] := IntToStr(i);
                  {$IFDEF WINDOWS}
                    frmDisplayGrid1.CopyAndHashGrid.Cells[1, i+1] := RemoveLongPathOverrideChars(FilesFoundToCopy.Strings[i], LongPathOverride);
                  {$else}
                     {$IFDEF Darwin}
                       frmDisplayGrid1.CopyAndHashGrid.Cells[1, i+1] := FilesFoundToCopy.Strings[i];
                     {$else}
                       {$IFDEF UNIX and !$ifdef Darwin}
                         frmDisplayGrid1.CopyAndHashGrid.Cells[1, i+1] := FilesFoundToCopy.Strings[i];
                       {$ENDIF}
                     {$ENDIF}
                  {$ENDIF}
                  frmDisplayGrid1.CopyAndHashGrid.Cells[2, i+1] := SourceFileHasHash;
                  frmDisplayGrid1.CopyAndHashGrid.Cells[3, i+1] := CopiedFilePathAndName;
                  frmDisplayGrid1.CopyAndHashGrid.Cells[4, i+1] := DestinationFileHasHash;
                  frmDisplayGrid1.CopyAndHashGrid.Cells[5, i+1] := CrDateModDateAccDate;
                  frmDisplayGrid1.CopyAndHashGrid.row           := i + 1; //NoOfFilesCopiedOK +1 ;
                  frmDisplayGrid1.CopyAndHashGrid.col           := 1;
                end;

              // Progress Status Elements:
              lblNoOfFilesToExamine.Caption := strNoOfFilesToExamine;
              NoFilesExamined := (i + 1);  // The total of files examined plus those that didnt hash or copy OK
              lblNoOfFilesToExamine2.Caption := IntToStr(NoFilesExamined);
              SizeOfFile2 := FileSize(FilesFoundToCopy.Strings[i]);
              TotalBytesRead2 := TotalBytesRead2 + SizeOfFile2;
              lblDataCopiedSoFar.Caption := FormatByteSize(TotalBytesRead2);
              lblFilesCopiedPercentage.Caption := IntToStr((NoFilesExamined * 100) DIV FilesFoundToCopy.Count) + '%';
              Application.ProcessMessages;
              end; // End of the if m > 0 then statement

            // Otherwise file is probably a zero byte file
            if m = 0 then
              begin
              ZeroByteFilesCounter := ZeroByteFilesCounter + 1; // A file of zero bytes was found in this loop
              end;
          end; // End of the "If Stop button pressed" if
        end;   // End of the 'for Count' of Memo StringList loop

      // Now we can show the grid. Having it displayed for every file as it goes
      // wastes time and isn't especially necessary given the other progress indicators

      frmDisplayGrid1.CopyAndHashGrid.Visible := true;
      frmDisplayGrid1.Show;
      EndTime := Now;
      lblTimeTaken6B.Caption  := FormatDateTime('dd/mm/yy hh:mm:ss', EndTime);
      TimeDifference          := EndTime - StartTime;
      strTimeDifference       := FormatDateTime('h" hrs, "n" min, "s" sec"', TimeDifference);
      lblTimeTaken6C.Caption  := strTimeDifference;


      // Now lets save the generated values to a CSV file.

      if SaveToCSVCheckBox2.Checked then
      begin
        SaveDialog3.Title := 'DONE! Save your CSV text log of results as...';
        // Try to help make sure the log file goes to the users destination dir and NOT source dir!:
        SaveDialog3.InitialDir := DestDir;
        SaveDialog3.Filter     := 'Comma Sep|*.csv|Text file|*.txt';
        SaveDialog3.DefaultExt := 'csv';
        if SaveDialog3.Execute then
          begin
            CSVLogFile2 := SaveDialog3.FileName;
            SaveOutputAsCSV(CSVLogFile2, frmDisplayGrid1.CopyAndHashGrid);
          end;
      end;

      if SaveToHTMLCheckBox2.Checked then
        begin
          i := 0;
          j := 0;
          SaveDialog4.Title := 'DONE! Save your HTML log file of results as...';
          // Try to help make sure the log file goes to the users destination dir and NOT source dir!:
          SaveDialog4.InitialDir := DestDir;
          SaveDialog4.Filter := 'HTML|*.html';
          SaveDialog4.DefaultExt := 'html';
          if SaveDialog4.Execute then
           begin
             HTMLLogFile2 := SaveDialog4.FileName;
             with TStringList.Create do
               try
                 Add('<html>');
                 Add('<title> QuickHash HTML Output </title>');
                 Add('<body>');
                 Add('<p><strong>' + MainForm.Caption + '. ' + 'Log Created: ' + DateTimeToStr(Now)+'</strong></p>');
                 Add('<p><strong>File and Hash listing for: ' + SourceDirName + '</strong></p>');
                 Add('<p>System date & time was ' + FormattedSystemDate + #$0D#$0A +'</p>');
                 Add('<br />');
                 Add('<table border=1>');
                 Add('<tr>');
                 Add('<td>' + 'ID');
                 Add('<td>' + 'Source Name');
                 Add('<td>' + 'Source Hash');
                 Add('<td>' + 'Destination Name');
                 Add('<td>' + 'Destination Hash');
                 Add('<td>' + 'Source Date Attributes');
                 for i := 0 to frmDisplayGrid1.CopyAndHashGrid.RowCount-1 do
                   begin
                     Add('<tr>');
                     for j := 0 to frmDisplayGrid1.CopyAndHashGrid.ColCount-1 do
                       Add('<td>' + frmDisplayGrid1.CopyAndHashGrid.Cells[j,i] + '</td>');
                       add('</tr>');
                   end;
                 Add('</table>');
                 Add('</body>');
                 Add('</html>');
                 SaveToFile(HTMLLogFile2);
               finally
                 Free;
                 HTMLLogFile2 := '';
               end;
           end;
        end;

      // If there is one or more errors, display them to the user and save to a log file
      if Length(SLCopyErrors.Text) > 0 then
       begin
        SLCopyErrors.SaveToFile(IncludeTrailingPathDelimiter(DestDir)+'QHErrorsLog.txt');
        ShowMessage(SLCopyErrors.Text);
      end;

      // All done. End the loops, free resources and notify user
      finally
        FilesFoundToCopy.Free;
        SLCopyErrors.Free;
        StatusBar3.SimpleText := 'Finished.';
        frmDisplayGrid1.btnClipboardResults2.Enabled := true;
      end;
    ShowMessage('Files copied (zero based counter): ' + IntToStr(NoOfFilesCopiedOK) + '. Copy errors : ' + IntToStr(FileCopyErrors) + '. Hash mismatches: ' + IntToStr(HashMismtachCount) + '. Zero byte files: '+ (IntToStr(ZeroByteFilesCounter)));
    end
  else // End of Proceed Message dialog "Yes, No, Cancel".
   ShowMessage('Process aborted by user.');
   Button8CopyAndHash.Enabled := true;
end;

// Since adding the LongPathOverride prefix to deal with any file in any folder
// up to length of 32K chars, the FileSize function can't deal with the prefix
// to size obtained by use of a stream instead.
// Returns the size of the file in bytes on success. -1 otherwise.
function TMainForm.FileSizeWithLongPath(strFileName : string) : Int64;
var
  fs : TFileStream;
begin
  result := -1;
  try
    fs := TFileStreamUTF8.Create(strFileName, faReadOnly);
    result := fs.size;
  finally
    fs.free;
  end;
end;

{$IFDEF Windows}
// FUNCTION FileTimeToDTime - Windows specific,
// kindly acknowledged from xenblaise @ http://forum.lazarus.freepascal.org/index.php?topic=10869.0
function TMainForm.FileTimeToDTime(FTime: TFileTime): TDateTime;
var
  LocalFTime  : TFileTime;
  STime       : TSystemTime;

begin
  FileTimeToLocalFileTime(FTime, LocalFTime);
  FileTimeToSystemTime(LocalFTime, STime);
  Result := SystemTimeToDateTime(STime);
end;

// FUNCTION DateAttributesOfCurrentFile (renamed and customised by Ted) - Windows specific,
// to account for the Created, Modified and Accessed dates of NTFS etc. Linux version
// is below. Code is kindly acknowledged from xenblaise @ http://forum.lazarus.freepascal.org/index.php?topic=10869.0
function TMainForm.DateAttributesOfCurrentFile(var SourceDirectoryAndFileName:string):string;

var
  SR: TSearchRec;
  CreatedDT, LastModifiedDT, LastAccessedDT: TDateTime;

begin
  if FindFirst(SourceDirectoryAndFileName, faAnyFile, SR) = 0 then
    begin
      CreatedDT := FileTimeToDTime(SR.FindData.ftCreationTime);
      LastModifiedDT := FileTimeToDTime(SR.FindData.ftLastWriteTime);;
      LastAccessedDT := FileTimeToDTime(SR.FindData.ftLastAccessTime);;

      Result := ('  Cr: ' + DateTimeToStr(CreatedDT) +
                 '  LMod: ' + DateTimeToStr(LastModifiedDT) +
                 '  LAcc: ' + DateTimeToStr(LastAccessedDT));
    end
  else
    Result := 'Date attributes could not be computed';
end;
{$ENDIF}

{$IFDEF LINUX}
// FUNCTION DateAttributesOfCurrentFileLinux - Same as above but for Linux version
// http://www.freepascal.org/docs-html/rtl/sysutils/fileage.html
function TMainForm.DateAttributesOfCurrentFileLinux(var SourceDirectoryAndFileName:string):string;
var
  SR: TSearchRec;
  LastModifiedDT: TDateTime;

begin
  if FindFirst(SourceDirectoryAndFileName, faAnyFile, SR) = 0 then
    begin
      LastModifiedDT := FileDateTodateTime(FileAgeUTF8(SourceDirectoryAndFileName));
      Result := ('  LMod: ' + DateTimeToStr(LastModifiedDT));
    end
  else
    Result := 'Date attributes could not be computed';
end;

{$ENDIF}

{$IFDEF UNIX}
  {$IFDEF Darwin}
  // FUNCTION DateAttributesOfCurrentFileLinux - Same as above but for Apple Mac version
  // http://www.freepascal.org/docs-html/rtl/sysutils/fileage.html
  function TMainForm.DateAttributesOfCurrentFileLinux(var SourceDirectoryAndFileName:string):string;
  var
    SR: TSearchRec;
    LastModifiedDT: TDateTime;

  begin
    if FindFirst(SourceDirectoryAndFileName, faAnyFile, SR) = 0 then
      begin
        LastModifiedDT := FileDateTodateTime(FileAgeUTF8(SourceDirectoryAndFileName));
        Result := ('  LMod: ' + DateTimeToStr(LastModifiedDT));
      end
    else
      Result := 'Date attributes could not be computed';
  end;

  {$ENDIF}
{$ENDIF}

procedure TMainForm.CheckBoxListOfDirsAndFilesOnlyChange(Sender: TObject);
begin
  if CheckBoxListOfDirsAndFilesOnly.Checked then
    begin
      CheckBoxListOfDirsOnly.Hide;
      Button8CopyAndHash.Enabled := true;
      Edit3DestinationPath.Text := '';
      DirListB.Visible := false;
      DestDir := ''
    end
    else if CheckBoxListOfDirsAndFilesOnly.Checked = false then
      begin
        CheckBoxListOfDirsOnly.Visible := true;
        DirListB.Visible := true;
      end;
end;

procedure TMainForm.CheckBoxListOfDirsOnlyChange(Sender: TObject);
begin
  if CheckBoxListOfDirsOnly.Checked then
    begin
      CheckBoxListOfDirsAndFilesOnly.Hide;
      Button8CopyAndHash.Enabled := true;
      Edit3DestinationPath.Text := '';
      DirListB.Visible := false;
      DestDir := ''
    end
  else if CheckBoxListOfDirsOnly.Checked = false then
    begin
      CheckBoxListOfDirsAndFilesOnly.Visible := true;
      DirListB.Visible := true;
    end;
end;

procedure TMainForm.chkUNCModeChange(Sender: TObject);
begin
  if chkUNCMode.Checked then
    begin
    Edit2SourcePath.Color      := clWhite;
    Edit2SourcePath.Text       := 'Enter source UNC path, prefixed with \\';

    Edit3DestinationPath.Color := clWhite;
    Edit3DestinationPath.Text  := 'Enter destination UNC path, prefixed with \\';;

    Button8CopyAndHash.Enabled := true;
    DirListA.Enabled           := false;
    DirListA.Visible           := false;
    DirListB.Enabled           := false;
    DirListB.Visible           := false;
    end
  else
    begin
    Edit2SourcePath.Color      := clSilver;
    Edit3DestinationPath.Color := clSilver;
    Edit2SourcePath.Text       := 'Select source directory ';
    Edit3DestinationPath.Text  := 'Select source directory ';
    Button8CopyAndHash.Enabled := false;
    DirListA.Enabled           := true;
    DirListA.Visible           := true;
    DirListB.Enabled           := true;
    DirListB.Visible           := true;
    end;
end;

procedure TMainForm.DirListAClick(Sender: TObject);
begin
  SourceDir := UTF8ToSys(DirListA.GetSelectedNodePath);
  if DirectoryExists(SourceDir) then
   begin
     Edit2SourcePath.Text := SourceDir;
     SourceDirValid := TRUE;
     if SourceDirValid AND DestDirValid = TRUE then
       begin
         // Now enable the 'Go!' button as both SourceDir and DestDir are valid
         Button8CopyAndHash.Enabled := true;
      end;
   end;
end;

procedure TMainForm.DirListBClick(Sender: TObject);
begin
  DestDir := UTF8ToSys(DirListB.GetSelectedNodePath);
  if DirectoryExists(DestDir) then
   begin
     Edit3DestinationPath.Text := DestDir;
     DestDirValid := TRUE;
     if SourceDirValid AND DestDirValid = TRUE then
       begin
         // Now enable the 'Go!' button as both SourceDir and DestDir are valid
         Button8CopyAndHash.Enabled := true;
      end;
   end;
end;

procedure TMainForm.Edit2SourcePathEnter(Sender: TObject);
begin
  Edit2SourcePath.Text:= '';
end;

procedure TMainForm.Edit3DestinationPathEnter(Sender: TObject);
begin
  Edit3DestinationPath.Text:= '';
end;

function TMainForm.FormatByteSize(const bytes: QWord): string;
var
  B: byte;
  KB: word;
  MB: QWord;
  GB: QWord;
  TB: QWord;
begin

  B  := 1; //byte
  KB := 1024 * B; //kilobyte
  MB := 1024 * KB; //megabyte
  GB := 1024 * MB; //gigabyte
  TB := 1024 * GB; //terabyte

  if bytes > TB then
    result := FormatFloat('#.## TiB', bytes / TB)
  else
    if bytes > GB then
      result := FormatFloat('#.## GiB', bytes / GB)
    else
      if bytes > MB then
        result := FormatFloat('#.## MiB', bytes / MB)
      else
        if bytes > KB then
          result := FormatFloat('#.## KiB', bytes / KB)
        else
          result := FormatFloat('#.## bytes', bytes) ;
end;



{------------------------------------------------------------------------------
  TO DO : function CustomisedForceDirectoriesUTF8(const Dir: string): Boolean;
  Copied from function ForceDirectoriesUTF8 of 'fileutil.inc' for the purpose
  of eventually ensuring original date attributes are retained by the copied
  directories in Windows. The library function sets the date of the copy process.
 ------------------------------------------------------------------------------}
function TMainForm.CustomisedForceDirectoriesUTF8(const Dir: string; PreserveTime: Boolean): Boolean;

  var
    E: EInOutError;
    ADrv : String;

  function DoForceDirectories(Const Dir: string): Boolean;

  var
    ADir : String;
    APath: String;

  begin
    Result:=True;
    ADir:=ExcludeTrailingPathDelimiter(Dir);

    if (ADir='') then Exit;
    if Not DirectoryExistsUTF8(ADir) then
      begin
        APath := ExtractFilePath(ADir);
        //this can happen on Windows if user specifies Dir like \user\name/test/
        //and would, if not checked for, cause an infinite recusrsion and a stack overflow
        if (APath = ADir) then Result := False
          else Result:=DoForceDirectories(APath);
      If Result then
        {//TO DO : Make this work so that date attr of source directories match the copy
        if PreserveTime then
        DirDate := DateToStr(FileAge(ADir));
        ShowMessage('Value of ADirDate : ' + DirDate);  }
        Result := CreateDirUTF8(ADir);
      end;
  end;

  function IsUncDrive(const Drv: String): Boolean;
  begin
    Result := (Length(Drv) > 2) and (Drv[1] = PathDelim) and (Drv[2] = PathDelim);
  end;

begin
  Result := False;
  ADrv := ExtractFileDrive(Dir);
  if (ADrv<>'') and (not DirectoryExistsUTF8(ADrv))
  {$IFNDEF FORCEDIR_NO_UNC_SUPPORT} and (not IsUncDrive(ADrv)){$ENDIF} then Exit;
  if Dir='' then
    begin
      E:=EInOutError.Create(SCannotCreateEmptyDir);
      E.ErrorCode:=3;
      Raise E;
    end;
  Result := DoForceDirectories(SetDirSeparators(Dir));
end;

procedure TMainForm.SHA1RadioButton3Change(Sender: TObject);
begin

end;

procedure TMainForm.TabSheet3ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

initialization
  {$I unit2.lrs}

end.

