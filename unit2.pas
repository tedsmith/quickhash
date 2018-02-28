{
    Quick Hash - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
    and generating hash values for them.

    Copyright (C) 2011-2018  Ted Smith www.quickhash-gui.org

    The use of the word 'quick' refers to the ease in which the software operates
    in both Linux, Apple Mac and Windows (very few options to worry about, no
    syntax to remember etc) though tests suggest that in most cases the hash
    values are generated as quick or quicker than most mainstream tools.

    The user should be aware of other data hashing tools and use them to cross-check
    findings for critical data :
    md5sum, sha1sum, sha256sum and sha512sum (for Linux),
    FTK Imager, X-Ways Forensics, WinHex, EnCase, FTK (Windows) and many more

    Benchmark tests are welcomed.

    Contributions from members at the Lazarus forums, Stackoverflow and other
    StackExchnage groups are welcomed and acknowledged. Contributions from
    DaReal Shinji are also welcomed and acknowledged, particularly helping with
    Debian package creation and ideas

    NOTE: Date and time values, as computed in recursive directory hashing, are not
    daylight saving time adjusted. Source file date and time values are recorded.

    Open-Source license:

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

    HashLib4Pascal and xxHash64 libraries are both licensed under the MIT License
    https://opensource.org/licenses/MIT

    HashLib4Pascal : https://github.com/Xor-el/HashLib4Pascal and developed by
                     Github user Xor-el (Ugochukwu Stanley). Use of the
                     library is welcomed and acknowledged and very much appreciated,
                     as is the help that was offered by the developer of said library

    xxHash64       : https://github.com/Cyan4973/xxHash and http://cyan4973.github.io/xxHash/
                     Github user Cyan4973. Use of the library is also welcomed and acknowledged
                     and very much appreciated

    QuickHash is created using the Freepascal Compiler and Lazarus-IDE
    http://www.lazarus-ide.org/ developed by Sourceforge users :
    mgaertner,
    mhess,
    user4martin,
    vlx,
    vsnijders

    QuickHash was first registered on sourceforge on 29th May 2011 and was later
    migrated to the domain www.quickhash-gui.org in December 2016.
    Read more about it's development history online at :
    https://quickhash-gui.org/about-quickhash-gui/

}

unit Unit2; // Unit 1 was superseeded with v2.0.0

{$mode objfpc}{$H+} // {$H+} ensures strings are of unlimited size

interface

uses

  {$IFDEF UNIX}
    {$IFDEF UseCThreads}
      cthreads,
    {$ENDIF}
  {$ENDIF}

  Classes, SysUtils, Strutils, FileUtil, LResources, Forms, Controls,
  Graphics, Dialogs, StdCtrls, Menus, ComCtrls, LazUTF8, LazUTF8Classes,
  LazFileUtils, Grids, ExtCtrls, sysconst, lclintf, ShellCtrls,
  XMLPropStorage, uDisplayGrid, diskmodule, clipbrd, DBGrids, DbCtrls,
  ZVDateTimePicker, frmAboutUnit, base64,

  FindAllFilesEnhanced, // an enhanced version of FindAllFiles, to ensure hidden files are found, if needed

  // New as of v2.8.0 - HashLib4Pascal Unit, superseeds DCPCrypt.
  HlpHashFactory,
  HlpIHash,
  HlpIHashResult,

  // New as of v3.0.0
  dbases_sqlite,
  // Also new as of v3.0.0, for creating hash lists for faster comparisons of two folders
  contnrs,
  // Also new as of v3.0.0, for importing hash lists
  uKnownHashLists,

  // Remaining Uses clauses for specific OS's
  {$IFDEF Windows}
    Windows,
    // For Windows, this is a specific disk hashing tab for QuickHash. Not needed for Linux
     types;
  {$ENDIF}
  {$IFDEF Darwin}
    MacOSAll;
  {$else}
    {$IFDEF UNIX and !$ifdef Darwin}
      UNIX;
    {$ENDIF}
  {$ENDIF}
  { Deprecated uses clauses, discarded as a result of migrating to HashLib4Pascal
     with QuickHash v2.8.0 in Feb 2017.

  // previously we had to use a customised MD5 & SHA-1 library to process Unicode on Windows and
  // to run a customised MD5Transform and SHA1Transform function that was converted to assembly.
  // No longer needed but the source code remains in the project because the
  // Assembly transforms that forum user Engkin helped me with rocked!

  md5customised,
  sha1customised,

  // The DCPCrypt library was used for SHA256 and SHA512 which are not part of FPC
  // but as of v2.80, DCPCrypt was discarded in favour of HashLib4Pascal

  DCPsha512, DCPsha256, DCPsha1, DCPmd5,
  }
type

  { TMainForm }

   MEMORYSTATUSEX = record
     dwLength : DWORD;
     dwMemoryLoad : DWORD;
     ullTotalPhys : uint64;
     ullAvailPhys : uint64;
     ullTotalPageFile : uint64;
     ullAvailPageFile : uint64;
     ullTotalVirtual : uint64;
     ullAvailVirtual : uint64;
     ullAvailExtendedVirtual : uint64;
  end;

   TMainForm = class(TForm)
    AlgorithmChoiceRadioBox3: TRadioGroup;
    AlgorithmChoiceRadioBox4: TRadioGroup;
    AlgorithmChoiceRadioBox1: TRadioGroup;
    AlgorithmChoiceRadioBox6: TRadioGroup;
    AlgorithmChoiceRadioBox5: TRadioGroup;
    AlgorithmChoiceRadioBox7: TRadioGroup;
    b64FileGridPopupMenu: TPopupMenu;
    b64DecoderProgress: TEdit;
    b64StringGrid2FileS: TStringGrid;
    btnClearTextArea: TButton;
    btnCompare: TButton;
    btnCompareTwoFiles: TButton;
    btnCompareTwoFilesSaveAs: TButton;
    btnFileACompare: TButton;
    btnFileBCompare: TButton;
    btnFLBL: TButton;
    btnHashFile: TButton;
    btnLBL: TButton;
    btnRecursiveDirectoryHashing: TButton;
    btnClipboardResults: TButton;
    btnCallDiskHasherModule: TButton;
    btnStopScan1: TButton;
    btnStopScan2: TButton;
    btnClearHashField: TButton;
    btnB64FileChooser: TButton;
    btnB64FileSChooser: TButton;
    btnB64JustDecodeFiles: TButton;
    btnMakeTextUpper: TButton;
    btnMakeTextLower: TButton;
    btnLoadHashList: TButton;
    Button8CopyAndHash: TButton;
    cbFlipCaseFILE: TCheckBox;
    cbToggleInputDataToOutputFile: TCheckBox;
    b64ProgressFileS: TEdit;
    cbFlipCaseTEXT: TCheckBox;
    cbUNCModeCompFolders: TCheckBox;
    cbSaveComparisons: TCheckBox;
    cbOverrideFileCountDiffer: TCheckBox;
    cbLoadHashList: TCheckBox;
    edtUNCPathCompareA: TEdit;
    edtUNCPathCompareB: TEdit;
    FileSDBNavigator: TDBNavigator;
    lblTotalFileCountNumberA: TLabel;
    lblTotalFileCountA: TLabel;
    lblCompareTwoFoldersInstruction1: TLabel;
    lblCompareTwoFoldersInstruction2: TLabel;

    lblTotalFileCountB: TLabel;
    lblTotalFileCountNumberB: TLabel;
    memFolderCompareSummary: TMemo;
    MenuItem_FilterOutYes: TMenuItem;
    MenuItem_FilterOutNo: TMenuItem;
    MenuItem_SortByHashList: TMenuItem;
    MenuItem_SortByID: TMenuItem;
    MenuItem_DeleteDups: TMenuItem;
    MenuItem_SaveFILESTabToHTML: TMenuItem;
    MenuItem_CopyGridToClipboardFILES: TMenuItem;
    MenuItem_CopySelectedRow: TMenuItem;
    MenuItem_SaveToHTML: TMenuItem;
    HashListChooserDialog: TOpenDialog;
    pbCompareDirA: TProgressBar;
    pbCompareDirB: TProgressBar;
    RecursiveDisplayGrid1: TDBGrid;
    MenuItem_CopyFilepathOfSelectedCell: TMenuItem;
    MenuItem_CopyHashOfSelectedCell: TMenuItem;
    MenuItem_CopyFileNameOfSelectedCell: TMenuItem;
    MenuItem_CopySelectedRowFILESTAB: TMenuItem;
    MenuItem_SortByFilePath: TMenuItem;
    MenuItem_SortByFilename: TMenuItem;
    MenuItem_SortByHash: TMenuItem;
    MenuItem_RestoreListFILES: TMenuItem;
    MenuItem_SaveToCSV: TMenuItem;
    MenuItem_ShowDuplicates: TMenuItem;
    popmenuDBGrid_Files: TPopupMenu;
    lblPercentageProgressFileTab: TLabel;
    lblB64Warning: TLabel;
    lblB64DecoderWarning: TLabel;
    lblNoOfFilesToExamine2: TLabel;
    lblschedulertickboxCompareTab: TCheckBox;
    lblschedulertickboxCompareDirsTab: TCheckBox;
    lblschedulertickboxFileSTab: TCheckBox;
    lblschedulertickboxCopyTab: TCheckBox;
    lblschedulertickboxFileTab: TCheckBox;
    edtFileBName: TEdit;
    edtFileAName: TEdit;
    FileTypeMaskCheckBox2: TCheckBox;
    chkUNCMode: TCheckBox;
    chkHiddenFiles: TCheckBox;
    chkCopyHidden: TCheckBox;
    CheckBoxListOfDirsAndFilesOnly: TCheckBox;
    CheckBoxListOfDirsOnly: TCheckBox;
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
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label15: TLabel;
    lbEndedFileAt: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem2A: TMenuItem;
    MenuItem1C: TMenuItem;
    MenuItem1A: TMenuItem;
    MenuItem1B: TMenuItem;
    b64FileChooserDialog: TOpenDialog;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pbFileS: TProgressBar;
    pbCopy: TProgressBar;
    b64FilesGridPopupMenu: TPopupMenu;
    b64SaveDialog: TSaveDialog;
    pbFile: TProgressBar;
    FilesDBGrid_SaveCSVDialog: TSaveDialog;
    FilesSaveAsHTMLDialog: TSaveDialog;
    sdFileAndFolderListOnly: TSaveDialog;
    sdHashListLookupResults: TSaveDialog;
    SaveErrorsCompareDirsSaveDialog8: TSaveDialog;
    b64FileSChooserDialog: TSelectDirectoryDialog;
    b64FileSSourceDecoderDialog: TSelectDirectoryDialog;
    b64FileSDestinationDecoderDialog: TSelectDirectoryDialog;
    ShellTreeView_FolderA: TShellTreeView;
    ShellTreeView_FolderB: TShellTreeView;
    StatusBar6: TStatusBar;
    b64StringGrid1File: TStringGrid;
    SystemRAMGroupBox: TGroupBox;
    ImageList1: TImageList;
    lblRAM: TLabel;
    lbleExpectedHash: TLabeledEdit;
    lbleExpectedHashText: TLabeledEdit;
    lblURLBanner: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lblFolderAName: TLabel;
    lblFolderBName: TLabel;
    lblFileAHash: TLabel;
    lblFileBHash: TLabel;
    lblFilesCopiedPercentage: TLabel;
    lblDataCopiedSoFar: TLabel;
    lblHashMatchResult: TLabel;
    lblNoOfFilesToExamine: TLabel;
    lblPercentageComplete: TLabel;
    lblTotalBytesExamined: TLabel;
    lblFilesExamined: TLabel;
    lblNoFilesInDir: TLabel;
    lblDragAndDropNudge: TLabel;
    lblDiskHashingRunAsAdminWarning: TLabel;
    lblStatusA: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
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
    lblStartedFileAt: TLabel;
    lblFileTimeTaken: TLabel;
    AlgorithmChoiceRadioBox2: TRadioGroup;
    memFileHashField: TMemo;
    FLBLDialog: TOpenDialog;
    SaveDialog5: TSaveDialog;
    SaveDialog6: TSaveDialog;
    SaveDialog7: TSaveDialog;
    SelectDirectoryDialog4: TSelectDirectoryDialog;
    SelectDirectoryDialog5: TSelectDirectoryDialog;
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
    SaveToCSVCheckBox2: TCheckBox;
    SaveFILESTabToHTMLCheckBox2: TCheckBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SelectDirectoryDialog2: TSelectDirectoryDialog;
    SelectDirectoryDialog3: TSelectDirectoryDialog;
    sysRAMTimer: TTimer;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    TextHashingGroupBox: TGroupBox;
    QH_MainFormXMLPropStorage: TXMLPropStorage;
    SchedulerTimer: TTimer;
    TextHashingGroupBox1: TGroupBox;
    ZVDateTimePickerCompareDirsTab: TZVDateTimePicker;
    ZVDateTimePickerCopyTab    : TZVDateTimePicker;
    ZVDateTimePickerCompareTab : TZVDateTimePicker;
    ZVDateTimePickerFileTab    : TZVDateTimePicker;
    ZVDateTimePickerFileSTab   : TZVDateTimePicker;
    procedure AlgorithmChoiceRadioBox1Click(Sender: TObject);
    procedure AlgorithmChoiceRadioBox2Click(Sender: TObject);
    procedure AlgorithmChoiceRadioBox3Click(Sender: TObject);
    procedure AlgorithmChoiceRadioBox4Click(Sender: TObject);
    procedure AlgorithmChoiceRadioBox5Click(Sender: TObject);
    procedure AlgorithmChoiceRadioBox6Click(Sender: TObject);
    procedure btnB64FileSChooserClick(Sender: TObject);
    procedure btnClearHashFieldClick(Sender: TObject);
    procedure btnClearHashFieldKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure btnB64FileChooserClick(Sender: TObject);
    procedure btnB64JustDecodeFilesClick(Sender: TObject);
    procedure btnLoadHashListClick(Sender: TObject);
    procedure btnMakeTextLowerClick(Sender: TObject);
    procedure btnMakeTextUpperClick(Sender: TObject);
    procedure cbFlipCaseFILEChange(Sender: TObject);
    procedure cbFlipCaseTEXTChange(Sender: TObject);
    procedure cbLoadHashListChange(Sender: TObject);
    procedure cbOverrideFileCountDifferChange(Sender: TObject);
    procedure cbSaveComparisonsChange(Sender: TObject);
    procedure cbToggleInputDataToOutputFileChange(Sender: TObject);
    procedure cbUNCModeCompFoldersChange(Sender: TObject);
    procedure edtUNCPathCompareAChange(Sender: TObject);
    procedure edtUNCPathCompareBChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure lblDonateClick(Sender: TObject);
    procedure lbleExpectedHashChange(Sender: TObject);
    procedure lbleExpectedHashEnter(Sender: TObject);
    procedure lbleExpectedHashTextChange(Sender: TObject);
    procedure lblFileAHashClick(Sender: TObject);
    procedure lblFileBHashClick(Sender: TObject);
    procedure lblschedulertickboxFileSTabChange(Sender: TObject);
    procedure lblschedulertickboxFileTabChange(Sender: TObject);
    procedure lblschedulertickboxCopyTabChange(Sender: TObject);
    procedure lblschedulertickboxCompareTabChange(Sender: TObject);
    procedure lblschedulertickboxCompareTwoDirectoriesTabChange(Sender: TObject);
    procedure MenuItem1AClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2AClick(Sender: TObject);
    procedure MenuItem1CClick(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    procedure MenuItem6Click(Sender: TObject);
    procedure MenuItem_DeleteDupsClick(Sender: TObject);
    procedure MenuItem_CopyGridToClipboardFILESClick(Sender: TObject);
    procedure MenuItem_CopyHashOfSelectedCellClick(Sender: TObject);
    procedure MenuItem_CopyFilepathOfSelectedCellClick(Sender: TObject);
    procedure MenuItem_CopyFileNameOfSelectedCellClick(Sender: TObject);
    procedure MenuItem_CopySelectedRowFILESTABClick(Sender: TObject);
    procedure MenuItem_FilterOutNoClick(Sender: TObject);
    procedure MenuItem_FilterOutYesClick(Sender: TObject);
    procedure MenuItem_RestoreListFILESClick(Sender: TObject);
    procedure MenuItem_SaveToCSVClick(Sender: TObject);
    procedure MenuItem_SaveToHTMLClick(Sender: TObject);
    procedure MenuItem_ShowDuplicatesClick(Sender: TObject);
    procedure MenuItem_SortByFilenameClick(Sender: TObject);
    procedure MenuItem_SortByFilePathClick(Sender: TObject);
    procedure MenuItem_SortByHashClick(Sender: TObject);
    procedure MenuItem_SortByHashListClick(Sender: TObject);
    procedure MenuItem_SortByIDClick(Sender: TObject);
    procedure Panel1CopyAndHashOptionsClick(Sender: TObject);
    procedure popmenuDBGrid_FilesPopup(Sender: TObject);
    procedure ShellTreeView_FolderAChange(Sender: TObject; Node: TTreeNode);
    procedure ShellTreeView_FolderBChange(Sender: TObject; Node: TTreeNode);
    procedure sysRAMTimerTimer(Sender: TObject);
    procedure AlgorithmChoiceRadioBox2SelectionChanged(Sender: TObject);
    procedure AlgorithmChoiceRadioBox5SelectionChanged(Sender: TObject);
    procedure btnClipboardHashValueClick(Sender: TObject);
    procedure btnCompareTwoFilesClick(Sender: TObject);
    procedure btnCompareTwoFilesSaveAsClick(Sender: TObject);
    procedure btnDirAClick(Sender: TObject);
    procedure btnDirBClick(Sender: TObject);
    procedure btnFileACompareClick(Sender: TObject);
    procedure btnFileBCompareClick(Sender: TObject);
    //procedure btnHashTextClick(Sender: TObject);
    procedure btnHashFileClick(Sender: TObject);
    procedure btnLaunchDiskModuleClick(Sender: TObject);
    procedure btnLBLClick(Sender: TObject);
    procedure btnRecursiveDirectoryHashingClick(Sender: TObject);
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
    procedure ProcessDir(SourceDirName: string);
    procedure CompareTwoHashes(FileAHash, FileBHash : string);
    procedure HashText(Sender: TObject);
    procedure ClearText(Sender: TObject);
    procedure TabSheet6ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    function  ValidateTextWithHash(strToBeHashed:ansistring): string;
    function  CalcTheHashString(strToBeHashed:ansistring):string;
    function  CalcTheHashFile(FileToBeHashed:string):string;
    function  FormatByteSize(const bytes: QWord): string;
    function  RemoveLongPathOverrideChars(strPath : string; LongPathOverrideVal : string) : string;
    procedure SaveOutputAsCSV(Filename : string; GridName : TStringGrid);
    procedure EmptyDisplayGrid(Grid : TStringGrid);
    procedure CheckSchedule(DesiredStartTime : TDateTime);
    procedure InvokeScheduler(Sender : TObject);
    function RoundToNearest(TheDateTime,TheRoundStep:TDateTime):TdateTime;
    procedure CommitCount(Sender : TObject);
    function RetrieveFileList(FolderName : string) : TStringList;
    function HashFolderAList(Path : string; slFileListA : TStringList; intFileCount : integer; SaveData : Boolean) : TFPHashList;
    function HashFolderBList(Path : string; slFileListB : TStringList; intFileCount : integer; SaveData : Boolean) : TFPHashList;
    function CompareHashLists(aHashList1, aHashlist2: TFPHashList): Boolean;
    function ComputeWhatHashesAreMissing(aHashList1, aHashList2 : TFPHashList) : TStringList;
    function GetSubDirListing(FolderName : string) : TStringList;
    function GetSubDirAndFileListing(FolderName : string) : TStringList;
    // function FileSizeWithLongPath(strFileName : string) : Int64;
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
    procedure TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TabSheet3ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);

  private
   // Global handle exception controller, courtesy of GetMem from the forums
   // http://forum.lazarus.freepascal.org/index.php/topic,39842.0.html
   procedure HandleExceptions(Sender: TObject; E: Exception);
    { private declarations }
  public
    { public declarations }

   FileCounter, NoOfFilesInDir2: integer; // Used jointly by Button3Click and Hashfile procedures
   CommitFrequencyChecker : integer; // To keep track of SQLite commits
   TotalBytesRead : UInt64;
   StopScan1, StopScan2, SourceDirValid, DestDirValid : Boolean;
   SourceDir, DestDir : string; // For the joint copy and hash routines

    DirA, DirB : string;
   sValue1 : string; // Set by GetWin32_DiskDriveInfo then used by ListDisks OnClick event - Windows only

   slMultipleDirNames : TStringList;
   fsSaveFolderComparisonsLogFile : TFileStream;

   MultipleDirsChosen, StartHashing : boolean;

   tmp : integer;

   {$IFDEF WINDOWS}
   // For coping better with 260 MAX_PATH limits of Windows. Instead we invoke Unicode
   // variant of FindAllFiles by using '\\?\' and '\\?\UNC\' prefixes. LongPathOverride
   // will always either be '\\?\' or '\\?\UNC\'

   LongPathOverride : string;

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

  {$R *.lfm}

// Global function, CommitCount, keeps track of file counts and updates the SQLIte DB periodically
// to avoid unnecessary database commits, which slow it down

implementation

procedure TMainForm.HandleExceptions(Sender: TObject; E: Exception);
begin
// see http://forum.lazarus.freepascal.org/index.php/topic,39842.0.html
end;

procedure TMainForm.CommitCount(Sender : TObject);
begin
  inc(CommitFrequencyChecker, 1);
  if CommitFrequencyChecker = 1000 then
  begin
    frmSQLiteDBases.SQLTransaction1.CommitRetaining;
    CommitFrequencyChecker := 0;
  end;
end;

{$IFDEF WINDOWS}
// Populate interface with quick view to RAM status
function GlobalMemoryStatusEx(var Buffer: MEMORYSTATUSEX): BOOL; stdcall; external 'kernel32' name 'GlobalMemoryStatusEx';
{$ENDIF}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  x, y : integer;

begin
  x := screen.Width;
  y := screen.Height;
  tmp := 1;

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

  StartHashing := false;
  StopScan1 := false;
  StopScan2 := false;

  {$ifdef Windows}
  // These are the default values to be prefixed before a path to invoke the 32K
  // NTFS filename length over the 260 MAX_PATH. Where the user opts for UNC paths
  // as well, it becomes '\\?\UNC\'
  LongPathOverride := '\\?\';

  {$endif}
  // In Lazarus versions  < 1.4.4, the 'FileSortType' property of ShellTreeViews
  // would cause the listing to be doubled if anything other than fstNone was chosen
  // So this will ensure I have sorting until that is fixed.
  // http://bugs.freepascal.org/view.php?id=0028565
  DirListA.AlphaSort;
  DirListB.AlphaSort;

  // The DBGrid in FileS tab to be hidden initially
  RecursiveDisplayGrid1.Visible:= false;

  {$ifdef CPU64}
  AlgorithmChoiceRadioBox1.Items.Strings[4] := 'xxHash64';
  AlgorithmChoiceRadioBox2.Items.Strings[4] := 'xxHash64';
  AlgorithmChoiceRadioBox3.Items.Strings[4] := 'xxHash64';
  AlgorithmChoiceRadioBox4.Items.Strings[4] := 'xxHash64';
  AlgorithmChoiceRadioBox5.Items.Strings[4] := 'xxHash64';
  AlgorithmChoiceRadioBox6.Items.Strings[4] := 'xxHash64';
  {$else if CPU32}
  AlgorithmChoiceRadioBox1.Items.Strings[4] := 'xxHash32';
  AlgorithmChoiceRadioBox2.Items.Strings[4] := 'xxHash32';
  AlgorithmChoiceRadioBox3.Items.Strings[4] := 'xxHash32';
  AlgorithmChoiceRadioBox4.Items.Strings[4] := 'xxHash32';
  AlgorithmChoiceRadioBox5.Items.Strings[4] := 'xxHash32';
  AlgorithmChoiceRadioBox6.Items.Strings[4] := 'xxHash32';
  {$endif}

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
    SystemRAMGroupBox.Visible := true;
    sysRAMTimer.enabled := true;
    lblRAM.Caption := GetSystemMem;
    Edit2SourcePath.Enabled:=true;
    Edit2SourcePath.Visible:=true;
    Edit3DestinationPath.Enabled:=true;
    Edit3DestinationPath.Visible:=true;
  {$ENDIF}

  {$IFDEF Windows}
    btnCallDiskHasherModule.Enabled := true;
  {$ENDIF}
    {$IFDEF Darwin}
      btnCallDiskHasherModule.Enabled := false; // disabled for OSX currently
    {$else}
      {$IFDEF UNIX and !$ifdef Darwin}
        btnCallDiskHasherModule.Enabled := true; // as of v2.7.0 - disabled for Linux previously
      {$ENDIF}
  {$ENDIF}

  {$IFDEF LINUX}
   Label8.Caption := 'LINUX USERS - You may prefer to hash disks using ' + #13#10 +
                     'the "File" tab and navigate to /dev/sdX or /dev/sdXX as root';

   // For Linux users, it's helpful for the user to see as a full path the folder
   // they have chosen, so make source and destination edit fields visible, but
   // disabled, as we don't want them to be used.
   Edit2SourcePath.Visible:= true;
   Edit2SourcePath.Enabled:= false;
   Edit3DestinationPath.Visible:=true;
   Edit3DestinationPath.Enabled:=false;

   Tabsheet5.Enabled      := true;
   Tabsheet5.Visible      := true;
   chkCopyHidden.Enabled  := true;
   chkCopyHidden.ShowHint := true;
   chkCopyHidden.Hint     := 'In Linux, tick this to ensure hidden directories and hidden files in them are detected, if you want them';

   // UNC mode is for Windows only so disable in Linux
   chkUNCMode.Enabled            := false;
   chkUNCMode.Visible            := false;
   cbUNCModeCompFolders.Enabled  := false;
   cbUNCModeCompFolders.Visible  := false;
   Edit2SourcePath.Text          := 'Source directory selection';
   Edit3DestinationPath.Text     := 'Destination directory selection';

   // RAM status stuff needs to be disabled on Linux
   sysRAMTimer.enabled       := false;
   SystemRAMGroupBox.Visible := false;
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
      chkUNCMode.Enabled             := false;
      chkUNCMode.Visible             := false;
      cbUNCModeCompFolders.Enabled   := false;
      cbUNCModeCompFolders.Visible   := false;
      Edit2SourcePath.Text           := 'Source directory selection';
      Edit3DestinationPath.Text      := 'Destination directory selection';
    {$ENDIF}
 {$ENDIF}
end;

// Checks if the desired start date and time has arrived yet by starting timer
// If it has, disable timer. Otherwise, keep it going.
procedure TMainForm.CheckSchedule(DesiredStartTime : TDateTime);
var
  t : TDateTime;
begin
  t := Now;
  // Round the chosen time and the current time to the nearest second
  // https://stackoverflow.com/questions/4122218/in-delphi-how-do-i-round-a-tdatetime-to-closest-second-minute-five-minute-etc
  t := RoundToNearest(t, EncodeTime(0,0,1,0));
  DesiredStartTime := RoundToNearest(DesiredStartTime, EncodeTime(0,0,1,0));
  if t = DesiredStartTime then
    begin
      SchedulerTimer.Enabled := false;
      StartHashing := true;
    end
  else
  begin
    // and to avoid 100% CPU usage, sleep every 1/3 of a second
    sleep(300);
    SchedulerTimer.Enabled := true;
    StartHashing := false;
  end;
end;

function TMainForm.RoundToNearest(TheDateTime,TheRoundStep:TDateTime):TdateTime;
  begin
  if 0=TheRoundStep
  then
    begin // If round step is zero there is no round at all
      RoundToNearest:=TheDateTime;
    end
  else
  begin // Just round to nearest multiple of TheRoundStep
    RoundToNearest:=Round(TheDateTime/TheRoundStep)*TheRoundStep;
  end;
end;

// Start a timer schedule for future hashing
procedure TMainForm.InvokeScheduler(Sender : TObject);
var
  scheduleStartTime : TDateTime;
begin
  // File Tab scheduling
  if PageControl1.ActivePage = TabSheet2 then  // File tab
    begin
    if ZVDateTimePickerFileTab.DateTime < Now then
      begin
        ShowMessage('Scheduled start time is in the past. Correct it.');
        exit;
      end
      else begin
        StartHashing := false;
        scheduleStartTime     := ZVDateTimePickerFileTab.DateTime;
        StatusBar1.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM', schedulestarttime);
        // Set the interval as the milliseconds remaining until the future start time
        SchedulerTimer.Interval:= trunc((schedulestarttime - Now) * 24 * 60 * 60 * 1000);
        // and then enable the timer
        SchedulerTimer.Enabled := true;
        // and then check if current date and time is equal to desired scheduled date and time
        repeat
          Application.ProcessMessages;
          CheckSchedule(scheduleStartTime);
        until (StartHashing = true);
      end
    end
  // FileS Tab scheduling
  else if PageControl1.ActivePage = TabSheet3 then  // FileS tab
    begin
    if ZVDateTimePickerFileSTab.DateTime < Now then
      begin
        ShowMessage('Scheduled start time is in the past. Correct it.');
        exit;
      end
      else begin
        StartHashing := false;
        scheduleStartTime     := ZVDateTimePickerFileSTab.DateTime;
        StatusBar2.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM', schedulestarttime);
        // Set the interval as the milliseconds remaining until the future start time
        SchedulerTimer.Interval:= trunc((schedulestarttime - Now) * 24 * 60 * 60 * 1000);
        // and then enable the timer
        SchedulerTimer.Enabled := true;
        // and then check if current date and time is equal to desired scheduled date and time
        repeat
          Application.ProcessMessages;
          CheckSchedule(scheduleStartTime);
        until (StartHashing = true);
      end;
    end
  else if PageControl1.ActivePage = TabSheet4 then  // Copy tab
    begin
    if ZVDateTimePickerCopyTab.DateTime < Now then
      begin
        ShowMessage('Scheduled start time is in the past. Correct it.');
        exit;
      end
      else begin
        StartHashing := false;
        scheduleStartTime     := ZVDateTimePickerCopyTab.DateTime;
        StatusBar3.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM', schedulestarttime);
        // Set the interval as the milliseconds remaining until the future start time
        SchedulerTimer.Interval:= trunc((schedulestarttime - Now) * 24 * 60 * 60 * 1000);
        // and then enable the timer
        SchedulerTimer.Enabled := true;
        // and then check if current date and time is equal to desired scheduled date and time
        repeat
          Application.ProcessMessages;
          CheckSchedule(scheduleStartTime);
        until (StartHashing = true);
      end
    end
  // Compare Two Files scheduler
  else if PageControl1.ActivePage = TabSheet5 then  // Compare Two Files tab
    begin
    if ZVDateTimePickerCompareTab.DateTime < Now then
      begin
        ShowMessage('Scheduled start time is in the past. Correct it.');
        exit;
      end
    else begin
        StartHashing := false;
        scheduleStartTime     := ZVDateTimePickerCompareTab.DateTime;
        StatusBar4.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM', schedulestarttime);
        // Set the interval as the milliseconds remaining until the future start time
        SchedulerTimer.Interval:= trunc((schedulestarttime - Now) * 24 * 60 * 60 * 1000);
        // and then enable the timer
        SchedulerTimer.Enabled := true;
        // and then check if current date and time is equal to desired scheduled date and time
        repeat
         Application.ProcessMessages;
         CheckSchedule(scheduleStartTime);
        until (StartHashing = true);
      end;
    end
  else if PageControl1.ActivePage = TabSheet6 then  // Compare Two Folders tab
    begin
    if ZVDateTimePickerCompareDirsTab.DateTime < Now then
      begin
        ShowMessage('Scheduled start time is in the past. Correct it.');
        exit;
      end
    else begin
        StartHashing := false;
        scheduleStartTime     := ZVDateTimePickerCompareDirsTab.DateTime;
        StatusBar6.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM', schedulestarttime);
        // Set the interval as the milliseconds remaining until the future start time
        SchedulerTimer.Interval:= trunc((schedulestarttime - Now) * 24 * 60 * 60 * 1000);
        // and then enable the timer
        SchedulerTimer.Enabled := true;
        // and then check if current date and time is equal to desired scheduled date and time
        repeat
         Application.ProcessMessages;
         CheckSchedule(scheduleStartTime);
        until (StartHashing = true);
      end;
    end;
end;

// FormDropFiles is the same as btnHashFileClick, except it disables the OpenDialog
// element and computes the filename from the drag n drop variable and hashes the file.
procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);

var
  filename, fileHashValue : ansistring;
  start, stop, elapsed, scheduleStartTime : TDateTime;
  LoopCounter : integer;
begin
  // First, clear the captions from any earlier file hashing actions
  StatusBar1.SimpleText    := '';
  lblStartedFileAt.Caption := '...';
  lblFileTimeTaken.Caption := '...';
  memFileHashField.Clear;
  LoopCounter              := 0;

   begin
    filename := FileNames[0];
    if LazFileUtils.DirectoryExistsUTF8(filename) then
    begin
      ShowMessage('Drag and drop of folders is not supported in this tab.');
    end
    else
    // User has selected a file, so check its valid
      if LazFileUtils.FileExistsUTF8(filename) then
      begin
        // Now start a scheduled time, if selected
        if lblschedulertickboxFileTab.Checked then
          begin
            InvokeScheduler(self);
          end;

      start := Now;
      lblStartedFileAt.Caption := 'Started at : '+ FormatDateTime('dd/mm/yyyy hh:mm:ss', Start);

      edtFileNameToBeHashed.Caption := (filename);
      StatusBar1.SimpleText := ' H A S H I N G  F I L E...P L E A S E  W A I T';
      Application.ProcessMessages;
      fileHashValue := CalcTheHashFile(Filename); // Custom function
      memFileHashField.Lines.Add(UpperCase(fileHashValue));
      StatusBar1.SimpleText := ' H A S H I N G  C OM P L E T E !';

      OpenDialog1.Close;

      stop := Now;
      elapsed := stop - start;

      lbEndedFileAt.Caption    := 'Ended at   : '+ DateTimeToStr(stop);
      lblFileTimeTaken.Caption := 'Time taken : '+ TimeToStr(elapsed);
      Application.ProcessMessages;

      // If the user has ane existing hash to check, compare it here
      if (lbleExpectedHash.Text = '') then exit
      else
        if (lbleExpectedHash.Text <> '...') then
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
     end
  else
    ShowMessage('An error occured opening the file. Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
  end;
end;

procedure TMainForm.HashText(Sender: TObject);
var
  s : string;
  strHashValueOfText : string;
begin
  // Initialise case sensitivity to the default of no conversion

  if memoHashText.Lines[0] = 'Type or paste text here - hash will update as you type' then
  begin
    StrHashValue.Caption := 'Awaiting valid input in text field...';
  end
  else
    if Length(memoHashText.Text) = 0 then
      begin
        StrHashValue.Caption := 'Awaiting input in text field...';
      end
      else
        begin
         s := memoHashText.Text;
         strHashValueOfText := Trim(CalcTheHashString(s));
         StrHashValue.Caption := strHashValueOfText;

         // If the user has pasted an expected hash, see if they match
         if (lbleExpectedHash.Text = '') then exit
         else
           if (lbleExpectedHash.Text = '...') then exit
           else
             if strHashValueOfText = Trim(Uppercase(lbleExpectedHashText.Text)) then
               begin
                 Showmessage('Expected hash matches the generated text hash, OK');
               end
             else
               begin
                 Showmessage('Expected hash DOES NOT match the generated text hash!');
               end;
        end;
end;

procedure TMainForm.btnHashFileClick(Sender: TObject);
var
  filename : string;
  fileHashValue : ansistring;
  start, stop, elapsed, scheduleStartTime : TDateTime;
  LoopCounter : integer;
begin
  PageControl1.ActivePage := Tabsheet2;  // Ensure File tab activated if triggered via menu
  filename := '';
  StatusBar1.SimpleText := '';
  LoopCounter := 0;

  if OpenDialog1.Execute then
    begin
      filename := OpenDialog1.Filename;
    end;
  // First, clear the captions from any earlier file hashing actions
  lblStartedFileAt.Caption := '';
  lbEndedFileAt.Caption    := '';
  lblFileTimeTaken.Caption := '';
  memFileHashField.Clear;

  if LazFileUtils.FileExistsUTF8(filename) then
    begin
      if FileSize(filename) > 0 then
      begin
        if lblschedulertickboxFileTab.Checked then
        begin
          if ZVDateTimePickerFileTab.DateTime < Now then
          begin
            ShowMessage('Scheduled start time is in the past. Correct it.');
            exit;
          end
          else
          scheduleStartTime     := ZVDateTimePickerFileTab.DateTime;
          StatusBar1.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM:SS', schedulestarttime);
            repeat
              // This sleep loop avoids straining the CPU too much but also ensures the
              // interface stays responsive to button clicks etc.
              // So every 1K itteration, refresh the interface until the scheduled start
              // arrives or the user clicks Abort.
              inc(LoopCounter,1);
              if LoopCounter = 1000 then
                begin
                  Application.ProcessMessages;
                  LoopCounter := 0;
                end;
              sleep(0);
            until scheduleStartTime = Now;
        end;

        start := Now;
        lblStartedFileAt.Caption := 'Started at : '+ FormatDateTime('dd/mm/yyyy hh:mm:ss', Start);

        edtFileNameToBeHashed.Caption := (filename);
        StatusBar1.SimpleText := ' H A S H I N G  F I L E...P L E A S E  W A I T';
        Application.ProcessMessages;
        fileHashValue := CalcTheHashFile(Filename); // Custom function
        memFileHashField.Lines.Add(UpperCase(fileHashValue));
        StatusBar1.SimpleText := ' H A S H I N G  C OM P L E T E !';

        OpenDialog1.Close;

        stop := Now;
        elapsed := stop - start;
        lbEndedFileAt.Caption    := 'Ended at   : ' + FormatDateTime('DD/MM/YYYY HH:MM:SS', stop);
        lblFileTimeTaken.Caption := 'Time taken : ' + FormatDateTime('HH:MM:SS', elapsed);
        Application.ProcessMessages;

        // If the user has ane existing hash to check in expected hash value field,
        // compare it here
        lbleExpectedHashChange(Sender);
      end
      else
      begin
       ShowMessage('File size is zero. The file cannot be hashed');
       Abort;
      end;
    end;
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
  slLBL : TStringListUTF8;
  i     : Longword;
  strToHash : string;
begin
  if memoHashText.Lines.Count = 0 then
    begin
      ShowMessage('Enter text into the text field first.');
      exit;
    end;

  try
    slLBL := TStringListUTF8.Create;
    if not cbToggleInputDataToOutputFile.Checked then
      begin
      for i := 0 to memoHashText.Lines.Count -1 do
        begin
          strToHash := memoHashText.Lines[i];
          // Add the source data and the hash to the output
          slLBL.Add(strToHash + ',' + Trim(CalcTheHashString(strToHash)));
        end;
      end
    else
    begin
      for i := 0 to memoHashText.Lines.Count -1 do
        begin
          strToHash := memoHashText.Lines[i];
          // Add the hash only to the output
          slLBL.Add(Trim(CalcTheHashString(strToHash)));
        end;
    end;
  finally
    SaveDialog7.Title := 'Save line-by-line results as...';
    SaveDialog7.InitialDir := GetCurrentDir;
    SaveDialog7.Filter := 'Comma Sep|*.csv';
    SaveDialog7.DefaultExt := 'csv';

    if SaveDialog7.Execute then
      begin
        slLBL.SaveToFile(SaveDialog7.FileName);
        memoHashText.Text := 'See output file ' + SaveDialog7.FileName + ' for results.';
      end
    else ShowMessage('Unable to save file ' + SaveDialog7.FileName);
    slLBL.Free;
  end;
end;

// Load a text file and hash it line by line, outputting to the memo field
// Similar to the procedure for hashing the memo line-by-line in "Text" tab
procedure TMainForm.btnFLBLClick(Sender: TObject);
var
  slFLBLInput, slFLBLOutput : TStringListUTF8; // FLBL = File, Line by Line
  i : Longword;
begin
  i := 0;
  if FLBLDialog.Execute then
    begin
      try
      slFLBLInput := TStringListUTF8.Create;
      slFLBLOutput:= TStringListUTF8.Create;
      slFLBLInput.Sorted:= false;
      slFLBLOutput.Sorted:= false;

      // Load the input file to memory
      slFLBLInput.LoadFromFile(FLBLDialog.FileName);

      // Write the input to a new stringlist, hash each line
      if not cbToggleInputDataToOutputFile.Checked then
        begin
          for i := 0 to slFLBLInput.Count -1 do
            begin
             if Length(slFLBLInput.Strings[i]) > 0 then
              slFLBLOutput.Add(slFLBLInput.Strings[i] + ',' + Trim(CalcTheHashString(slFLBLInput.Strings[i])));
            end;
        end
      else
        begin
         for i := 0 to slFLBLInput.Count -1 do
          begin
            if Length(slFLBLInput.Strings[i]) > 0 then
            begin
              slFLBLOutput.Add(Trim(CalcTheHashString(slFLBLInput.Strings[i])));
            end;
          end;
      // If the output is smaller than 30Mb, load it to the screen
      if Length(slFLBLOutput.Text) < 30000000 then
        begin
        for i := 0 to slFLBLOutput.Count -1 do
          begin
            memoHashText.Lines.Add(slFLBLOutput.Strings[i]);
          end;
        {$ifdef Windows}
          memoHashText.Perform(EM_SCROLLCARET, 0, i);
        {$endif}
        end
      else
      // Otherwise, just save it and be done with it
        begin
          memoHashText.Clear;
          memoHashText.Lines.Add('Data set too large for display. Save the output file');
        end;
      end;

      finally
        SaveDialog7.Title      := 'Save line-by-line hashing results as...';
        SaveDialog7.InitialDir := GetCurrentDir;
        SaveDialog7.Filter     := 'Comma Sep|*.csv';
        SaveDialog7.DefaultExt := 'csv';
        SaveDialog7.FileName   := 'Output';
        if SaveDialog7.Execute then
          begin
            slFLBLOutput.SaveToFile(SaveDialog7.FileName);
            memoHashText.Lines.Add('Results saved to file : ' + SaveDialog7.FileName);
            // If MS Windows, launch 'Windows Explorer' to show the output file.
            {$ifdef windows}
            SysUtils.ExecuteProcess(LazUTF8.UTF8ToSys('explorer.exe'), '/select,'+ SaveDialog7.FileName, []);
            {$endif}
          end
        else
          begin
            ShowMessage('Unable to save output file ' + SaveDialog7.FileName);
          end;
        slFLBLOutput.Free;
        slFLBLInput.Free;
      end;
    end
  else ShowMessage('Unable to open text file for line-by-line analysis');
end;


procedure TMainForm.sysRAMTimerTimer(Sender: TObject);
{$ifdef windows}
var
  MemFigures : string;
{$endif}
begin
  {$IFDEF WINDOWS}
  MemFigures := GetSystemMem;
  lblRAM.Caption := MemFigures;
  {$ENDIF}
  // Do nothing with Linux
end;

procedure TMainForm.cbToggleInputDataToOutputFileChange(Sender: TObject);
begin
  if cbToggleInputDataToOutputFile.Checked then
    cbToggleInputDataToOutputFile.Caption := 'Source text EXcluded in output'
  else cbToggleInputDataToOutputFile.Caption := 'Source text INcluded in output';
end;

// Behaviours for the UNC tick box in "Compare Two Folders" tab
procedure TMainForm.cbUNCModeCompFoldersChange(Sender: TObject);
begin
  if cbUNCModeCompFolders.Checked then
    begin
    ShellTreeView_FolderA.Visible := false;
    ShellTreeView_FolderB.Visible := false;
    edtUNCPathCompareA.Visible:= true;
    edtUNCPathCompareB.Visible:= true;
    lblFolderAName.Caption:= '';
    lblFolderBName.Caption:= '';
    end
  else
  begin
    ShellTreeView_FolderA.Visible := true;
    ShellTreeView_FolderB.Visible := true;
    edtUNCPathCompareA.Visible:= false;
    edtUNCPathCompareB.Visible:= false;
    lblFolderAName.Caption:= '';
    lblFolderBName.Caption:= '';
  end;
end;

// Ensure the UNC path of FolderA is echoed in the path label
procedure TMainForm.edtUNCPathCompareAChange(Sender: TObject);
begin
  lblFolderAName.Caption := edtUNCPathCompareA.Text;
end;

// Ensure the UNC path of FolderB is echoed in the path label
procedure TMainForm.edtUNCPathCompareBChange(Sender: TObject);
begin
  lblFolderBName.Caption := edtUNCPathCompareB.Text;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  DataBasefilename : string;
begin
  if assigned(frmSQLiteDBases.SQLite3Connection1) then
    begin
      // Before closing DB connection, get the databasefilename
      DataBasefilename := frmSQLiteDBases.SQLite3Connection1.DatabaseName;
      // Now close the Database instances
      try
        frmSQLiteDBases.SQLite3Connection1.Close(true);
      finally
       frmSQLiteDBases.SQLite3Connection1.Free;
      end;
      // Now we can delete the database files
      try
        SysUtils.DeleteFile(DatabaseFilename);
      except
        Showmessage('Could not delete sqlite database ' + DataBasefilename + '. Please delete the manually.');
      end;
    end;

  // Free the Hashlist if one was imported at any stage
  if assigned(uKnownHashLists.HL1) then
  try
    uKnownHashLists.Free;
  except
    Showmessage('Had difficulty releasing hashlist memory while exiting.');
  end;
end;

procedure TMainForm.lblDonateClick(Sender: TObject);
var
  QuickHashDonateURL : string;
begin
  QuickHashDonateURL := 'https://paypal.me/quickhash';
  OpenURL(QuickHashDonateURL);
end;


// In the event that the user pastes an expected hash value AFTER computing
// the hash of the file, this onKeyUp event will then see if the pasted value
// matches the value just computed. New to v2.8.3
// QH expects the entered hash values to at least be of the proper length
procedure TMainForm.lbleExpectedHashChange(Sender: TObject);
begin
   if memFileHashField.Lines[0] = 'Computed hash will appear here...' then
    exit
   else if (lbleExpectedHash.Text = '') then
     exit
    else if (lbleExpectedHash.Text = '...') then
      exit
     else if (Length(trim(lbleExpectedHash.Text)) = 32) or (Length(trim(lbleExpectedHash.Text)) = 40)
          or (Length(trim(lbleExpectedHash.Text)) = 64) or (Length(trim(lbleExpectedHash.Text)) = 128)
          or (Length(trim(lbleExpectedHash.Text)) = 8) then
     begin
       if Uppercase(memFileHashField.Lines[0]) = Trim(Uppercase(lbleExpectedHash.Text)) then
         begin
           Showmessage('Expected hash matches the computed file hash, OK');
         end
     else
       begin
         Showmessage('Expected hash DOES NOT match the computed file hash!');
       end;
     end;
end;

procedure TMainForm.lbleExpectedHashEnter(Sender: TObject);
begin

end;

// Checks if the pasted expected hash for the text is the same as the computed hash
// QH expects the entered hash values to at least be of the proper length
procedure TMainForm.lbleExpectedHashTextChange(Sender: TObject);
begin
  if StrHashValue.Lines[0] = '...hash value' then
    exit
   else if (lbleExpectedHashText.Text = '') then
     exit
    else if (lbleExpectedHashText.Text = '...') then
      exit
     else if (Length(trim(lbleExpectedHashText.Text)) = 32) or (Length(trim(lbleExpectedHashText.Text)) = 40)
          or (Length(trim(lbleExpectedHashText.Text)) = 64) or (Length(trim(lbleExpectedHashText.Text)) = 128)
          or (Length(trim(lbleExpectedHashText.Text)) = 8) then
     begin
       if Uppercase(StrHashValue.Lines[0]) = Trim(Uppercase(lbleExpectedHashText.Text)) then
         begin
           Showmessage('Expected hash matches the computed file hash, OK');
         end
     else
       begin
         Showmessage('Expected hash DOES NOT match the computed file hash!');
       end;
     end;
end;


procedure TMainForm.lblFileAHashClick(Sender: TObject);
var
  ChosenHashAlg : string;
begin
  ChosenHashAlg := 'MD5';
  case AlgorithmChoiceRadioBox5.ItemIndex of
      0: begin
      ChosenHashAlg := 'MD5';
      end;
      1: begin
      ChosenHashAlg := 'SHA-1';
      end;
      2: begin
      ChosenHashAlg := 'SHA256';
      end;
      3: begin
      ChosenHashAlg := 'SHA512';
      end;
      4: begin
      ChosenHashAlg := 'xxHash';
      end;
  end;
  if lblFileAHash.Caption = '...' then
    exit
  else
    begin
      Clipboard.AsText := ChosenHashAlg + ' ' + lblFileAHash.Caption;
      ShowMessage(ChosenHashAlg + ' Hash value now in clipboard');
    end;
end;

procedure TMainForm.lblFileBHashClick(Sender: TObject);
var
  ChosenHashAlg : string;
begin
  ChosenHashAlg := 'MD5';
  case AlgorithmChoiceRadioBox5.ItemIndex of
      0: begin
      ChosenHashAlg := 'MD5';
      end;
      1: begin
      ChosenHashAlg := 'SHA-1';
      end;
      2: begin
      ChosenHashAlg := 'SHA256';
      end;
      3: begin
      ChosenHashAlg := 'SHA512';
      end;
      4: begin
      ChosenHashAlg := 'xxHash';
      end;
  end;
  if lblFileBHash.Caption = '...' then
    exit
  else
    begin
      Clipboard.AsText := ChosenHashAlg + ' ' + lblFileBHash.Caption;
      ShowMessage(ChosenHashAlg + ' Hash value now in clipboard');
    end;
end;

// Enables or disables time scheduler of FileS tab
procedure TMainForm.lblschedulertickboxFileSTabChange(Sender: TObject);
begin
  if lblschedulertickboxFileSTab.Checked then
  begin
    ZVDateTimePickerFileSTab.Visible := true;
    ZVDateTimePickerFileSTab.Enabled := true;
  end
  else
  begin
    ZVDateTimePickerFileSTab.Visible := false;
    ZVDateTimePickerFileSTab.Enabled := false;
  end;
end;

// Enables or disables time scheduler of File tab
procedure TMainForm.lblschedulertickboxFileTabChange(Sender: TObject);
begin
  if lblschedulertickboxFileTab.Checked then
  begin
    ZVDateTimePickerFileTab.Visible := true;
    ZVDateTimePickerFileTab.Enabled := true;
  end
  else
  begin
    ZVDateTimePickerFileTab.Visible := false;
    ZVDateTimePickerFileTab.Enabled := false;
  end;
end;

// Enables or disables time scheduler of Copy tab
procedure TMainForm.lblschedulertickboxCopyTabChange(Sender: TObject);
begin
  if lblschedulertickboxCopyTab.Checked then
  begin
    ZVDateTimePickerCopyTab.Visible := true;
    ZVDateTimePickerCopyTab.Enabled := true;
  end
  else
  begin
    ZVDateTimePickerCopyTab.Visible := false;
    ZVDateTimePickerCopyTab.Enabled := false;
  end;
end;

// Enables or disables time scheduler of Compare Two Files tab
procedure TMainForm.lblschedulertickboxCompareTabChange(Sender: TObject);
begin
  if lblschedulertickboxCompareTab.Checked then
  begin
    ZVDateTimePickerCompareTab.Visible := true;
    ZVDateTimePickerCompareTab.Enabled := true;
  end
  else
  begin
    ZVDateTimePickerCompareTab.Visible := false;
    ZVDateTimePickerCompareTab.Enabled := false;
  end;
end;

// Enables or disables time scheduler of Compare Directories tab
procedure TMainForm.lblschedulertickboxCompareTwoDirectoriesTabChange(Sender: TObject);
begin
  if lblschedulertickboxCompareDirsTab.Checked then
  begin
    ZVDateTimePickerCompareDirsTab.Visible := true;
    ZVDateTimePickerCompareDirsTab.Enabled := true;
  end
  else
  begin
    ZVDateTimePickerCompareDirsTab.Visible := false;
    ZVDateTimePickerCompareDirsTab.Enabled := false;
  end;
end;


procedure TMainForm.MenuItem1AClick(Sender: TObject);
begin

end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin

end;

procedure TMainForm.MenuItem2AClick(Sender: TObject);
begin
  frmAbout.Show
end;

procedure TMainForm.MenuItem1CClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.MenuItem3Click(Sender: TObject);
begin
  b64StringGrid2FileS.CopyToClipboard(false);
  Showmessage('Grid copied to clipboard OK');
end;

procedure TMainForm.MenuItem4Click(Sender: TObject);
begin
  b64StringGrid2FileS.CopyToClipboard(true);
  Showmessage('Grid row(s) copied to clipboard OK');
end;

// Save data from the Base64 hashing data grid to TSV text file
procedure TMainForm.MenuItem5Click(Sender: TObject);
begin
  b64SaveDialog.Title := 'Save grid results as...';
  b64SaveDialog.InitialDir := GetCurrentDir;
  b64SaveDialog.Filter := 'Comma Sep|*.csv';
  b64SaveDialog.DefaultExt := 'csv';

  if b64SaveDialog.Execute then
  begin
    b64StringGrid2FileS.SaveToCSVFile(b64SaveDialog.FileName);
    ShowMessage('Grid data saved as ' + b64SaveDialog.FileName);
  end;
end;

procedure TMainForm.MenuItem6Click(Sender: TObject);
begin
  b64StringGrid1File.CopyToClipboard(true);
  Showmessage('Grid row data copied to clipboard OK');
end;

procedure TMainForm.MenuItem_DeleteDupsClick(Sender: TObject);

begin
  // Firstly change the grid to list only the files with duplicates
  frmSQLiteDBases.ShowDuplicates(RecursiveDisplayGrid1);
  // Now go through and delete duplicate entries
  frmSQLiteDBases.DeleteDuplicates(RecursiveDisplayGrid1);
end;

// Copy entire FILES tab grid to clipboard
procedure TMainForm.MenuItem_CopyGridToClipboardFILESClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(RecursiveDisplayGrid1);
end;

// Copy file path of selected row from FILES tab grid to clipboard
procedure TMainForm.MenuItem_CopyFilepathOfSelectedCellClick(Sender: TObject);
begin
  frmSQLiteDBases.CopyFilePathOfSelectedCell(RecursiveDisplayGrid1);
end;

// Copy file name of selected row from FILES tab grid to clipboard
procedure TMainForm.MenuItem_CopyFileNameOfSelectedCellClick(Sender: TObject);
begin
  frmSQLiteDBases.CopyFileNameOfSelectedCell(RecursiveDisplayGrid1);
end;

// Copy entire selected row from FILES tab grid to clipboard
procedure TMainForm.MenuItem_CopySelectedRowFILESTABClick(Sender: TObject);
begin
  frmSQLiteDBases.CopySelectedRowFILESTAB(RecursiveDisplayGrid1);
end;

// Copy hash value of selected row from FILES tab grid to clipboard
procedure TMainForm.MenuItem_CopyHashOfSelectedCellClick(Sender: TObject);
var
  CellOfInterest : string;
begin
  CellOfInterest := '';
  frmSQLiteDBases.CopyHashOfSelectedCell(RecursiveDisplayGrid1);
end;

// Restore list of all values in FILES grid
procedure TMainForm.MenuItem_RestoreListFILESClick(Sender: TObject);
begin
  frmSQLiteDBases.ShowAll(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_SaveToCSVClick(Sender: TObject);
begin
  FilesDBGrid_SaveCSVDialog.Title := 'Save grid results as...';
  FilesDBGrid_SaveCSVDialog.InitialDir := GetCurrentDir;
  FilesDBGrid_SaveCSVDialog.Filter := 'Comma Sep|*.csv';
  FilesDBGrid_SaveCSVDialog.DefaultExt := 'csv';
  if FilesDBGrid_SaveCSVDialog.Execute then
  begin
    frmSQLiteDBases.SaveDBToCSV(RecursiveDisplayGrid1, FilesDBGrid_SaveCSVDialog.Filename);
  end;
end;

// Saves the content of a grid as HTML
procedure TMainForm.MenuItem_SaveToHTMLClick(Sender: TObject);
begin
  FilesSaveAsHTMLDialog.Title := 'Save grid as HTML file...';
  FilesSaveAsHTMLDialog.InitialDir := GetCurrentDir;
  FilesSaveAsHTMLDialog.Filter := 'HTML|*.html';
  FilesSaveAsHTMLDialog.DefaultExt := 'html';
  if FilesSaveAsHTMLDialog.Execute then
  frmSQLiteDBases.SaveFILESTabToHTML(RecursiveDisplayGrid1, FilesSaveAsHTMLDialog.FileName);
end;

procedure TMainForm.MenuItem_ShowDuplicatesClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.ShowDuplicates(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_SortByFilenameClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.SortByFilename(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_SortByFilePathClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.SortByFilePath(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_SortByHashClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.SortByHash(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_SortByHashListClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.SoryByHashList(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_FilterOutNoClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.FilterOutHashListNO(RecursiveDisplayGrid1);
end;

procedure TMainForm.MenuItem_FilterOutYesClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.FilterOutHashListYES(RecursiveDisplayGrid1);
end;


procedure TMainForm.MenuItem_SortByIDClick(Sender: TObject);
begin
  RecursiveDisplayGrid1.Clear;
  frmSQLiteDBases.SortByID(RecursiveDisplayGrid1);
end;


// These radio click events are to ensure the same hash algorithm is chosen
// for all the tabs, if the user changes it from the default. New to v.2.8.2
procedure TMainForm.AlgorithmChoiceRadioBox1Click(Sender: TObject);
begin
  AlgorithmChoiceRadioBox2.ItemIndex := AlgorithmChoiceRadioBox1.ItemIndex;
  AlgorithmChoiceRadioBox3.ItemIndex := AlgorithmChoiceRadioBox1.ItemIndex;
  AlgorithmChoiceRadioBox4.ItemIndex := AlgorithmChoiceRadioBox1.ItemIndex;
  AlgorithmChoiceRadioBox5.ItemIndex := AlgorithmChoiceRadioBox1.ItemIndex;
  AlgorithmChoiceRadioBox6.ItemIndex := AlgorithmChoiceRadioBox1.ItemIndex;
end;

procedure TMainForm.AlgorithmChoiceRadioBox2Click(Sender: TObject);
begin
  AlgorithmChoiceRadioBox1.ItemIndex := AlgorithmChoiceRadioBox2.ItemIndex;
  AlgorithmChoiceRadioBox3.ItemIndex := AlgorithmChoiceRadioBox2.ItemIndex;
  AlgorithmChoiceRadioBox4.ItemIndex := AlgorithmChoiceRadioBox2.ItemIndex;
  AlgorithmChoiceRadioBox5.ItemIndex := AlgorithmChoiceRadioBox2.ItemIndex;
  AlgorithmChoiceRadioBox6.ItemIndex := AlgorithmChoiceRadioBox2.ItemIndex;
end;

procedure TMainForm.AlgorithmChoiceRadioBox3Click(Sender: TObject);
begin
  AlgorithmChoiceRadioBox1.ItemIndex := AlgorithmChoiceRadioBox3.ItemIndex;
  AlgorithmChoiceRadioBox2.ItemIndex := AlgorithmChoiceRadioBox3.ItemIndex;
  AlgorithmChoiceRadioBox4.ItemIndex := AlgorithmChoiceRadioBox3.ItemIndex;
  AlgorithmChoiceRadioBox5.ItemIndex := AlgorithmChoiceRadioBox3.ItemIndex;
  AlgorithmChoiceRadioBox6.ItemIndex := AlgorithmChoiceRadioBox3.ItemIndex;
end;

procedure TMainForm.AlgorithmChoiceRadioBox4Click(Sender: TObject);
begin
  AlgorithmChoiceRadioBox1.ItemIndex := AlgorithmChoiceRadioBox4.ItemIndex;
  AlgorithmChoiceRadioBox2.ItemIndex := AlgorithmChoiceRadioBox4.ItemIndex;
  AlgorithmChoiceRadioBox3.ItemIndex := AlgorithmChoiceRadioBox4.ItemIndex;
  AlgorithmChoiceRadioBox5.ItemIndex := AlgorithmChoiceRadioBox4.ItemIndex;
  AlgorithmChoiceRadioBox6.ItemIndex := AlgorithmChoiceRadioBox4.ItemIndex;
end;

procedure TMainForm.AlgorithmChoiceRadioBox5Click(Sender: TObject);
begin
  AlgorithmChoiceRadioBox1.ItemIndex := AlgorithmChoiceRadioBox5.ItemIndex;
  AlgorithmChoiceRadioBox2.ItemIndex := AlgorithmChoiceRadioBox5.ItemIndex;
  AlgorithmChoiceRadioBox3.ItemIndex := AlgorithmChoiceRadioBox5.ItemIndex;
  AlgorithmChoiceRadioBox4.ItemIndex := AlgorithmChoiceRadioBox5.ItemIndex;
  AlgorithmChoiceRadioBox6.ItemIndex := AlgorithmChoiceRadioBox5.ItemIndex;
end;

procedure TMainForm.AlgorithmChoiceRadioBox6Click(Sender: TObject);
begin
  AlgorithmChoiceRadioBox1.ItemIndex := AlgorithmChoiceRadioBox6.ItemIndex;
  AlgorithmChoiceRadioBox2.ItemIndex := AlgorithmChoiceRadioBox6.ItemIndex;
  AlgorithmChoiceRadioBox3.ItemIndex := AlgorithmChoiceRadioBox6.ItemIndex;
  AlgorithmChoiceRadioBox4.ItemIndex := AlgorithmChoiceRadioBox6.ItemIndex;
  AlgorithmChoiceRadioBox5.ItemIndex := AlgorithmChoiceRadioBox6.ItemIndex;
end;


// New to v2.8.3, to better facilitate use of the Expected Hash field
procedure TMainForm.btnClearHashFieldClick(Sender: TObject);
begin
  lbleExpectedHash.Text:= '';
end;

// New to v2.8.3, to better facilitate use of the Expected Hash field
procedure TMainForm.btnClearHashFieldKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  lbleExpectedHash.Text:= '';
end;

// Attempts to validate if the source data is Base64 encoded, to reduce stream
// read errors if the user gives QuickHash a load of non-encoded data
// *** Not implemented, yet, as results not perfect and perhaps not necessary anyway ***
{function TMainForm.CanBeValidBase64EncodedString(var Buf: array of byte): Boolean;
const
  // Base64 often ends with a padding character of one or two equal signs
  // Though it doesn't have to necessarily. Spaces and carriage returns are not
  // part of the B64 alphabet, but are needed for formatting.
  Base64Alphabet = ['A'..'Z', 'a'..'z', '0'..'9', '+', '/', '=', ' '];
var
  I: Integer;
begin
  result := true;
  for i := 0 to SizeOf(Buf) do
    begin
      // Is the byte NOT in the Base64 range?
      if (not (Chr(Buf[I]) in Base64Alphabet)) then
      begin
        // Is the byte NOT a carriage return?
        if (not Buf[i] = 13) or (not Buf[i] = 10) then
        begin
          // It's not in the Base64 range and its not a carriage return. So reject
          Result := False;
        end;
      end;
    end;
end;
}
// Select, decode and then hash a Base64 encoded file
procedure TMainForm.btnB64FileChooserClick(Sender: TObject);
var
  DecodedStream  : TMemoryStream;
  EncodedStream  : TFileStream;
  Decoder        : TBase64DecodingStream;
  HashValA, HashValB : string;
begin
  if b64FileChooserDialog.Execute then
  begin
    b64StringGrid1File.Clear;
    // Compute hash of encoded file first
    HashValA := CalcTheHashFile(b64FileChooserDialog.FileName);
    // Now compute hash of decoded file
    try
      EncodedStream := TFileStream.Create(b64FileChooserDialog.FileName, fmOpenRead);
      try
        DecodedStream := TMemoryStream.Create;
        // Create a Base64 decoder stream. Note than passing bdmStrict can exclude
        // relevant Base64, despite it being better in theory. bdmMIME is less strict
        // but gives some checking and follows follows RFC2045.
        Decoder       := TBase64DecodingStream.Create(EncodedStream, bdmMIME);
        DecodedStream.CopyFrom(Decoder, Decoder.Size);
        DecodedStream.SaveToFile((b64FileChooserDialog.FileName) + '-Base64Decoded');
        try
          HashValB := CalcTheHashFile((b64FileChooserDialog.FileName) + '-Base64Decoded');
          SysUtils.DeleteFile(b64FileChooserDialog.FileName + '-Base64Decoded');
        finally
          b64StringGrid1File.RowCount:= 2;
          b64StringGrid1File.Cells[0,1] := '1';
          b64StringGrid1File.Cells[1,1] := b64FileChooserDialog.FileName;
          b64StringGrid1File.Cells[2,1] := HashValA;
          b64StringGrid1File.Cells[3,1] := HashValB;
        end;
      finally
        DecodedStream.Free;
        Decoder.Free;
      end;
    finally
      EncodedStream.Free;
    end;
  end;
end;


// Recursively find, decode and then hash Base64 encoded files
procedure TMainForm.btnB64FileSChooserClick(Sender: TObject);
var
  TotalB64FilesToExamine : TStringList;
  i : integer;
  DecodedStream  : TMemoryStream;
  EncodedStream  : TFileStream;
  Decoder        : TBase64DecodingStream;
  DirToHash, HashValA, HashValB : string;
begin
  i        := 0;
  HashValA := '';
  HashValB := '';

  if b64FileSChooserDialog.Execute then
    begin
      {$ifdef Windows}
      LongPathOverride := '\\?\';
      {$endif}

      // Where we are going to look for Base64 files, then create a list of those files
      DirToHash := b64FileSChooserDialog.FileName;
      try
        TotalB64FilesToExamine := TStringList.Create;
        b64ProgressFileS.Caption := 'Finding and counting files...please wait';
        Application.ProcessMessages;
        // Ensure files in long paths on that Windows OS can be examined...urrggh
        {$ifdef Windows}
          TotalB64FilesToExamine := FindAllFilesEx(LongPathOverride+DirToHash, '*', False, True);
        {$else}
          TotalB64FilesToExamine := FindAllFilesEx(DirToHash, '*', False, True);
        {$endif}
      finally
      end;

      // Make sure the display grid has sufficient rows for the file count
      b64StringGrid2FileS.RowCount := TotalB64FilesToExamine.Count + 1;

      // For each file, compute the Base64 encoded and decoded values and output to grid
      for i := 0 to TotalB64FilesToExamine.Count -1 do
        begin
          Application.ProcessMessages;
          b64ProgressFileS.Text:= 'Currently decoding and hashing ' + ExtractFileName(TotalB64FilesToExamine.Strings[i]);
          HashValA := CalcTheHashFile(TotalB64FilesToExamine.Strings[i]);
          try
            EncodedStream := TFileStream.Create(TotalB64FilesToExamine.Strings[i], fmOpenRead);
            try
             DecodedStream := TMemoryStream.Create;
             Decoder       := TBase64DecodingStream.Create(EncodedStream, bdmMIME);
             DecodedStream.CopyFrom(Decoder, Decoder.Size);
             // Create a temporary copy of the decoded file to hash it
             DecodedStream.SaveToFile((TotalB64FilesToExamine.Strings[i]) + '-Base64Decoded');
             try
               HashValB := CalcTheHashFile((TotalB64FilesToExamine.Strings[i]) + '-Base64Decoded');
               // And now delete the temporary copy
               SysUtils.DeleteFile(TotalB64FilesToExamine.Strings[i] + '-Base64Decoded');
             finally
             end;
            finally
             DecodedStream.Free;
             Decoder.Free;
            end;
          finally
            EncodedStream.Free;
          end;
          b64StringGrid2FileS.Cells[0, i+1] := IntToStr(i);
          {$ifdef Windows}
          b64StringGrid2FileS.Cells[1, i+1] := RemoveLongPathOverrideChars(TotalB64FilesToExamine.Strings[i], LongPathOverride);
          {$else}
          b64StringGrid2FileS.Cells[1, i+1] := TotalB64FilesToExamine.Strings[i];
          {$endif}
          b64StringGrid2FileS.Cells[2, i+1] := HashValA;
          b64StringGrid2FileS.Cells[3, i+1] := HashValB;
        end;
    end;
  if assigned(TotalB64FilesToExamine) then TotalB64FilesToExamine.Free;

  // Reset the long path override for any other procedures triggered
  LongPathOverride := '';
  b64ProgressFileS.Caption:= 'Decoded and hashed ' + IntToStr(i) + ' files. Completed at ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', Now);
end;

// Recursively find and decode all Base64 encoded files found in the selected folder
// Output decoded versions with the appended name '-Base64Decoded'
// So taking the example of a PDF that may be Base64 encoded as an e-mail attachment:
// MsgAttachment.emlx, which is the encoded attachment, becomes MsgAttachment.emlx-Base64Decoded
// and the user will then have to adjust the extension to MsgAttachment.emlx-Base64Decoded.pdf
procedure TMainForm.btnB64JustDecodeFilesClick(Sender: TObject);
var
  TotalB64FilesToExamine : TStringList;
  i : integer;
  DecodedStream  : TMemoryStream;
  EncodedStream  : TFileStream;
  Decoder        : TBase64DecodingStream;
  DirToDecode    : string;
  DecodedDestDir : string;
begin
  lblB64DecoderWarning.Caption := '';
  DecodedDestDir := '';
  i := 0;

  // ** SOURCE LOCATION
  b64FileSSourceDecoderDialog.Title:= 'Choose SOURCE folder of Base64 ENcoded files';
  b64FileSSourceDecoderDialog.InitialDir := GetCurrentDir;
  if b64FileSSourceDecoderDialog.Execute then
    begin
    {$ifdef Windows}
      LongPathOverride := '\\?\';
    {$endif}
    // Where we are going to look for Base64 encoded files
    DirToDecode := b64FileSSourceDecoderDialog.FileName;

    // ** DESTINATION LOCATION
    // Where are are going to save Base64 decoded files
    b64FileSDestinationDecoderDialog.Title := 'Choose DESTINATION folder for DEcoded files';
    b64FileSDestinationDecoderDialog.InitialDir := GetCurrentDir;
    if b64FileSDestinationDecoderDialog.Execute then
      begin
        DecodedDestDir := IncludeTrailingPathDelimiter(b64FileSDestinationDecoderDialog.FileName);
        // ** FIND THE FILES IN SOURCE
        try
          TotalB64FilesToExamine := TStringList.Create;
          b64DecoderProgress.Caption := 'Finding and counting files...please wait';
          Application.ProcessMessages;
          // Ensure files in long paths on that Windows OS can be examined...urrggh
          {$ifdef Windows}
          TotalB64FilesToExamine := FindAllFilesEx(LongPathOverride+DirToDecode, '*', False, True);
          {$else}
          TotalB64FilesToExamine := FindAllFilesEx(DirToDecode, '*', False, True);
          {$endif}
        finally
        end;

        // ** DECODE THE FILES TO DESTINATION
        // For each file, compute the Base64 encoded and decoded values and output to grid
        for i := 0 to TotalB64FilesToExamine.Count -1 do
          begin
            Application.ProcessMessages;
            b64DecoderProgress.Text:= 'Currently decoding ' + ExtractFileName(TotalB64FilesToExamine.Strings[i] + ' ...please wait');
            try
              EncodedStream := TFileStream.Create(TotalB64FilesToExamine.Strings[i], fmOpenRead);
              try
               DecodedStream := TMemoryStream.Create;
               Decoder       := TBase64DecodingStream.Create(EncodedStream, bdmMIME);
               DecodedStream.CopyFrom(Decoder, Decoder.Size);
               DecodedStream.SaveToFile((DecodedDestDir+(ExtractFileName(TotalB64FilesToExamine.Strings[i])) + '-Base64Decoded'));
              finally
               DecodedStream.Free;
               Decoder.Free;
              end;
            finally
              EncodedStream.Free;
            end;
          end;

        lblB64DecoderWarning.Caption := 'Add appropriate file extensions to your decoded files. e.g MsgAttach-Base64Decoded to MsgAttach-Base64Decoded.pdf';
        if TotalB64FilesToExamine.Count > 0 then
          begin
            b64DecoderProgress.Caption:= 'Decoded ' + IntToStr(i) + ' Base64 files. Completed at ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', Now);
          end;
        TotalB64FilesToExamine.Free;
      end;
      // Reset the long path override for any other procedures triggered
      LongPathOverride := '';
    end;
end;

// New to v3.0.0 Beta2 upwards
// Import an existing text file of hashes
// If successfull, known hashes can be accessed via uKnownHashLists.HashListSourceList.HL1
procedure TMainForm.btnLoadHashListClick(Sender: TObject);
var
  HashListFilename : string;
begin
  HashListFilename := '';
  ShowMessage('Ensure your hash list contains JUST hash values. ' + #13#10 +
              'No other columns and no row heading(s) please.'    + #13#10 +
              '(and ensure you choose the correct hash to match ' + #13#10 +
              'your imported list (e.g. MD5, SHA-1...)');

  HashListChooserDialog.Title := 'Choose exisiting text hash set...';
  HashListChooserDialog.InitialDir := GetCurrentDir;
  HashListChooserDialog.Filter := 'Text|*.txt';
  HashListChooserDialog.DefaultExt:= 'txt';
  HashListChooserDialog.Options:= [ofReadOnly];
  if HashListChooserDialog.Execute then
  begin
    // Create to memory addresses for the source hash list to live in
    // Only create it if not already created previously. It is freed on FormClose
    if not assigned(uKnownHashLists.HL1) then
    uKnownHashLists.CreateMemResidentHashLists();

    // Now load existing hashlist to memory, accessible as uKnownHashLists.HL1
    HashListFilename := HashListChooserDialog.FileName;
    uKnownHashLists.ImportHashList(HashListFilename);

    // Summarise the import process for the user
    StatusBar2.SimpleText := IntToStr(uKnownHashLists.CountHashesInKnownList()) + ' unique hashes imported OK. Awaiting user to select folder for hashing...';
  end;
end;

procedure TMainForm.btnMakeTextLowerClick(Sender: TObject);
var
  s : string;
  i : integer;
begin
  s := memoHashText.Text;
    for i := 1 to Length(s) do
      begin
        if s[i] in ['A'..'Z'] then
        begin
          s := Lowercase(s);
          memoHashText.Text := s;
        end;
      end;
    HashText(memoHashText);
    application.ProcessMessages;
end;

procedure TMainForm.btnMakeTextUpperClick(Sender: TObject);
var
  s : string;
  i : integer;
begin
  s := memoHashText.Text;
    for i := 1 to Length(s) do
      begin
        if s[i] in ['a'..'z'] then
        begin
          s := Uppercase(s);
          memoHashText.Text := s;
        end;
      end;
    HashText(memoHashText);
    application.ProcessMessages;
end;


procedure TMainForm.cbFlipCaseFILEChange(Sender: TObject);
var
  i : integer;
  s : string;
begin
  s := memFileHashField.Text;
  if cbFlipCaseFILE.Checked then
  begin
    cbFlipCaseFILE.Caption:= 'Switch case (now in lower mode)?';
    for i := 0 to Length(s) do
      begin
        if (s[i] = 'A') or (s[i] = 'B') or (s[i] = 'C') or (s[i] = 'D') or (s[i] = 'E') or (s[i] = 'F')
        then memFileHashField.Text := LowerCase(memFileHashField.Text);
      end
  end
  else
    begin
      cbFlipCaseFILE.Caption:= 'Switch case (now in UPPER mode)?';
      memFileHashField.Text := UpperCase(memFileHashField.Text);
    end;
end;

procedure TMainForm.cbFlipCaseTEXTChange(Sender: TObject);
var
  i : integer;
  s : string;
begin
  s := StrHashValue.Text;
  if cbFlipCaseTEXT.Checked then
  begin
    cbFlipCaseTEXT.Caption:= 'Switch case (now in lower mode)?';
    for i := 0 to Length(s) do
      begin
        if (s[i] = 'A') or (s[i] = 'B') or (s[i] = 'C') or (s[i] = 'D') or (s[i] = 'E') or (s[i] = 'F')
        then StrHashValue.Text := LowerCase(StrHashValue.Text);
      end
  end
  else
  begin
    cbFlipCaseTEXT.Caption:= 'Switch case (now in UPPER mode)?';
    StrHashValue.Text := UpperCase(StrHashValue.Text);
  end;
end;

// New to v3.0.0 Beta2 upwards
procedure TMainForm.cbLoadHashListChange(Sender: TObject);
begin
  if cbLoadHashList.Checked then
  begin
    btnLoadHashList.Enabled:= true;
    btnLoadHashList.Visible:= true;
  end
  else
  begin
    btnLoadHashList.Enabled:= false;
    btnLoadHashList.Visible:= false;
  end;
end;

procedure TMainForm.cbOverrideFileCountDifferChange(Sender: TObject);
begin
  if cbOverrideFileCountDiffer.Checked then cbSaveComparisons.Checked := true;
  if not cbOverrideFileCountDiffer.Checked then cbSaveComparisons.Checked := false;
end;

// Whenever the cbSaveComparisons checkbox is changed, check if the cbOverrideFileCountDiffer
// checkbox is also checked. If it is, prevent the user from making cbSaveComparison unchecked
// Because the program has to be able to save the results if the user wishes to
// override the file count check and hash the files even if there is mis-count
procedure TMainForm.cbSaveComparisonsChange(Sender: TObject);
begin
  if cbOverrideFileCountDiffer.Checked then cbSaveComparisons.Checked := true;
end;


procedure TMainForm.Panel1CopyAndHashOptionsClick(Sender: TObject);
begin

end;

// When user right clicks grid, if Hash List import unchecked, do not allow sorting by hash lookup column
procedure TMainForm.popmenuDBGrid_FilesPopup(Sender: TObject);
begin
  if cbLoadHashList.Checked then
  begin
    MenuItem_FilterOutYes.Enabled   := true;
    MenuItem_FilterOutNo.Enabled    := true;
    MenuItem_SortByHashList.Enabled := true;
  end else
  begin
    MenuItem_FilterOutYes.Enabled   := false;
    MenuItem_FilterOutNo.Enabled    := false;
    MenuItem_SortByHashList.Enabled := false;
  end;
end;


procedure TMainForm.ShellTreeView_FolderAChange(Sender: TObject; Node: TTreeNode
  );
begin
  lblFolderAName.Caption := ShellTreeView_FolderA.GetSelectedNodePath;
end;

procedure TMainForm.ShellTreeView_FolderBChange(Sender: TObject; Node: TTreeNode
  );
begin
  lblFolderBName.Caption := ShellTreeView_FolderB.GetSelectedNodePath;
end;


{$IFDEF WINDOWS}
// http://stackoverflow.com/questions/7859978/get-total-and-available-memory-when-4-gb-installed
function TMainForm.GetSystemMem: string;  { Returns installed RAM (as viewed by your OS) in Gb\Tb}
VAR
  MS_Ex : MemoryStatusEx;
  strTotalPhysMem, strTotalPhysAvail : string;
begin
 FillChar(MS_Ex{%H-}, SizeOf(MemoryStatusEx), #0);
 MS_Ex.dwLength := SizeOf(MemoryStatusEx);
 if GlobalMemoryStatusEx(MS_Ex) then
   begin
     strTotalPhysMem := FormatByteSize(MS_Ex.ullTotalPhys);
     strTotalPhysAvail := FormatByteSize(MS_Ex.ullAvailPhys);
     Result:= strTotalPhysMem + ' total' + #10#13 +
              strTotalPhysAvail + ' avail' + #10#13;
   end
 else Result := 'No Data';
end;
{$ENDIF}

// Procedure SaveOutputAsCSV
// Save any given display grid to CSV with a timestamped header
procedure TMainForm.SaveOutputAsCSV(Filename : string; GridName : TStringGrid);
begin
  // Here we insert the title line and version number of QuickHash, then save it
  // back to CSV.
  try
    Gridname.InsertRowWithValues(0, MainForm.Caption + '. Log generated: ' + DateTimeToStr(Now));
    GridName.SaveToCSVFile(FileName);
  finally
  end;
end;

// Procedure btnRecursiveDirectoryHashingClick
// Finds the files in a directory and hashes them, recursively by default
procedure TMainForm.btnRecursiveDirectoryHashingClick(Sender: TObject);

var
  DirToHash, FileMask, SearchMask : string;
  FS                           : TFileSearcher;
  TotalFilesToExamine          : TStringList;
  start, stop, elapsed         : TDateTime;

  begin
  PageControl1.ActivePage := Tabsheet3;  // Ensure FileS tab activated if triggered via menu
  FileCounter                   := 1;
  TotalBytesRead                := 0;
  lblNoFilesInDir.Caption       := '...';
  lblTimeTaken3.Caption         := '...';
  lblTimeTaken4.Caption         := '...';
  lblFilesExamined.Caption      := '...';
  lblPercentageComplete.Caption := '...';
  lblTotalBytesExamined.Caption := '...';
  pbFileS.Position              := 0;
  Label5.Caption                := 'This area will be populated once the scan is complete...please wait!';
  StopScan1 := false;

  // In case user pressed stop prior to just selecting a folder, free the resources
  TotalFilesToExamine := nil;
  FS := nil;
  // Empty database table TBL_FILES from earlier runs, otherwise entries from
  // previous runs will be listed with this new run
  frmSQLiteDBases.EmptyDBTable('TBL_FILES', RecursiveDisplayGrid1);

  // Now get the user to choose his folder for hashing
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
        Delete(DirToHash, 1, 2); // Delete the \\ from the UNC DirToHash path which always start \\path (otherwise it becomes '\\?\UNC\\\')
      end;
      {$endif}

      RecursiveDisplayGrid1.Visible := false;
      RecursiveDisplayGrid1.Clear;
      // If a scheduler has been set, wait for that future time to arrive
      if lblschedulertickboxFileSTab.Checked then
      begin
        InvokeScheduler(self);
      end;

       // Now lets recursively count each file,
       start := Now;
       lblTimeTaken3.Caption := 'Started: '+ FormatDateTime('YY/MM/DD HH:MM:SS', Start);
       StatusBar2.SimpleText := ' C O U N T I N G  F I L E S...P L E A S E  W A I T   A   M O M E N T ...';
       Label5.Visible        := true;

       Application.ProcessMessages;

       // If the user has a filemask enabled, we need to ensure our filecount figures
       // take account of only files that match the mask.

       if FileTypeMaskCheckBox2.Checked then
       begin
         FileMask := FileMaskField2.Text;
       end
       else FileMask := '*';

       // By default, the recursive dir hashing will hash all files of all sub-dirs
       // from the root of the chosen dir. If the box is ticked, the user just wants
       // to hash the files in the root of the chosen dir.

       if chkRecursiveDirOverride.Checked then   // User does NOT want recursive
         begin
           if chkHiddenFiles.Checked then        // ...but does want hidden files
             begin
               TotalFilesToExamine := FindAllFilesEx(LongPathOverride+DirToHash, FileMask, False, True);
             end
           else                                  // User does not want hidden
             begin
               TotalFilesToExamine := FindAllFiles(LongPathOverride+DirToHash, FileMask, False);
             end;
         end
       else
         begin                                  // User DOES want recursive
           if chkHiddenFiles.Checked then         // ...and he wants hidden
             begin
               TotalFilesToExamine := FindAllFilesEx(LongPathOverride+DirToHash, FileMask, true, true);
             end
           else                                  // ...but not want hidden
             begin
               TotalFilesToExamine := FindAllFiles(LongPathOverride+DirToHash, FileMask, true);
             end;
         end;

       lblNoFilesInDir.Caption := IntToStr(TotalFilesToExamine.count);
       NoOfFilesInDir2 := StrToInt(lblNoFilesInDir.Caption);  // A global var
       //RecursiveDisplayGrid1.rowcount := TotalFilesToExamine.Count +1;
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
               begin
                 SearchMask := FileMask;
               end
               else SearchMask := '';
             FS.Search(LongPathOverride+DirToHash, SearchMask, False, False);
           end
         else
           begin
             if FileTypeMaskCheckBox2.Checked then
               begin
                 SearchMask := FileMask;
               end
               else SearchMask := '';
             FS.Search(LongPathOverride+DirToHash, SearchMask, True, False);
           end;
       finally
         // Hashing complete. Now free resources
         FS.Free;
         TotalFilesToExamine.Free;
       end;

       // Now that the data is all computed, display the grid in the GUI.
       // Update the SQLite database with any remaining commits and display
       // content in DBGrid
       frmSQLiteDBases.SQLTransaction1.CommitRetaining;
       frmSQLiteDBases.UpdateGridFILES(nil);
       RecursiveDisplayGrid1.Visible := true;

       // and conclude timings and update display
       stop := Now;
       elapsed := stop - start;
       lblTimeTaken4.Caption := 'Time taken : '+ FormatDateTime('HH:MM:SS', elapsed);
       StatusBar2.SimpleText := ' DONE! ';
       btnClipboardResults.Enabled := true;

       // If user has imported an existing hash list, check new results against it
       if cbLoadHashList.Checked then
       begin
         StatusBar2.SimpleText:= 'See rightmost column for hashset correlations. ' + IntToStr(CountHashesInKnownList) + ' unique hashes are in the imported hash list';
       end;
    end; // end of SelectDirectoryDialog1.Execute
end;


// The clipboard button on the 'FileS' tab, this will copy the DBGrid to clipboard
procedure TMainForm.btnClipboardResultsClick(Sender: TObject);
begin
  frmSQLiteDBases.DatasetToClipBoard(RecursiveDisplayGrid1);
end;

procedure TMainForm.btnStopScan1Click(Sender: TObject);
begin
  StopScan1 := TRUE;
  if StopScan1 = TRUE then
  begin
    Label5.Caption := 'Populating display with database values...please wait';
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
  diskmodule.frmDiskHashingModule.Show;
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
    result := Copy(strPath, 5, (Length(strPath)));
  end
  else if LongPathOverrideVal = '\\?\UNC\' then
  begin
    // Delete the UNC API prefix and restore the UNC path chars of '\\'
    result := '\' + Copy(strPath, 8, (Length(strPath)));
  end
end;

// Get the list of all files from a folder, including hidden ones
function TMainForm.RetrieveFileList(FolderName : string) : TStringList;
begin
   result := FindAllFilesEx(FolderName, '*', true, true);
end;

function TMainForm.CompareHashLists(aHashList1, aHashlist2: TFPHashList): Boolean;
var
  i: Integer;
begin
  Result := False;
  if (aHashList1.Count <> aHashlist2.Count) then
    Exit;
  for i := 0 to aHashList1.Count-1 do
    if (aHashlist2.FindIndexOf(aHashList1.NameOfIndex(i)) < 0) then
      Exit;
  Result := True;
end;

function TMainForm.ComputeWhatHashesAreMissing(aHashList1, aHashList2 : TFPHashList) : TStringList;
var
  i, j : integer;
  sl : TStringList;
begin
  sl := TStringList.create;
  sl.Sorted:=true;

  if aHashList1.Count > aHashList2.Count then
  for i := 0 to aHashList1.Count-1 do
    begin
      if (aHashlist2.FindIndexOf(aHashList1.NameOfIndex(i)) < 0) then
      begin
        sl.Add(aHashList1.NameOfIndex(i));
      end;
    end
  else
    begin
    if aHashList2.Count > aHashList1.Count then
    for j := 0 to aHashList2.Count-1 do
      if (aHashlist1.FindIndexOf(aHashList2.NameOfIndex(j)) < 0) then
      begin
        sl.Add(aHashList2.NameOfIndex(j));
      end;
    end;
  result := sl;
end;

 // btnCompareClick : Will compare the listings of two directories, inc hidden files
 // The user is not presented with a choice for hiddne files because a comparison
 // of directories must be an exacting process.
procedure TMainForm.btnCompareClick(Sender: TObject);

var
  FolderA, FolderB, LongPathOveride, HashVal, StringToWrite, RogueHash : string;

  slFileListA, slFileListB, slMissingHashes  : TStringList;

  HashListA, HashListB : TFPHashList;

  NeedToSave : Boolean;

  i, lenRogueHash : integer;

  FolderAFileCount, FolderBFileCount, FileCountDifference, StringLength: integer;

  StartTime, EndTime, TimeTaken : TDateTime;


begin
  // Initialise vars and display captions, to ensure any previous runs are cleared

  pbCompareDirA.Position := 0;
  pbCompareDirB.Position := 0;
  FolderA                := '';
  FolderB                := '';
  FileCountDifference    := -1;
  NeedToSave             := false;
  FileCountDifference := 0;
  lblTotalFileCountNumberA.Caption := '';
  lblTotalFileCountNumberB.Caption := '';
  memFolderCompareSummary.Clear;

  if cbUNCModeCompFolders.Checked then
  begin
    if edtUNCPathCompareA.Text = 'Enter UNC path (e.g.\\DATASTORE\FOLDERA)' then
    begin
    ShowMessage('Enter a valid UNC path in FolderA field.');
    exit;
    end;

    if edtUNCPathCompareB.Text = 'Enter UNC path (e.g.\\DATASTORE\FOLDERB)' then
    begin
    ShowMessage('Enter a valid UNC path in FolderB field.');
    exit;
    end;
  end;

  // If UNC mode is DISabled, get FolderA and FolderB paths from list view
  if not cbUNCModeCompFolders.Checked then
  begin
    FolderA := ShellTreeView_FolderA.GetSelectedNodePath;
    lblFolderAName.Caption:= FolderA;

    FolderB := ShellTreeView_FolderB.GetSelectedNodePath;
    lblFolderBName.Caption:= FolderB;
  end;

  // If UNC mode is ENabled, get FolderA and FolderB paths from UNC text fields
  if cbUNCModeCompFolders.Checked then
  begin
    FolderA := edtUNCPathCompareA.Text;
    lblFolderAName.Caption:= FolderA;

    FolderB := edtUNCPathCompareB.Text;
    lblFolderBName.Caption:= FolderB;
  end;

  {$ifdef Windows}
  // Check if a UNC server path has been passed for either FolderA or FolderB.
  // If so, adjust LongPathOverride for UNC mode and append the UNC prefix to
  // ensure that the 32K path length limit and the UNC rules are adhered to

  LongPathOveride := '\\?\';

  if (Pos('\\', FolderA) > 0) or (Pos('\\', FolderB) > 0) then
  begin
    LongPathOverride := '\\?\UNC\';
  end;

  if LongPathOverride = '\\?\UNC\' then
  begin
    FolderA := LongPathOverride+FolderA;
    FolderB := LongPathOverride+FolderB;
    // Delete the two \\ from the UNC path so that \\Data becomes \\?\UNC\Data
    Delete(FolderA, 9, 2);
    Delete(FolderB, 9, 2);
  end
  else
  begin
    // So non-UNC path is in force, e.g. \\?\C:\MyData
    FolderA := LongPathOverride+FolderA;
    FolderB := LongPathOverride+FolderB;
  end;
  {$else}
   // If we are running on Linux or OSX just blank the long path overide to nothing
    LongPathOverride := '';
  {$endif}


  If DirectoryExistsUTF8(lblFolderAName.Caption) and DirectoryExistsUTF8(lblFolderBName.Caption) then
    begin
    // Check if a scheduler has been set for the comparison to start in the future
    if lblschedulertickboxCompareDirsTab.Checked then
    begin
      InvokeScheduler(self);
    end;

    // Lets begin
    StartTime                        := Now;
    lblTotalFileCountNumberA.Caption := '...';
    lblTotalFileCountNumberB.Caption := '...';
    memFolderCompareSummary.Lines.Add('Time started: ' + FormatDateTime('YYYY/MM/DD HH:MM:SS', StartTime));

    if cbSaveComparisons.Checked then
    begin
      NeedToSave := true;
      // Create the log file if it does not exist already
      if ForceDirectories(GetAppConfigDir(false)) then // Create .config folder in users home folder
      fsSaveFolderComparisonsLogFile := TFileStream.Create(GetAppConfigDir(false) +'QH_CompareResults'+FormatDateTime('_YYYY_MM_DD_HH_MM_SS', StartTime)+'.txt', fmCreate);
    end;

    // Process FolderA first. Find all the files initially
    try
       {$ifdef Windows}
          StatusBar6.SimpleText:= 'Currently searching for files in ' + RemoveLongPathOverrideChars(FolderA, LongPathOverride);
          memFolderCompareSummary.Lines.Add('Currently searching for files in ' + RemoveLongPathOverrideChars(FolderA, LongPathOverride));
          {$else}
            {$ifdef Darwin}
            StatusBar6.SimpleText:= 'Currently searching for files in ' + (FolderA);
            memFolderCompareSummary.Lines.Add('Currently searching for files in ' + (FolderA));
            {$endif}
            {$IFDEF UNIX and !$ifdef Darwin}
            StatusBar6.SimpleText:= 'Currently searching for files in ' + (FolderA);
            memFolderCompareSummary.Lines.Add('Currently searching for files in ' + (FolderA));
            {$ENDIF}
        {$endif}
      slFileListA := TStringList.Create;
      slFileListA.Sorted := true;
      slFileListA := RetrieveFileList(FolderA);
      FolderAFileCount := slFileListA.Count;
      lblTotalFileCountNumberA.Caption := IntToStr(FolderAFileCount);

      // Now move to FolderB. Find all the files initially
      try
        {$ifdef Windows}
          StatusBar6.SimpleText:= 'Currently searching for files in ' + RemoveLongPathOverrideChars(FolderB, LongPathOverride);
          memFolderCompareSummary.Lines.Add('Currently searching for files in ' + RemoveLongPathOverrideChars(FolderB, LongPathOverride));
          {$else}
            {$ifdef Darwin}
            StatusBar6.SimpleText:= 'Currently searching for files in ' + (FolderB);
            memFolderCompareSummary.Lines.Add('Currently searching for files in ' + (FolderB));
            {$endif}
            {$IFDEF UNIX and !$ifdef Darwin}
            StatusBar6.SimpleText:= 'Currently searching for files in ' + (FolderB);
            memFolderCompareSummary.Lines.Add('Currently searching for files in ' + (FolderB));
            {$ENDIF}
        {$endif}

        slFileListB := TStringList.Create;
        slFileListB.Sorted := true;
        slFileListB := RetrieveFileList(FolderB);
        FolderBFileCount := slFileListB.Count;
        lblTotalFileCountNumberB.Caption := IntToStr(FolderBFileCount);

        // If the file counts match in both Folders

        if FolderAFileCount = FolderBFileCount then
        begin
          // Compare the result.
          StatusBar6.SimpleText:= 'File count matches. Now comparing files in both folders using hashing...';
          HashListA := HashFolderAList(FolderA, slFileListA, FolderAFileCount, NeedToSave);
          HashListB := HashFolderBList(FolderB, slFileListB, FolderBFileCount, NeedToSave);

          if CompareHashLists(HashListA, HashListB) then
            begin
              memFolderCompareSummary.Lines.Add('Result : MATCH!');
              StatusBar6.SimpleText := 'The files of both folders are the same. MATCH!';
            end
          else
          begin
            memFolderCompareSummary.Lines.Add('Result : MIS-MATCH!');
            StatusBar6.SimpleText := 'The files of both folders are NOT the same. The file count is the same, but file hashes differ. MIS-MATCH!';
          end;
          HashListA.Free;
          HashListB.Free;
        end; // End of if FileCounts match

        // If the Folder A has less files than FolderB and user is not interested in proceeding
        if (FolderAFileCount < FolderBFileCount) AND (cbOverrideFileCountDiffer.Checked = false) then
          begin
            FileCountDifference    := FolderBFileCount-FolderAFileCount;
            StatusBar6.SimpleText  := 'The file count of both folders are NOT the same by ' + IntToStr(FileCountDifference) + ' files.';
            memFolderCompareSummary.Lines.Add('The file count of both folders are NOT the same by ' + IntToStr(FileCountDifference) + ' files.');
            memFolderCompareSummary.Lines.Add('To establish differences, tick box "Cont. if count differs?" and re-run');
            pbCompareDirA.Position := 100;
            pbCompareDirB.Position := 100;
          end
        else
          // If the Folder B has less files than FolderA and user is not interested in proceeding
          if (FolderAFileCount > FolderBFileCount) AND (cbOverrideFileCountDiffer.Checked = false) then
          begin
            FileCountDifference      := FolderAFileCount-FolderBFileCount;
            StatusBar6.SimpleText    := 'The file count of both folders are NOT the same by ' + IntToStr(FileCountDifference) + ' files.';
            memFolderCompareSummary.Lines.Add('The file count of both folders are NOT the same by ' + IntToStr(FileCountDifference) + ' files.');
            memFolderCompareSummary.Lines.Add('To establish differences, tick box "Cont. if count differs?" and re-run');
            pbCompareDirA.Position   := 100;
            pbCompareDirB.Position   := 100;
          end
            else
            // There is a file count difference, but the user still wants to proceed with the comparison anyway
            // He has checked the box cbOverrideFileCountDiffer "Cont. if count differs?"
            if ((FolderAFileCount > FolderBFileCount) OR (FolderBFileCount > FolderAFileCount)) AND (cbOverrideFileCountDiffer.Checked = true) then
            begin
              FileCountDifference      := FolderAFileCount-FolderBFileCount;
              StatusBar6.SimpleText:= 'File count mis-matches by ' + IntToStr(FileCountDifference) + ' but you chose to hash anyway. Comparing files in both folders using hashing...';
              memFolderCompareSummary.Lines.Add('File count mis-matches by ' + IntToStr(FileCountDifference) + ' but you chose to hash anyway.');
              memFolderCompareSummary.Lines.Add(lblFolderAName.Caption + ' contains ' + lblTotalFileCountNumberA.Caption + ' files, ' + lblFolderAName.Caption + ' contains ' + lblTotalFileCountNumberB.Caption + ' files.');
              memFolderCompareSummary.Lines.Add('Now hashing files...please wait');
              HashListA := HashFolderAList(FolderA, slFileListA, FolderAFileCount, NeedToSave);
              HashListB := HashFolderBList(FolderB, slFileListB, FolderBFileCount, NeedToSave);
              try
                slMissingHashes := TStringList.Create;
                slMissingHashes := ComputeWhatHashesAreMissing(HashListA, HashListB);
                for i := 0 to slMissingHashes.Count -1 do
                  begin
                    RogueHash := 'Missing Hash Value: ' + slMissingHashes.Strings[i] + #13#10;
                    lenRogueHash := Length(RogueHash);
                    fsSaveFolderComparisonsLogFile.Write(RogueHash[1], lenRogueHash);
                  end;
              finally
                slMissingHashes.free;
                HashListA.Free;
                HashListB.Free;
              end;
              StatusBar6.SimpleText := 'Completed but with differences. MIS-MATCH. Check the log file';
            end;
      finally
        slFileListB.Free; // Release FileListB
      end;
  finally
    slFileListA.free;  // Release FileListA
  end;

  // Compute timings and display them
  EndTime := Now;
  TimeTaken := EndTime-StartTime;
  memFolderCompareSummary.Lines.Add('Ended at : '               + FormatDateTime('YYYY/MM/DD HH:MM:SS', EndTime));
  memFolderCompareSummary.Lines.Add('Time taken : '             + FormatDateTime('HH:MM:SS', TimeTaken));
  memFolderCompareSummary.Lines.Add('Files in Folder A : '      + IntToStr(FolderAFileCount));
  memFolderCompareSummary.Lines.Add('Files in Folder B : '      + IntToStr(FolderBFileCount));
  memFolderCompareSummary.Lines.Add('File count differs by : '  + IntToStr(FileCountDifference));
  if (FileCountDifference > 0) AND (cbOverrideFileCountDiffer.Checked = false) then
   begin
     memFolderCompareSummary.Lines.Add('To establish differences, tick box "Cont. if count differs?" and re-run');
   end;
  memFolderCompareSummary.Lines.Add('Finished analysis');

  if cbSaveComparisons.Checked then
    begin
      try
      // Save the memo data to the file too. Useful regardless of whether files were
      // just counted and found to be different by file count, or whether they were
      // hashed and found to be the same or different
      if NeedToSave then
        begin
          fsSaveFolderComparisonsLogFile.Write(memFolderCompareSummary.Text[1], Length(memFolderCompareSummary.Text));
        end;
      finally
        memFolderCompareSummary.Lines.Add('Results saved to ' + fsSaveFolderComparisonsLogFile.FileName);
        fsSaveFolderComparisonsLogFile.Free;
      end;
    end;
    Application.ProcessMessages;
  end // End of If DirectoryExists...
  else
  begin
    ShowMessage('Invalid folders. Please select valid FolderA and FolderB');
    exit;
  end;
end;

function TMainForm.HashFolderAList(Path : string; slFileListA : TStringList; intFileCount : integer; SaveData : Boolean) : TFPHashList;
var
  HashListA  : TFPHashList;
  i, FilesProcessedA, StringLength : integer;
  HashVal, StringToWrite, HeaderLineA, HeaderLineB : string;
begin
  FilesProcessedA := 0;
  // Now hash the files in FolderA
  try
    {$ifdef Windows}
    StatusBar6.SimpleText:= 'Now hashing files in ' + RemoveLongPathOverrideChars(Path, LongPathOverride);
    memFolderCompareSummary.Lines.Add('Now hashing files in ' + RemoveLongPathOverrideChars(Path, LongPathOverride));
    {$else}
      {$ifdef Darwin}
        StatusBar6.SimpleText:= 'Now hashing files in ' + (Path);
        memFolderCompareSummary.Lines.Add('Now hashing files in ' + (Path));
      {$endif}
        {$IFDEF UNIX and !$ifdef Darwin}
          StatusBar6.SimpleText:= 'Now hashing files in ' + (Path);
          memFolderCompareSummary.Lines.Add('Now hashing files in ' + (Path));
        {$ENDIF}
      {$endif}

    HashListA := TFPHashList.Create;
    HeaderLineA := 'Computed hashes from ' + Path + ' : ' + #13#10;
    HeaderLineB := '=====================' + #13#10;

    fsSaveFolderComparisonsLogFile.Write(HeaderLineA[1], Length(HeaderLineA));
    fsSaveFolderComparisonsLogFile.Write(HeaderLineB[1], Length(HeaderLineB));

    for i := 0 to slFileListA.Count -1 do
    begin
      if FileSize(slFileListA.Strings[i]) > 0 then
        begin
        HashVal := CalcTheHashFile(slFileListA.Strings[i]);
        HashListA.Add(HashVal, Pointer(HashVal));
        if SaveData then
          begin
            StringLength := -1;
            StringToWrite := HashVal + ',' + (RemoveLongPathOverrideChars(slFileListA.Strings[i], LongPathOverride)) + #13#10;
            StringLength := Length(StringToWrite);
            fsSaveFolderComparisonsLogFile.Write(StringToWrite[1], StringLength);
          end;
        inc(FilesProcessedA, 1);
        pbCompareDirA.Position := ((FilesProcessedA * 100) DIV intFileCount);
        end
      else
      begin
        HashVal := 'ZERO BYTE FILE';
        HashListA.Add(HashVal, Pointer(HashVal));
        if SaveData then
          begin
            StringLength := -1;
            StringToWrite := HashVal + ',' + (RemoveLongPathOverrideChars(slFileListA.Strings[i], LongPathOverride)) + #13#10;
            StringLength := Length(StringToWrite);
            fsSaveFolderComparisonsLogFile.Write(StringToWrite[1], StringLength);
          end;
        inc(FilesProcessedA, 1);
        pbCompareDirA.Position := ((FilesProcessedA * 100) DIV intFileCount);
      end;
    end;
  finally
    result := HashListA;
  end;
end;

function TMainForm.HashFolderBList(Path : string; slFileListB : TStringList; intFileCount : integer; SaveData : Boolean) : TFPHashList;
var
  HashListB : TFPHashList;
  j, FilesProcessedB, StringLength : integer;
  HashVal, StringToWrite, HeaderLineA, HeaderLineB : string;
begin
  FilesProcessedB := 0;
  // Now hash the files in FolderB
  try
    {$ifdef Windows}
    StatusBar6.SimpleText:= 'Now hashing files in ' + RemoveLongPathOverrideChars(Path, LongPathOverride);
    memFolderCompareSummary.Lines.Add('Now hashing files in ' + RemoveLongPathOverrideChars(Path, LongPathOverride));
    {$else}
      {$ifdef Darwin}
        StatusBar6.SimpleText:= 'Now hashing files in ' + (Path);
        memFolderCompareSummary.Lines.Add('Now hashing files in ' + (Path));
      {$endif}
        {$IFDEF UNIX and !$ifdef Darwin}
          StatusBar6.SimpleText:= 'Now hashing files in ' + (Path);
          memFolderCompareSummary.Lines.Add('Now hashing files in ' + (Path));
        {$ENDIF}
      {$endif}

    HashListB := TFPHashList.Create;
    HeaderLineA := 'Computed hashes from ' + Path + ' : ' + #13#10;
    HeaderLineB := '=====================' + #13#10;

    fsSaveFolderComparisonsLogFile.Write(HeaderLineA[1], Length(HeaderLineA));
    fsSaveFolderComparisonsLogFile.Write(HeaderLineB[1], Length(HeaderLineB));

    for j := 0 to slFileListB.Count -1 do
    begin
      if FileSize(slFileListB.Strings[j]) > 0 then
        begin
          HashVal := CalcTheHashFile(slFileListB.Strings[j]);
          HashListB.Add(HashVal, Pointer(HashVal));
          if SaveData then
            begin
              StringLength := -1;
              StringToWrite := HashVal + ',' + (RemoveLongPathOverrideChars(slFileListB.Strings[j], LongPathOverride)) + #13#10;
              StringLength := Length(StringToWrite);
              fsSaveFolderComparisonsLogFile.Write(StringToWrite[1], StringLength);
            end;
          inc(FilesProcessedB, 1);
          pbCompareDirB.Position := ((FilesProcessedB * 100) DIV intFileCount);
        end
      else
      begin
        HashVal := 'ZERO BYTE FILE';
        HashListB.Add(HashVal, Pointer(HashVal));
        if SaveData then
          begin
            StringLength := -1;
            StringToWrite := HashVal + ',' + (RemoveLongPathOverrideChars(slFileListB.Strings[j], LongPathOverride)) + #13#10;
            StringLength := Length(StringToWrite);
            fsSaveFolderComparisonsLogFile.Write(StringToWrite[1], StringLength);
          end;
        inc(FilesProcessedB, 1);
        pbCompareDirB.Position := ((FilesProcessedB * 100) DIV intFileCount);
      end;
    end;
  finally
    result := HashListB;
  end;
end;
// btnClearTextAreaClick : Clears the whole text field if the user requests to do so
procedure TMainForm.btnClearTextAreaClick(Sender: TObject);
begin
  memoHashText.Clear;
  StrHashValue.Text:='...hash value';
end;

// ClearText : Invoked OnEnter of the text field only if the standing text exists
procedure TMainForm.ClearText(Sender: TObject);
begin
  if memoHashText.Lines[0] = 'Type or paste text here - hash will update as you type' then memoHashText.Clear;
end;

procedure TMainForm.TabSheet6ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;


// EmptyDisplayGrid will quickly empty the display grid from previous runs
procedure TMainForm.EmptyDisplayGrid(Grid : TStringGrid);
var
  i : integer;
begin
  for i := 0 to Grid.ColCount - 1 do
    Grid.Cols[i].Clear;
end;

// Generates a text file of folder names only
function TMainForm.GetSubDirListing(FolderName : string) : TStringList;
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  sl := FindAllDirectories(FolderName, True);
  result := sl;
end;

// Generates a text file of folder names and files only
function TMainForm.GetSubDirAndFileListing(FolderName : string) : TStringList;
var
  sl : TStringList;
begin
  sl := TStringList.Create;
  sl := FindAllFiles(FolderName, '*', True);
  result := sl;
end;

procedure TMainForm.Button8CopyAndHashClick(Sender: TObject);
var
  scheduleStartTime : TDateTime;
  LoopCounter       : Integer;
  slSubDirListing, slSubDirAndFilesListing     : TStringList;
begin
  frmDisplayGrid1.RecursiveDisplayGrid_COPY.Visible := false; // Hide the grid if it was left visible from an earlier run
  lblNoOfFilesToExamine.Caption    := '';
  lblNoOfFilesToExamine2.Caption   := '';
  lblFilesCopiedPercentage.Caption := '';
  lblDataCopiedSoFar.Caption       := '';
  lblTimeTaken6A.Caption           := '...';
  lblTimeTaken6B.Caption           := '...';
  lblTimeTaken6C.Caption           := '...';
  pbCopy.Position                  := 0;
  LoopCounter                      := 0;
  Button8CopyAndHash.Enabled       := false; // disable the go button until finished
  StopScan2                        := false;

  // User just wants a sub-folder listing as text
  if CheckBoxListOfDirsOnly.Checked then
  begin
    SourceDir := DirListA.GetSelectedNodePath;

    sdFileAndFolderListOnly.Title := 'Save listing as text file...';
    sdFileAndFolderListOnly.InitialDir := GetCurrentDir;
    sdFileAndFolderListOnly.Filter := 'Text|*.txt';
    sdFileAndFolderListOnly.DefaultExt:= 'txt';

    try
      slSubDirListing := TStringList.Create;
      slSubDirListing := GetSubDirListing(SourceDir);
      if sdFileAndFolderListOnly.Execute then
      begin
        slSubDirListing.SaveToFile(sdFileAndFolderListOnly.FileName);
      end;
    finally
      slSubDirListing.Free;
    end;
  end
    else   // User just wants a sub-folder and file listing as text
      if CheckBoxListOfDirsAndFilesOnly.Checked then
      begin
        SourceDir := DirListA.GetSelectedNodePath;
        try
          slSubDirAndFilesListing := TStringList.Create;
          slSubDirAndFilesListing := GetSubDirAndFileListing(SourceDir);
          if sdFileAndFolderListOnly.Execute then
          begin
            slSubDirAndFilesListing.SaveToFile(sdFileAndFolderListOnly.FileName);
          end;
        finally
          slSubDirAndFilesListing.Free;
        end;
      end
        else // User wants some actual hashing and copying doing
          begin
            // Empty database table TBL_COPY from any earlier runs, otherwise entries from
            // previous runs will be listed with this new run
            frmSQLiteDBases.EmptyDBTable('TBL_COPY', frmDisplayGrid1.RecursiveDisplayGrid_COPY);

            Application.ProcessMessages;

            // First, wait for the scheduler time to arrive, if set by the user
            if lblschedulertickboxCopyTab.Checked then
              begin
                if ZVDateTimePickerCopyTab.DateTime < Now then
                begin
                  ShowMessage('Scheduled start time is in the past. Correct it.');
                  Button8CopyAndHash.Enabled       := true;
                  exit;
                end
                else
                scheduleStartTime     := ZVDateTimePickerCopyTab.DateTime;
                StatusBar3.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM:SS', schedulestarttime);
                repeat
                  // This sleep loop avoids straining the CPU too much but also ensures the
                  // interface stays responsive to button clicks etc.
                  // So every 1K itteration, refresh the interface until the scheduled start
                  // arrives or the user clicks Abort.
                  inc(LoopCounter,1);
                  if LoopCounter = 1000 then
                    begin
                      Application.ProcessMessages;
                      LoopCounter := 0;
                    end;
                  sleep(0);
                until (scheduleStartTime = Now) or (StopScan2 = true);
              end;

            if chkUNCMode.Checked then
              begin
                SourceDir := Edit2SourcePath.Text;
                DestDir   := Edit3DestinationPath.Text;

                if Pos(':', SourceDir) > 0 then
                  begin
                    ShowMessage('Drive letter detected in source path but UNC mode selected');
                    StatusBar3.SimpleText := 'Aborted due drive letter in source UNC path selection';
                  end
                  else if Pos(':', DestDir) > 0 then
                    begin
                      ShowMessage('Drive letter detected in destination path but UNC mode selected');
                      StatusBar3.SimpleText := 'Aborted due to drive letter in destinatination UNC path selection';
                    end
                    else
                      begin
                        Button8CopyAndHash.Enabled       := false;
                        {$ifdef Windows}
                        // If chosen source path is a UNC path, we need to append the UNC prefix to the
                        // Unicode 32K long API call of \\?\
                        if (Pos('\\', SourceDir) > 0) then
                        begin
                          LongPathOverride := '\\?\UNC\';
                          Delete(SourceDir, 1, 2); // Delete the \\ from the UNC path DirToHash (otherwise it becomes '\\?\UNC\\\')
                        end;
                        // If chosen destination path is a UNC path too, we need to append the UNC prefix
                        if (Pos('\\', DestDir) > 0) then
                        begin
                          LongPathOverride := '\\?\UNC\';
                          Delete(DestDir, 1, 2); // Delete the \\ from the UNC path DirToHash (otherwise it becomes '\\?\UNC\\\')
                        end;
                        {$endif}
                        // Now process the copy and paste in UNC mode
                        ProcessDir(SourceDir);
                      end;
              end
            else
            begin
              // In case the user changes either the source or destination after already
              // running a job once, and so without necessarily clicking with the mouse,
              // get the source and destination paths again
              DirListAClick(Sender);
              DirListBClick(Sender);

              // Now process the selected source and destination folders in non-UNC mode
              // If the user has chosen multiple folders...
              if MultipleDirsChosen then
                try
                  if slMultipleDirNames.Count > 0 then
                  begin
                    // Only show the user a prompt if scheduler was NOT selected
                    If lblschedulertickboxCopyTab.Checked = false then
                    begin
                      ShowMessage('The following multiple directories will be hashed and copied:' + #13#10 + slMultipleDirNames.Text);
                    end;
                    // give ProcessDir function the first folder name in the list for now...
                    // ProcessDir will then do the itterations itself using the same stringlist
                    SourceDir := slMultipleDirNames.Strings[0];
                    Button8CopyAndHash.Enabled       := false;
                    // Now process the chosen folders
                    ProcessDir(SourceDir);
                  end
                finally
                  if assigned(slMultipleDirNames) then slMultipleDirNames.free;
                end
              // or copy single selected folder as normal if only one folder selected
              else ProcessDir(SourceDir);

              if SourceDirValid AND DestDirValid = FALSE then
                begin
                  // Now disable the 'Go!' button again
                  Button8CopyAndHash.Enabled := false;
                end;
            end;
            Application.ProcessMessages;
          end;
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
  schedulestarttime : TDateTime;
  LoopCounter : integer;
begin
  FileA                      := '';
  FileB                      := '';
  FileAHash                  := '';
  FileBHash                  := '';
  lblHashMatchResult.Caption := '';
  lblFileAHash.Caption       := '';
  lblFileBHash.Caption       := '';
  LoopCounter                := 0;

  FileA := Trim(edtFileAName.Caption);
  FileB := Trim(edtFileBName.Caption);

  if (LazFileUtils.FileExistsUTF8(FileA) = false) or (LazFileUtils.FileExistsUTF8(FileB) = false) then
  begin
    StatusBar4.SimpleText := 'BOTH FILES MUST BE SELECTED!';
    Application.ProcessMessages;
    Abort;
  end
  else
    begin
      if lblschedulertickboxCompareTab.Checked then
      begin
        if ZVDateTimePickerCompareTab.DateTime < Now then
        begin
         ShowMessage('Scheduled start time is in the past. Correct it.');
         exit;
        end
        else
        scheduleStartTime     := ZVDateTimePickerCompareTab.DateTime;
        StatusBar4.SimpleText := 'Waiting....scheduled for a start time of ' + FormatDateTime('YY/MM/DD HH:MM:SS', schedulestarttime);
         repeat
           // This sleep loop avoids straining the CPU too much but also ensures the
           // interface stays responsive to button clicks etc.
           // So every 1K itteration, refresh the interface until the scheduled start
           // arrives or the user clicks Abort.
           inc(LoopCounter,1);
           if LoopCounter = 1000 then
             begin
               Application.ProcessMessages;
               LoopCounter := 0;
             end;
           sleep(0);
         until scheduleStartTime = Now;
      end;
      // FileA
      StatusBar4.SimpleText := 'Computing hash of ' + FileA + '...';

      if LazFileUtils.FileExistsUTF8(FileA) then
      begin
        Application.ProcessMessages;
        FileAHash := Uppercase(CalcTheHashFile(FileA));
        lblFileAHash.Caption := FileAHash;
      end
      else ShowMessage('File A is invalid or cannot be accessed');

      //FileB
      StatusBar4.SimpleText := 'Computing hash of ' + FileB + '...';
      if LazFileUtils.FileExistsUTF8(FileB) then
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
  ChosenHashAlg : string;
begin
  ChosenHashAlg := 'MD5';
  case AlgorithmChoiceRadioBox5.ItemIndex of
      0: begin
      ChosenHashAlg := 'MD5';
      end;
      1: begin
      ChosenHashAlg := 'SHA-1';
      end;
      2: begin
      ChosenHashAlg := 'SHA256';
      end;
      3: begin
      ChosenHashAlg := 'SHA512';
      end;
      4: begin
      ChosenHashAlg := 'xxHash';
      end;
  end;
  slCompareTwoFiles := TStringList.Create;
  slCompareTwoFiles.Add('File A: ' + edtFileAName.Caption + ', ' + ChosenHashAlg + ' Hash: ' + lblFileAHash.Caption);
  slCompareTwoFiles.Add('File B: ' + edtFileBName.Caption + ', ' + ChosenHashAlg + ' Hash: ' + lblFileBHash.Caption);
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
    lblHashMatchResult.Font.Color := clBlack;
    lblHashMatchResult.Caption:= 'MATCH!';
  end
  else
    begin
      lblHashMatchResult.Font.Color := clRed;
      lblHashMatchResult.Caption:= 'MIS-MATCH!';
    end;
end;

procedure TMainForm.btnDirAClick(Sender: TObject);
begin
  SelectDirectoryDialog4.Execute;
  lblFolderAName.Caption := SelectDirectoryDialog4.FileName;
end;

procedure TMainForm.btnDirBClick(Sender: TObject);
begin
  SelectDirectoryDialog5.Execute;
  lblFolderBName.Caption := SelectDirectoryDialog5.FileName;
end;


// Used in "Compare Two Files" tab, to select File A
procedure TMainForm.btnFileACompareClick(Sender: TObject);
begin
  btnCompareTwoFilesSaveAs.Enabled := false;
  if OpenDialog1.Execute then
  begin
    if FileSize(OpenDialog1.Filename) = 0 then
    begin
      ShowMessage('File is zero bytes. Will not hash');
      exit;
    end else edtFileAName.Caption := OpenDialog1.FileName;
  end;
end;
// Used in "Compare Two Files" tab, to select FileB
procedure TMainForm.btnFileBCompareClick(Sender: TObject);
begin
  btnCompareTwoFilesSaveAs.Enabled := false;
  if OpenDialog1.Execute then
  begin
    if FileSize(OpenDialog1.Filename) = 0 then
    begin
      ShowMessage('File is zero bytes. Will not hash');
      exit;
    end else edtFileBName.Caption := OpenDialog1.FileName;
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
  start, stop, elapsed : TDateTime;
begin
  if edtFileNameToBeHashed.Text <> 'File being hashed...' then
    begin
      // First, clear the captions from any earlier file hashing actions
      lblStartedFileAt.Caption := '';
      lbEndedFileAt.Caption    := 'awaiting new end time...';
      lblFileTimeTaken.Caption := 'awaiting recomputation...';
      memFileHashField.Clear;
      StatusBar1.SimpleText := 'RECOMPUTING NEW HASH VALUE...Please wait.';
      start := Now;
      lblStartedFileAt.Caption := 'Started at : '+ TimeToStr(start);
      Application.ProcessMessages;
      HashValue := CalcTheHashFile(edtFileNameToBeHashed.Text);
      memFileHashField.Lines.Add(Uppercase(HashValue));
      stop := Now;
      elapsed := stop - start;
      StatusBar1.SimpleText := 'RECOMPUTED NEW HASH VALUE.';
      lbEndedFileAt.Caption:= 'Ended at : '+ TimeToStr(stop);
      lblFileTimeTaken.Caption := 'Time taken : '+ TimeToStr(elapsed);
      // If the user has pasted an expected hash value, since the last hash computation,
      // then check if it matches the newly computed hash
      lbleExpectedHashChange(self);
      Application.ProcessMessages;
    end;
end;

procedure TMainForm.AlgorithmChoiceRadioBox5SelectionChanged(Sender: TObject);
var
  HashValueA, HashValueB : ansistring;
begin
  HashValueA := '';
  HashValueB := '';
  if LazFileUtils.FileExistsUTF8(edtFileAName.Caption) and LazFileUtils.FileExistsUTF8(edtFileBName.Caption) then
    begin
      StatusBar4.SimpleText := 'RECOMPUTING NEW HASH VALUES...Please wait.';
      Application.ProcessMessages;
      HashValueA := Uppercase(CalcTheHashFile(edtFileAName.Caption));
      lblFileAHash.Caption := HashValueA;
      Application.ProcessMessages;
      HashValueB := Uppercase(CalcTheHashFile(edtFileBName.Caption));
      lblFileBHash.Caption := HashValueB;
      StatusBar4.SimpleText := 'RECOMPUTED NEW HASH VALUES.';
      Application.ProcessMessages;
    end;
end;

// New as of v2.8.0 : ValidateTextWithHash : Used as a generic text hashing function for use elsewhere
// besides the 'Text' tab, for example, when comparing lists of data in the compare tab
// The result is a SHA256 string on success, empty on failure.
function TMainForm.ValidateTextWithHash(strToBeHashed:ansistring) : string;
begin
  result := '';
  result := THashFactory.TCrypto.CreateSHA2_256().ComputeString(PWideChar(strToBeHashed), TEncoding.UTF8).ToString();
end;

// For use in the 'Text' tab only, for hashing text elements. Not to be used
// for general text hashing. To do that, use ValidateTextWithHash and examine
// the resulting SHA256 value
function TMainForm.CalcTheHashString(strToBeHashed:ansistring):string;
var
  TabRadioGroup1: TRadioGroup;
begin
  TabRadioGroup1 := AlgorithmChoiceRadioBox1;
  result := '';
  if Length(strToBeHashed) > 0 then
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
             result := THashFactory.TCrypto.CreateMD5().ComputeString(strToBeHashed, TEncoding.UTF8).ToString();
           end;
        1: begin
             result := THashFactory.TCrypto.CreateSHA1().ComputeString(strToBeHashed, TEncoding.UTF8).ToString();
           end;
        2: begin
             result := THashFactory.TCrypto.CreateSHA2_256().ComputeString(strToBeHashed, TEncoding.UTF8).ToString();
           end;
        3: begin
             result := THashFactory.TCrypto.CreateSHA2_512().ComputeString(strToBeHashed, TEncoding.UTF8).ToString();
           end;
        4: begin
           {$ifdef CPU64}
            result := THashFactory.THash64.CreateXXHash64().ComputeString(strToBeHashed, TEncoding.UTF8).ToString();
           {$else if CPU32}
            result := THashFactory.THash32.CreateXXHash32().ComputeString(strToBeHashed, TEncoding.UTF8).ToString();
           {$endif}
           end;
      end;
    end; // End of string length check
end;

{ DEPRECATED AS OF V2.8.0 in favour of HashLib4Pascal library instead of DCPCrypt
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
}

function TMainForm.CalcTheHashFile(FileToBeHashed:string):string;
const
  BufSize = 64 * 1024;  // 64kb buffer
var
  TabRadioGroup2: TRadioGroup;
  fsFileToBeHashed: TFileStream;
  // HashLib4Pascal types for MD5, SHA-1, SHA256 and SHA-512
  HashInstanceMD5, HashInstanceSHA1, HashInstanceSHA256, HashInstanceSHA512 : IHash;
  HashInstanceResultMD5, HashInstanceResultSHA1, HashInstanceResultSHA256,
    HashInstanceResultSHA512 : IHashResult;
  // HashLib4Pascal types for xxHash. xxHash64 is crazy fast on 64, but if run on a 32-bit
  // system, performance is hindered considerably. So for this algorithm, CPU dependant
  // instances are created
{$ifdef CPU64}
    HashInstancexxHash64       : IHash;
    HashInstanceResultxxHash64 : IHashResult;
{$else if CPU32}
  HashInstancexxHash32         : IHash;
  HashInstanceResultxxHash32   : IHashResult;
{$endif}
  Buffer: array [0 .. BufSize - 1] of Byte;
  i : Integer;
  TotalBytesRead_B, LoopCounter, IntFileSize : QWord;
  strFileSize : string;

begin
  TotalBytesRead_B := 0;
  IntFileSize      := 0;
  strFileSize      := '';
  LoopCounter      := 0; // Used for periodic interface refresh, to avoid slowing things down.

  result := '';

  case PageControl1.TabIndex of
        0: TabRadioGroup2 := AlgorithmChoiceRadioBox1;  //RadioGroup for Text.
        1: TabRadioGroup2 := AlgorithmChoiceRadioBox2;  //RadioGroup for File.
        2: TabRadioGroup2 := AlgorithmChoiceRadioBox3;  //RadioGroup for FileS.
        3: TabRadioGroup2 := AlgorithmChoiceRadioBox4;  //RadioGroup for Copy.
        4: TabRadioGroup2 := AlgorithmChoiceRadioBox5;  //RadioGroup for Compare Two Files.
        5: TabRadioGroup2 := AlgorithmChoiceRadioBox6;  //RadioGroup for Compare Two Folders.
        7: TabRadioGroup2 := AlgorithmChoiceRadioBox7;  //RadioGroup for Base64
  end;

  { For each hash instance, it has to be created, then initialised, populated,
    and finally converted to a string result.
  }

  Application.OnException := @HandleExceptions;
  try
    fsFileToBeHashed := nil;
    fsFileToBeHashed := TFileStream.Create(FileToBeHashed, fmOpenRead or fmShareDenyNone);
  except
  end;
  Application.OnException := nil;

  {  try
    fsFileToBeHashed := TFileStream.Create(FileToBeHashed, fmOpenRead or fmShareDenyNone);
    except
    On E :Exception do
      begin
        result := (FileToBeHashed + ' could not be accessed' + E.Message);
      end;
    end;
   }

  // Only continue if valid file handle
  if assigned(fsFileToBeHashed) then
  begin
    IntFileSize      := fsFileToBeHashed.Size;
    pbFile.Position  := 0;
    pbFile.Max       := 100;

    case TabRadioGroup2.ItemIndex of
      0: begin
        // MD5
        HashInstanceMD5 := THashFactory.TCrypto.CreateMD5();
        HashInstanceMD5.Initialize();
          repeat
          i := fsFileToBeHashed.Read(Buffer, BufSize);
          if i <= 0 then
            break
          else
            begin
              HashInstanceMD5.TransformUntyped(Buffer, i);
              // If the File tab is the tab doing the hashing, refresh the interface
              if PageControl1.ActivePage = TabSheet2 then
               begin
                inc(TotalBytesRead_B, i);
                inc(LoopCounter, 1);
                if LoopCounter = 40 then // Every X buffer reads, refresh interface
                  begin
                  pbFile.Position := ((TotalBytesRead_B * 100) DIV IntFileSize);
                  lblPercentageProgressFileTab.Caption:= IntToStr(pbFile.Position) + '%';
                  LoopCounter := 0;
                  Application.ProcessMessages;
                  end;
                end;
            end;
          until false;
        HashInstanceResultMD5 := HashInstanceMD5.TransformFinal();
        result := HashInstanceResultMD5.ToString()
        end; // End of MD5

      1: begin
        // SHA-1
        HashInstanceSHA1 := THashFactory.TCrypto.CreateSHA1();
        HashInstanceSHA1.Initialize();
          repeat
          i := fsFileToBeHashed.Read(Buffer, BufSize);
          if i <= 0 then
            break
          else
            begin
              HashInstanceSHA1.TransformUntyped(Buffer, i);
                // If the File tab is the tab doing the hashing, refresh the interface
                if PageControl1.ActivePage = TabSheet2 then
                begin
                inc(TotalBytesRead_B, i);
                inc(LoopCounter, 1);
                if LoopCounter = 40 then
                  begin
                  pbFile.Position := ((TotalBytesRead_B * 100) DIV IntFileSize);
                  lblPercentageProgressFileTab.Caption:= IntToStr(pbFile.Position) + '%';
                  LoopCounter := 0;
                  Application.ProcessMessages;
                  end;
                end;
            end;
          until false;
        HashInstanceResultSHA1 := HashInstanceSHA1.TransformFinal();
        result := HashInstanceResultSHA1.ToString()
        end; // End of SHA-1

      2: begin
        // SHA256
        HashInstanceSHA256 := THashFactory.TCrypto.CreateSHA2_256();
        HashInstanceSHA256.Initialize();
          repeat
          i := fsFileToBeHashed.Read(Buffer, BufSize);
          if i <= 0 then
            break
          else
            begin
              HashInstanceSHA256.TransformUntyped(Buffer, i);
              // If the File tab is the tab doing the hashing, refresh the interface
              if PageControl1.ActivePage = TabSheet2 then
               begin
                inc(TotalBytesRead_B, i);
                inc(LoopCounter, 1);
                if LoopCounter = 40 then
                  begin
                  pbFile.Position := ((TotalBytesRead_B * 100) DIV IntFileSize);
                  lblPercentageProgressFileTab.Caption:= IntToStr(pbFile.Position) + '%';
                  LoopCounter := 0;
                  Application.ProcessMessages;
                  end;
                end;
            end;
          until false;
        HashInstanceResultSHA256 := HashInstanceSHA256.TransformFinal();
        result := HashInstanceResultSHA256.ToString()
        end;  // End of SHA256

      3: begin
        // SHA512
        HashInstanceSHA512 := THashFactory.TCrypto.CreateSHA2_512();
        HashInstanceSHA512.Initialize();
          repeat
          i := fsFileToBeHashed.Read(Buffer, BufSize);
          if i <= 0 then
            break
          else
            begin
              HashInstanceSHA512.TransformUntyped(Buffer, i);
              // If the File tab is the tab doing the hashing, refresh the interface
              if PageControl1.ActivePage = TabSheet2 then
                begin
                inc(TotalBytesRead_B, i);
                inc(LoopCounter, 1);
                if LoopCounter = 40 then
                  begin
                  pbFile.Position := ((TotalBytesRead_B * 100) DIV IntFileSize);
                  lblPercentageProgressFileTab.Caption:= IntToStr(pbFile.Position) + '%';
                  LoopCounter := 0;
                  Application.ProcessMessages;
                  end;
                end;
            end;
          until false;
        HashInstanceResultSHA512 := HashInstanceSHA512.TransformFinal();
        result := HashInstanceResultSHA512.ToString()
        end;  // End of SHA512

      4: begin
        // xxHash
        {$ifdef CPU64}
        HashInstancexxHash64 := THashFactory.THash64.CreateXXHash64();
        HashInstancexxHash64.Initialize();
          repeat
          i := fsFileToBeHashed.Read(Buffer, BufSize);
          if i <= 0 then
            break
          else
            begin
              HashInstancexxHash64.TransformUntyped(Buffer, i);
            end;
          until false;
        HashInstanceResultxxHash64 := HashInstancexxHash64.TransformFinal();
        result := HashInstanceResultxxHash64.ToString()
        {$else if CPU32}
        HashInstancexxHash32 := THashFactory.THash32.CreateXXHash32();
        HashInstancexxHash32.Initialize();
          repeat
          i := fsFileToBeHashed.Read(Buffer, BufSize);
          if i <= 0 then
            break
          else
            begin
              HashInstancexxHash32.TransformUntyped(Buffer, i);
              // If the File tab is the tab doing the hashing, refresh the interface
              if PageControl1.ActivePage = TabSheet2 then
               begin
                inc(TotalBytesRead_B, i);
                inc(LoopCounter, 1);
                if LoopCounter = 40 then
                  begin
                  pbFile.Position := ((TotalBytesRead_B * 100) DIV IntFileSize);
                  lblPercentageProgressFileTab.Caption:= IntToStr(pbFile.Position) + '%';
                  LoopCounter := 0;
                  Application.ProcessMessages;
                  end;
                end;
            end;
          until false;
        HashInstanceResultxxHash32 := HashInstancexxHash32.TransformFinal();
        result := HashInstanceResultxxHash32.ToString()
        {$endif}
        end;  // End of xxHash
    end; // end of case statement

  if PageControl1.ActivePage = TabSheet2 then
     begin
       // Last sweep to catch data that fell outside the loop counter
       // i.e. if the loop counter is 40, then the last 40 reads won't be in the
       // progress updater. So you end up with "95%" complete when its actually finished.
       // This will clear that up.
       pbFile.Position := ((TotalBytesRead_B * 100) DIV IntFileSize);
       lblPercentageProgressFileTab.Caption:= IntToStr(pbFile.Position) + '%';
       LoopCounter := 0;
     end;
    Application.ProcessMessages;
    // Free the source file if it was successfully opened for read access
    if fsFileToBeHashed.Handle > -1 then fsFileToBeHashed.free;
  end
  else result := 'File could not be accessed.'
end;

procedure TMainForm.HashFile(FileIterator: TFileIterator);
var
  SizeOfFile : int64;
  NameOfFileToHashFull, PathOnly, NameOnly, PercentageProgress : string;
  fileHashValue : ansistring;
  SG : TStringGrid;
  DoesHashExistAlready : Boolean;
begin
  SG            := TStringGrid.Create(self);
  SizeOfFile    := 0;
  fileHashValue := '';
  DoesHashExistAlready := false;

  if StopScan1 = FALSE then    // If Stop button NOT clicked, work
    begin
      NameOfFileToHashFull := FileIterator.FileName;
      PathOnly   := FileIterator.Path;
      NameOnly   := ExtractFileName(FileIterator.FileName);
      SizeOfFile := FileIterator.FileInfo.Size;

      if PageControl1.ActivePage = TabSheet2 then  // File tab
        begin
          StatusBar1.SimpleText := 'Currently Hashing: ' + RemoveLongPathOverrideChars(NameOfFileToHashFull, '\\?\');
        end else
      if PageControl1.ActivePage = TabSheet3 then  // FileS tab
        begin
          StatusBar2.SimpleText := 'Currently Hashing: ' + RemoveLongPathOverrideChars(NameOfFileToHashFull, '\\?\');
        end else
      if PageControl1.ActivePage = TabSheet4 then  // Copy tab
        begin
          StatusBar3.SimpleText := 'Currently Hashing: ' + RemoveLongPathOverrideChars(NameOfFileToHashFull, '\\?\');
        end;

    // Now generate the hash value using a custom function and convert the result to uppercase
    if cbLoadHashList.Checked then
    begin
      FileHashValue := UpperCase(CalcTheHashFile(NameOfFileToHashFull));
      DoesHashExistAlready := IsHashInTheKnownList(FileHashValue); // We pass this as a flag to SQLIte later
    end
      else FileHashValue := UpperCase(CalcTheHashFile(NameOfFileToHashFull));
    {$IFDEF Windows}
      PathOnly := RemoveLongPathOverrideChars(PathOnly, LongPathOverride); // Remove the \\?\ for display purposes
    {$ENDIF}

    // Save to database
    frmSQLiteDBases.WriteFILESValuesToDatabase(NameOnly, PathOnly, FileHashValue, FormatByteSize(SizeOfFile), DoesHashExistAlready);
    // Periodically commit database changes. If too often, slows it down
    CommitCount(nil);
    // Progress Status Elements:

    lblFilesExamined.Caption:= IntToStr(FileCounter);
    PercentageProgress := IntToStr((FileCounter * 100) DIV NoOfFilesInDir2);
    lblPercentageComplete.Caption := PercentageProgress + '%';
    TotalBytesRead := TotalBytesRead + SizeOfFile;
    lblTotalBytesExamined.Caption := FormatByteSize(TotalBytesRead);
    pbFileS.Position := ((FileCounter *100) DIV NoOfFilesInDir2);

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
  QuickHashURL := 'http://quickhash-gui.org';
  OpenURL(QuickHashURL);
end;

procedure TMainForm.ProcessDir(SourceDirName: string);

{$IFDEF WINDOWS}
type
  TRange = 'A'..'Z';   // For the drive lettering of Windows systems
{$ENDIF}
var
  i, NoOfFilesCopiedOK, j, HashMismtachCount,
    FileCopyErrors, ZeroByteFilesCounter, DupCount : integer;

  SizeOfFile2, TotalBytesRead2, NoFilesExamined, SizeOfCurrentFile: Int64;

  SubDirStructure, SourceFileHasHash, DestinationFileHasHash, FinalisedDestDir,
    FinalisedFileName, CopiedFilePathAndName, SourceDirectoryAndFileName,
    FormattedSystemDate, OutputDirDateFormatted, CrDateModDateAccDate,
    CSVLogFile2, HTMLLogFile2, strNoOfFilesToExamine, SubDirStructureParent,
    strTimeDifference,  FileMask,
    Col1SourceFilePathAndName, Col2SourceHash, Col3CopiedFilePathAndName, Col4DestinationHash, Col5DateAttribute: string;

  SystemDate, StartTime, EndTime, TimeDifference : TDateTime;

  FilesFoundToCopy, DirectoriesFoundList, SLCopyErrors : TStringList;

  SummaryMessage : TForm;   // This is the summary message that appears at the end

  {$IFDEF WINDOWS}
  k : integer;
  CurrentFile : string;
  slTemp : TStringList;
  DriveLetter : char;  // For MS Windows drive letter irritances only
  {$ENDIF}

begin
  SubDirStructure         := '';
  FinalisedDestDir        := '';
  SourceFileHasHash       := '';
  DestinationFileHasHash  := '';
  CrDateModDateAccDate    := '';
  FileMask                := '';
  NoOfFilesCopiedOK       := 0;
  HashMismtachCount       := 0;
  FileCopyErrors          := 0;
  ZeroByteFilesCounter    := 0;
  SizeOfFile2             := 0;
  TotalBytesRead2         := 0;
  DupCount                := 0;
  i                       := 0;
  j                       := 0;
  {$IFDEF Windows}
  k                       := 0;
  {$ENDIF}
  SizeOfCurrentFile       := -1;

  SLCopyErrors := TStringListUTF8.Create;

  // Ensures the selected source directory is set as the directory to be searched
  // and then finds all the files and directories within, storing as a StringList.
  // Generate part of the output directory based on time of execution

  // This is for the GUI output
  StartTime  := Now;
  // This is for the user, to alert him if it is incorrect
  SystemDate := Now();
  DateTimeToStr(SystemDate);

  // Date and time for the user, to be displayed later
  FormattedSystemDate := FormatDateTime('YYYY/MM/DD HH:MM:SS', SystemDate);

  // Date and time for the output directory, to be used later with other dir structures
  OutputDirDateFormatted := FormatDateTime('YYYY-MM-DD_HH-MM-SS', SystemDate);

  SetCurrentDir(SourceDirName);

   // If the user has a filemask enabled, we need to ensure our filecount figures
   // take account of only files that match the mask.

   if FileTypeMaskCheckBox1.Checked then
   begin
     FileMask := FileMaskField.Text;
   end
   else FileMask := '*';

  {$IFDEF WINDOWS}

  // First the default behaviour, which is a recurisve copy of one selected folder
  if MultipleDirsChosen = false then              // User has only selected one folder
    begin
      if not chkNoRecursiveCopy.Checked then      // and does want recursive copy
        begin
          FilesFoundToCopy := FindAllFilesEx(LongPathOverride+SourceDirName, FileMask, True, True);
        end
      else
      begin
        FilesFoundToCopy := FindAllFilesEx(LongPathOverride+SourceDirName, FileMask, False, True);
      end;
    end
  else
  // Now non default. User has chosen multiple folders, but still wants recursive
  begin
    if not chkNoRecursiveCopy.Checked then        // and does want recursive
     begin
       try
       FilesFoundToCopy := TStringListUTF8.Create;
       FilesFoundToCopy.Sorted := true;
       for i := 0 to slMultipleDirNames.Count -1 do
         begin
           SourceDirName := slMultipleDirNames.Strings[i];
           try
             slTemp        := TStringListUTF8.Create;
             slTemp.Sorted := true;
             slTemp        := FindAllFilesEx(LongPathOverride+SourceDirName, FileMask, True, True);
             for j := 0 to slTemp.Count -1 do
               begin
                 FilesFoundToCopy.Add(slTemp.Strings[j]);
               end;
           finally
             slTemp.Free;
           end;
         end;
       finally
         //
       end;
      end
      // User has chosen multiple folders, and does NOT want recursive
      else
      begin
        try
        FilesFoundToCopy := TStringListUTF8.Create;
        FilesFoundToCopy.Sorted := true;
        for i := 0 to slMultipleDirNames.Count -1 do
         begin
           SourceDirName    := slMultipleDirNames.Strings[i];
           try
             slTemp           := TStringListUTF8.Create;
             slTemp.Sorted    := true;
             slTemp           := FindAllFilesEx(LongPathOverride+SourceDirName, FileMask, False, True);
             for j := 0 to slTemp.Count -1 do
               begin
                 FilesFoundToCopy.Add(slTemp.Strings[j]);
               end;
           finally
             slTemp.Free;
           end;
         end;
        finally
          //
        end;
      end;
  end; // End of file list generation. We now know what needs to be examined
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
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, True);
          end;

      if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
        if not chkCopyHidden.Checked then         // ... but does want hidden files
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, False);
          end;

       if chkCopyHidden.Checked then         // ... but does want hidden files
         if not FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
           begin
             FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, False);
           end;

       if not FileTypeMaskCheckBox1.Checked then
         if not chkCopyHidden.Checked then
           begin
             FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, False);
           end;
    end;

  if not chkNoRecursiveCopy.Checked then
    begin
      if chkCopyHidden.Checked then         // ... but does want hidden files
        if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, True, True);
          end;

      if chkCopyHidden.Checked then         // ... but does want hidden files
        if not FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, True, True);
          end;

      if not chkCopyHidden.Checked then         // ... but does want hidden files
        if FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, True, False);
          end;

      if not chkCopyHidden.Checked then         // ... but does want hidden files
        if not FileTypeMaskCheckBox1.Checked then   // ...but does want a file mask
          begin
            FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, true, False);
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
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, True);
            end;

        if FileTypeMaskCheckBox1.Checked then
          if not chkCopyHidden.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, False);
            end;

         if chkCopyHidden.Checked then
           if not FileTypeMaskCheckBox1.Checked then
             begin
               FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, False);
             end;

         if not FileTypeMaskCheckBox1.Checked then
           if not chkCopyHidden.Checked then
             begin
               FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, False, False);
             end;
      end;

    if not chkNoRecursiveCopy.Checked then
      begin
        if chkCopyHidden.Checked then
          if FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, True, True);
            end;

        if chkCopyHidden.Checked then
          if not FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, True, True);
            end;

        if not chkCopyHidden.Checked then
          if FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, True, False);
            end;

        if not chkCopyHidden.Checked then
          if not FileTypeMaskCheckBox1.Checked then
            begin
              FilesFoundToCopy := FindAllFilesEx(SourceDirName, FileMask, true, False);
            end;
       end;
    {$ENDIF}
  {$ENDIF}

  // This prompt was used for ages, to ensure the user is happy to proceed
  // after discovering how many files there are in source.
  // But adding the scheduler makes it more complictaed and prevent automation.
  // Several users also complained that they came back to it after thinking
  // it had finished, to find it waiting for the user to press Yes.
  // So discarded in v2.8.3 upwards.

  {if MessageDlg('Proceed?', 'Source directory contains ' + IntToStr(FilesFoundToCopy.Count) + ' mask-matched files, inc sub-dirs. FYI, the host system date settings are : ' + FormattedSystemDate + '. Do you want to proceed?', mtConfirmation,
   [mbCancel, mbNo, mbYes],0) = mrYes then}


    strNoOfFilesToExamine := IntToStr(FilesFoundToCopy.Count);
    lblTimeTaken6A.Caption := FormatDateTime('YYYY/MM/DD HH:MM:SS', SystemDate);
    Application.ProcessMessages;

    try
      for i := 0 to FilesFoundToCopy.Count -1 do
        begin
          // StopScan2 is set to false if the "Stop" button in 'Copy' tab is pressed
          if StopScan2 = FALSE then
            begin
            SourceFileHasHash      := '';
            DestinationFileHasHash := '';

            // Check the file has a size greater than 0 bytes to avoid default hash values.
            SizeOfCurrentFile := FileSize(FilesFoundToCopy.Strings[i]);

            StatusBar3.SimpleText := 'Processing: ' + RemoveLongPathOverrideChars(FilesFoundToCopy.Strings[i], LongPathOverride);
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

              We only do this if UNC mode is not selected though, because if
              it isn't, drive letters should not be needed anyway.

              This loop finds 'C:' in the middle of the concatanated path and
              return its position. It then deletes 'C:' of 'C:\' if found, or any
              other A-Z drive letter, leaving the '\' for the path
              So, C:\SrcDir\SubDirA becomes E:\NewDestDir\SrcDir\SubDirA instead of
              E:\NewDestDir\C:SrcDir\SubDirA. UNC paths are taken care of by ForceDirectories }

              if chkUNCMode.Checked = false then
                begin
                  for DriveLetter in TRange do
                    begin
                      k := posex(DriveLetter+':', FinalisedDestDir, 4);
                      Delete(FinalisedDestDir, k, 2);
                    end;
                end;

            // *** SOURCE DIRECTORY ***
            // SourceDirectoryAndFileName may include '\\' at the start, which
            // will become '\\\MyPath\SubFolder' by the time the longpathoverride is added.
            // So we just reduce it back to one, to follow immediately after the prefix.
            // i.e \\?\MyData\MyFolder instead of \\?\\\MyData\MyFolder

            SourceDirectoryAndFileName := LongPathOverride+SourceDirectoryAndFileName;
            if Pos('\\\', SourceDirectoryAndFileName) > 0 then
            begin
              SourceDirectoryAndFileName := StringReplace(SourceDirectoryAndFileName, '\\\', '\', [rfReplaceAll]);
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
            if SizeOfCurrentFile > 0 then
            begin
              SourceFileHasHash := Uppercase(CalcTheHashFile(SourceDirectoryAndFileName));
            end
            else
              begin
                SourceFileHasHash := 'zero byte file';
                inc(ZeroByteFilesCounter, 1);
              end;

            // Now create the destination directory structure, if it is not yet created.


            if not LazFileUtils.DirectoryExistsUTF8(FinalisedDestDir) then
              begin
                try
                  if not CustomisedForceDirectoriesUTF8(LongPathOverride+FinalisedDestDir, true) then
                    begin
                      ShowMessage(FinalisedDestDir+' cannot be created. Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
                    end;
                finally
                end;
              end;

            // *** DESTINATION DIRECTORY ***
            // CopiedFilePathAndName may include '\\' at the start, which
            // will become '\\\MyPath\SubFolder' by the time the longpathoverride is added.
            // So we just reduce it back to one, to follow immediately after the prefix.
            // i.e \\?\MyData\MyFolder instead of \\?\\\MyData\MyFolder
            // We add a Windows compiler directive because UNC mode isnt in the Linux version

            // Now copy the file to the newly formed or already existing destination dir
            // and hash it. Then check that source and destination hashes match.
            // Then total up how many copied and hashed OK, or not.
            // If the user chooses not to reconstruct source dir structure,
            // check for filename conflicts, create an incrementer to ensure uniqueness,
            // and rename to "name.ext_DuplicatedNameX". Otherwise, reconstruct source path
            {$ifdef Windows}
            CopiedFilePathAndName := LongPathOverride+CopiedFilePathAndName;
            if Pos('\\\', CopiedFilePathAndName) > 0 then
            begin
              CopiedFilePathAndName := StringReplace(CopiedFilePathAndName, '\\\', '\', [rfReplaceAll]);
            end;
            {$endif}

            if chkNoPathReconstruction.Checked = false then
              begin
                CopiedFilePathAndName := IncludeTrailingPathDelimiter(LongPathOverride+FinalisedDestDir) + FinalisedFileName;
              end
              else
                begin
                  if LazFileUtils.FileExistsUTF8(IncludeTrailingPathDelimiter(LongPathOverride+FinalisedDestDir) + FinalisedFileName) then
                  begin
                    DupCount := DupCount + 1;
                    CopiedFilePathAndName := IncludeTrailingPathDelimiter(LongPathOverride+FinalisedDestDir) + FinalisedFileName + '_DuplicatedName' + IntToStr(DupCount);
                  end
                  else
                  CopiedFilePathAndName := IncludeTrailingPathDelimiter(LongPathOverride+FinalisedDestDir) + FinalisedFileName;
                end;

            // Now copy the file, either to the reconstructed path or to the root
            // Note that FileCopyEx from JawWindows unit is better for monitoring copy progress.
            // though it seems unable to adjust created date from Vol1 to Vol2 too, same as CopyFile from FileUtil
            // But one day, look at adding it for user feedback when copying large files if nothing else
            if not FileUtil.CopyFile(SourceDirectoryAndFileName, CopiedFilePathAndName, true) then
              begin
                ShowMessage('Failed to copy file : ' + SourceDirectoryAndFileName + ' Error code: ' +  SysErrorMessageUTF8(GetLastOSError));
                SLCopyErrors.Add('Failed to copy: ' + SourceDirectoryAndFileName + ' ' + SourceFileHasHash);
                FileCopyErrors := FileCopyErrors + 1;
              end
            else
            begin
              if SizeOfCurrentFile > 0 then
                begin
                  DestinationFileHasHash := UpperCase(CalcTheHashFile(CopiedFilePathAndName));
                  NoOfFilesCopiedOK := NoOfFilesCopiedOK + 1;
                end
              else
              begin
                DestinationFileHasHash := 'zero byte file';
                NoOfFilesCopiedOK := NoOfFilesCopiedOK + 1; // copy still valid, even if it is zero byte
              end;
            end;

            // Check for hash errors. Does source and destination hashes match?
            // If not, log it to text file and also display in grid.
            if SourceFileHasHash <> DestinationFileHasHash then
              begin
                HashMismtachCount := HashMismtachCount + 1;
                SLCopyErrors.Add('Hash mismatch. Source file ' + SourceDirectoryAndFileName + ' ' + SourceFileHasHash + ' Hash of copied file: ' + CopiedFilePathAndName + ' ' + DestinationFileHasHash);

                {$IFDEF WINDOWS}
                Col1SourceFilePathAndName := RemoveLongPathOverrideChars(FilesFoundToCopy.Strings[i], LongPathOverride);
                  {$else}
                     {$IFDEF Darwin}
                       Col1SourceFilePathAndName := FilesFoundToCopy.Strings[i];
                     {$else}
                       {$IFDEF UNIX and !$ifdef Darwin}
                         Col1SourceFilePathAndName := FilesFoundToCopy.Strings[i];
                       {$ENDIF}
                     {$ENDIF}
                  {$ENDIF}
                 Col2SourceHash := SourceFileHasHash;
                  {$IFDEF WINDOWS}
                 Col3CopiedFilePathAndName := RemoveLongPathOverrideChars(CopiedFilePathAndName, LongPathOverride);
                  {$else}
                    {$IFDEF Darwin}
                      Col3CopiedFilePathAndName := CopiedFilePathAndName;
                    {$else}
                       {$IFDEF UNIX and !$ifdef Darwin}
                         Col3CopiedFilePathAndName := CopiedFilePathAndName;
                       {$endif}
                    {$endif}
                  {$endif}
                  Col4DestinationHash := DestinationFileHasHash;
                  Col5DateAttribute   := CrDateModDateAccDate;
              end
            // Else, no errors. No need to log to file but still display to user
            else if SourceFileHasHash = DestinationFileHasHash then
              begin
                {$IFDEF WINDOWS}
                Col1SourceFilePathAndName := RemoveLongPathOverrideChars(FilesFoundToCopy.Strings[i], LongPathOverride);
                  {$else}
                     {$IFDEF Darwin}
                       Col1SourceFilePathAndName := FilesFoundToCopy.Strings[i];
                     {$else}
                       {$IFDEF UNIX and !$ifdef Darwin}
                         Col1SourceFilePathAndName := FilesFoundToCopy.Strings[i];
                       {$ENDIF}
                     {$ENDIF}
                  {$ENDIF}
                 Col2SourceHash := SourceFileHasHash;
                  {$IFDEF WINDOWS}
                 Col3CopiedFilePathAndName := RemoveLongPathOverrideChars(CopiedFilePathAndName, LongPathOverride);
                  {$else}
                    {$IFDEF Darwin}
                      Col3CopiedFilePathAndName := CopiedFilePathAndName;
                    {$else}
                       {$IFDEF UNIX and !$ifdef Darwin}
                         Col3CopiedFilePathAndName := CopiedFilePathAndName;
                       {$endif}
                    {$endif}
                  {$endif}
                  Col4DestinationHash := DestinationFileHasHash;
                  Col5DateAttribute   := CrDateModDateAccDate;
              end;

            // Write values to database
            frmSQLiteDBases.WriteCOPYValuesToDatabase(Col1SourceFilePathAndName, Col2SourceHash, Col3CopiedFilePathAndName, Col4DestinationHash, Col5DateAttribute);
            CommitCount(nil);

            // Progress Status Elements:
            lblNoOfFilesToExamine.Caption := strNoOfFilesToExamine;
            NoFilesExamined := (i + 1);  // The total of files examined plus those that didnt hash or copy OK
            lblNoOfFilesToExamine2.Caption := IntToStr(NoFilesExamined);
            SizeOfFile2 := FileSize(FilesFoundToCopy.Strings[i]);
            TotalBytesRead2 := TotalBytesRead2 + SizeOfFile2;
            lblDataCopiedSoFar.Caption := '(' + FormatByteSize(TotalBytesRead2) + ')';
            // When or if the stop button is pressed, we need to prevent any
            // division by zero, thus the count check next...
            if FilesFoundToCopy.Count > 0 then
              begin
                lblFilesCopiedPercentage.Caption := IntToStr((NoFilesExamined * 100) DIV FilesFoundToCopy.Count) + '%';
                pbCopy.Position := ((NoFilesExamined *100) DIV FilesFoundToCopy.Count);
                Application.ProcessMessages;
              end;

          end // End of the "If Stop button not pressed" if
          else
            begin
              StatusBar3.SimpleText:= 'Aborted by user';
            end;
        end;   // End of the 'for Count' of Memo StringList loop

      // Commit any final database values that may not have yet been comitted
      frmSQLiteDBases.SQLTransaction1.CommitRetaining;
      frmSQLiteDBases.UpdateGridCOPYTAB(nil);

      // Now we can show the grid. Having it display for every file as it processes
      // wastes time and isn't especially necessary given the other progress indicators

      frmDisplayGrid1.RecursiveDisplayGrid_COPY.Visible := true;
      frmDisplayGrid1.Show;
      EndTime := Now;
      lblTimeTaken6B.Caption  := FormatDateTime('YYYY/MM/DD HH:MM:SS', EndTime);
      TimeDifference          := EndTime - StartTime;
      //strTimeDifference       := FormatDateTime('h" hrs, "n" min, "s" sec"', TimeDifference);  // This way doesn't return days elapsed.
      strTimeDifference := (Format('%d days %s', [trunc(TimeDifference), FormatDateTime('h" hrs, "n" min, "s" sec"', TimeDifference)]));  // But this way does return days elapsed. Thanks WP in the forum!

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
            frmSQLiteDBases.SaveDBToCSV(frmDisplayGrid1.RecursiveDisplayGrid_COPY, CSVLogFile2);
          end;
      end;

      // If there is one or more errors, save them to a log file of users choosing
      if Length(SLCopyErrors.Text) > 0 then
       begin
         ShowMessage('Errors detected! You will now be prompted to save a text log...');
         if SaveDialog7.Execute then
           begin
             SaveDialog7.Title := 'Save error log...';
             SaveDialog7.InitialDir := GetCurrentDir;
             SaveDialog7.Filter := 'Text|*.txt';
             SaveDialog7.DefaultExt := 'txt';
             SLCopyErrors.SaveToFile(SaveDialog7.FileName);
           end;
      end;

      // All done. End the loops, free resources and notify user
      finally
        FilesFoundToCopy.Free;
        SLCopyErrors.Free;
        StatusBar3.SimpleText := 'Finished.';
        frmDisplayGrid1.btnClipboardResultsCOPYTAB.Enabled := true;
      end;


  SummaryMessage := CreateMessageDialog(
                    'Files copied : '   + IntToStr(NoOfFilesCopiedOK)    + #13#10 +
                    'Copy errors : '    + IntToStr(FileCopyErrors)       + #13#10 +
                    'Hash mismatches: ' + IntToStr(HashMismtachCount)    + #13#10 +
                    'Zero byte files: ' + IntToStr(ZeroByteFilesCounter), mtCustom, [mbOK]
                    );
  SummaryMessage.Position := poOwnerFormCenter;
  SummaryMessage.ShowModal;

  Button8CopyAndHash.Enabled := true;
end;

// Returns the size of the file in bytes on success. -1 otherwise.
// Needed only if the UNC prefixes cause a problem which they used to but seem not
// to anymore. So I just use FileSize now for all three operating systems.
{
function TMainForm.FileSizeWithLongPath(strFileName : string) : Int64;
var
  fs : TFileStream;
  FileSize : Int64;
begin
  result := -1;
  try
    fs := TFileStream.Create(strFileName, faReadOnly);
    FileSize := 0;
    FileSize := fs.size;
  finally
    fs.free;
  end;
  if FileSize > 0 then result := FileSize;
end;
}
{$IFDEF Windows}
// FUNCTION FileTimeToDTime - Windows specific,
// kindly acknowledged from xenblaise @ http://forum.lazarus.freepascal.org/index.php?topic=10869.0
function TMainForm.FileTimeToDTime(FTime: TFileTime): TDateTime;
var
  LocalFTime  : TFileTime;
  STime       : TSystemTime;

begin
  FillChar(LocalFTime{%H-}, SizeOf(LocalFTime), 0);
  FillChar(STime{%H-}, SizeOf(STime), 0);
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
    Edit2SourcePath.Color         := clWhite;
    Edit2SourcePath.Text          := 'Enter source UNC path (e.g. \\DCSERVER\DATA-1)';
    Edit2SourcePath.Visible       := true;
    Edit2SourcePath.ReadOnly      := false;
    Edit3DestinationPath.Color    := clWhite;

    Edit3DestinationPath.Text     := 'Enter destination UNC path (e.g \\FILESERVER\DATA-2';;
    Edit3DestinationPath.Visible  :=true;
    Edit3DestinationPath.ReadOnly := false;

    Button8CopyAndHash.Enabled    := true;
    DirListA.Enabled              := false;
    DirListA.Visible              := false;
    DirListB.Enabled              := false;
    DirListB.Visible              := false;
    end
  else
    begin
      Edit2SourcePath.Color         := clSilver;
      Edit2SourcePath.ReadOnly      := true;
      Edit2SourcePath.Text          := 'Select source directory below ';

      Edit3DestinationPath.Color    := clSilver;
      Edit3DestinationPath.Text     := 'Select destitnation directory below ';
      Edit3DestinationPath.ReadOnly := true;

      Button8CopyAndHash.Enabled    := false;
      DirListA.Enabled              := true;
      DirListA.Visible              := true;
      DirListB.Enabled              := true;
      DirListB.Visible              := true;
    end;
end;

procedure TMainForm.DirListAClick(Sender: TObject);
var
  NoOfDirsSelected, i : integer;
begin
  MultipleDirsChosen := false;
  NoOfDirsSelected := DirListA.SelectionCount;
  // If only one folder selected, do as as we always have
  if NoOfDirsSelected = 1 then
    begin
      MultipleDirsChosen := false;
      SourceDir := UTF8ToSys(DirListA.Path);
      if LazFileUtils.DirectoryExistsUTF8(SourceDir) then
       begin
         Edit2SourcePath.Text := SourceDir;
         SourceDirValid := TRUE;
         if SourceDirValid AND DestDirValid = TRUE then
           begin
             // Now enable the 'Go!' button as both SourceDir and DestDir are valid
             Button8CopyAndHash.Enabled := true;
          end;
       end;
   end
  else if NoOfDirsSelected > 1 then
    // The number of folders selected is greater than 1 so we must itterate
    try
      slMultipleDirNames := TStringList.Create;
      begin
        MultipleDirsChosen := true;
        for i := 0 to NoOfDirsSelected -1 do
          begin
            {$ifdef Windows}
            slMultipleDirNames.Add(StringReplace(DirListA.Selections[i].GetTextPath, '/', '\', [rfReplaceAll]));
            {$else}
              {$IFDEF Darwin}
                slMultipleDirNames.Add(DirListA.Selections[i].GetTextPath);
              {$else}
                {$IFDEF UNIX and !$ifdef Darwin}
                  slMultipleDirNames.Add(DirListA.Selections[i].GetTextPath);
                {$ENDIF}
              {$ENDIF}
            {$endif}
          end;
      end;
    finally
      // nothing to do
    end
  else MultipleDirsChosen := false;
end;


procedure TMainForm.DirListBClick(Sender: TObject);
begin
  DestDir := UTF8ToSys(DirListB.Path);
  if LazFileUtils.DirectoryExistsUTF8(DestDir) then
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
  if chkUNCMode.Checked then
    Edit2SourcePath.Text:= ''
  else Edit2SourcePath.Text:= 'Select source directory below';
end;

procedure TMainForm.Edit3DestinationPathEnter(Sender: TObject);
begin
  if chkUNCMode.Checked then
    Edit3DestinationPath.Text:= ''
  else Edit3DestinationPath.Text:= 'Select destination directory below';
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
    if Not LazFileUtils.DirectoryExistsUTF8(ADir) then
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
  if (ADrv<>'') and (not LazFileUtils.DirectoryExistsUTF8(ADrv))
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

procedure TMainForm.TabSheet1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TMainForm.TabSheet3ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

initialization

end.

