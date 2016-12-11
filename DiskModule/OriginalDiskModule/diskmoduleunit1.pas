{
Copyright (C) 2011-2014  Ted Smith https://sourceforge.net/users/tedtechnology

This is the disk hashing module for Windows. It is specifically written as
a unit for ease of manageability and it could, if needed, be run as a standalone
executable. Help from members of Stackoverflow.com and the Lazarus forums are
appreciated and acknowledged. It is not compiled for Linux or Mac.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You can read a copy of the GNU General Public License at
   http://www.gnu.org/licenses/>. Also, http://www.gnu.org/copyleft/gpl.html
}

unit DiskModuleUnit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, EditBtn, strutils, sha1Customised, DateUtils,

   {$IFDEF Windows}
   // For physical disk access in Windows:
   ActiveX, ComObj, Variants, Windows;
   {$ENDIF}
   {$ifdef Darwin}
     MacOSAll;
     {$else}
     {$ifdef UNIX and !$ifdef Darwin}
     UNIX;
     {$endif}
   {$endif}

type

  { TfrmDiskHashingModule }

  AverageCyclicSpeeds = Record
                          CyclicSpeed: Int64;
                          NumValues: Integer; { holds the actual number of points in the array }
                          Average: Real { holds the average or mean of the values in the array }
                        End;

  TfrmDiskHashingModule = class(TForm)
    btnListDisks: TButton;
    btbStop: TButton;
    btnSaveResults: TButton;
    DateEdit1: TDateEdit;
    edtComputedHash: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lbledtStartAtTime: TLabeledEdit;
    lblTimeTakenB: TLabel;
    lblTimeTakenA: TLabel;
    lblSectorsB: TLabel;
    lblSectorsA: TLabel;
    lblManufacturerB: TLabel;
    lblManufacturerA: TLabel;
    lblInterfaceA: TLabel;
    lblMediaTypeB: TLabel;
    lblMediaTypeA: TLabel;
    lblModelA: TLabel;
    lblModelB: TLabel;
    lblInterfaceB: TLabel;
    lblSpeedB: TLabel;
    lblSpeedA: TLabel;
    lblStartTimeB: TLabel;
    lblStartTimeA: TLabel;
    lblEndTimeB: TLabel;
    lblEndTimeA: TLabel;
    lblBytesLeftToHashB: TLabel;
    lblBytesLeftToHashA: TLabel;
    lblByteCapacityB: TLabel;
    lblByteCapacityA: TLabel;
    lblDiskNameB: TLabel;
    lblDiskNameA: TLabel;
    ListBox1: TListBox;
    SaveResultsDialog: TSaveDialog;
    Timer1: TTimer;
    {$ifdef Windows}
    procedure btbStopClick(Sender: TObject);
    procedure btnSaveResultsClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure ListBox1DblClick(Sender: TObject);
    procedure btnListDisksClick(Sender: TObject);
    procedure GetWin32_DiskDriveInfo();
    function FormatByteSize(const bytes: QWord): string;
    function GetDiskLengthInBytes(hSelectedDisk : THandle) : Int64;
    function GetDiskGeometry(hSelectedDisk : THandle) : Int64;
    function GetTimeInMilliseconds(theTime : TTime):Int64;
    function getDiskID(const inStr:String):string;
    function getModelNo(const inStr:String):string;
    function getMediaType(const inStr:String):string;
    function getManufacturer(const inStr:String):string;
    function getInterfaceType(const inStr:String):string;
    function ElapsedTimeAsString(t: Double): String;
    {$endif}
  private
    { private declarations }
  public
    { public declarations }
    DiskName : widestring; // Set by GetWin32_DiskDriveInfo then used by btnListDisks OnClick event
    intDiskSize, sValue5Sectors : Int64;
    Stop : Boolean;
    hSelectedDisk: THandle;
    Logfile : TStringList;
  end;

var
  frmDiskHashingModule: TfrmDiskHashingModule;

implementation
uses unit2; // Just for form sharing between disk module and QuickHash main

{$R *.lfm}

{ TfrmDiskHashingModule }

{$ifdef Windows}
procedure TfrmDiskHashingModule.FormCreate(Sender: TObject);
begin
  Stop := false;
  btbStop.Enabled:= false;
  frmDiskHashingModule.Caption := MainForm.Caption; // Make the title caption the same as QuickHash main
  DateEdit1.DefaultToday := true;
end;


procedure TfrmDiskHashingModule.btnListDisksClick(Sender: TObject);
begin
  try
    // Reset flags, clear listbox and interface labels from earlier runs
    Stop := false;
    ListBox1.Clear;
    lblSpeedB.Caption             := '...';
    lblDiskNameB.Caption          := '...';
    lblModelB.Caption             := '...';
    lblMediaTypeB.Caption         := '...';
    lblInterfaceB.Caption         := '...';
    lblSectorsB.Caption           := '...';
    lblManufacturerB.Caption      := '...';
    lblByteCapacityB.Caption      := '...';
    lblBytesLeftToHashB.Caption   := '...';
    lblStartTimeB.Caption         := '...';
    lblEndTimeB.Caption           := '...';
    lblTimeTakenB.Caption         := '...';
    lblSpeedB.Caption             := '...';
    edtComputedHash.Text          := '...';
    lbledtStartAtTime.Text        := 'HH:MM';

    // Now get the list of attached disks and display them in the ListBox
    GetWin32_DiskDriveInfo;
    except
      on E:EOleException do
         WriteLn(Format('EOleException %s %x', [E.Message,E.ErrorCode]));
      on E:Exception do
         WriteLn(E.Classname, ':', E.Message);
    end;
end;

procedure TfrmDiskHashingModule.ListBox1Click(Sender: TObject);
var
  index : integer;
  ListBox : TListBox;
  ModelNo, Manufacturer, MediaType, InterfaceType : string;

begin
   // Cast the passed object to its correct type
  listBox := TListBox(Sender);
   // Get the index of the selected list item
  index := listBox.ItemIndex;
  if index = -1 then Exit;

  DiskName       := getDiskID(ListBox.GetSelectedText);
  ModelNo        := getModelNo(ListBox.GetSelectedText);
  Manufacturer   := getManufacturer(ListBox.GetSelectedText);
  MediaType      := getMediaType(ListBox.GetSelectedText);
  InterfaceType  := getInterfaceType(ListBox.GetSelectedText);

  lblDiskNameB.Caption       := DiskName;
  lblModelB.Caption          := ModelNo;
  lblManufacturerB.Caption   := Manufacturer;
  lblMediaTypeB.Caption      := MediaType;
  lblInterfaceB.Caption      := InterfaceType;
  lblSectorsB.Caption        := 'Not yet computed';
end;

procedure TfrmDiskHashingModule.ListBox1DblClick(Sender: TObject);

var
  listBox                                 : TListBox;
  Buffer                                  : array [0..65535] of Byte;   // 1048576 (1Mb) or 262144 (240Kb) or 131072 (120Kb buffer) or 65536 (64Kb buffer)
  ctx                                     : TSHA1Context;
  Digest                                  : TSHA1Digest;
  index, ProgressCounter, BytesRead       : integer;

  NewPos, ExactDiskSize, SectorCount,
    BytesPerMinute, TimeTakenForInterval,
    BytesExaminedDuringInterval,
    BytesPerSecond, TotalBytesRead: Int64;

    StartTime, IntervalReadTime,
    TimeStartRead, EndTime, TimeDifference, DateToStart, TimeToStart, StartAt,
    CurrentTime: TDateTime;

  strTimeDifference                        : string;

  // TODO : AverageSpeed : AverageCyclicSpeeds;

const
  DataTransferTrigger : Integer = 6000; // The number of buffer reads to allow before a transfer speed computation

begin
  // First, check if a timer has been invoked. If not, bypass this...
  if pos('HH:MM', lbledtStartAtTime.Text) = 0 then   // Only execute timer if user has entered a start time
  begin
   // Check the TIME is valid number of chars
   if (Length(lbledtStartAtTime.Text) > 5) or (Length(lbledtStartAtTime.Text) < 5) then
     begin
       ShowMessage('Enter time as 5 characters, e.g. 22:15');
       Stop := true;
     end
   // Check the DATE is today or later
   else if CompareDate(DateEdit1.Date, Now) < 0 then
     begin
       ShowMessage('Enter date equal to or ahead of today');
       Stop := true;
       Abort;
     end
   else
   begin
     btbStop.Enabled := true;
     DateToStart := DateEdit1.Date;
     TimeToStart := StrToTime(lbledtStartAtTime.Text);
     StartAt := DateToStart + TimeToStart;
     // Check the DATE AND TIME is now or later
     if StartAt < Now then
       begin
        ShowMessage('Start time is in the past. Enter a valid start time.');
        Stop := true;
       end
     else
     // All timer checks passed. Start timer...
     begin
       ShowMessage('Hashing will start at: ' + FormatDateTime('dd/mm/yy hh:mm', StartAt) + '. Click OK to continue...');
       // The user MAY have waited hours before clicking OK to the message above.
       // In rare cases, the scheduled start time may, by now, have come and gone. So check that:
       if CompareDateTime(StartAt, Now) < 0 then
         begin
           ShowMessage('The scheduled start time you entered has now elasped.');
           abort;
         end
       else
       repeat // start the timer...
         lblStartTimeB.Caption := 'Counting down to users schedule...';
         CurrentTime := Now;
         Application.ProcessMessages;
       until (CurrentTime = StartAt) or (Stop = true); // Timer date and time has arrived. Start hashing (unless user has aborted)...
     end;
   end;
  end;   // End of timer invokation check

  NewPos              := 0;
  ExactDiskSize       := 0;
  SectorCount         := 0;
  ProgressCounter     := 0;
  IntervalReadTime    := 0;
  BytesRead           := 0;
  TotalBytesRead      := 0;

  // Cast the passed object to its correct type
  listBox := TListBox(Sender);

  // Get the index of the selected list item
  index := listBox.ItemIndex;
  if index = -1 then Exit;

  // DiskName is a global variable containing string '\\.\PHYSICALDISKX',
  // populated by procedure TfrmDiskHashingModule.GetWin32_DiskDriveInfo;

  DiskName := getDiskID(Listbox.GetSelectedText);

  {
  I use CreateFileW here because the MSDN Win API styates that for
  DEVICES (not files) you should, rather than use CreateFile

  See http://msdn.microsoft.com/en-us/library/windows/desktop/aa363858%28v=vs.85%29.aspx
      http://blogs.msdn.com/b/larryosterman/archive/2004/05/13/131263.aspx              }
  if not stop then  // Only handle the disk if the user has not clicked stop following a timer initiation
    begin
      hSelectedDisk := CreateFileW(PWideChar(DiskName), FILE_READ_DATA,
                   FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_FLAG_SEQUENTIAL_SCAN, 0);
    end;

  // If the selected disk is anything other than a proper HDD, we don't want to wait for QH to read XXXX
  // 64Kb buffers before it updates the interface. So if its anything other than HDD,
  // refresh interface with fewer buffer reads.
  if not (Pos('Fixed hard disk media', getMediaType(ListBox.GetSelectedText)) > 0) then
    begin
     DataTransferTrigger := 700;
    end;

  // Check that the handle is assigned OK and raise error if not
  if hSelectedDisk = INVALID_HANDLE_VALUE then
    begin
      RaiseLastOSError;
    end
  else if not stop then
    begin
      btbStop.Enabled          := true;
      // We need the exact disk size in bytes to know when to stop reading
      ExactDiskSize            := GetDiskLengthInBytes(hSelectedDisk);

      // SectorCount   := GetDiskGeometry(hSelectedDisk); // This seems not to be entirely accurate?! LBA is always < true LBA.
      SectorCount              := ExactDiskSize DIV 512;
      lblSectorsB.Caption      := IntToStr(SectorCount);

      // Update interface with timers etc
      lblDiskNameB.Caption     := DiskName;
      lblByteCapacityB.Caption := IntToStr(ExactDiskSize) + ' bytes, ' + FormatByteSize(ExactDiskSize);
      StartTime := Now;           // This is for the GUI and log file...not the progress computations
      lblStartTimeB.Caption    := FormatDateTime('dd/mm/yy hh:mm:ss', StartTime);

      // Generate a log in memory to record the disk details and dates of actions
      try
        LogFile := TStringList.Create;
      finally
        Logfile.Add('Disk ID: '      + DiskName);
        Logfile.Add('Model Number: ' + getModelNo(ListBox.GetSelectedText));
        LogFile.Add('Disk Size: '    + FormatByteSize(ExactDiskSize) + ' , ' + IntToStr(ExactDiskSize) + ' bytes.');
        Logfile.Add('Started at: '   + lblStartTimeB.Caption);
      end;


      // Now read the disk FROM START TO END and hash it until completion or the user aborts it
      try
        SHA1Init(ctx);
        FileSeek(hSelectedDisk, 0, 0);
        repeat
          ProgressCounter := ProgressCounter + 1; // We use this update the progress display occasionally, instead of every buffer read
          TimeStartRead   := Now;

          // The hashing bit...read the disk in buffers, hash each buffer and then
          // finalise the finished hash. If there's a read error, abort.

          // Step 1 : Check we are not at the end of the disk where bytes remaining
          // could be less than the size of the buffer
          if (ExactDiskSize - TotalBytesRead) < SizeOf(Buffer) then
            begin
              // Read 65535 or less bytes
              BytesRead    := FileRead(hSelectedDisk, Buffer, (ExactDiskSize - TotalBytesRead));
            end
          else
            begin
              // Read 65536 (64kb) at a time
              BytesRead     := FileRead(hSelectedDisk, Buffer, SizeOf(Buffer));
            end;
          if BytesRead = -1 then
            begin
              ShowMessage('There was a read error encountered. Aborting');
              exit;
            end
          else
            // Step 2 : No read errors, so now we hash ...
            // Update positions, update hash sequence, and update GUI
            begin
            inc(TotalBytesRead, BytesRead);
            NewPos := NewPos + BytesRead;
            SHA1Update(ctx, Buffer, BytesRead);

            // Step 3 : Update the interface only if the loop has gone round a while
            // (DataTransferTrigger times), update the progress display

            if ProgressCounter = DataTransferTrigger then
               begin
                 BytesExaminedDuringInterval := 0;
                 // This should always be the same but it is whatever loop cycle is chosen by the size of the chosen buffer
                 BytesExaminedDuringInterval := (DataTransferTrigger * SizeOf(Buffer));
               { Now compute the number of Ms between the start time of this
                 particular buffer read and the time from when the last loop was execute, and convert to seconds
                 Then with the number of bytes examined since the last progress loop known,
                 and with the time in seconds since the last progress loop known,
                 divide the two for bytes p\s then mumltipl 60 for p\min
                 Only do the divisions if the times are > 0 to avoid division by zero errors
               }
                 TimeTakenForInterval        := (MilliSecondsBetween(IntervalReadTime, TimeStartRead) DIV 1000);
                 BytesPerSecond              := (BytesExaminedDuringInterval DIV TimeTakenForInterval);
                 BytesPerMinute              := BytesPerSecond * 60;

                 if BytesPerMinute > 0 then
                    begin
                      lblSpeedB.Caption := FormatByteSize(BytesPerMinute) + ' p\min';
                      lblBytesLeftToHashB.Caption := IntToStr(ExactDiskSize - NewPos) + ' bytes, ' + FormatByteSize(ExactDiskSize - NewPos);
                    end;
                 // Reset the counter for another looping cycle
                 ProgressCounter  := 0;
                 // And now get the time that this progress loop ended,
                 // in order to compute the time taken for next time
                 IntervalReadTime := Now;
              end; // End of Step 3, ProgressCounter if statement
            end; // end of BytesRead else statement
            Application.ProcessMessages;
        until (TotalBytesRead = ExactDiskSize) or (Stop = true);
        // Compute the final hash value
        SHA1Final(ctx, Digest);
        lblBytesLeftToHashB.Caption:= '0';
        // End of the hashing bit...
      finally
        // The handle may have been released by pressing stop. If not, the handle will still be active so lets close it.
        if not hSelectedDisk = INVALID_HANDLE_VALUE then CloseHandle(hSelectedDisk);

        btbStop.Enabled       := false;

        // Compute the end time and how long it took. Only for the GUI.
        // http://forum.lazarus.freepascal.org/index.php?topic=25000.msg151229#msg151229
        EndTime               := Now;
        lblEndTimeB.Caption   := FormatDateTime('dd/mm/yy hh:mm:ss', EndTime);
        TimeDifference        := EndTime - StartTime;
        strTimeDifference     := FormatDateTime('h" hrs, "n" min, "s" sec"', TimeDifference);
        lblTimeTakenB.Caption := strTimeDifference;

        // Populate the hash field with the hash value
        if not stop then edtComputedHash.Text := Uppercase(SHA1Print(Digest));

        // Add to the logfile stringlist, unless it has since been freed elsewhere
        if assigned (Logfile) then
           begin
            Logfile.Add('Computed Hash: '    + edtComputedHash.Text);
            Logfile.Add('Hash computed at: ' + FormatDateTime('dd/mm/yy hh:mm:ss', EndTime));
            Logfile.Add('Time taken: '       + strTimeDifference);
           end;

        Application.ProcessMessages;
      end;
 end;
end;


// Custom functions and procedures

{ The Win32_DiskDrive class represents a physical disk drive as seen by a computer
running the Win32 operating system. Any interface to a Win32 physical disk drive is
a descendent (or member) of this class. The features of the disk drive seen through
this object correspond to the logical and management characteristics of the drive.
In some cases, this may not reflect the actual physical characteristics of the device.
Any object based on another logical device would not be a member of this class. }
// Example: IDE Fixed Disk.

procedure TfrmDiskHashingModule.GetWin32_DiskDriveInfo;
const
  WbemUser            ='';
  WbemPassword        ='';
  WbemComputer        ='localhost';
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  // http://forum.lazarus.freepascal.org/index.php?topic=24490.0
  FWbemObject   : OLEVariant;                // Changed from variant to OLE Variant for FPC 3.0.
  oEnum         : IEnumvariant;
  nrValue       : LongWord;                  // Added to replace nil pointer for FPC 3.0
  nr            : LongWord absolute nrValue; // FPC 3.0 requires IEnumvariant.next to supply a longword variable for # returned values
  sValue1, sValue2, sValue3, sValue4, sValue8, sValue9  : string;
begin
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer(WbemComputer, 'root\CIMV2', WbemUser, WbemPassword);
  FWbemObjectSet:= FWMIService.ExecQuery('SELECT * FROM Win32_DiskDrive','WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;

  while oEnum.Next(1, FWbemObject, nr) = 0 do
  begin
    DiskName:= FWbemObject.Properties_.Item('DeviceID').Value; //global var for storing PhysicalDisk value
    // Size is OK for labels etc but not exact enough for passing to the hash functions so dont use elsewhere.
    sValue1 := FormatByteSize(FWbemObject.Properties_.Item('Size').Value);
    sValue2 := FWbemObject.Properties_.Item('Model').Value;
    sValue3 := FWbemObject.Properties_.Item('Partitions').Value;
    sValue4 := FWbemObject.Properties_.Item('Manufacturer').Value;
//  sValue6 := FWbemObject.Properties_.Item('SerialNumber').Value; // Not available for WinXP! :-(
    sValue8 := FWbemObject.Properties_.Item('MediaType').Value;
    sValue9 := FWbemObject.Properties_.Item('InterfaceType').Value;
    ListBox1.Items.Add(Format('ID %s',[DiskName]) +
                              '  [' + sValue1 +
                              '] [Model: ' + sValue2 +
                          //  '] [# Sectors: '+ IntToStr(sValue5Sectors) +
                              '] [# Partitions: '+ sValue3 +
                              '] [Manufacturer: ' + sValue4 +
                          //  '] [Serial Number of Disk: ' + sValue6 +  Not available for WinXP!
                              '] [Type of Media: ' + sValue8 +
                              '] [Interface: ' + sValue9 + '] ');
    FWbemObject:= Unassigned;
  end;
end;


// Stop the hashing process
procedure TfrmDiskHashingModule.btbStopClick(Sender: TObject);
begin
  Stop := true;
  try
    CloseHandle(hSelectedDisk);
  finally
    edtComputedHash.Text := 'Hashing process aborted by user';
  end;
end;

// Enable the user to save the results of the endeavor to a log file of their choice
procedure TfrmDiskHashingModule.btnSaveResultsClick(Sender: TObject);
begin
  try
    SaveResultsDialog.Execute;
    Logfile.SaveToFile(SaveResultsDialog.FileName);
  finally
    if assigned (Logfile) then LogFile.Free;
  end;

end;


procedure TfrmDiskHashingModule.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  // Reserved for when and if I choose to add something when the disk hash module is closed
end;


function TfrmDiskHashingModule.getModelNo(const inStr:String):string;
Var
  vPos1, vPos2 : Integer;
begin
  vpos1 := pos('[Model:', inSTr);
  vpos2 := posex(' [', instr,vpos1+7);
  result := copy(instr, vpos1, vpos2-vpos1);
end;

function TfrmDiskHashingModule.getManufacturer(const inStr:String):string;
Var
  vPos1, vPos2 : Integer;
begin
  vpos1 := pos('[Manufacturer:', inSTr);
  vpos2 := posex(' [', instr,vpos1+14);
  result := copy(instr, vpos1, vpos2-vpos1);
end;

function TfrmDiskHashingModule.getMediaType(const inStr:String):string;
Var
  vPos1, vPos2 : Integer;
begin
  vpos1 := pos('[Type of Media:', inSTr);
  vpos2 := posex(' [', instr,vpos1+15);
  result := copy(instr, vpos1, vpos2-vpos1);
end;

function TfrmDiskHashingModule.getInterfaceType(const inStr:String):string;
Var
  vPos1, vPos2 : Integer;
begin
  vpos1 := pos('[Interface:', inSTr);
  vpos2 := posex('] ', instr,vpos1+12);
  result := copy(instr, vpos1, (vpos2+1)-vpos1);
end;


function TfrmDiskHashingModule.getDiskID(const inStr:String):string;
Var
  vPos1, vPos2 : Integer;
begin
  vpos1 := pos('\\.\', inSTr);
  vpos2 := posex(' [', instr,vpos1+4);
  result := copy(instr, vpos1, vpos2-vpos1);
end;

// Convert the time taken to "X days, X hours, X mins, X secs"
function TfrmDiskHashingModule.ElapsedTimeAsString(t: Double): String;
begin
  Result := IntToStr(trunc(t)) + ' day';
  if trunc(t) > 1 then Result := Result + 's';
  Result := Result + ', ' + FormatDateTime('h" minutes, "s" seconds"', frac(t));
end;

// Not actually needed as there's a FPC command "MillisecondsElapsed" but useful code to retain
function TfrmDiskHashingModule.GetTimeInMilliseconds(theTime : TTime):Int64;
var
  Hour, Min, Sec, MSec: Word;
begin
  DecodeTime(theTime, Hour, Min, Sec, MSec);
  Result := (Hour * 3600000) + (Min * 60000) + (Sec * 1000) + MSec;
end;

function TfrmDiskHashingModule.FormatByteSize(const bytes: QWord): string;
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

// We need the exact length of the full disk in bytes to use as the end point
// for the hashing. IOCTL_DISK_GET_LENGTH_INFO works regardless of how the disk
// is setup, partitioned, etc. The others seem to be volume specific, so OK
// for C:\, D:\ etc but not PhysicalDiskX

function TfrmDiskHashingModule.GetDiskLengthInBytes(hSelectedDisk : THandle) : Int64;
const
  // These are defined at the MSDN.Microsoft.com website for DeviceIOControl
  IOCTL_DISK_GET_LENGTH_INFO  = $0007405C;

type
  TDiskLength = packed record
    Length : Int64;
  end;

var
  BytesReturned: DWORD;
  DLength: TDiskLength;
  ByteSize: int64;

begin
  BytesReturned := 0;
  // Get the length, in bytes, of the physical disk
  if not DeviceIOControl(hSelectedDisk, IOCTL_DISK_GET_LENGTH_INFO, nil, 0,
         @DLength, SizeOf(TDiskLength), BytesReturned, nil) then
           raise Exception.Create('Unable to determine byte capacity of disk ' + DiskName);
  ByteSize := DLength.Length;
  result := ByteSize;
end;

// This returns the Windows centric sector count for the disk, i.e. what Windows
// thinks the disk geometry is. In reality, this is usually slightly less than
// its ACTUAL sector count as reported by proper forensic tools.
// TODO : Try and work out how WinHex, FTKi etc get the exact number.
function TfrmDiskHashingModule.GetDiskGeometry(hSelectedDisk : THandle) : Int64;

const
  // These are defined at the MSDN.Microsoft.com website for DeviceIOControl
  IOCTL_DISK_GET_DRIVE_GEOMETRY  = $00070000;

type
{$MINENUMSIZE 4}
  TMediaType = (
    Unknown,                // Format is unknown
    F5_1Pt2_512,            // 5.25", 1.2MB,  512 bytes/sector
    F3_1Pt44_512,           // 3.5",  1.44MB, 512 bytes/sector
    F3_2Pt88_512,           // 3.5",  2.88MB, 512 bytes/sector
    F3_20Pt8_512,           // 3.5",  20.8MB, 512 bytes/sector
    F3_720_512,             // 3.5",  720KB,  512 bytes/sector
    F5_360_512,             // 5.25", 360KB,  512 bytes/sector
    F5_320_512,             // 5.25", 320KB,  512 bytes/sector
    F5_320_1024,            // 5.25", 320KB,  1024 bytes/sector
    F5_180_512,             // 5.25", 180KB,  512 bytes/sector
    F5_160_512,             // 5.25", 160KB,  512 bytes/sector
    RemovableMedia,         // Removable media other than floppy
    FixedMedia,             // Fixed hard disk media
    F3_120M_512             // 3.5", 120M Floppy
  );
{$MINENUMSIZE 1}

  TDiskGeometry = packed record
    Cylinders: int64;
    MediaType: TMediaType;
    TracksPerCylinder: DWORD;
    SectorsPerTrack: DWORD;
    BytesPerSector: DWORD;
  end;

var
  BytesReturned: DWORD;
  DG: TDiskGeometry;
  SectorCount: int64;

begin
  BytesReturned := 0;
  SectorCount   := 0;

  if not DeviceIOControl(hSelectedDisk, IOCTL_DISK_GET_DRIVE_GEOMETRY, nil, 0,
  @DG, SizeOf(TDiskGeometry), BytesReturned, nil) then
    raise Exception.Create('Unable to determine disk geometry of ' + DiskName);

  SectorCount := (DG.TracksPerCylinder * DG.Cylinders) * DG.SectorsPerTrack;
  // ShowMessage('Sector Count: ' + IntToStr(SectorCount));
  result := SectorCount;
end;
{$endif}

end.

