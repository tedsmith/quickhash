// A custom class built to handle GPT partitioned disks (c) Ted Smith YAFFI Project
// The class is built to more easily enable YAFFI to both read and query individual
// bytes of GPT headers. The Windows API is a little too restrictive.
unit uGPT;
// This unit is also from my disk imager, YAFFI https://github.com/tedsmith/yaffi.
{
    Quick Hash GUI - A Linux, Windows and Apple Mac GUI for quickly selecting one or more files
                     and generating hash values for them.

    Copyright (C) 2011-2022  Ted Smith www.quickhash-gui.org

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

{$mode objfpc}{$H+}

interface

uses
  {$ifdef Windows}
    Windows, SysUtils, Dialogs, Forms, Classes, StrUtils;
  {$endif}
  {$ifdef UNIX}
    SysUtils;
  {$endif}
type
  // Ref http://lockandcode.com/wp-content/uploads/2012/05/LockCode-Computer-Forensic-Examiner-Quick-Reference-Guide-Version-2.0-Sample.pdf
  // https://en.wikipedia.org/wiki/GUID_Partition_Table
  // http://thestarman.narod.ru/asm/mbr/GPT.htm
  //
  // the first 512 byte Protected MBR that came when GPT was introduced
  // At offset 446 starts some of the GPT data. The MBR data that preseeds it is not needed
  TProtectiveMBR = packed record
      StartOfSector            : array [0..445] of byte; // The first 446 bytes are MBR specific. Ignore
      BootIndicator            : byte;                   // One byte 8-bit integer
      StartingHead             : byte;                   // One hex byte
      StartingSector           : byte;                   // One hex byte
      StartingCylinder         : byte;                   // One hex byte
      SystemID                 : byte;                   // One hex byte. Should be 0xEE for GPT
      EndingHead               : byte;                   // One hex byte
      EndingSector             : byte;                   // One hex byte
      EndingCylinder           : byte;                   // One hex byte
      StartLBA                 : longword;               // 4 byte integer
      SizeInLBA                : longword;               // 4 byte integer
      EndOfSector              : array [0..49] of byte   // The last 50 bytes. Ignore
  end;

  TGUIDPartitionTableHeader = packed record
      Signature                : array [0..7] of byte;   // 8 hex digits, starting 0x45 and ending 0x54
      RevisionNo               : array [0..3] of byte;   // 4 hex bytes
      HeaderSize               : longword;               // 32-bit 4 byte integer, and should equal 92
      HeaderCRC32              : longword;               // to be displayed as hex but a 4 byte integer
      EmptyData                : integer;                // 4 bytes padding
      PrimaryLBA               : Int64;                  // 8 byte integer that should equal 1
      BackupLBA                : Int64;                  // 8 byte integer
      FirstUseableLBA          : Int64;                  // 8 bytes
      LastUseableLBA           : Int64;                  // 8 bytes
      DiskGUID                 : array [0..15] of byte;  // 16 hex values
      PartitionEntryLBA        : Int64;                  // Should equal 2
      MaxPossiblePartitions    : longword;               // 4 bytes
      SizeOfPartitionEntry     : longword;               // 4 bytes
      PartitionEntryArrayCRC32 : longword;               // 4 bytes displayed as hex
      // 92 bytes to here. Remaining sector size is 420
      EndOfSector              : array [0..419] of byte; // Ignore
  end;

  // This last class was superseeded by the memory stream technique. The class was
  // flawed in that only one GPT partition table entry could be traversed.
  // The others ended up in the EndOfSector region, untraversable.
  // The code is retained below though as its a useful reference
  {
  TGUIDPartitionTableEntry = packed record
      PartitionTypeGUID    : TGUID;                    //array [0..15] of byte;   // 16 byte hex string
      UniquePartitionGUID  : TGUID;                    // array [0..15] of byte; // 16 byte hex string
      StartingLBA          : Int64;                    // 8 byte integer
      EndingLBA            : Int64;                    // 8 byte integer
      AttributeBits        : array [0..7] of byte;     // 8 byte hex string
      PartitionName        : array [0..71] of widechar;// 36 byte Unicode string
      // This sums to 128 bytes - the size of a GPT partition table entry
      EndOfSector          : array [0..383] of byte;   // assuming 512 sector size, the first records = 128 bytes. So 384 left as padding
  end;
  }

function QueryGPT(SelectedDisk : widestring) : ansistring;
function ReadProtectiveMBR(Drive : THandle; ExactSectorSize : Longword) : ansistring;
function ReadGUIDPartitionTableHeader(Drive : THandle; ExactSectorSize : LongWord) : ansistring;
function TraverseEachPartitionTableEntry(Buffer : array of byte; ExactSectorSize : Longword) : ansistring;
function GetGUIDTypeID (TablePortion : TMemoryStream; Position : LongWord) : TGUID;
function GetUniquePartitionGUID (TablePortion : TMemoryStream; Position : LongWord) : TGUID;
function GetPartitionAttributes(TablePortion : TMemoryStream; Position : Longword) : string;
function GetPartitionLabel(TablePortion : TMemoryStream; Position : Longword) : string;
function CreatorLookup(TypeGUID : TGUID) : string;
function LoadPartitionGUIDTypes() : TStringList;
function FormatByteSize(const bytes: QWord): string;

implementation

uses
   diskmodule;

var
slGUIDList : TStringList;
intPartitionTableEntryLBA : longword;

// Returns a human readable view of the number of bytes as Mb, Gb Tb, etc
function FormatByteSize(const bytes: QWord): string;
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

// The next few functions are specific to the main GPT Partition Table Entry list

// GetGUIDTypeID looks up the first 16 bytes; the GUID Type identifier
// It is later looked up against a list of known GUIDs for various sytems

function GetGUIDTypeID (TablePortion : TMemoryStream; Position : LongWord) : TGUID;
var
  GUID1      : TGUID;
  Data4Array : array [0..7] of byte;
  i          : integer;

begin
  i := 0;
  GUID1.Data1:=  TablePortion.ReadDWord;   // First 4 bytes of GUID
  GUID1.Data2 := TablePortion.ReadWord;   // Bytes 5 & 6
  GUID1.Data3 := TablePortion.ReadWord;   // Bytes 7 & 8

  FillChar(Data4Array{%H-}, 8, 0);
  for i := 0 to 7 do
  begin
    Data4Array[i] := TablePortion.ReadByte;
  end;
  if SizeOf(Data4Array) = 8 then
    begin
      GUID1.Data4 := Data4Array;
    end;
  result := GUID1;
end;

// GetUniquePartitionGUID gets the next 16 bytes; the unique GUID identifier for the partition
function GetUniquePartitionGUID(TablePortion : TmemoryStream; Position : Longword) : TGUID;
var
  GUID2      : TGUID;
  Data4Array : array [0..7] of byte;
  i          : integer;
begin
  i := 0;
  GUID2.Data1 := TablePortion.ReadDWord;   // First 4 bytes of GUID
  GUID2.Data2 := TablePortion.ReadWord;   // Bytes 5 & 6
  GUID2.Data3 := TablePortion.ReadWord;   // Bytes 7 & 8

  FillChar(Data4Array{%H-}, 8, 0);
  for i := 0 to 7 do
  begin
    Data4Array[i] := TablePortion.ReadByte;
  end;
  if SizeOf(Data4Array) = 8 then
    begin
      GUID2.Data4 := Data4Array;
    end;
  result := GUID2;
end;

// GetPartitionAttributes looks up the 8 byte hex attributes
// TODO : Drill down further to bit level lookups
function GetPartitionAttributes(TablePortion : TMemoryStream; Position : Longword) : string;
var
  i, j             : integer;
  AttributeBits    : array [0..7] of byte;
  strAttributeBits : string;
begin
  strAttributeBits := '';
  for i := 0 to 7 do
    begin
      AttributeBits[i] := TablePortion.ReadByte;
    end;
    if SizeOf(AttributeBits) > 0 then
      for j := 0 to 7 do
      begin
        strAttributeBits := strAttributeBits + ' ' + IntToHex(AttributeBits[j], 2);
      end;
    result := strAttributeBits;
end;

// Gets the Unicode partition label. Commonly it is the Microsoft label, which is
// ironic given that they seldom follow the specifications exactly!
function GetPartitionLabel(TablePortion : TMemoryStream; Position : Longword) : string;
var
  PartitionLabel    : array [0..71] of byte;
  i                 : integer;
  strPartitionLabel : ansistring;
begin
  i := 0;
  strPartitionLabel := '';
  // The name is 72 byte Unicode value, so each char is 2 bytes.
  // So itterate 36 times, reading in two byte pairs
  for i := 0 to 71 do
  begin
    // Read in two byte Unicode pairings for 72 bytes
    PartitionLabel[i] := TablePortion.ReadByte;
  end;
  if SizeOf(PartitionLabel) > 0 then
    begin
      strPartitionLabel := WideCharToString(@PartitionLabel);
    end;
result := strPartitionLabel;
end;

// Lookup the TypeGUID against the creator list. Often the values will mirror
// each other, but not always. Useful to alert as to the prescence of VMFS, dm-crypt etc
function CreatorLookup(TypeGUID : TGUID) : string;
const
  StdWordDelimsCustom = [','] + Brackets;
var
  i, indx, ExtractFrom             : integer;
  strGUID, GUIDLabel, CreatorLabel : string;
begin
  indx := 0;
  i := 0;
  slGUIDList := LoadPartitionGUIDTypes();
  strGUID    := GuidToString(TypeGUID);

  for i := 0 to slGUIDList.Count -1 do
    begin
     indx := Pos(strGUID, slGUIDList.Strings[i]);
     if indx > 0 then
       begin
         // Extract the first portion of the line containing the GUID, which will be the creator label
         ExtractFrom := 1;
         GUIDLabel := ExtractSubStr(slGUIDList.Strings[i], ExtractFrom, StdWordDelimsCustom);
         CreatorLabel := GUIDLabel;
       end;
    end;
  slGUIDList.Free;
  result := CreatorLabel;
end;

// LoadPartitionGUIDTypes returns a sorted String List of known GUID Partition types
// and is used to lookup the creating system, e.g. VMFS is VMWare FileSystem
function LoadPartitionGUIDTypes() : TStringList;
  begin
    try
      slGUIDList := TStringList.Create;
      slGUIDList.Sorted:= true;
      slGUIDList.Delimiter:= Chr($2C); // comma character

      slGUIDList.Add('Unused entry,{00000000-0000-0000-0000-000000000000}');
      slGUIDList.Add('MBR partition scheme,{024DEE41-33E7-11D3-9D69-0008C781F39F}');
      slGUIDList.Add('EFI System partition,{C12A7328-F81F-11D2-BA4B-00A0C93EC93B}');
      slGUIDList.Add('BIOS Boot partition,{21686148-6449-6E6F-744E-656564454649}');
      slGUIDList.Add('Intel Fast Flash (iFFS) partition (for Intel Rapid Start technology),{D3BFE2DE-3DAF-11DF-BA40-E3A556D89593}');
      slGUIDList.Add('Sony boot partition,{F4019732-066E-4E12-8273-346C5641494F}');
      slGUIDList.Add('Lenovo boot partition,{BFBFAFE7-A34F-448A-9A5B-6213EB736C22}');
      slGUIDList.Add('Microsoft Reserved Partition,{E3C9E316-0B5C-4DB8-817D-F92DF00215AE}');
      slGUIDList.Add('Basic data partition[g],{EBD0A0A2-B9E5-4433-87C0-68B6B72699C7}');
      slGUIDList.Add('Logical Disk Manager (LDM) metadata partition,{5808C8AA-7E8F-42E0-85D2-E1E90434CFB3}');
      slGUIDList.Add('Logical Disk Manager data partition,{AF9B60A0-1431-4F62-BC68-3311714A69AD}');
      slGUIDList.Add('Windows Recovery Environment,{DE94BBA4-06D1-4D40-A16A-BFD50179D6AC}');
      slGUIDList.Add('IBM General Parallel File System (GPFS) partition,{37AFFC90-EF7D-4E96-91C3-2D7AE055B174}');
      slGUIDList.Add('Storage Spaces partition,{E75CAF8F-F680-4CEE-AFA3-B001E56EFC2D}');
      slGUIDList.Add('Data partition,{75894C1E-3AEB-11D3-B7C1-7B03A0000000}');
      slGUIDList.Add('Service Partition,{E2A1E728-32E3-11D6-A682-7B03A0000000}');
      slGUIDList.Add('Linux filesystem data,{0FC63DAF-8483-4772-8E79-3D69D8477DE4}');
      slGUIDList.Add('RAID partition,{A19D880F-05FC-4D3B-A006-743F0F84911E}');
      slGUIDList.Add('Swap partition,{0657FD6D-A4AB-43C4-84E5-0933C84B4F4F}');
      slGUIDList.Add('Logical Volume Manager (LVM) partition,{E6D6D379-F507-44C2-A23C-238F2A3DF928}');
      slGUIDList.Add('/home partition,{933AC7E1-2EB4-4F13-B844-0E14E2AEF915}');
      slGUIDList.Add('/srv (server data) partition,{3B8F8425-20E0-4F3B-907F-1A25A76F98E8}');
      slGUIDList.Add('Plain dm-crypt partition,{7FFEC5C9-2D00-49B7-8941-3EA10A5586B7}');
      slGUIDList.Add('LUKS partition,{CA7D7CCB-63ED-4C53-861C-1742536059CC}');
      slGUIDList.Add('Reserved,{8DA63339-0007-60C0-C436-083AC8230908}');
      slGUIDList.Add('Boot partition,{83BD6B9D-7F41-11DC-BE0B-001560B84F0F}');
      slGUIDList.Add('Data partition,{516E7CB4-6ECF-11D6-8FF8-00022D09712B}');
      slGUIDList.Add('Swap partition,{516E7CB5-6ECF-11D6-8FF8-00022D09712B}');
      slGUIDList.Add('Unix File System (UFS) partition,{516E7CB6-6ECF-11D6-8FF8-00022D09712B}');
      slGUIDList.Add('Vinum volume manager partition,{516E7CB8-6ECF-11D6-8FF8-00022D09712B}');
      slGUIDList.Add('ZFS partition,{516E7CBA-6ECF-11D6-8FF8-00022D09712B}');
      slGUIDList.Add('Hierarchical File System Plus (HFS+) partition,{48465300-0000-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Apple UFS,{55465300-0000-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('ZFS,{6A898CC3-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Apple RAID partition,{52414944-0000-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Apple RAID partition (offline),{52414944-5F4F-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Apple Boot partition (Recovery HD),{426F6F74-0000-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Apple Label,{4C616265-6C00-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Apple TV Recovery partition,{5265636F-7665-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Apple Core Storage (i.e. Lion FileVault) partition,{53746F72-6167-11AA-AA11-00306543ECAC}');
      slGUIDList.Add('Boot partition,{6A82CB45-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Root partition,{6A85CF4D-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Swap partition,{6A87C46F-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Backup partition,{6A8B642B-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('/usr partition,{6A898CC3-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('/var partition,{6A8EF2E9-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('/home partition,{6A90BA39-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Alternate sector,{6A9283A5-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Reserved partition,{6A945A3B-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Reserved partition,{6A9630D1-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Reserved partition,{6A980767-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Reserved partition,{6A96237F-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Reserved partition,{6A8D2AC7-1DD2-11B2-99A6-080020736631}');
      slGUIDList.Add('Swap partition,{49F48D32-B10E-11DC-B99B-0019D1879648}');
      slGUIDList.Add('FFS partition,{49F48D5A-B10E-11DC-B99B-0019D1879648}');
      slGUIDList.Add('LFS partition,{49F48D82-B10E-11DC-B99B-0019D1879648}');
      slGUIDList.Add('RAID partition,{49F48DAA-B10E-11DC-B99B-0019D1879648}');
      slGUIDList.Add('Concatenated partition,{2DB519C4-B10F-11DC-B99B-0019D1879648}');
      slGUIDList.Add('Encrypted partition,{2DB519EC-B10F-11DC-B99B-0019D1879648}');
      slGUIDList.Add('ChromeOS kernel,{FE3A2A5D-4F32-41A7-B725-ACCC3285A309}');
      slGUIDList.Add('ChromeOS rootfs,{3CB8E202-3B7E-47DD-8A3C-7FF2A13CFCEC}');
      slGUIDList.Add('ChromeOS future use,{2E0A753D-9E48-43B0-8337-B15192CB1B5E}');
      slGUIDList.Add('Haiku BFS,{42465331-3BA3-10F1-802A-4861696B7521}');
      slGUIDList.Add('Boot partition,{85D5E45E-237C-11E1-B4B3-E89A8F7FC3A7}');
      slGUIDList.Add('Data partition,{85D5E45A-237C-11E1-B4B3-E89A8F7FC3A7}');
      slGUIDList.Add('Swap partition,{85D5E45B-237C-11E1-B4B3-E89A8F7FC3A7}');
      slGUIDList.Add('Unix File System (UFS) partition,{0394EF8B-237E-11E1-B4B3-E89A8F7FC3A7}');
      slGUIDList.Add('Vinum volume manager partition,{85D5E45C-237C-11E1-B4B3-E89A8F7FC3A7}');
      slGUIDList.Add('ZFS partition,{85D5E45D-237C-11E1-B4B3-E89A8F7FC3A7}');
      slGUIDList.Add('Ceph Journal,{45B0969E-9B03-4F30-B4C6-B4B80CEFF106}');
      slGUIDList.Add('Ceph dm-crypt Encrypted Journal,{45B0969E-9B03-4F30-B4C6-5EC00CEFF106}');
      slGUIDList.Add('Ceph OSD,{4FBD7E29-9D25-41B8-AFD0-062C0CEFF05D}');
      slGUIDList.Add('Ceph dm-crypt OSD,{4FBD7E29-9D25-41B8-AFD0-5EC00CEFF05D}');
      slGUIDList.Add('Ceph disk in creation,{89C57F98-2FE5-4DC0-89C1-F3AD0CEFF2BE}');
      slGUIDList.Add('Ceph dm-crypt disk in creation,{89C57F98-2FE5-4DC0-89C1-5EC00CEFF2BE}');
      slGUIDList.Add('Data partition,{824CC7A0-36A8-11E3-890A-952519AD3F61}');
      slGUIDList.Add('Power-safe (QNX6) file system,{CEF5A9AD-73BC-4601-89F3-CDEEEEE321A1}');
      slGUIDList.Add('Plan 9 partition,{C91818F9-8025-47AF-89D2-F030D7000C2C}');
      slGUIDList.Add('vmkcore (coredump partition),{9D275380-40AD-11DB-BF97-000C2911D1B8}');
      slGUIDList.Add('VMWares VMFS filesystem partition,{AA31E02A-400F-11DB-9590-000C2911D1B8}');
      slGUIDList.Add('VMware Reserved partition,{9198EFFC-31C0-11DB-8F78-000C2911D1B8}');
    finally
      result := slGUIDList;
    end;
  end;

// SECTOR 0 Traversal
// Read in the Protective MBR - the kind created by GPT disks, not normal MBRs
function ReadProtectiveMBR(Drive : THandle; ExactSectorSize : LongWord) : ansistring;
var
  BytesRead, DiskPos : integer;
  ProtectiveMBR      : TProtectiveMBR;
  Tmp                : ansistring;
begin
  result := 'Failed';
  BytesRead := -1;
  DiskPos := -1;

  DiskPos := FileSeek(Drive, 0, fsFromBeginning);

  if DiskPos > -1 then
    begin
      FillChar(ProtectiveMBR{%H-}, SizeOf(ProtectiveMBR), 0);
      BytesRead := FileRead(Drive, ProtectiveMBR, ExactSectorSize);
      if BytesRead > -1 then
          begin
            tmp :=' Protective MBR Table Reports: ' + #13#10 +
                  '  Boot ID : '            + IntToStr(ProtectiveMBR.BootIndicator) + #13#10 +
                  ' Starting Head : '     + IntToHex(ProtectiveMBR.StartingHead, 2) + #13#10 +
                  ' Starting Sector : '   + IntToHex(ProtectiveMBR.StartingSector, 2) + #13#10 +
                  ' Starting Cylinder : ' + IntToHex(ProtectiveMBR.StartingCylinder, 2) + #13#10 +
                  ' System ID : '         + IntToHex(ProtectiveMBR.SystemID,2) + #13#10 +
                  ' Ending Head : '       + IntToHex(ProtectiveMBR.EndingHead, 2) + #13#10 +
                  ' Ending Sector : '     + IntToHex(ProtectiveMBR.EndingSector, 2) + #13#10 +
                  ' Ending Cylinder : '   + IntToHex(ProtectiveMBR.EndingCylinder, 2) + #13#10 +
                  ' Start LBA : '         + IntToStr(ProtectiveMBR.StartLBA) + #13#10 +
                  ' Size in LBA : '       + IntToStr(ProtectiveMBR.SizeInLBA) + #13#10 +
                  ' =================================================================' + #13#10;
          result := tmp;
          end
      else RaiseLastOSError; // BytesRead = -1
    end
  else RaiseLastOSError; // DiskPos = -1
end;

// SECTOR 1 Traversal
// Read in the GUID Partition Table Header
function ReadGUIDPartitionTableHeader(Drive : THandle; ExactSectorSize : LongWord) : ansistring;
var
  GUIDPartitionTableHeader : TGUIDPartitionTableHeader;

  i, BytesRead, DiskPos : integer;

  Signature, RevisionNo, DiskGUID, HeaderSize, HeaderCRC32, PrimaryLBA,
    BackupLBA, FirstUseableLBA, LastUsableLBA, PartitionEntryLBA,
    MaxPossiblePartitions, SizeOfPartitionEntry, PartitionEntryCRC32 : ansistring;
begin
  result    := 'false';
  BytesRead := -1;
  DiskPos   := -1;
  i         := 0;

  // Move read point to offset 512,
  // Even if ExactSectorSize value <> 512, the alignment will still be 512
  // bytes on from the start
  DiskPos := FileSeek(Drive, 512, fsFromBeginning);

  if DiskPos > -1 then
    begin
      FillChar(GUIDPartitionTableHeader{%H-}, SizeOf(GUIDPartitionTableHeader), 0);
      BytesRead := FileRead(Drive, GUIDPartitionTableHeader, ExactSectorSize);

      if BytesRead > -1 then
        begin

          for i := 0 to 7 do
          begin
            Signature := Signature + IntToHex(GUIDPartitionTableHeader.Signature[i], 2);
          end;

          for i := 0 to 3 do
          begin
            RevisionNo := RevisionNo + IntToHex(GUIDPartitionTableHeader.RevisionNo[i], 2);
          end;

          HeaderSize      := IntToStr(GUIDPartitionTableHeader.HeaderSize);
          HeaderCRC32     := IntToHex(GUIDPartitionTableHeader.HeaderCRC32, 2);
          PrimaryLBA      := IntToStr(GUIDPartitionTableHeader.PrimaryLBA);
          BackupLBA       := IntToStr(GUIDPartitionTableHeader.BackupLBA);
          FirstUseableLBA := IntToStr(GUIDPartitionTableHeader.FirstUseableLBA);
          LastUsableLBA   := IntToStr(GUIDPartitionTableHeader.LastUseableLBA);

          for i := 0 to 15 do
          begin
            DiskGUID := DiskGUID + IntToHex(GUIDPartitionTableHeader.DiskGUID[i], 2);
          end;

          // Store the Partition Table offset in a global var, to be used by other funcs
          // This is needed to locate the partition table list, just in case the
          // location is not the usual LBA of '2'. Mostly, it will be '2', i.e. sector 3
          intPartitionTableEntryLBA := GUIDPartitionTableHeader.PartitionEntryLBA;

          PartitionEntryLBA      := IntToStr(GUIDPartitionTableHeader.PartitionEntryLBA);
          MaxPossiblePartitions  := IntToStr(GUIDPartitionTableHeader.MaxPossiblePartitions);
          SizeOfPartitionEntry   := IntToStr(GUIDPartitionTableHeader.SizeOfPartitionEntry);
          PartitionEntryCRC32    := IntToHex(GUIDPartitionTableHeader.PartitionEntryArrayCRC32, 2);

          result := (' GPT Partition Table Header Reads: '           + #13#10 +
                  ' Signature : '            + Signature             + #13#10 +
                  ' Rev No : '               + RevisionNo            + #13#10 +
                  ' Header Size : '          + HeaderSize            + #13#10 +
                  ' Header CRC32 : '         + HeaderCRC32           + #13#10 +
                  ' Primary LBA : '          + PrimaryLBA            + #13#10 +
                  ' Backup LBA : '           + BackupLBA             + #13#10 +
                  ' First Useable LBA : '    + FirstUseableLBA       + #13#10 +
                  ' Last Useable LBA : '     + LastUsableLBA         + #13#10 +
                  ' Disk GUID : '            + DiskGUID              + #13#10 +
                  ' Partition Entry LBA : '  + PartitionEntryLBA     + #13#10 +
                  ' Max No of Partitions : ' + MaxPossiblePartitions + #13#10 +
                  ' Partition Entry Size : ' + SizeOfPartitionEntry  + #13#10 +
                  ' Partition Entry CRC32 : '+ PartitionEntryCRC32   + #13#10 +
                  '=================================================================' + #13#10);
        end
      else
      begin
        result := 'false';
        RaiseLastOSError; // BytesRead = -1
      end;
    end
  else
  begin
    result := 'false';
    RaiseLastOSError; // DiskPos = -1
  end;
end;

// SECTOR 2 Traversal
// Lookup the data in the third part of the GPT - the partition table entry itself
function ReadGUIDPartitionTableEntry(Drive : THandle; ExactSectorSize : Integer) : ansistring;
var
  BytesRead, DiskPos : integer;
  GPTData            : TStringList;
  Buffer             : array [0..4095] of byte;
begin
    result           := 'false';
    BytesRead        := -1;
    DiskPos          := -1;

    // Move read point to offset 1024, i.e. offset zero of sector three (if 512 byte sector aligned)
    // Even if ExactSectorSize value <> 512, the alignment will still be 1024
    // bytes on from the start
    DiskPos := FileSeek(Drive, 1024, fsFromBeginning);

    // If seek was successfull:
    if DiskPos > -1 then
      begin
        FillChar(Buffer, SizeOf(Buffer), 0);
        // Move to the start of the partition table entry offset
        DiskPos   := FileSeek(Drive, intPartitionTableEntryLBA * ExactSectorSize, fsFromBeginning);
        // Read in a 4Kb block, as that will contain the values needed no matter what
        // ExactSectorSize is; either in the first 512 bytes, or next few sectors
        BytesRead := FileRead(Drive, Buffer, 4096);
        if BytesRead > -1 then
        begin
          try
            GPTData := TStringList.Create;
            GPTData.Add('GPT Partition Table Entries:');
            GPTData.AddStrings(TraverseEachPartitionTableEntry(Buffer, ExactSectorSize));
          finally
            result := GPTData.Text;
            GPTData.free;
          end;
        end // End of BytesRead
        else
          begin
            result := 'false';
            RaiseLastOSError; // BytesRead = -1
          end;
      end // End of seek if
    else
    begin
      result := 'false';
      RaiseLastOSError; // DiskPos = -1
    end;
  end;

// TraverseEachPartitionTableEntry : The main traverser. It takes the buffer
// read from the start of the disk, navigates to sector 3 (usually offset 1024
// but will be higher with disks using non-512 byte sector sizes) and then
// traverses 4Kb pulling out any GPT partition tables it finds. It returns
// a string list (of sorts) with all the entries
function TraverseEachPartitionTableEntry(Buffer : array of byte; ExactSectorSize : Longword) : ansistring;
{
    PartitionTypeGUID    : TGUID;                      // array [0..15] of byte;   a 16 byte hex string
    UniquePartitionGUID  : TGUID;                      // array [0..15] of byte;   a 16 byte hex string
    StartingLBA          : Int64;                      // 8 byte integer
    EndingLBA            : Int64;                      // 8 byte integer
    AttributeBits        : array [0..7] of byte;       // 8 byte hex string
    PartitionName        : array [0..71] of widechar;  // 36 byte Unicode string
    // This sums to 128 bytes - the size of a GPT partition table entry
    EndOfSector          : array [0..383] of byte; // THis will contains other tables!
}
var
  TablePortion : TMemoryStream;

  strStartingLBA, strEndingLBA, strAttributeBits, strPartitionLabel,
    strGUID1, strGUID2, strCreatorLabel, strPartSize: ansistring;

  intStartingLBA, intEndingLBA, PartSize : Int64;

  ReturnData : TStringList;

  Tmp, Tmp2 : TGUID; // temp GUID values, just do reverse lookup checks

  i, ZeroesCountedInGUID1 : integer;

begin
  i                    := 0;
  PartSize             := 0;
  intEndingLBA         := 0;
  intStartingLBA       := 0;
  ZeroesCountedInGUID1 := 0;

  TablePortion := TMemoryStream.Create;
  TablePortion.WriteBuffer(Buffer, SizeOf(Buffer));
  // Given that the disk position is set to the Partition Table Entry LBA value
  // in earlier functions, the passed memory stream should be 4Kb in size, starting from
  // the beginning of the GPT Partition Table list. So we'll start from the start of
  // the memory stream
  TablePortion.Position := 0;

  ReturnData := TStringList.Create;

  while TablePortion.Position < 4096 do
  repeat
    strGUID1         := '';
    strGUID2         := '';
    strAttributeBits := '';

    strGUID1 := GUIDToString(GetGUIDTypeID(TablePortion, TablePortion.Position)); // GUIDToString doesn't return false on failure so we cant check
    strCreatorLabel := CreatorLookup(StringToGUID(strGUID1));

    if TryStringToGUID(strGUID1, tmp) = true then  // TryStringToGUID does return false though if invalid
      begin
        // Study the GUID string to see if lots of the hex bytes are consecutive zeroes
        // If they are, probably a false GUID, because valid GUIDs don't have dozens of 0x00 pairs
        for i := 0 to Length(strGUID1) do
        begin
          if (strGUID1[i] = '0') and (strGUID1[i+1] = '0') then
            begin
              inc(ZeroesCountedInGUID1, 1);
            end;
        end;
        if ZeroesCountedInGUID1 > 8 then
          begin
            // The GUID is probably garbage if more than half of it are 0x00.
            // So move the pointer forward 16 bytes and re-scan
            TablePortion.Position := TablePortion.Position + 16;
          end
        else
          begin
          // First 16 bytes are a valid GUID so now lets check the second, the unique partition GUID
          strGUID2 := GUIDToString(GetUniquePartitionGUID(TablePortion, TablePortion.Position));

          if TryStringToGUID(strGUID2, tmp2) = true then
            begin
              // First 32 bytes are valid GUIDs so work out start and end LBA offsets
              intStartingLBA := TablePortion.ReadQWord;
              strStartingLBA := IntToStr(intStartingLBA);

              intEndingLBA :=  TablePortion.ReadQWord;
              strEndingLBA := IntToStr(intEndingLBA);

              PartSize := (intEndingLBA - intStartingLBA) * ExactSectorSize;
              strPartSize := FormatByteSize(PartSize) + '  (' + IntToStr(PartSize) + ' bytes)';

              strAttributeBits := GetPartitionAttributes(TablePortion, TablePortion.Position);

              strPartitionLabel := GetPartitionLabel(TablePortion, TablePortion.Position);

              // To here should be 128 bytes, so Position should be 128 bytes further than where
              // it started

              // Populate the string list with the results
              ReturnData.Add('Partition Type GUID : ' + strGUID1);
              ReturnData.Add('(Creator Label : ' + strCreatorLabel + ')');
              ReturnData.Add('Unique Partition GUID : ' + strGUID2);
              ReturnData.Add('Starting LBA : ' + strStartingLBA);
              ReturnData.Add('Ending LBA : ' + strEndingLBA);
              ReturnData.Add('Size: ' + strPartSize);
              ReturnData.Add('Attribute Flags : ' + strAttributeBits);
              ReturnData.Add('Partition Label : ' + strPartitionLabel);
              ReturnData.Add('===================================');
            end; // The first and second GUIDs were valid
        end; // The first GUID was valid but contained too many zeroes, thus invalid
      end // The first GUID was invalid
    else
    begin
      TablePortion.Position := TablePortion.Position + 16 // move forward 16 bytes and re-scan
    end;
  until TablePortion.Position = 4096; // 512 byte sector sizes are still the norm
                                      // so this should catch 1K, 2K and 4K sector sizes too
                                      // and the GPT structure typically occupies the first 34 sectors
  result := ReturnData.Text;

  ReturnData.Free;
  TablePortion.free;
end;

// Returns the partitioning style of a physical disk by utilising sector 0
// offset 446 for MBR or offset 38 of sector 1 for GPT. Returns resulting
// text string and Windows signature
function QueryGPT(SelectedDisk : widestring) : ansistring;
var
  Drive           : widestring;
  hDevice         : THandle;
  ExactSectorSize : LongWord;
  ProtectedMBRData, GUIDPartitionTableHeader, GUIDPartitionTableEntry  : ansistring;

begin
  ExactSectorSize := 0;
  result := '';
  Drive := SelectedDisk;

  // Assign handle to the disk, opening in Read Only mode
  // Internally, FileOpen calls CreateFileW, so we don't need to use the direct
  // Windows disk call; the function does it for us
  hDevice := FileOpen(PWideChar(Drive), fmOpenRead);
  if int(hDevice) = -1 then
    begin
      RaiseLastOSError;  // disk handle opening failed
      result := 'Disk could not be accessed.';
    end
  else
    begin
      // First, check disk sector size. Mostly it will be 512, but with GPT, could be higher
      ExactSectorSize               := GetSectorSizeInBytes(hDevice);
      if ExactSectorSize > 0 then
        begin
          ProtectedMBRData          := ReadProtectiveMBR(hDevice, ExactSectorSize);
          GUIDPartitionTableHeader  := ReadGUIDPartitionTableHeader(hDevice, ExactSectorSize);
          GUIDPartitionTableEntry   := ReadGUIDPartitionTableEntry(hDevice, ExactSectorSize);
          result := ProtectedMBRData + GUIDPartitionTableHeader + GUIDPartitionTableEntry;
        end
      else result := 'Sector size lookup failed';
      CloseHandle(hDevice);
    end;
end;

end.

