// Original Delphi code supplied by David Heffernan as answer to Stack Overflow URL
// http://stackoverflow.com/a/17132506
// Adjusted for use with sister project YAFFI and Freepascal and then converted
// to GPTMBR unit and added to QuickHash v2.7.0 Dec 2016 by T Smith
{
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

unit GPTMBR;

{$mode objfpc}{$H+}

interface

uses
  Windows, SysUtils;

type
  TDriveLayoutInformationMbr = record
    Signature: DWORD;
  end;

  TDriveLayoutInformationGpt = record
    DiskId: TGuid;
    StartingUsableOffset: Int64;
    UsableLength: Int64;
    MaxPartitionCount: DWORD;
  end;

  TPartitionInformationMbr = record
    PartitionType: Byte;
    BootIndicator: Boolean;
    RecognizedPartition: Boolean;
    HiddenSectors: DWORD;
  end;

  TPartitionInformationGpt = record
    PartitionType: TGuid;
    PartitionId: TGuid;
    Attributes: Int64;
    Name: array [0..35] of WideChar;
  end;

  TPartitionInformationEx = record
    PartitionStyle: Integer;
    StartingOffset: Int64;
    PartitionLength: Int64;
    PartitionNumber: DWORD;
    RewritePartition: Boolean;
    case Integer of
      0: (Mbr: TPartitionInformationMbr);
      1: (Gpt: TPartitionInformationGpt);
  end;

  TDriveLayoutInformationEx = record
    PartitionStyle: DWORD;
    PartitionCount: DWORD;
    DriveLayoutInformation: record
      case Integer of
      0: (Mbr: TDriveLayoutInformationMbr);
      1: (Gpt: TDriveLayoutInformationGpt);
    end;
    PartitionEntry: array [0..15] of TPartitionInformationGpt;
    //hard-coded maximum of 16 partitions
  end;


function MBR_or_GPT(SelectedDisk : widestring) : string;

implementation

// Returns the partitioning style of a physical disk by utilising sector 0
// offset 440 for MBR or offset 38 of sector 1 for GPT. Returns resulting
// text string and Windows signature
function MBR_or_GPT(SelectedDisk : widestring) : string; 

const
  PARTITION_STYLE_MBR = 0;
  PARTITION_STYLE_GPT = 1;
  PARTITION_STYLE_RAW = 2;

  IOCTL_DISK_GET_DRIVE_LAYOUT_EX = $00070050;
var
  i: Integer;
  Drive: widestring;
  hDevice: THandle;
  DriveLayoutInfo: TDriveLayoutInformationEx;
  BytesReturned: DWORD;
begin
    result := '';
    Drive := SelectedDisk;
    // This particular handle assignment does not require admin rights as it allows
    // simply to query the device attributes without accessing actual disk data as such
    hDevice := CreateFileW(PWideChar(Drive),
                          0,
                          FILE_SHARE_READ or FILE_SHARE_WRITE,
                          nil,
                          OPEN_EXISTING,
                          0,
                          0);

    if hDevice <> INVALID_HANDLE_VALUE then
    begin
      if DeviceIoControl(hDevice, IOCTL_DISK_GET_DRIVE_LAYOUT_EX, nil, 0,
        @DriveLayoutInfo, SizeOf(DriveLayoutInfo), BytesReturned, nil) then
      begin
        if DriveLayoutInfo.PartitionStyle = 0 then result := 'MBR (sig: ' + IntToHex(SwapEndian(DriveLayoutInfo.DriveLayoutInformation.Mbr.Signature), 8) + ')';
        if DriveLayoutInfo.PartitionStyle = 1 then result := 'GPT (sig: ' + GUIDToString(DriveLayoutInfo.DriveLayoutInformation.Gpt.DiskId) + ')';
        if DriveLayoutInfo.PartitionStyle = 2 then result := 'RAW (no signature)';
      end;
    end;
    CloseHandle(hDevice);
  end;

end.

