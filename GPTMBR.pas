// Original Delphi code supplied by David Heffernan as answer to Stack Overflow URL
// http://stackoverflow.com/a/17132506
// Adjusted for use with sister project YAFFI and Freepascal and then converted
// to GPTMBR unit and added to QuickHash v2.7.0 Dec 2016 by T Smith

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

