unit HlpHash;

{$I ..\Include\HashLib.inc}

interface

uses
  Classes,
  SysUtils,
  HlpHashLibTypes,
  HlpConverters,
  HlpIHash,
  HlpIHashResult;

resourcestring
  SIndexOutOfRange = 'Current Index Is Out Of Range';
  SInvalidBufferSize = '"BufferSize" Must Be Greater Than Zero';
  SUnAssignedStream = 'Input Stream Is Unassigned';
  SFileNotExist = 'Specified File Not Found';
  SCloneNotYetImplemented = 'Clone Not Yet Implemented For "%s"';

type
  THash = class abstract(TInterfacedObject, IHash)

  strict private
  var
    FBufferSize, FBlockSize, FHashSize: Int32;

  const
    BUFFER_SIZE = Int32(64 * 1024); // 64Kb

  strict protected

    function GetBlockSize: Int32; virtual;
    procedure SetBlockSize(AValue: Int32); virtual;

    function GetHashSize: Int32; virtual;
    procedure SetHashSize(AValue: Int32); virtual;

    function GetBufferSize: Int32; inline;
    procedure SetBufferSize(AValue: Int32); inline;

    function GetName: String; virtual;

  public

    constructor Create(AHashSize, ABlockSize: Int32);

    procedure Initialize(); virtual; abstract;

    procedure TransformString(const AData: String; const AEncoding: TEncoding);
    procedure TransformBytes(const AData: THashLibByteArray); overload;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex: Int32); overload;
    procedure TransformBytes(const AData: THashLibByteArray; AIndex: Int32;
      ALength: Int32); overload; virtual; abstract;
    procedure TransformUntyped(const AData; ALength: Int64);
    procedure TransformStream(const AStream: TStream; ALength: Int64 = -1);
    procedure TransformFile(const AFileName: String; AFrom: Int64 = 0;
      ALength: Int64 = -1);

    function TransformFinal(): IHashResult; virtual; abstract;

    function ComputeString(const AData: String; AEncoding: TEncoding)
      : IHashResult; virtual;
    function ComputeBytes(const AData: THashLibByteArray): IHashResult; virtual;
    function ComputeUntyped(const AData; ALength: Int64): IHashResult;
    function ComputeStream(const AStream: TStream; ALength: Int64 = -1)
      : IHashResult;
    function ComputeFile(const AFileName: String; AFrom: Int64 = 0;
      ALength: Int64 = -1): IHashResult;

    function Clone(): IHash; virtual;

    property Name: String read GetName;
    property BlockSize: Int32 read GetBlockSize write SetBlockSize;
    property HashSize: Int32 read GetHashSize write SetHashSize;
    property BufferSize: Int32 read GetBufferSize write SetBufferSize;
  end;

implementation

{ THash }

constructor THash.Create(AHashSize, ABlockSize: Int32);
begin
  Inherited Create();
{$IFDEF DEBUG}
  System.Assert((ABlockSize > 0) or (ABlockSize = -1));
  System.Assert((AHashSize > 0) or (AHashSize = -1));
{$ENDIF DEBUG}
  FBlockSize := ABlockSize;
  FHashSize := AHashSize;
  FBufferSize := BUFFER_SIZE;
end;

function THash.GetName: String;
begin
  result := Self.ClassName;
end;

function THash.GetBufferSize: Int32;
begin
  result := FBufferSize;
end;

procedure THash.SetBufferSize(AValue: Int32);
begin
  if AValue > 0 then
  begin
    FBufferSize := AValue;
  end
  else
  begin
    raise EArgumentHashLibException.CreateRes(@SInvalidBufferSize);
  end;
end;

function THash.GetBlockSize: Int32;
begin
  result := FBlockSize;
end;

procedure THash.SetBlockSize(AValue: Int32);
begin
  FBlockSize := AValue;
end;

function THash.GetHashSize: Int32;
begin
  result := FHashSize;
end;

procedure THash.SetHashSize(AValue: Int32);
begin
  FHashSize := AValue;
end;

function THash.ComputeString(const AData: String; AEncoding: TEncoding)
  : IHashResult;
begin
  result := ComputeBytes(TConverters.ConvertStringToBytes(AData, AEncoding));
end;

function THash.ComputeUntyped(const AData; ALength: Int64): IHashResult;
begin
  Initialize();
  TransformUntyped(AData, ALength);
  result := TransformFinal();
end;

procedure THash.TransformUntyped(const AData; ALength: Int64);
var
  LPtrStart, LPtrEnd: PByte;
  LBuffer: THashLibByteArray;
  LBufferSize: Int32;
begin
  LPtrStart := @AData;

  if BufferSize > ALength then // Sanity Check
  begin
    LBufferSize := BUFFER_SIZE;
  end
  else
  begin
    LBufferSize := BufferSize;
  end;

  if LPtrStart <> Nil then
  begin
    System.SetLength(LBuffer, LBufferSize);
    LPtrEnd := LPtrStart + ALength;

    while LPtrStart < LPtrEnd do
    begin

      if (LPtrEnd - LPtrStart) >= LBufferSize then
      begin
        System.Move(LPtrStart^, LBuffer[0], LBufferSize);
        TransformBytes(LBuffer);
        System.Inc(LPtrStart, LBufferSize);
      end
      else
      begin
        System.SetLength(LBuffer, LPtrEnd - LPtrStart);
        System.Move(LPtrStart^, LBuffer[0], System.Length(LBuffer));
        TransformBytes(LBuffer);
        break;
      end;

    end;

  end;
end;

function THash.ComputeStream(const AStream: TStream; ALength: Int64)
  : IHashResult;
begin
  Initialize();
  TransformStream(AStream, ALength);
  result := TransformFinal();
end;

function THash.ComputeFile(const AFileName: String; AFrom, ALength: Int64)
  : IHashResult;
begin
  Initialize();
  TransformFile(AFileName, AFrom, ALength);
  result := TransformFinal();
end;

function THash.Clone(): IHash;
begin
  raise ENotImplementedHashLibException.CreateResFmt
    (@SCloneNotYetImplemented, [Name]);
end;

function THash.ComputeBytes(const AData: THashLibByteArray): IHashResult;
begin
  Initialize();
  TransformBytes(AData);
  result := TransformFinal();
end;

procedure THash.TransformString(const AData: String;
  const AEncoding: TEncoding);
begin
  TransformBytes(TConverters.ConvertStringToBytes(AData, AEncoding));
end;

procedure THash.TransformBytes(const AData: THashLibByteArray);
begin
  TransformBytes(AData, 0, System.Length(AData));
end;

procedure THash.TransformBytes(const AData: THashLibByteArray; AIndex: Int32);
var
  LLength: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
{$ENDIF DEBUG}
  LLength := System.Length(AData) - AIndex;

{$IFDEF DEBUG}
  System.Assert(LLength >= 0);
{$ENDIF DEBUG}
  TransformBytes(AData, AIndex, LLength);
end;

procedure THash.TransformStream(const AStream: TStream; ALength: Int64);
var
  LData: THashLibByteArray;
  LRead, LBufferSize: Int32;
  LTotal: Int64;
begin
{$IFDEF DEBUG}
  System.Assert((ALength = -1) or (ALength > 0));
{$ENDIF DEBUG}
  LTotal := 0;
  if (AStream <> Nil) then
  begin
    if (ALength > -1) then
    begin
      if ((AStream.Position + ALength) > AStream.Size) then
      begin
        raise EIndexOutOfRangeHashLibException.CreateRes(@SIndexOutOfRange);
      end;
    end;

    if (AStream.Position >= AStream.Size) then
    begin
      Exit;
    end;
  end
  else
  begin
    raise EArgumentNilHashLibException.CreateRes(@SUnAssignedStream);
  end;

  if BufferSize > AStream.Size then // Sanity Check
  begin
    LBufferSize := BUFFER_SIZE;
  end
  else
  begin
    LBufferSize := BufferSize;
  end;

  System.SetLength(LData, LBufferSize);

  if (ALength = -1) then
  begin

    while true do
    begin

      LRead := AStream.Read(LData[0], LBufferSize);

      if (LRead <> LBufferSize) then
      begin
        TransformBytes(LData, 0, LRead);
        break;
      end
      else
      begin
        TransformBytes(LData, 0, LRead);
      end;
    end;

  end
  else
  begin
    while true do
    begin

      LRead := AStream.Read(LData[0], LBufferSize);

      if ((LTotal + Int64(LRead)) >= ALength) then
      begin
        TransformBytes(LData, 0, Int32(ALength - LTotal));
        break;
      end
      else
      begin
        TransformBytes(LData, 0, LRead);
        LTotal := LTotal + LRead;
      end;
    end;

  end;

end;

procedure THash.TransformFile(const AFileName: String; AFrom, ALength: Int64);
var
  LFileStream: TFileStream;
begin
{$IFDEF DEBUG}
  System.Assert(AFrom >= 0);
  System.Assert((ALength = -1) or (ALength > 0));
{$ENDIF DEBUG}
  if not FileExists(AFileName) then
  begin
    raise EArgumentHashLibException.CreateRes(@SFileNotExist);
  end;

  LFileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);

  try
    LFileStream.Seek(AFrom, TSeekOrigin.soBeginning);
    TransformStream(LFileStream, ALength);
  finally
    LFileStream.Free;
  end;
end;

end.
