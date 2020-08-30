unit HlpXXHash32;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpConverters,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpNullable,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TXXHash32 = class sealed(THash, IHash32, IHashWithKey, ITransformBlock)

  strict private
  var
    FKey, FHash: UInt32;

  const
    CKEY = UInt32(0);

    PRIME32_1 = UInt32(2654435761);
    PRIME32_2 = UInt32(2246822519);
    PRIME32_3 = UInt32(3266489917);
    PRIME32_4 = UInt32(668265263);
    PRIME32_5 = UInt32(374761393);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  type

    TXXH_State = record

    private
    var
      FTotalLength: UInt64;
      FMemorySize, FV1, FV2, FV3, FV4: UInt32;
      FMemory: THashLibByteArray;

      function Clone(): TXXH_State; inline;

    end;

  strict private
  var
    FState: TXXH_State;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TXXHash32.TXXH_State }

function TXXHash32.TXXH_State.Clone(): TXXH_State;
begin
  result := Default (TXXH_State);
  result.FTotalLength := FTotalLength;
  result.FMemorySize := FMemorySize;
  result.FV1 := FV1;
  result.FV2 := FV2;
  result.FV3 := FV3;
  result.FV4 := FV4;
  result.FMemory := System.Copy(FMemory);
end;

{ TXXHash32 }

function TXXHash32.Clone(): IHash;
var
  LHashInstance: TXXHash32;
begin
  LHashInstance := TXXHash32.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FHash := FHash;
  LHashInstance.FState := FState.Clone();
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TXXHash32.Create;
begin
  Inherited Create(4, 16);
  FKey := CKEY;
  System.SetLength(FState.FMemory, 16);
end;

function TXXHash32.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(FKey);
end;

function TXXHash32.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TXXHash32.Initialize;
begin
  FHash := 0;
  FState.FV1 := FKey + PRIME32_1 + PRIME32_2;
  FState.FV2 := FKey + PRIME32_2;
  FState.FV3 := FKey + 0;
  FState.FV4 := FKey - PRIME32_1;
  FState.FTotalLength := 0;
  FState.FMemorySize := 0;
end;

procedure TXXHash32.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := CKEY;
  end
  else
  begin
    if System.Length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;
    FKey := TConverters.ReadBytesAsUInt32LE(PByte(AValue), 0);
  end;
end;

procedure TXXHash32.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LV1, LV2, LV3, LV4: UInt32;
  LPtrLimit, LPtrEnd, LPtrADataStart, LPtrMemoryStart, LPtrMemory: PByte;
  LPtrADataStartCardinal: PCardinal;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LPtrADataStart := PByte(AData) + AIndex;
  LPtrMemory := PByte(FState.FMemory);
  FState.FTotalLength := FState.FTotalLength + UInt64(ALength);

  if ((FState.FMemorySize + UInt32(ALength)) < UInt32(16)) then
  begin

    LPtrMemoryStart := PByte(FState.FMemory) + FState.FMemorySize;

    System.Move(LPtrADataStart^, LPtrMemoryStart^, ALength);

    FState.FMemorySize := FState.FMemorySize + UInt32(ALength);

    Exit;
  end;

  LPtrEnd := LPtrADataStart + UInt32(ALength);

  if FState.FMemorySize > 0 then
  begin
    LPtrMemoryStart := PByte(FState.FMemory) + FState.FMemorySize;
    System.Move(LPtrADataStart^, LPtrMemoryStart^, 16 - FState.FMemorySize);

    FState.FV1 := PRIME32_1 * TBits.RotateLeft32(FState.FV1 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(LPtrMemory, 0), 13);
    FState.FV2 := PRIME32_1 * TBits.RotateLeft32(FState.FV2 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(LPtrMemory, 4), 13);
    FState.FV3 := PRIME32_1 * TBits.RotateLeft32(FState.FV3 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(LPtrMemory, 8), 13);
    FState.FV4 := PRIME32_1 * TBits.RotateLeft32(FState.FV4 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(LPtrMemory, 12), 13);

    LPtrADataStart := LPtrADataStart + (16 - FState.FMemorySize);
    FState.FMemorySize := 0;
  end;

  if LPtrADataStart <= (LPtrEnd - 16) then
  begin
    LV1 := FState.FV1;
    LV2 := FState.FV2;
    LV3 := FState.FV3;
    LV4 := FState.FV4;

    LPtrLimit := LPtrEnd - 16;
    repeat

      LPtrADataStartCardinal := PCardinal(LPtrADataStart);

      LV1 := PRIME32_1 * TBits.RotateLeft32
        (LV1 + PRIME32_2 * TConverters.ReadPCardinalAsUInt32LE
        (LPtrADataStartCardinal), 13);
      LV2 := PRIME32_1 * TBits.RotateLeft32
        (LV2 + PRIME32_2 * TConverters.ReadPCardinalAsUInt32LE
        (LPtrADataStartCardinal + 1), 13);
      LV3 := PRIME32_1 * TBits.RotateLeft32
        (LV3 + PRIME32_2 * TConverters.ReadPCardinalAsUInt32LE
        (LPtrADataStartCardinal + 2), 13);
      LV4 := PRIME32_1 * TBits.RotateLeft32
        (LV4 + PRIME32_2 * TConverters.ReadPCardinalAsUInt32LE
        (LPtrADataStartCardinal + 3), 13);

      System.Inc(LPtrADataStart, 16);

    until not(LPtrADataStart <= LPtrLimit);

    FState.FV1 := LV1;
    FState.FV2 := LV2;
    FState.FV3 := LV3;
    FState.FV4 := LV4;
  end;

  if LPtrADataStart < LPtrEnd then
  begin
    LPtrMemoryStart := PByte(FState.FMemory);
    System.Move(LPtrADataStart^, LPtrMemoryStart^, LPtrEnd - LPtrADataStart);
    FState.FMemorySize := LPtrEnd - LPtrADataStart;
  end;

end;

function TXXHash32.TransformFinal: IHashResult;
var
  LPtrEnd, LPtrBuffer: PByte;
begin

  if FState.FTotalLength >= UInt64(16) then
  begin
    FHash := TBits.RotateLeft32(FState.FV1, 1) + TBits.RotateLeft32(FState.FV2,
      7) + TBits.RotateLeft32(FState.FV3, 12) + TBits.RotateLeft32
      (FState.FV4, 18)
  end
  else
  begin
    FHash := FKey + PRIME32_5;
  end;
  System.Inc(FHash, FState.FTotalLength);

  LPtrBuffer := PByte(FState.FMemory);

  LPtrEnd := LPtrBuffer + FState.FMemorySize;
  while ((LPtrBuffer + 4) <= LPtrEnd) do
  begin
    FHash := FHash + TConverters.ReadBytesAsUInt32LE(LPtrBuffer, 0) * PRIME32_3;
    FHash := TBits.RotateLeft32(FHash, 17) * PRIME32_4;
    System.Inc(LPtrBuffer, 4);
  end;

  while LPtrBuffer < LPtrEnd do
  begin
    FHash := FHash + LPtrBuffer^ * PRIME32_5;
    FHash := TBits.RotateLeft32(FHash, 11) * PRIME32_1;
    System.Inc(LPtrBuffer);
  end;

  FHash := FHash xor (FHash shr 15);
  FHash := FHash * PRIME32_2;
  FHash := FHash xor (FHash shr 13);
  FHash := FHash * PRIME32_3;
  FHash := FHash xor (FHash shr 16);

  result := THashResult.Create(FHash);
  Initialize();
end;

end.
