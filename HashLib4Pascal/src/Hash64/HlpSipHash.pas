unit HlpSipHash;

{$I ..\Include\HashLib.inc}

interface

uses

{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
  HlpConverters,
  HlpIHashInfo,
  HlpNullable,
  HlpHash,
  HlpIHash,
  HlpHashResult,
  HlpIHashResult,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type
  TSipHash = class abstract(THash, IHash64, IHashWithKey, ITransformBlock)

  strict private

{$REGION 'Consts'}
  const
    V0 = UInt64($736F6D6570736575);
    V1 = UInt64($646F72616E646F6D);
    V2 = UInt64($6C7967656E657261);
    V3 = UInt64($7465646279746573);
    KEY0 = UInt64($0706050403020100);
    KEY1 = UInt64($0F0E0D0C0B0A0908);

{$ENDREGION}
    procedure Compress(); inline;
    procedure CompressTimes(ATimes: Int32); inline;
    procedure ProcessBlock(ABlock: UInt64); inline;
    procedure ByteUpdate(AByte: Byte); inline;
    function ProcessFinalBlock(): UInt64;
    procedure Finish();

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);

  strict protected
  var
    FV0, FV1, FV2, FV3, FKey0, FKey1, FTotalLength, FPartA, FPartB: UInt64;
    FCompressionRounds, FFinalizationRounds, FIdx: Int32;
    FBuffer: THashLibByteArray;

    function GetMagicXor(): Byte; virtual;

  public
    constructor Create(AHashSize, ABlockSize: Int32);
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal: IHashResult; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

type
  /// <summary>
  /// SipHash 2 - 4 algorithm.
  /// <summary>
  TSipHash2_4 = class sealed(TSipHash)

  public

    constructor Create(ACompressionRounds: Int32 = 2;
      AFinalizationRounds: Int32 = 4);
    function Clone(): IHash; override;

  end;

implementation

{ TSipHash2_4 }

function TSipHash.GetMagicXor: Byte;
begin
  Result := $FF;
end;

function TSipHash2_4.Clone(): IHash;
var
  LHashInstance: TSipHash2_4;
begin
  LHashInstance := TSipHash2_4.Create();
  LHashInstance.FV0 := FV0;
  LHashInstance.FV1 := FV1;
  LHashInstance.FV2 := FV2;
  LHashInstance.FV3 := FV3;
  LHashInstance.FKey0 := FKey0;
  LHashInstance.FKey1 := FKey1;
  LHashInstance.FPartA := FPartA;
  LHashInstance.FTotalLength := FTotalLength;
  LHashInstance.FCompressionRounds := FCompressionRounds;
  LHashInstance.FFinalizationRounds := FFinalizationRounds;
  LHashInstance.FIdx := FIdx;
  LHashInstance.FBuffer := System.Copy(FBuffer);
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TSipHash2_4.Create(ACompressionRounds, AFinalizationRounds: Int32);
begin
  Inherited Create(8, 8);
  FCompressionRounds := ACompressionRounds;
  FFinalizationRounds := AFinalizationRounds;
end;

{ TSipHash }

procedure TSipHash.Compress;
var
  LV0, LV1, LV2, LV3: UInt64;
begin
  LV0 := FV0;
  LV1 := FV1;
  LV2 := FV2;
  LV3 := FV3;

  LV0 := LV0 + LV1;
  LV2 := LV2 + LV3;
  LV1 := TBits.RotateLeft64(LV1, 13);
  LV3 := TBits.RotateLeft64(LV3, 16);
  LV1 := LV1 xor LV0;
  LV3 := LV3 xor LV2;
  LV0 := TBits.RotateLeft64(LV0, 32);
  LV2 := LV2 + LV1;
  LV0 := LV0 + LV3;
  LV1 := TBits.RotateLeft64(LV1, 17);
  LV3 := TBits.RotateLeft64(LV3, 21);
  LV1 := LV1 xor LV2;
  LV3 := LV3 xor LV0;
  LV2 := TBits.RotateLeft64(LV2, 32);

  FV0 := LV0;
  FV1 := LV1;
  FV2 := LV2;
  FV3 := LV3;
end;

procedure TSipHash.CompressTimes(ATimes: Int32);
var
  LIdx: Int32;
begin
  LIdx := 0;
  while LIdx < ATimes do
  begin
    Compress();
    System.Inc(LIdx);
  end;
end;

procedure TSipHash.ProcessBlock(ABlock: UInt64);
begin
  FV3 := FV3 xor ABlock;
  CompressTimes(FCompressionRounds);
  FV0 := FV0 xor ABlock;
end;

function TSipHash.ProcessFinalBlock: UInt64;
begin
  Result := UInt64(FTotalLength and $FF) shl 56;

  if (FIdx <> 0) then
  begin
    case (FIdx) of

      7:
        begin
          Result := Result or (UInt64(FBuffer[6]) shl 48);
          Result := Result or (UInt64(FBuffer[5]) shl 40);
          Result := Result or (UInt64(FBuffer[4]) shl 32);
          Result := Result or (UInt64(FBuffer[3]) shl 24);
          Result := Result or (UInt64(FBuffer[2]) shl 16);
          Result := Result or (UInt64(FBuffer[1]) shl 8);
          Result := Result or (UInt64(FBuffer[0]));
        end;
      6:
        begin
          Result := Result or (UInt64(FBuffer[5]) shl 40);
          Result := Result or (UInt64(FBuffer[4]) shl 32);
          Result := Result or (UInt64(FBuffer[3]) shl 24);
          Result := Result or (UInt64(FBuffer[2]) shl 16);
          Result := Result or (UInt64(FBuffer[1]) shl 8);
          Result := Result or (UInt64(FBuffer[0]));
        end;
      5:
        begin
          Result := Result or (UInt64(FBuffer[4]) shl 32);
          Result := Result or (UInt64(FBuffer[3]) shl 24);
          Result := Result or (UInt64(FBuffer[2]) shl 16);
          Result := Result or (UInt64(FBuffer[1]) shl 8);
          Result := Result or (UInt64(FBuffer[0]));
        end;

      4:
        begin
          Result := Result or (UInt64(FBuffer[3]) shl 24);
          Result := Result or (UInt64(FBuffer[2]) shl 16);
          Result := Result or (UInt64(FBuffer[1]) shl 8);
          Result := Result or (UInt64(FBuffer[0]));
        end;

      3:
        begin
          Result := Result or (UInt64(FBuffer[2]) shl 16);
          Result := Result or (UInt64(FBuffer[1]) shl 8);
          Result := Result or (UInt64(FBuffer[0]));
        end;

      2:
        begin
          Result := Result or (UInt64(FBuffer[1]) shl 8);
          Result := Result or (UInt64(FBuffer[0]));
        end;

      1:
        begin
          Result := Result or (UInt64(FBuffer[0]));
        end;
    end;
  end;
end;

procedure TSipHash.ByteUpdate(AByte: Byte);
var
  LPtrBuffer: PByte;
  LBlock: UInt64;
begin
  FBuffer[FIdx] := AByte;
  System.Inc(FIdx);
  if FIdx >= 8 then
  begin
    LPtrBuffer := PByte(FBuffer);
    LBlock := TConverters.ReadBytesAsUInt64LE(LPtrBuffer, 0);
    ProcessBlock(LBlock);
    FIdx := 0;
  end;
end;

constructor TSipHash.Create(AHashSize, ABlockSize: Int32);
begin
  Inherited Create(AHashSize, ABlockSize);
  FKey0 := KEY0;
  FKey1 := KEY1;
  System.SetLength(FBuffer, 8);
end;

function TSipHash.GetKey: THashLibByteArray;
var
  LKey: THashLibByteArray;
begin
  System.SetLength(LKey, KeyLength.value);

  TConverters.ReadUInt64AsBytesLE(FKey0, LKey, 0);
  TConverters.ReadUInt64AsBytesLE(FKey1, LKey, 8);

  Result := LKey;
end;

function TSipHash.GetKeyLength: TNullableInteger;
begin
  Result := 16;
end;

procedure TSipHash.Initialize;
begin
  FV0 := V0;
  FV1 := V1;
  FV2 := V2;
  FV3 := V3;
  FTotalLength := 0;
  FIdx := 0;

  FV3 := FV3 xor FKey1;
  FV2 := FV2 xor FKey0;
  FV1 := FV1 xor FKey1;
  FV0 := FV0 xor FKey0;

  FPartA := 0;

  case HashSize of
    16:
      begin
        FPartB := 0;
        FV1 := FV1 xor $EE;
      end;
  end;
end;

procedure TSipHash.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey0 := KEY0;
    FKey1 := KEY1;
  end
  else
  begin
    if System.Length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;

    FKey0 := TConverters.ReadBytesAsUInt64LE(PByte(AValue), 0);
    FKey1 := TConverters.ReadBytesAsUInt64LE(PByte(AValue), 8);
  end;
end;

procedure TSipHash.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LIdx, LLength, LBlockCount, LOffset: Int32;
  LPtrData, LPtrBuffer: PByte;
  LBlock: UInt64;
  LPtrDataUInt64: PUInt64;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LLength := ALength;
  LIdx := AIndex;

  LPtrData := PByte(AData);
  System.Inc(FTotalLength, LLength);

  // consume last pending bytes

  if ((FIdx <> 0) and (ALength <> 0)) then
  begin
{$IFDEF DEBUG}
    System.Assert(AIndex = 0); // nothing would work anyways if AIndex is !=0
{$ENDIF DEBUG}
    while ((FIdx < 8) and (LLength <> 0)) do
    begin
      FBuffer[FIdx] := (LPtrData + AIndex)^;
      System.Inc(FIdx);
      System.Inc(AIndex);
      System.Dec(LLength);
    end;
    if (FIdx = 8) then
    begin
      LPtrBuffer := PByte(FBuffer);
      LBlock := TConverters.ReadBytesAsUInt64LE(LPtrBuffer, 0);
      ProcessBlock(LBlock);
      FIdx := 0;
    end;
  end
  else
  begin
    LIdx := 0;
  end;

  LBlockCount := LLength shr 3;

  // body
  LPtrDataUInt64 := PUInt64(LPtrData + AIndex);
  while LIdx < LBlockCount do
  begin
    LBlock := TConverters.ReadPUInt64AsUInt64LE(LPtrDataUInt64 + LIdx);
    ProcessBlock(LBlock);
    System.Inc(LIdx);
  end;

  // save pending end bytes
  LOffset := AIndex + (LIdx * 8);

  while LOffset < (LLength + AIndex) do
  begin
    ByteUpdate(AData[LOffset]);
    System.Inc(LOffset);
  end;
end;

procedure TSipHash.Finish;
var
  LFinalBlock: UInt64;
begin
  LFinalBlock := ProcessFinalBlock();

  FV3 := FV3 xor LFinalBlock;
  CompressTimes(FCompressionRounds);
  FV0 := FV0 xor LFinalBlock;

  FV2 := FV2 xor GetMagicXor();
  CompressTimes(FFinalizationRounds);
  FPartA := FV0 xor FV1 xor FV2 xor FV3;

  case HashSize of
    16:
      begin
        FV1 := FV1 xor $DD;
        CompressTimes(FFinalizationRounds);
        FPartB := FV0 xor FV1 xor FV2 xor FV3;
      end;
  end;
end;

function TSipHash.TransformFinal: IHashResult;
var
  LBufferBytes: THashLibByteArray;
begin
  Finish();

  System.SetLength(LBufferBytes, HashSize);
  TConverters.ReadUInt64AsBytesLE(FPartA, LBufferBytes, 0);

  case HashSize of
    16:
      begin
        TConverters.ReadUInt64AsBytesLE(FPartB, LBufferBytes, 8);
      end;
  end;

  Result := THashResult.Create(LBufferBytes);
  Initialize();
end;

end.
