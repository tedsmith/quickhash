unit HlpMurmurHash3_x64_128;

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
  TMurmurHash3_x64_128 = class sealed(THash, IHash128, IHashWithKey,
    ITransformBlock)

  strict private
  var
    FH1, FH2, FTotalLength: UInt64;
    FKey: UInt32;
    FIdx: Int32;
    FBuffer: THashLibByteArray;

    procedure ByteUpdate(AByte: Byte); inline;
    procedure Finish();
    procedure ProcessPendings();

{$REGION 'Consts'}

  const
    CKEY = UInt32($0);

{$IFDEF FPC}
    // to bypass Internal error (200706094) on FPC, We use "Typed Constant".

    C1: UInt64 = UInt64($87C37B91114253D5);
    C5: UInt64 = UInt64($FF51AFD7ED558CCD);
    C6: UInt64 = UInt64($C4CEB9FE1A85EC53);

{$ELSE}
    C1 = UInt64($87C37B91114253D5);
    C5 = UInt64($FF51AFD7ED558CCD);
    C6 = UInt64($C4CEB9FE1A85EC53);
{$ENDIF FPC}
    C2 = UInt64($4CF5AD432745937F);
    C3 = UInt32($52DCE729);
    C4 = UInt32($38495AB5);
{$ENDREGION}
    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;
  end;

implementation

{ TMurmurHash3_x64_128 }

procedure TMurmurHash3_x64_128.ProcessPendings;
var
  LK1, LK2: UInt64;
  LPtrBuffer: PByte;
begin
  if FIdx >= 16 then
  begin
    LPtrBuffer := PByte(FBuffer);
    LK1 := TConverters.ReadBytesAsUInt64LE(LPtrBuffer, 0);
    LK2 := TConverters.ReadBytesAsUInt64LE(LPtrBuffer, 8);

    LK1 := LK1 * C1;
    LK1 := TBits.RotateLeft64(LK1, 31);
    LK1 := LK1 * C2;
    FH1 := FH1 xor LK1;

    FH1 := TBits.RotateLeft64(FH1, 27);
    FH1 := FH1 + FH2;
    FH1 := FH1 * 5 + C3;

    LK2 := LK2 * C2;
    LK2 := TBits.RotateLeft64(LK2, 33);
    LK2 := LK2 * C1;
    FH2 := FH2 xor LK2;

    FH2 := TBits.RotateLeft64(FH2, 31);
    FH2 := FH2 + FH1;
    FH2 := FH2 * 5 + C4;

    FIdx := 0;
  end;
end;

procedure TMurmurHash3_x64_128.ByteUpdate(AByte: Byte);
begin
  FBuffer[FIdx] := AByte;
  System.Inc(FIdx);
  ProcessPendings();
end;

function TMurmurHash3_x64_128.Clone(): IHash;
var
  LHashInstance: TMurmurHash3_x64_128;
begin
  LHashInstance := TMurmurHash3_x64_128.Create();
  LHashInstance.FH1 := FH1;
  LHashInstance.FH2 := FH2;
  LHashInstance.FTotalLength := FTotalLength;
  LHashInstance.FKey := FKey;
  LHashInstance.FIdx := FIdx;
  LHashInstance.FBuffer := System.Copy(FBuffer);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMurmurHash3_x64_128.Create;
begin
  Inherited Create(16, 16);
  FKey := CKEY;
  System.SetLength(FBuffer, 16);
end;

procedure TMurmurHash3_x64_128.Finish;
var
  LLength: Int32;
  LK1, LK2: UInt64;
begin

  // tail

  LK1 := 0;
  LK2 := 0;

  LLength := FIdx;

  if (LLength <> 0) then
  begin

    case LLength of
      15:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[14]) shl 48);
          LK2 := LK2 xor (UInt64(FBuffer[13]) shl 40);
          LK2 := LK2 xor (UInt64(FBuffer[12]) shl 32);
          LK2 := LK2 xor (UInt64(FBuffer[11]) shl 24);
          LK2 := LK2 xor (UInt64(FBuffer[10]) shl 16);
          LK2 := LK2 xor (UInt64(FBuffer[9]) shl 8);
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

      14:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[13]) shl 40);
          LK2 := LK2 xor (UInt64(FBuffer[12]) shl 32);
          LK2 := LK2 xor (UInt64(FBuffer[11]) shl 24);
          LK2 := LK2 xor (UInt64(FBuffer[10]) shl 16);
          LK2 := LK2 xor (UInt64(FBuffer[9]) shl 8);
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

      13:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[12]) shl 32);
          LK2 := LK2 xor (UInt64(FBuffer[11]) shl 24);
          LK2 := LK2 xor (UInt64(FBuffer[10]) shl 16);
          LK2 := LK2 xor (UInt64(FBuffer[9]) shl 8);
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

      12:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[11]) shl 24);
          LK2 := LK2 xor (UInt64(FBuffer[10]) shl 16);
          LK2 := LK2 xor (UInt64(FBuffer[9]) shl 8);
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

      11:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[10]) shl 16);
          LK2 := LK2 xor (UInt64(FBuffer[9]) shl 8);
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

      10:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[9]) shl 8);
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

      9:
        begin
          LK2 := LK2 xor (UInt64(FBuffer[8]) shl 0);
          LK2 := LK2 * C2;
          LK2 := TBits.RotateLeft64(LK2, 33);
          LK2 := LK2 * C1;
          FH2 := FH2 xor LK2;
        end;

    end;

    if (LLength > 8) then
    begin
      LLength := 8;
    end;

    case LLength of
      8:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[7]) shl 56);
          LK1 := LK1 xor (UInt64(FBuffer[6]) shl 48);
          LK1 := LK1 xor (UInt64(FBuffer[5]) shl 40);
          LK1 := LK1 xor (UInt64(FBuffer[4]) shl 32);
          LK1 := LK1 xor (UInt64(FBuffer[3]) shl 24);
          LK1 := LK1 xor (UInt64(FBuffer[2]) shl 16);
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      7:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[6]) shl 48);
          LK1 := LK1 xor (UInt64(FBuffer[5]) shl 40);
          LK1 := LK1 xor (UInt64(FBuffer[4]) shl 32);
          LK1 := LK1 xor (UInt64(FBuffer[3]) shl 24);
          LK1 := LK1 xor (UInt64(FBuffer[2]) shl 16);
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      6:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[5]) shl 40);
          LK1 := LK1 xor (UInt64(FBuffer[4]) shl 32);
          LK1 := LK1 xor (UInt64(FBuffer[3]) shl 24);
          LK1 := LK1 xor (UInt64(FBuffer[2]) shl 16);
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      5:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[4]) shl 32);
          LK1 := LK1 xor (UInt64(FBuffer[3]) shl 24);
          LK1 := LK1 xor (UInt64(FBuffer[2]) shl 16);
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      4:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[3]) shl 24);
          LK1 := LK1 xor (UInt64(FBuffer[2]) shl 16);
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      3:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[2]) shl 16);
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      2:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[1]) shl 8);
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

      1:
        begin
          LK1 := LK1 xor (UInt64(FBuffer[0]) shl 0);
          LK1 := LK1 * C1;
          LK1 := TBits.RotateLeft64(LK1, 31);
          LK1 := LK1 * C2;
          FH1 := FH1 xor LK1;
        end;

    end;

  end;

  FH1 := FH1 xor FTotalLength;
  FH2 := FH2 xor FTotalLength;

  FH1 := FH1 + FH2;
  FH2 := FH2 + FH1;

  FH1 := FH1 xor (FH1 shr 33);
  FH1 := FH1 * C5;
  FH1 := FH1 xor (FH1 shr 33);
  FH1 := FH1 * C6;
  FH1 := FH1 xor (FH1 shr 33);

  FH2 := FH2 xor (FH2 shr 33);
  FH2 := FH2 * C5;
  FH2 := FH2 xor (FH2 shr 33);
  FH2 := FH2 * C6;
  FH2 := FH2 xor (FH2 shr 33);

  FH1 := FH1 + FH2;
  FH2 := FH2 + FH1;
end;

function TMurmurHash3_x64_128.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(FKey);
end;

function TMurmurHash3_x64_128.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmurHash3_x64_128.Initialize;
begin
  FH1 := FKey;
  FH2 := FKey;

  FTotalLength := 0;
  FIdx := 0;
end;

procedure TMurmurHash3_x64_128.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := CKEY;
  end
  else
  begin
    if System.length(AValue) <> KeyLength.value then
    begin
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    end;
    FKey := TConverters.ReadBytesAsUInt32LE(PByte(AValue), 0);
  end;
end;

procedure TMurmurHash3_x64_128.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LLength, LNBlocks, LIndex, LOffset, LIdx: Int32;
  LK1, LK2, LH1, LH2: UInt64;
  LPtrData: PByte;
  LPtrDataUInt64: PUInt64;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.length(AData));
{$ENDIF DEBUG}
  LLength := ALength;
  LIndex := AIndex;
  LIdx := 0;
  System.Inc(FTotalLength, LLength);
  LPtrData := PByte(AData);

  // consume last pending bytes

  if ((FIdx <> 0) and (ALength <> 0)) then
  begin
{$IFDEF DEBUG}
    System.Assert(AIndex = 0); // nothing would work anyways if AIndex <> 0
{$ENDIF DEBUG}
    while ((FIdx < 16) and (LLength <> 0)) do
    begin
      FBuffer[FIdx] := (LPtrData + AIndex)^;
      System.Inc(FIdx);
      System.Inc(AIndex);
      System.Dec(LLength);
    end;
    if (FIdx = 16) then
    begin
      ProcessPendings;
    end;
  end
  else
  begin
    LIndex := 0;
  end;

  LNBlocks := LLength shr 4;

  // body

  LH1 := FH1;
  LH2 := FH2;
  LPtrDataUInt64 := PUInt64(LPtrData + AIndex);
  while LIndex < LNBlocks do
  begin

    LK1 := TConverters.ReadPUInt64AsUInt64LE(LPtrDataUInt64 + LIdx);
    System.Inc(LIdx);
    LK2 := TConverters.ReadPUInt64AsUInt64LE(LPtrDataUInt64 + LIdx);
    System.Inc(LIdx);

    LK1 := LK1 * C1;
    LK1 := TBits.RotateLeft64(LK1, 31);
    LK1 := LK1 * C2;
    LH1 := LH1 xor LK1;

    LH1 := TBits.RotateLeft64(LH1, 27);
    LH1 := LH1 + LH2;
    LH1 := LH1 * 5 + C3;

    LK2 := LK2 * C2;
    LK2 := TBits.RotateLeft64(LK2, 33);
    LK2 := LK2 * C1;
    LH2 := LH2 xor LK2;

    LH2 := TBits.RotateLeft64(LH2, 31);
    LH2 := LH2 + LH1;
    LH2 := LH2 * 5 + C4;

    System.Inc(LIndex);
  end;

  FH1 := LH1;
  FH2 := LH2;

  LOffset := AIndex + (LIndex * 16);

  while (LOffset < (AIndex + LLength)) do
  begin
    ByteUpdate(AData[LOffset]);
    System.Inc(LOffset);
  end;
end;

function TMurmurHash3_x64_128.TransformFinal: IHashResult;
var
  LBufferBytes: THashLibByteArray;
begin
  Finish();

  System.SetLength(LBufferBytes, HashSize);
  TConverters.ReadUInt64AsBytesBE(FH1, LBufferBytes, 0);
  TConverters.ReadUInt64AsBytesBE(FH2, LBufferBytes, 8);

  result := THashResult.Create(LBufferBytes);
  Initialize();
end;

end.
