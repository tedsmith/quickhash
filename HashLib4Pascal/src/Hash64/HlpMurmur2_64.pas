unit HlpMurmur2_64;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock,
  HlpNullable;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type
  // MurmurHash64A (64-bit) algorithm by Austin Appleby.
  TMurmur2_64 = class sealed(TMultipleTransformNonBlock, IHash64, IHashWithKey,
    ITransformBlock)

  strict private
  var
    FKey, FWorkingKey: UInt64;

  const
    CKEY = UInt64($0);
{$IFDEF FPC}
    // to bypass Internal error (200706094) on FPC, We use "Typed Constant".

    M: UInt64 = UInt64($C6A4A7935BD1E995);

{$ELSE}
    M = UInt64($C6A4A7935BD1E995);
{$ENDIF FPC}
    R = Int32(47);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const AValue: THashLibByteArray); inline;

  strict protected
    function ComputeAggregatedBytes(const AData: THashLibByteArray)
      : IHashResult; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TMurmur2_64 }

function TMurmur2_64.Clone(): IHash;
var
  LHashInstance: TMurmur2_64;
begin
  LHashInstance := TMurmur2_64.Create();
  LHashInstance.FKey := FKey;
  LHashInstance.FWorkingKey := FWorkingKey;
  FBuffer.Position := 0;
  LHashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TMurmur2_64.ComputeAggregatedBytes(const AData: THashLibByteArray)
  : IHashResult;
var
  LLength, LCurrentIndex, LNBlocks, LIdx: Int32;
  LH, LK: UInt64;
  LPtrData: PByte;
  LPtrDataUInt64: PUInt64;
begin
  LLength := System.length(AData);
  LPtrData := PByte(AData);

  if (LLength = 0) then
  begin
    result := THashResult.Create(UInt64(0));
    Exit;
  end;

  LH := FWorkingKey xor (UInt64(LLength) * M);
  LCurrentIndex := 0;
  LIdx := 0;
  LPtrDataUInt64 := PUInt64(LPtrData);
  LNBlocks := LLength shr 3;

  while LIdx < LNBlocks do
  begin
    LK := TConverters.ReadPUInt64AsUInt64LE(LPtrDataUInt64 + LIdx);

    LK := LK * M;
    LK := LK xor (LK shr R);
    LK := LK * M;

    LH := LH xor LK;
    LH := LH * M;

    System.Inc(LIdx);
    System.Inc(LCurrentIndex, 8);
    System.Dec(LLength, 8);
  end;

  case LLength of
    7:
      begin
        LH := LH xor ((UInt64(AData[LCurrentIndex + 6]) shl 48));

        LH := LH xor (UInt64(AData[LCurrentIndex + 5]) shl 40);

        LH := LH xor (UInt64(AData[LCurrentIndex + 4]) shl 32);

        LH := LH xor (UInt64(AData[LCurrentIndex + 3]) shl 24);

        LH := LH xor (UInt64(AData[LCurrentIndex + 2]) shl 16);

        LH := LH xor (UInt64(AData[LCurrentIndex + 1]) shl 8);

        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

    6:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex + 5]) shl 40);

        LH := LH xor (UInt64(AData[LCurrentIndex + 4]) shl 32);

        LH := LH xor (UInt64(AData[LCurrentIndex + 3]) shl 24);

        LH := LH xor (UInt64(AData[LCurrentIndex + 2]) shl 16);

        LH := LH xor (UInt64(AData[LCurrentIndex + 1]) shl 8);

        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

    5:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex + 4]) shl 32);

        LH := LH xor (UInt64(AData[LCurrentIndex + 3]) shl 24);

        LH := LH xor (UInt64(AData[LCurrentIndex + 2]) shl 16);

        LH := LH xor (UInt64(AData[LCurrentIndex + 1]) shl 8);

        LH := LH xor UInt64(AData[LCurrentIndex]);
        LH := LH * M;
      end;

    4:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex + 3]) shl 24);

        LH := LH xor (UInt64(AData[LCurrentIndex + 2]) shl 16);

        LH := LH xor (UInt64(AData[LCurrentIndex + 1]) shl 8);

        LH := LH xor UInt64(AData[LCurrentIndex]);
        LH := LH * M;
      end;

    3:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex + 2]) shl 16);

        LH := LH xor (UInt64(AData[LCurrentIndex + 1]) shl 8);

        LH := LH xor UInt64(AData[LCurrentIndex]);
        LH := LH * M;
      end;

    2:
      begin
        LH := LH xor (UInt64(AData[LCurrentIndex + 1]) shl 8);

        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

    1:
      begin
        LH := LH xor UInt64(AData[LCurrentIndex]);

        LH := LH * M;
      end;

  end;

  LH := LH xor (LH shr R);
  LH := LH * M;
  LH := LH xor (LH shr R);

  result := THashResult.Create(LH);
end;

constructor TMurmur2_64.Create;
begin
  Inherited Create(8, 8);
  FKey := CKEY;
end;

function TMurmur2_64.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt64AsBytesLE(FKey);
end;

function TMurmur2_64.GetKeyLength: TNullableInteger;
begin
  result := 8;
end;

procedure TMurmur2_64.Initialize;
begin
  FWorkingKey := FKey;
  Inherited Initialize();
end;

procedure TMurmur2_64.SetKey(const AValue: THashLibByteArray);
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
    FKey := TConverters.ReadBytesAsUInt64LE(PByte(AValue), 0);
  end;
end;

end.
