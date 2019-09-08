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

  TMurmur2_64 = class sealed(TMultipleTransformNonBlock, IHash64, IHashWithKey,
    ITransformBlock)

  strict private

    Fm_key, Fm_working_key: UInt32;

  const
    CKEY = UInt32($0);
{$IFDEF FPC}
    // to bypass Internal error (200706094) on FPC, We use "Typed Constant".

    M: UInt64 = UInt64($C6A4A7935BD1E995);

{$ELSE}
    M = UInt64($C6A4A7935BD1E995);
{$ENDIF FPC}
    R = Int32(47);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const value: THashLibByteArray); inline;

  strict protected
    function ComputeAggregatedBytes(const a_data: THashLibByteArray)
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
  HashInstance: TMurmur2_64;
begin
  HashInstance := TMurmur2_64.Create();
  HashInstance.Fm_key := Fm_key;
  HashInstance.Fm_working_key := Fm_working_key;
  FBuffer.Position := 0;
  HashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TMurmur2_64.ComputeAggregatedBytes(const a_data: THashLibByteArray)
  : IHashResult;
var
  &length, current_index: Int32;
  h, k: UInt64;
  ptr_a_data: PByte;
begin

  length := System.length(a_data);
  ptr_a_data := PByte(a_data);

  if (length = 0) then
  begin
    result := THashResult.Create(UInt64(0));
    Exit;
  end;

  h := Fm_working_key xor UInt64(length);
  current_index := 0;

  while (length >= 8) do
  begin

    k := TConverters.ReadBytesAsUInt64LE(ptr_a_data, current_index);

    k := k * M;
    k := k xor (k shr R);
    k := k * M;

    h := h xor k;
    h := h * M;

    System.Inc(current_index, 8);
    System.Dec(length, 8);

  end;

  case length of
    7:
      begin

        h := h xor ((UInt64(a_data[current_index]) shl 48));
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 40);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 32);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 24);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 16);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 8);
        System.Inc(current_index);

        h := h xor UInt64(a_data[current_index]);

        h := h * M;
      end;

    6:
      begin

        h := h xor (UInt64(a_data[current_index]) shl 40);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 32);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 24);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 16);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 8);
        System.Inc(current_index);

        h := h xor UInt64(a_data[current_index]);

        h := h * M;
      end;

    5:
      begin

        h := h xor (UInt64(a_data[current_index]) shl 32);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 24);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 16);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 8);
        System.Inc(current_index);

        h := h xor UInt64(a_data[current_index]);
        h := h * M;
      end;

    4:
      begin

        h := h xor (UInt64(a_data[current_index]) shl 24);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 16);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 8);
        System.Inc(current_index);

        h := h xor UInt64(a_data[current_index]);
        h := h * M;
      end;

    3:
      begin

        h := h xor (UInt64(a_data[current_index]) shl 16);
        System.Inc(current_index);

        h := h xor (UInt64(a_data[current_index]) shl 8);
        System.Inc(current_index);

        h := h xor UInt64(a_data[current_index]);
        h := h * M;
      end;

    2:
      begin

        h := h xor (UInt64(a_data[current_index]) shl 8);
        System.Inc(current_index);

        h := h xor UInt64(a_data[current_index]);

        h := h * M;
      end;

    1:
      begin

        h := h xor UInt64(a_data[current_index]);

        h := h * M;
      end;

  end;

  h := h xor (h shr R);
  h := h * M;
  h := h xor (h shr R);

  result := THashResult.Create(h);

end;

constructor TMurmur2_64.Create;
begin
  Inherited Create(8, 8);
  Fm_key := CKEY;

end;

function TMurmur2_64.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(Fm_key);
end;

function TMurmur2_64.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmur2_64.Initialize;
begin
  Fm_working_key := Fm_key;

  Inherited Initialize();

end;

procedure TMurmur2_64.SetKey(const value: THashLibByteArray);
begin
  if (value = Nil) then
  begin
    Fm_key := CKEY;
  end
  else
  begin
    if System.length(value) <> KeyLength.value then
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    Fm_key := TConverters.ReadBytesAsUInt32LE(PByte(value), 0);
  end;

end;

end.
