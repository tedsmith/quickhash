unit HlpMurmur2;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpConverters,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpMultipleTransformNonBlock,
  HlpNullable;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TMurmur2 = class sealed(TMultipleTransformNonBlock, IHash32, IHashWithKey,
    ITransformBlock)

  strict private

    Fm_key, Fm_working_key, Fm_h: UInt32;

  const
    CKEY = UInt32($0);
    M = UInt32($5BD1E995);
    R = Int32(24);

    function InternalComputeBytes(const a_data: THashLibByteArray): Int32;
    procedure TransformUInt32Fast(a_data: UInt32); inline;
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

{ TMurmur2 }

constructor TMurmur2.Create;
begin
  Inherited Create(4, 4);
  Fm_key := CKEY;

end;

function TMurmur2.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(Fm_key);
end;

procedure TMurmur2.SetKey(const value: THashLibByteArray);
begin
  if (value = Nil) then
  begin
    Fm_key := CKEY;
  end
  else
  begin
    if System.Length(value) <> KeyLength.value then
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);
    Fm_key := TConverters.ReadBytesAsUInt32LE(PByte(value), 0);
  end;
end;

procedure TMurmur2.TransformUInt32Fast(a_data: UInt32);
begin
  a_data := a_data * M;
  a_data := a_data xor (a_data shr R);
  a_data := a_data * M;

  Fm_h := Fm_h * M;
  Fm_h := Fm_h xor a_data;
end;

function TMurmur2.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmur2.Initialize;
begin
  Fm_working_key := Fm_key;
  inherited Initialize();
end;

function TMurmur2.InternalComputeBytes(const a_data: THashLibByteArray): Int32;
var
  &length, current_index: Int32;
  k: UInt32;
  ptr_a_data: PByte;
begin
  Length := System.Length(a_data);
  ptr_a_data := PByte(a_data);

  if (Length = 0) then
  begin
    result := 0;
    Exit;
  end;

  Fm_h := Fm_working_key xor UInt32(Length);
  current_index := 0;

  while (Length >= 4) do
  begin

    k := TConverters.ReadBytesAsUInt32LE(ptr_a_data, current_index);

    TransformUInt32Fast(k);
    System.Inc(current_index, 4);
    System.Dec(Length, 4);
  end;

  case Length of
    3:
      begin

        Fm_h := Fm_h xor (a_data[current_index + 2] shl 16);

        Fm_h := Fm_h xor (a_data[current_index + 1] shl 8);

        Fm_h := Fm_h xor (a_data[current_index]);

        Fm_h := Fm_h * M;
      end;

    2:
      begin

        Fm_h := Fm_h xor (a_data[current_index + 1] shl 8);

        Fm_h := Fm_h xor (a_data[current_index]);

        Fm_h := Fm_h * M;
      end;

    1:
      begin

        Fm_h := Fm_h xor (a_data[current_index]);

        Fm_h := Fm_h * M;
      end;

  end;

  Fm_h := Fm_h xor (Fm_h shr 13);

  Fm_h := Fm_h * M;
  Fm_h := Fm_h xor (Fm_h shr 15);

  result := Int32(Fm_h);
end;

function TMurmur2.Clone(): IHash;
var
  HashInstance: TMurmur2;
begin
  HashInstance := TMurmur2.Create();
  HashInstance.Fm_key := Fm_key;
  HashInstance.Fm_working_key := Fm_working_key;
  HashInstance.Fm_h := Fm_h;
  FBuffer.Position := 0;
  HashInstance.FBuffer.CopyFrom(FBuffer, FBuffer.Size);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

function TMurmur2.ComputeAggregatedBytes(const a_data: THashLibByteArray)
  : IHashResult;

begin
  result := THashResult.Create(InternalComputeBytes(a_data));
end;

end.
