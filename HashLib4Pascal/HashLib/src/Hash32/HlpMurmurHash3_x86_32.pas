unit HlpMurmurHash3_x86_32;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHashInfo,
  HlpNullable,
  HlpHash,
  HlpHashResult,
  HlpIHashResult,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TMurmurHash3_x86_32 = class sealed(THash, IHash32, IHashWithKey,
    ITransformBlock)

  strict private

    Fm_key, Fm_h, Fm_total_length: UInt32;
    Fm_idx: Int32;
    Fm_buf: THashLibByteArray;

    procedure ByteUpdate(a_b: Byte); inline;
    procedure Finish();

  const
    CKEY = UInt32($0);

    C1 = UInt32($CC9E2D51);
    C2 = UInt32($1B873593);
    C3 = UInt32($E6546B64);
    C4 = UInt32($85EBCA6B);
    C5 = UInt32($C2B2AE35);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(value: THashLibByteArray); inline;
    procedure TransformUInt32Fast(a_data: UInt32); inline;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal: IHashResult; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TMurmurHash3_x86_32 }

constructor TMurmurHash3_x86_32.Create;
begin
  Inherited Create(4, 4);
  Fm_key := CKEY;
  System.SetLength(Fm_buf, 4);

end;

procedure TMurmurHash3_x86_32.Finish;
var
  k: UInt32;
begin

  // tail

  k := 0;

  if (Fm_idx <> 0) then
  begin

    case (Fm_idx) of

      3:
        begin
          k := k xor (Fm_buf[2] shl 16);
          k := k xor (Fm_buf[1] shl 8);
          k := k xor Fm_buf[0];
          k := k * C1;
          k := TBits.RotateLeft32(k, 15);
          k := k * C2;
          Fm_h := Fm_h xor k;

        end;
      2:
        begin

          k := k xor (Fm_buf[1] shl 8);
          k := k xor Fm_buf[0];
          k := k * C1;
          k := TBits.RotateLeft32(k, 15);
          k := k * C2;
          Fm_h := Fm_h xor k;

        end;
      1:
        begin

          k := k xor Fm_buf[0];
          k := k * C1;
          k := TBits.RotateLeft32(k, 15);
          k := k * C2;
          Fm_h := Fm_h xor k;

        end;
    end;
  end;

  // finalization

  Fm_h := Fm_h xor Fm_total_length;

  Fm_h := Fm_h xor (Fm_h shr 16);
  Fm_h := Fm_h * C4;
  Fm_h := Fm_h xor (Fm_h shr 13);
  Fm_h := Fm_h * C5;
  Fm_h := Fm_h xor (Fm_h shr 16);
end;

procedure TMurmurHash3_x86_32.ByteUpdate(a_b: Byte);
var
  k: UInt32;
  ptr_Fm_buf: PByte;
begin

  Fm_buf[Fm_idx] := a_b;
  System.Inc(Fm_idx);
  if Fm_idx >= 4 then
  begin
    ptr_Fm_buf := PByte(Fm_buf);
    k := TConverters.ReadBytesAsUInt32LE(ptr_Fm_buf, 0);
    TransformUInt32Fast(k);
    Fm_idx := 0;
  end;

end;

function TMurmurHash3_x86_32.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(Fm_key);
end;

procedure TMurmurHash3_x86_32.SetKey(value: THashLibByteArray);
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

function TMurmurHash3_x86_32.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmurHash3_x86_32.Initialize;
begin
  Fm_h := Fm_key;
  Fm_total_length := 0;
  Fm_idx := 0;
end;

procedure TMurmurHash3_x86_32.TransformBytes(a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  len, nBlocks, i, offset: Int32;
  k: UInt32;
  ptr_a_data: PByte;

begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  len := a_length;
  i := a_index;
  ptr_a_data := PByte(a_data);
  nBlocks := len shr 2;


  // body

  while i < nBlocks do
  begin
    k := TConverters.ReadBytesAsUInt32LE(ptr_a_data, a_index + i * 4);

    TransformUInt32Fast(k);

    System.Inc(i);
  end;

  System.Inc(Fm_total_length, len);

  offset := (i * 4);

  while offset < len do
  begin

    ByteUpdate(a_data[offset]);
    System.Inc(offset);

  end;

end;

function TMurmurHash3_x86_32.TransformFinal: IHashResult;
begin
  Finish();
  result := THashResult.Create(Fm_h);
  Initialize();
end;

procedure TMurmurHash3_x86_32.TransformUInt32Fast(a_data: UInt32);
var
  k: UInt32;
begin
  k := a_data;

  k := k * C1;
  k := TBits.RotateLeft32(k, 15);
  k := k * C2;

  Fm_h := Fm_h xor k;
  Fm_h := TBits.RotateLeft32(Fm_h, 13);
  Fm_h := (Fm_h * 5) + C3;
end;

end.
