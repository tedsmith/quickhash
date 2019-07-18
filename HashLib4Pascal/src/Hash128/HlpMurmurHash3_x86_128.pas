unit HlpMurmurHash3_x86_128;

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
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpHash,
  HlpIHash,
  HlpHashResult,
  HlpIHashResult,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type
  TMurmurHash3_x86_128 = class sealed(THash, IHash128, IHashWithKey,
    ITransformBlock)

  strict private

    Fm_key, Fm_h1, Fm_h2, Fm_h3, Fm_h4, Fm_total_length: UInt32;
    Fm_idx: Int32;
    Fm_buf: THashLibByteArray;

    procedure ByteUpdate(a_b: Byte); inline;
    procedure Finish();
    procedure ProcessPendings();

{$REGION 'Consts'}

  const
    CKEY = UInt32($0);

    C1 = UInt32($239B961B);
    C2 = UInt32($AB0E9789);
    C3 = UInt32($38B34AE5);
    C4 = UInt32($A1E38B93);
    C5 = UInt32($85EBCA6B);
    C6 = UInt32($C2B2AE35);

    C7 = UInt32($561CCD1B);
    C8 = UInt32($0BCAA747);
    C9 = UInt32($96CD1C35);
    C10 = UInt32($32AC3B17);

{$ENDREGION}
    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const value: THashLibByteArray); inline;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;
  end;

implementation

{ TMurmurHash3_x86_128 }

procedure TMurmurHash3_x86_128.ProcessPendings;
var
  k1, k2, k3, k4: UInt32;
  ptr_Fm_buf: PByte;
begin
  if Fm_idx >= 16 then
  begin
    ptr_Fm_buf := PByte(Fm_buf);
    k1 := TConverters.ReadBytesAsUInt32LE(ptr_Fm_buf, 0);
    k2 := TConverters.ReadBytesAsUInt32LE(ptr_Fm_buf, 4);
    k3 := TConverters.ReadBytesAsUInt32LE(ptr_Fm_buf, 8);
    k4 := TConverters.ReadBytesAsUInt32LE(ptr_Fm_buf, 12);

    k1 := k1 * C1;
    k1 := TBits.RotateLeft32(k1, 15);
    k1 := k1 * C2;
    Fm_h1 := Fm_h1 xor k1;

    Fm_h1 := TBits.RotateLeft32(Fm_h1, 19);

    Fm_h1 := Fm_h1 + Fm_h2;
    Fm_h1 := Fm_h1 * 5 + C7;

    k2 := k2 * C2;
    k2 := TBits.RotateLeft32(k2, 16);
    k2 := k2 * C3;
    Fm_h2 := Fm_h2 xor k2;

    Fm_h2 := TBits.RotateLeft32(Fm_h2, 17);

    Fm_h2 := Fm_h2 + Fm_h3;
    Fm_h2 := Fm_h2 * 5 + C8;

    k3 := k3 * C3;
    k3 := TBits.RotateLeft32(k3, 17);
    k3 := k3 * C4;
    Fm_h3 := Fm_h3 xor k3;

    Fm_h3 := TBits.RotateLeft32(Fm_h3, 15);

    Fm_h3 := Fm_h3 + Fm_h4;
    Fm_h3 := Fm_h3 * 5 + C9;

    k4 := k4 * C4;
    k4 := TBits.RotateLeft32(k4, 18);
    k4 := k4 * C1;
    Fm_h4 := Fm_h4 xor k4;

    Fm_h4 := TBits.RotateLeft32(Fm_h4, 13);

    Fm_h4 := Fm_h4 + Fm_h1;
    Fm_h4 := Fm_h4 * 5 + C10;

    Fm_idx := 0;
  end;
end;

procedure TMurmurHash3_x86_128.ByteUpdate(a_b: Byte);
begin
  Fm_buf[Fm_idx] := a_b;
  System.Inc(Fm_idx);
  ProcessPendings();
end;

function TMurmurHash3_x86_128.Clone(): IHash;
var
  HashInstance: TMurmurHash3_x86_128;
begin
  HashInstance := TMurmurHash3_x86_128.Create();
    HashInstance.Fm_key := Fm_key;
  HashInstance.Fm_h1 := Fm_h1;
  HashInstance.Fm_h2 := Fm_h2;
  HashInstance.Fm_h3 := Fm_h3;
  HashInstance.Fm_h4 := Fm_h4;
  HashInstance.Fm_total_length := Fm_total_length;
  HashInstance.Fm_idx := Fm_idx;
  HashInstance.Fm_buf := System.Copy(Fm_buf);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMurmurHash3_x86_128.Create;
begin
  Inherited Create(16, 16);
  Fm_key := CKEY;
  System.SetLength(Fm_buf, 16);

end;

procedure TMurmurHash3_x86_128.Finish;
var
  k1, k2, k3, k4: UInt32;
  &length: Int32;
begin

  // tail

  k1 := 0;
  k2 := 0;
  k3 := 0;
  k4 := 0;

  length := Fm_idx;
  if (length <> 0) then
  begin
    case (length) of
      15:
        begin

          k4 := k4 xor (Fm_buf[14] shl 16);
          k4 := k4 xor (Fm_buf[13] shl 8);
          k4 := k4 xor (Fm_buf[12] shl 0);

          k4 := k4 * C4;
          k4 := TBits.RotateLeft32(k4, 18);
          k4 := k4 * C1;
          Fm_h4 := Fm_h4 xor k4;
        end;

      14:
        begin

          k4 := k4 xor (Fm_buf[13] shl 8);
          k4 := k4 xor (Fm_buf[12] shl 0);
          k4 := k4 * C4;
          k4 := TBits.RotateLeft32(k4, 18);
          k4 := k4 * C1;
          Fm_h4 := Fm_h4 xor k4;
        end;

      13:
        begin

          k4 := k4 xor (Fm_buf[12] shl 0);
          k4 := k4 * C4;
          k4 := TBits.RotateLeft32(k4, 18);
          k4 := k4 * C1;
          Fm_h4 := Fm_h4 xor k4;
        end;

    end;

    if (length > 12) then
      length := 12;

    case length of

      12:
        begin

          k3 := k3 xor (Fm_buf[11] shl 24);
          k3 := k3 xor (Fm_buf[10] shl 16);
          k3 := k3 xor (Fm_buf[9] shl 8);
          k3 := k3 xor (Fm_buf[8] shl 0);

          k3 := k3 * C3;
          k3 := TBits.RotateLeft32(k3, 17);
          k3 := k3 * C4;
          Fm_h3 := Fm_h3 xor k3;
        end;

      11:
        begin

          k3 := k3 xor (Fm_buf[10] shl 16);
          k3 := k3 xor (Fm_buf[9] shl 8);
          k3 := k3 xor (Fm_buf[8] shl 0);

          k3 := k3 * C3;
          k3 := TBits.RotateLeft32(k3, 17);
          k3 := k3 * C4;
          Fm_h3 := Fm_h3 xor k3;
        end;

      10:
        begin

          k3 := k3 xor (Fm_buf[9] shl 8);
          k3 := k3 xor (Fm_buf[8] shl 0);

          k3 := k3 * C3;
          k3 := TBits.RotateLeft32(k3, 17);
          k3 := k3 * C4;
          Fm_h3 := Fm_h3 xor k3;
        end;

      9:
        begin

          k3 := k3 xor (Fm_buf[8] shl 0);

          k3 := k3 * C3;
          k3 := TBits.RotateLeft32(k3, 17);
          k3 := k3 * C4;
          Fm_h3 := Fm_h3 xor k3;
        end;

    end;

    if (length > 8) then
      length := 8;

    case length of

      8:
        begin

          k2 := k2 xor (Fm_buf[7] shl 24);
          k2 := k2 xor (Fm_buf[6] shl 16);
          k2 := k2 xor (Fm_buf[5] shl 8);
          k2 := k2 xor (Fm_buf[4] shl 0);

          k2 := k2 * C2;
          k2 := TBits.RotateLeft32(k2, 16);
          k2 := k2 * C3;
          Fm_h2 := Fm_h2 xor k2;
        end;

      7:
        begin

          k2 := k2 xor (Fm_buf[6] shl 16);
          k2 := k2 xor (Fm_buf[5] shl 8);
          k2 := k2 xor (Fm_buf[4] shl 0);

          k2 := k2 * C2;
          k2 := TBits.RotateLeft32(k2, 16);
          k2 := k2 * C3;
          Fm_h2 := Fm_h2 xor k2;
        end;

      6:
        begin

          k2 := k2 xor (Fm_buf[5] shl 8);
          k2 := k2 xor (Fm_buf[4] shl 0);

          k2 := k2 * C2;
          k2 := TBits.RotateLeft32(k2, 16);
          k2 := k2 * C3;
          Fm_h2 := Fm_h2 xor k2;
        end;

      5:
        begin

          k2 := k2 xor (Fm_buf[4] shl 0);

          k2 := k2 * C2;
          k2 := TBits.RotateLeft32(k2, 16);
          k2 := k2 * C3;
          Fm_h2 := Fm_h2 xor k2;
        end;

    end;

    if (length > 4) then
      length := 4;

    case length of

      4:
        begin

          k1 := k1 xor (Fm_buf[3] shl 24);
          k1 := k1 xor (Fm_buf[2] shl 16);
          k1 := k1 xor (Fm_buf[1] shl 8);
          k1 := k1 xor (Fm_buf[0] shl 0);

          k1 := k1 * C1;
          k1 := TBits.RotateLeft32(k1, 15);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      3:
        begin

          k1 := k1 xor (Fm_buf[2] shl 16);
          k1 := k1 xor (Fm_buf[1] shl 8);
          k1 := k1 xor (Fm_buf[0] shl 0);

          k1 := k1 * C1;
          k1 := TBits.RotateLeft32(k1, 15);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      2:
        begin

          k1 := k1 xor (Fm_buf[1] shl 8);
          k1 := k1 xor (Fm_buf[0] shl 0);

          k1 := k1 * C1;
          k1 := TBits.RotateLeft32(k1, 15);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      1:
        begin

          k1 := k1 xor (Fm_buf[0] shl 0);

          k1 := k1 * C1;
          k1 := TBits.RotateLeft32(k1, 15);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

    end;
  end;

  // finalization

  Fm_h1 := Fm_h1 xor Fm_total_length;
  Fm_h2 := Fm_h2 xor Fm_total_length;
  Fm_h3 := Fm_h3 xor Fm_total_length;
  Fm_h4 := Fm_h4 xor Fm_total_length;

  Fm_h1 := Fm_h1 + Fm_h2;
  Fm_h1 := Fm_h1 + Fm_h3;
  Fm_h1 := Fm_h1 + Fm_h4;
  Fm_h2 := Fm_h2 + Fm_h1;
  Fm_h3 := Fm_h3 + Fm_h1;
  Fm_h4 := Fm_h4 + Fm_h1;

  Fm_h1 := Fm_h1 xor (Fm_h1 shr 16);
  Fm_h1 := Fm_h1 * C5;
  Fm_h1 := Fm_h1 xor (Fm_h1 shr 13);
  Fm_h1 := Fm_h1 * C6;
  Fm_h1 := Fm_h1 xor (Fm_h1 shr 16);

  Fm_h2 := Fm_h2 xor (Fm_h2 shr 16);
  Fm_h2 := Fm_h2 * C5;
  Fm_h2 := Fm_h2 xor (Fm_h2 shr 13);
  Fm_h2 := Fm_h2 * C6;
  Fm_h2 := Fm_h2 xor (Fm_h2 shr 16);

  Fm_h3 := Fm_h3 xor (Fm_h3 shr 16);
  Fm_h3 := Fm_h3 * C5;
  Fm_h3 := Fm_h3 xor (Fm_h3 shr 13);
  Fm_h3 := Fm_h3 * C6;
  Fm_h3 := Fm_h3 xor (Fm_h3 shr 16);

  Fm_h4 := Fm_h4 xor (Fm_h4 shr 16);
  Fm_h4 := Fm_h4 * C5;
  Fm_h4 := Fm_h4 xor (Fm_h4 shr 13);
  Fm_h4 := Fm_h4 * C6;
  Fm_h4 := Fm_h4 xor (Fm_h4 shr 16);

  Fm_h1 := Fm_h1 + Fm_h2;
  Fm_h1 := Fm_h1 + Fm_h3;
  Fm_h1 := Fm_h1 + Fm_h4;
  Fm_h2 := Fm_h2 + Fm_h1;
  Fm_h3 := Fm_h3 + Fm_h1;
  Fm_h4 := Fm_h4 + Fm_h1;

end;

function TMurmurHash3_x86_128.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(Fm_key);
end;

function TMurmurHash3_x86_128.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmurHash3_x86_128.Initialize;
begin
  Fm_h1 := Fm_key;
  Fm_h2 := Fm_key;
  Fm_h3 := Fm_key;
  Fm_h4 := Fm_key;

  Fm_total_length := 0;
  Fm_idx := 0;

end;

procedure TMurmurHash3_x86_128.SetKey(const value: THashLibByteArray);
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

procedure TMurmurHash3_x86_128.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  len, nBlocks, i, offset, lIdx: Int32;
  k1, k2, k3, k4: UInt32;
  ptr_a_data: PByte;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.length(a_data));
{$ENDIF DEBUG}
  len := a_length;
  i := a_index;
  lIdx := 0;
  System.Inc(Fm_total_length, len);
  ptr_a_data := PByte(a_data);

  // consume last pending bytes

  if ((Fm_idx <> 0) and (a_length <> 0)) then
  begin

{$IFDEF DEBUG}
    System.Assert(a_index = 0); // nothing would work anyways if a_index is !=0
{$ENDIF DEBUG}
    while ((Fm_idx < 16) and (len <> 0)) do
    begin
      Fm_buf[Fm_idx] := (ptr_a_data + a_index)^;
      System.Inc(Fm_idx);
      System.Inc(a_index);
      System.Dec(len);
    end;
    if (Fm_idx = 16) then
    begin
      ProcessPendings;
    end;
  end
  else
  begin
    i := 0;
  end;

  nBlocks := len shr 4;

  // body

  while i < nBlocks do
  begin

    k1 := TConverters.ReadBytesAsUInt32LE(ptr_a_data, a_index + lIdx);
    System.Inc(lIdx, 4);
    k2 := TConverters.ReadBytesAsUInt32LE(ptr_a_data, a_index + lIdx);
    System.Inc(lIdx, 4);
    k3 := TConverters.ReadBytesAsUInt32LE(ptr_a_data, a_index + lIdx);
    System.Inc(lIdx, 4);
    k4 := TConverters.ReadBytesAsUInt32LE(ptr_a_data, a_index + lIdx);
    System.Inc(lIdx, 4);

    k1 := k1 * C1;
    k1 := TBits.RotateLeft32(k1, 15);
    k1 := k1 * C2;
    Fm_h1 := Fm_h1 xor k1;

    Fm_h1 := TBits.RotateLeft32(Fm_h1, 19);

    Fm_h1 := Fm_h1 + Fm_h2;
    Fm_h1 := Fm_h1 * 5 + C7;

    k2 := k2 * C2;
    k2 := TBits.RotateLeft32(k2, 16);
    k2 := k2 * C3;
    Fm_h2 := Fm_h2 xor k2;

    Fm_h2 := TBits.RotateLeft32(Fm_h2, 17);

    Fm_h2 := Fm_h2 + Fm_h3;
    Fm_h2 := Fm_h2 * 5 + C8;

    k3 := k3 * C3;
    k3 := TBits.RotateLeft32(k3, 17);
    k3 := k3 * C4;
    Fm_h3 := Fm_h3 xor k3;

    Fm_h3 := TBits.RotateLeft32(Fm_h3, 15);

    Fm_h3 := Fm_h3 + Fm_h4;
    Fm_h3 := Fm_h3 * 5 + C9;

    k4 := k4 * C4;
    k4 := TBits.RotateLeft32(k4, 18);
    k4 := k4 * C1;
    Fm_h4 := Fm_h4 xor k4;

    Fm_h4 := TBits.RotateLeft32(Fm_h4, 13);

    Fm_h4 := Fm_h4 + Fm_h1;
    Fm_h4 := Fm_h4 * 5 + C10;

    System.Inc(i);
  end;

  offset := a_index + (i * 16);

  while offset < (a_index + len) do
  begin
    ByteUpdate(a_data[offset]);
    System.Inc(offset);
  end;

end;

function TMurmurHash3_x86_128.TransformFinal: IHashResult;
var
  tempBufByte: THashLibByteArray;
  tempBufUInt32: THashLibUInt32Array;
begin
  Finish();

  tempBufUInt32 := THashLibUInt32Array.Create(Fm_h1, Fm_h2, Fm_h3, Fm_h4);
  System.SetLength(tempBufByte, System.length(tempBufUInt32) *
    System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(tempBufUInt32), 0, PByte(tempBufByte), 0,
    System.length(tempBufByte));

  result := THashResult.Create(tempBufByte);

  Initialize();
end;

end.
