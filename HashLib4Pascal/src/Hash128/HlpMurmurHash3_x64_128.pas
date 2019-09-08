unit HlpMurmurHash3_x64_128;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
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

    Fm_h1, Fm_h2, Fm_total_length: UInt64;
    Fm_key: UInt32;
    Fm_idx: Int32;
    Fm_buf: THashLibByteArray;

    procedure ByteUpdate(a_b: Byte); inline;
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

{ TMurmurHash3_x64_128 }

procedure TMurmurHash3_x64_128.ProcessPendings;
var
  k1, k2: UInt64;
  ptr_Fm_buf: PByte;
begin
  if Fm_idx >= 16 then
  begin
    ptr_Fm_buf := PByte(Fm_buf);
    k1 := TConverters.ReadBytesAsUInt64LE(ptr_Fm_buf, 0);
    k2 := TConverters.ReadBytesAsUInt64LE(ptr_Fm_buf, 8);

    k1 := k1 * C1;
    k1 := TBits.RotateLeft64(k1, 31);
    k1 := k1 * C2;
    Fm_h1 := Fm_h1 xor k1;

    Fm_h1 := TBits.RotateLeft64(Fm_h1, 27);
    Fm_h1 := Fm_h1 + Fm_h2;
    Fm_h1 := Fm_h1 * 5 + C3;

    k2 := k2 * C2;
    k2 := TBits.RotateLeft64(k2, 33);
    k2 := k2 * C1;
    Fm_h2 := Fm_h2 xor k2;

    Fm_h2 := TBits.RotateLeft64(Fm_h2, 31);
    Fm_h2 := Fm_h2 + Fm_h1;
    Fm_h2 := Fm_h2 * 5 + C4;

    Fm_idx := 0;
  end;
end;

procedure TMurmurHash3_x64_128.ByteUpdate(a_b: Byte);
begin
  Fm_buf[Fm_idx] := a_b;
  System.Inc(Fm_idx);
  ProcessPendings();
end;

function TMurmurHash3_x64_128.Clone(): IHash;
var
  HashInstance: TMurmurHash3_x64_128;
begin
  HashInstance := TMurmurHash3_x64_128.Create();
  HashInstance.Fm_h1 := Fm_h1;
  HashInstance.Fm_h2 := Fm_h2;
  HashInstance.Fm_total_length := Fm_total_length;
  HashInstance.Fm_key := Fm_key;
  HashInstance.Fm_idx := Fm_idx;
  HashInstance.Fm_buf := System.Copy(Fm_buf);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMurmurHash3_x64_128.Create;
begin
  Inherited Create(16, 16);
  Fm_key := CKEY;
  System.SetLength(Fm_buf, 16);

end;

procedure TMurmurHash3_x64_128.Finish;
var
  &length: Int32;
  k2, k1: UInt64;
begin

  // tail

  k1 := 0;
  k2 := 0;

  length := Fm_idx;

  if (length <> 0) then
  begin

    case length of
      15:
        begin

          k2 := k2 xor (UInt64(Fm_buf[14]) shl 48);
          k2 := k2 xor (UInt64(Fm_buf[13]) shl 40);
          k2 := k2 xor (UInt64(Fm_buf[12]) shl 32);
          k2 := k2 xor (UInt64(Fm_buf[11]) shl 24);
          k2 := k2 xor (UInt64(Fm_buf[10]) shl 16);
          k2 := k2 xor (UInt64(Fm_buf[9]) shl 8);
          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

      14:
        begin

          k2 := k2 xor (UInt64(Fm_buf[13]) shl 40);
          k2 := k2 xor (UInt64(Fm_buf[12]) shl 32);
          k2 := k2 xor (UInt64(Fm_buf[11]) shl 24);
          k2 := k2 xor (UInt64(Fm_buf[10]) shl 16);
          k2 := k2 xor (UInt64(Fm_buf[9]) shl 8);
          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

      13:
        begin

          k2 := k2 xor (UInt64(Fm_buf[12]) shl 32);
          k2 := k2 xor (UInt64(Fm_buf[11]) shl 24);
          k2 := k2 xor (UInt64(Fm_buf[10]) shl 16);
          k2 := k2 xor (UInt64(Fm_buf[9]) shl 8);
          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

      12:
        begin

          k2 := k2 xor (UInt64(Fm_buf[11]) shl 24);
          k2 := k2 xor (UInt64(Fm_buf[10]) shl 16);
          k2 := k2 xor (UInt64(Fm_buf[9]) shl 8);
          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

      11:
        begin

          k2 := k2 xor (UInt64(Fm_buf[10]) shl 16);
          k2 := k2 xor (UInt64(Fm_buf[9]) shl 8);
          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

      10:
        begin

          k2 := k2 xor (UInt64(Fm_buf[9]) shl 8);
          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

      9:
        begin

          k2 := k2 xor (UInt64(Fm_buf[8]) shl 0);
          k2 := k2 * C2;
          k2 := TBits.RotateLeft64(k2, 33);
          k2 := k2 * C1;
          Fm_h2 := Fm_h2 xor k2;
        end;

    end;

    if (length > 8) then
      length := 8;

    case length of
      8:
        begin

          k1 := k1 xor (UInt64(Fm_buf[7]) shl 56);
          k1 := k1 xor (UInt64(Fm_buf[6]) shl 48);
          k1 := k1 xor (UInt64(Fm_buf[5]) shl 40);
          k1 := k1 xor (UInt64(Fm_buf[4]) shl 32);
          k1 := k1 xor (UInt64(Fm_buf[3]) shl 24);
          k1 := k1 xor (UInt64(Fm_buf[2]) shl 16);
          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      7:
        begin

          k1 := k1 xor (UInt64(Fm_buf[6]) shl 48);
          k1 := k1 xor (UInt64(Fm_buf[5]) shl 40);
          k1 := k1 xor (UInt64(Fm_buf[4]) shl 32);
          k1 := k1 xor (UInt64(Fm_buf[3]) shl 24);
          k1 := k1 xor (UInt64(Fm_buf[2]) shl 16);
          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      6:
        begin

          k1 := k1 xor (UInt64(Fm_buf[5]) shl 40);
          k1 := k1 xor (UInt64(Fm_buf[4]) shl 32);
          k1 := k1 xor (UInt64(Fm_buf[3]) shl 24);
          k1 := k1 xor (UInt64(Fm_buf[2]) shl 16);
          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      5:
        begin

          k1 := k1 xor (UInt64(Fm_buf[4]) shl 32);
          k1 := k1 xor (UInt64(Fm_buf[3]) shl 24);
          k1 := k1 xor (UInt64(Fm_buf[2]) shl 16);
          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      4:
        begin

          k1 := k1 xor (UInt64(Fm_buf[3]) shl 24);
          k1 := k1 xor (UInt64(Fm_buf[2]) shl 16);
          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      3:
        begin

          k1 := k1 xor (UInt64(Fm_buf[2]) shl 16);
          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      2:
        begin

          k1 := k1 xor (UInt64(Fm_buf[1]) shl 8);
          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

      1:
        begin

          k1 := k1 xor (UInt64(Fm_buf[0]) shl 0);
          k1 := k1 * C1;
          k1 := TBits.RotateLeft64(k1, 31);
          k1 := k1 * C2;
          Fm_h1 := Fm_h1 xor k1;
        end;

    end;

  end;

  Fm_h1 := Fm_h1 xor Fm_total_length;
  Fm_h2 := Fm_h2 xor Fm_total_length;

  Fm_h1 := Fm_h1 + Fm_h2;
  Fm_h2 := Fm_h2 + Fm_h1;

  Fm_h1 := Fm_h1 xor (Fm_h1 shr 33);
  Fm_h1 := Fm_h1 * C5;
  Fm_h1 := Fm_h1 xor (Fm_h1 shr 33);
  Fm_h1 := Fm_h1 * C6;
  Fm_h1 := Fm_h1 xor (Fm_h1 shr 33);

  Fm_h2 := Fm_h2 xor (Fm_h2 shr 33);
  Fm_h2 := Fm_h2 * C5;
  Fm_h2 := Fm_h2 xor (Fm_h2 shr 33);
  Fm_h2 := Fm_h2 * C6;
  Fm_h2 := Fm_h2 xor (Fm_h2 shr 33);

  Fm_h1 := Fm_h1 + Fm_h2;
  Fm_h2 := Fm_h2 + Fm_h1;

end;

function TMurmurHash3_x64_128.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(Fm_key);
end;

function TMurmurHash3_x64_128.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TMurmurHash3_x64_128.Initialize;
begin
  Fm_h1 := Fm_key;
  Fm_h2 := Fm_key;

  Fm_total_length := 0;
  Fm_idx := 0;

end;

procedure TMurmurHash3_x64_128.SetKey(const value: THashLibByteArray);
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

procedure TMurmurHash3_x64_128.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  len, nBlocks, i, offset, lIdx: Int32;
  k1, k2: UInt64;
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

    k1 := TConverters.ReadBytesAsUInt64LE(ptr_a_data, a_index + lIdx);

    System.Inc(lIdx, 8);

    k2 := TConverters.ReadBytesAsUInt64LE(ptr_a_data, a_index + lIdx);

    System.Inc(lIdx, 8);

    k1 := k1 * C1;
    k1 := TBits.RotateLeft64(k1, 31);
    k1 := k1 * C2;
    Fm_h1 := Fm_h1 xor k1;

    Fm_h1 := TBits.RotateLeft64(Fm_h1, 27);
    Fm_h1 := Fm_h1 + Fm_h2;
    Fm_h1 := Fm_h1 * 5 + C3;

    k2 := k2 * C2;
    k2 := TBits.RotateLeft64(k2, 33);
    k2 := k2 * C1;
    Fm_h2 := Fm_h2 xor k2;

    Fm_h2 := TBits.RotateLeft64(Fm_h2, 31);
    Fm_h2 := Fm_h2 + Fm_h1;
    Fm_h2 := Fm_h2 * 5 + C4;

    System.Inc(i);
  end;

  offset := a_index + (i * 16);

  while (offset < (a_index + len)) do
  begin

    ByteUpdate(a_data[offset]);
    System.Inc(offset);

  end;

end;

function TMurmurHash3_x64_128.TransformFinal: IHashResult;
var
  tempBufByte: THashLibByteArray;
  tempBufUInt64: THashLibUInt64Array;
begin
  Finish();

  tempBufUInt64 := THashLibUInt64Array.Create(Fm_h1, Fm_h2);
  System.SetLength(tempBufByte, System.length(tempBufUInt64) *
    System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(tempBufUInt64), 0, PByte(tempBufByte), 0,
    System.length(tempBufByte));

  result := THashResult.Create(tempBufByte);

  Initialize();
end;

end.
