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
    procedure CompressTimes(a_times: Int32); inline;
    procedure ProcessBlock(a_m: UInt64); inline;
    procedure ByteUpdate(a_b: Byte); inline;
    procedure Finish();

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray;
    procedure SetKey(const value: THashLibByteArray);

  strict protected

    Fm_v0, Fm_v1, Fm_v2, Fm_v3, Fm_key0, Fm_key1, Fm_total_length: UInt64;
    F_cr, F_fr, Fm_idx: Int32;
    Fm_buf: THashLibByteArray;

  public
    constructor Create(a_compression_rounds: Int32 = 2;
      a_finalization_rounds: Int32 = 4);
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
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

    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TSipHash2_4 }

function TSipHash2_4.Clone(): IHash;
var
  HashInstance: TSipHash2_4;
begin
  HashInstance := TSipHash2_4.Create();
  HashInstance.Fm_v0 := Fm_v0;
  HashInstance.Fm_v1 := Fm_v1;
  HashInstance.Fm_v2 := Fm_v2;
  HashInstance.Fm_v3 := Fm_v3;
  HashInstance.Fm_key0 := Fm_key0;
  HashInstance.Fm_key1 := Fm_key1;
  HashInstance.Fm_total_length := Fm_total_length;
  HashInstance.F_cr := F_cr;
  HashInstance.F_fr := F_fr;
  HashInstance.Fm_idx := Fm_idx;
  HashInstance.Fm_buf := System.Copy(Fm_buf);
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSipHash2_4.Create;
begin
  Inherited Create(2, 4);

end;

{ TSipHash }

procedure TSipHash.Compress;
begin
  Fm_v0 := Fm_v0 + Fm_v1;
  Fm_v2 := Fm_v2 + Fm_v3;
  Fm_v1 := TBits.RotateLeft64(Fm_v1, 13);
  Fm_v3 := TBits.RotateLeft64(Fm_v3, 16);
  Fm_v1 := Fm_v1 xor Fm_v0;
  Fm_v3 := Fm_v3 xor Fm_v2;
  Fm_v0 := TBits.RotateLeft64(Fm_v0, 32);
  Fm_v2 := Fm_v2 + Fm_v1;
  Fm_v0 := Fm_v0 + Fm_v3;
  Fm_v1 := TBits.RotateLeft64(Fm_v1, 17);
  Fm_v3 := TBits.RotateLeft64(Fm_v3, 21);
  Fm_v1 := Fm_v1 xor Fm_v2;
  Fm_v3 := Fm_v3 xor Fm_v0;
  Fm_v2 := TBits.RotateLeft64(Fm_v2, 32);
end;

procedure TSipHash.CompressTimes(a_times: Int32);
var
  i: Int32;
begin
  i := 0;
  while i < a_times do
  begin
    Compress();
    System.Inc(i);
  end;
end;

procedure TSipHash.ProcessBlock(a_m: UInt64);
begin
  Fm_v3 := Fm_v3 xor a_m;
  CompressTimes(F_cr);
  Fm_v0 := Fm_v0 xor a_m;
end;

procedure TSipHash.ByteUpdate(a_b: Byte);
var
  ptr_Fm_buf: PByte;
  m: UInt64;
begin

  Fm_buf[Fm_idx] := a_b;
  System.Inc(Fm_idx);
  if Fm_idx >= 8 then
  begin
    ptr_Fm_buf := PByte(Fm_buf);
    m := TConverters.ReadBytesAsUInt64LE(ptr_Fm_buf, 0);
    ProcessBlock(m);
    Fm_idx := 0;
  end;

end;

constructor TSipHash.Create(a_compression_rounds, a_finalization_rounds: Int32);
begin
  Inherited Create(8, 8);
  Fm_key0 := KEY0;
  Fm_key1 := KEY1;
  F_cr := a_compression_rounds;
  F_fr := a_finalization_rounds;
  System.SetLength(Fm_buf, 8);
end;

procedure TSipHash.Finish;
var
  b: UInt64;
begin

  b := UInt64(Fm_total_length and $FF) shl 56;

  if (Fm_idx <> 0) then
  begin

    case (Fm_idx) of

      7:
        begin
          b := b or (UInt64(Fm_buf[6]) shl 48);
          b := b or (UInt64(Fm_buf[5]) shl 40);
          b := b or (UInt64(Fm_buf[4]) shl 32);
          b := b or (UInt64(Fm_buf[3]) shl 24);
          b := b or (UInt64(Fm_buf[2]) shl 16);
          b := b or (UInt64(Fm_buf[1]) shl 8);
          b := b or (UInt64(Fm_buf[0]));
        end;
      6:
        begin
          b := b or (UInt64(Fm_buf[5]) shl 40);
          b := b or (UInt64(Fm_buf[4]) shl 32);
          b := b or (UInt64(Fm_buf[3]) shl 24);
          b := b or (UInt64(Fm_buf[2]) shl 16);
          b := b or (UInt64(Fm_buf[1]) shl 8);
          b := b or (UInt64(Fm_buf[0]));
        end;
      5:
        begin
          b := b or (UInt64(Fm_buf[4]) shl 32);
          b := b or (UInt64(Fm_buf[3]) shl 24);
          b := b or (UInt64(Fm_buf[2]) shl 16);
          b := b or (UInt64(Fm_buf[1]) shl 8);
          b := b or (UInt64(Fm_buf[0]));
        end;

      4:
        begin
          b := b or (UInt64(Fm_buf[3]) shl 24);
          b := b or (UInt64(Fm_buf[2]) shl 16);
          b := b or (UInt64(Fm_buf[1]) shl 8);
          b := b or (UInt64(Fm_buf[0]));
        end;

      3:
        begin
          b := b or (UInt64(Fm_buf[2]) shl 16);
          b := b or (UInt64(Fm_buf[1]) shl 8);
          b := b or (UInt64(Fm_buf[0]));
        end;

      2:
        begin
          b := b or (UInt64(Fm_buf[1]) shl 8);
          b := b or (UInt64(Fm_buf[0]));
        end;

      1:
        begin
          b := b or (UInt64(Fm_buf[0]));
        end;

    end;
  end;

  Fm_v3 := Fm_v3 xor b;
  CompressTimes(F_cr);
  Fm_v0 := Fm_v0 xor b;
  Fm_v2 := Fm_v2 xor $FF;
  CompressTimes(F_fr);
end;

function TSipHash.GetKey: THashLibByteArray;
var
  LKey: THashLibByteArray;
begin
  System.SetLength(LKey, KeyLength.value);

  TConverters.ReadUInt64AsBytesLE(Fm_key0, LKey, 0);
  TConverters.ReadUInt64AsBytesLE(Fm_key1, LKey, 8);

  result := LKey;
end;

function TSipHash.GetKeyLength: TNullableInteger;
begin
  result := 16;
end;

procedure TSipHash.Initialize;
begin
  Fm_v0 := V0;
  Fm_v1 := V1;
  Fm_v2 := V2;
  Fm_v3 := V3;
  Fm_total_length := 0;
  Fm_idx := 0;

  Fm_v3 := Fm_v3 xor Fm_key1;
  Fm_v2 := Fm_v2 xor Fm_key0;
  Fm_v1 := Fm_v1 xor Fm_key1;
  Fm_v0 := Fm_v0 xor Fm_key0;

end;

procedure TSipHash.SetKey(const value: THashLibByteArray);
begin
  if (value = Nil) then
  begin
    Fm_key0 := KEY0;
    Fm_key1 := KEY1;
  end

  else
  begin
    if System.Length(value) <> KeyLength.value then
      raise EArgumentHashLibException.CreateResFmt(@SInvalidKeyLength,
        [KeyLength.value]);

    Fm_key0 := TConverters.ReadBytesAsUInt64LE(PByte(value), 0);
    Fm_key1 := TConverters.ReadBytesAsUInt64LE(PByte(value), 8);
  end;
end;

procedure TSipHash.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  i, &length, iter, offset: Int32;
  ptr_a_data, ptr_Fm_buf: PByte;
  m: UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  Length := a_length;
  i := a_index;

  ptr_a_data := PByte(a_data);
  System.Inc(Fm_total_length, Length);

  // consume last pending bytes

  if ((Fm_idx <> 0) and (a_length <> 0)) then
  begin

{$IFDEF DEBUG}
    System.Assert(a_index = 0); // nothing would work anyways if a_index is !=0
{$ENDIF DEBUG}
    while ((Fm_idx < 8) and (Length <> 0)) do
    begin
      Fm_buf[Fm_idx] := (ptr_a_data + a_index)^;
      System.Inc(Fm_idx);
      System.Inc(a_index);
      System.Dec(Length);
    end;
    if (Fm_idx = 8) then
    begin
      ptr_Fm_buf := PByte(Fm_buf);
      m := TConverters.ReadBytesAsUInt64LE(ptr_Fm_buf, 0);
      ProcessBlock(m);
      Fm_idx := 0;
    end;
  end
  else
  begin
    i := 0;
  end;

  iter := Length shr 3;

  // body

  while i < iter do
  begin
    m := TConverters.ReadBytesAsUInt64LE(ptr_a_data, a_index + (i * 8));
    ProcessBlock(m);
    System.Inc(i);
  end;

  // save pending end bytes
  offset := a_index + (i * 8);

  while offset < (Length + a_index) do
  begin
    ByteUpdate(a_data[offset]);
    System.Inc(offset);
  end;

end;

function TSipHash.TransformFinal: IHashResult;
begin
  Finish();
  result := THashResult.Create(Fm_v0 xor Fm_v1 xor Fm_v2 xor Fm_v3);
  Initialize();
end;

end.

