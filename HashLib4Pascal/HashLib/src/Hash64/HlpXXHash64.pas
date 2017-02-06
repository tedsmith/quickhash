unit HlpXXHash64;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpConverters,
{$IFDEF DELPHI}
  HlpBitConverter,
{$ENDIF DELPHI}
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpNullable,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TXXHash64 = class sealed(THash, IHash64, IBlockHash, IHashWithKey,
    ITransformBlock)

  strict private

    Fm_key, Fm_hash: UInt64;

  const
    CKEY = UInt64(0);

{$IFDEF FPC}
    // to bypass Internal error (200706094) on FPC, We use "Typed Constant".
    PRIME64_1: UInt64 = (11400714785074694791);
    PRIME64_2: UInt64 = (14029467366897019727);
    PRIME64_3: UInt64 = (1609587929392839161);
    PRIME64_4: UInt64 = (9650029242287828579);
    PRIME64_5: UInt64 = (2870177450012600261);
{$ELSE}
    PRIME64_1 = UInt64(11400714785074694791);
    PRIME64_2 = UInt64(14029467366897019727);
    PRIME64_3 = UInt64(1609587929392839161);
    PRIME64_4 = UInt64(9650029242287828579);
    PRIME64_5 = UInt64(2870177450012600261);
{$ENDIF FPC}
    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(value: THashLibByteArray); inline;

  type

    TXXH_State = record

    private

      total_len, v1, v2, v3, v4: UInt64;
      memsize: UInt32;
      memory: THashLibByteArray;

    end;

  strict private
    F_state: TXXH_State;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TXXHash64 }

constructor TXXHash64.Create;
begin
  Inherited Create(8, 32);
  Fm_key := CKEY;
  System.SetLength(F_state.memory, 32);

end;

function TXXHash64.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt64AsBytesLE(Fm_key);
end;

function TXXHash64.GetKeyLength: TNullableInteger;
begin
  result := 8;
end;

procedure TXXHash64.Initialize;
begin
  Fm_hash := 0;
  F_state.v1 := Fm_key + PRIME64_1 + PRIME64_2;
  F_state.v2 := Fm_key + PRIME64_2;
  F_state.v3 := Fm_key + 0;
  F_state.v4 := Fm_key - PRIME64_1;
  F_state.total_len := 0;
  F_state.memsize := 0;

end;

procedure TXXHash64.SetKey(value: THashLibByteArray);
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

    Fm_key := TConverters.ReadBytesAsUInt64LE(PByte(value), 0);
  end;
end;

procedure TXXHash64.TransformBytes(a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  v1, v2, v3, v4: UInt64;
  ptrLimit, ptrEnd, ptrBuffer, ptrTemp, ptrMemory: PByte;
begin

{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  ptrBuffer := @a_data[a_index];
  ptrMemory := PByte(F_state.memory);
  F_state.total_len := F_state.total_len + UInt64(a_length);

  if ((F_state.memsize + UInt32(a_length)) < UInt32(32)) then
  begin

    ptrTemp := PByte(F_state.memory) + F_state.memsize;

    System.Move(ptrBuffer^, ptrTemp^, a_length);

    F_state.memsize := F_state.memsize + UInt32(a_length);
    Exit;
  end;

  ptrEnd := ptrBuffer + UInt32(a_length);

  if F_state.memsize > 0 then
  begin
    ptrTemp := PByte(F_state.memory) + F_state.memsize;
    System.Move(ptrBuffer^, ptrTemp^, 32 - F_state.memsize);

    F_state.v1 := PRIME64_1 * TBits.RotateLeft64(F_state.v1 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(ptrMemory, 0), 31);
    F_state.v2 := PRIME64_1 * TBits.RotateLeft64(F_state.v2 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(ptrMemory, 8), 31);
    F_state.v3 := PRIME64_1 * TBits.RotateLeft64(F_state.v3 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(ptrMemory, 16), 31);
    F_state.v4 := PRIME64_1 * TBits.RotateLeft64(F_state.v4 + PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(ptrMemory, 24), 31);

    ptrBuffer := ptrBuffer + (32 - F_state.memsize);
    F_state.memsize := 0;
  end;

  if ptrBuffer <= (ptrEnd - 32) then
  begin
    v1 := F_state.v1;
    v2 := F_state.v2;
    v3 := F_state.v3;
    v4 := F_state.v4;

    ptrLimit := ptrEnd - 32;
    repeat

      v1 := PRIME64_1 * TBits.RotateLeft64
        (v1 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE(ptrBuffer, 0), 31);
      v2 := PRIME64_1 * TBits.RotateLeft64
        (v2 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE(ptrBuffer, 8), 31);
      v3 := PRIME64_1 * TBits.RotateLeft64
        (v3 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE(ptrBuffer, 16), 31);
      v4 := PRIME64_1 * TBits.RotateLeft64
        (v4 + PRIME64_2 * TConverters.ReadBytesAsUInt64LE(ptrBuffer, 24), 31);

      System.Inc(ptrBuffer, 32);
    until not(ptrBuffer <= ptrLimit);

    F_state.v1 := v1;
    F_state.v2 := v2;
    F_state.v3 := v3;
    F_state.v4 := v4;
  end;

  if ptrBuffer < ptrEnd then
  begin
    ptrTemp := PByte(F_state.memory);
    System.Move(ptrBuffer^, ptrTemp^, ptrEnd - ptrBuffer);
    F_state.memsize := ptrEnd - ptrBuffer;
  end;

end;

function TXXHash64.TransformFinal: IHashResult;
var
  v1, v2, v3, v4: UInt64;
  ptrEnd, ptrBuffer: PByte;
begin

  if F_state.total_len >= UInt64(32) then
  begin
    v1 := F_state.v1;
    v2 := F_state.v2;
    v3 := F_state.v3;
    v4 := F_state.v4;

    Fm_hash := TBits.RotateLeft64(v1, 1) + TBits.RotateLeft64(v2, 7) +
      TBits.RotateLeft64(v3, 12) + TBits.RotateLeft64(v4, 18);

    v1 := TBits.RotateLeft64(v1 * PRIME64_2, 31) * PRIME64_1;
    Fm_hash := (Fm_hash xor v1) * PRIME64_1 + PRIME64_4;

    v2 := TBits.RotateLeft64(v2 * PRIME64_2, 31) * PRIME64_1;
    Fm_hash := (Fm_hash xor v2) * PRIME64_1 + PRIME64_4;

    v3 := TBits.RotateLeft64(v3 * PRIME64_2, 31) * PRIME64_1;
    Fm_hash := (Fm_hash xor v3) * PRIME64_1 + PRIME64_4;

    v4 := TBits.RotateLeft64(v4 * PRIME64_2, 31) * PRIME64_1;
    Fm_hash := (Fm_hash xor v4) * PRIME64_1 + PRIME64_4;
  end
  else
    Fm_hash := Fm_key + PRIME64_5;

  System.Inc(Fm_hash, F_state.total_len);

  ptrBuffer := PByte(F_state.memory);
  ptrEnd := ptrBuffer + F_state.memsize;

  while (ptrBuffer + 8) <= ptrEnd do
  begin
    Fm_hash := Fm_hash xor (PRIME64_1 * TBits.RotateLeft64(PRIME64_2 *
      TConverters.ReadBytesAsUInt64LE(ptrBuffer, 0), 31));
    Fm_hash := TBits.RotateLeft64(Fm_hash, 27) * PRIME64_1 + PRIME64_4;
    System.Inc(ptrBuffer, 8);
  end;

  if (ptrBuffer + 4) <= ptrEnd then
  begin
    Fm_hash := Fm_hash xor TConverters.ReadBytesAsUInt32LE(ptrBuffer, 0) *
      PRIME64_1;
    Fm_hash := TBits.RotateLeft64(Fm_hash, 23) * PRIME64_2 + PRIME64_3;
    System.Inc(ptrBuffer, 4);
  end;

  while ptrBuffer < ptrEnd do
  begin
    Fm_hash := Fm_hash xor ptrBuffer^ * PRIME64_5;
    Fm_hash := TBits.RotateLeft64(Fm_hash, 11) * PRIME64_1;
    System.Inc(ptrBuffer);
  end;

  Fm_hash := Fm_hash xor (Fm_hash shr 33);
  Fm_hash := Fm_hash * PRIME64_2;
  Fm_hash := Fm_hash xor (Fm_hash shr 29);
  Fm_hash := Fm_hash * PRIME64_3;
  Fm_hash := Fm_hash xor (Fm_hash shr 32);

  result := THashResult.Create(Fm_hash);
  Initialize();

end;

end.
