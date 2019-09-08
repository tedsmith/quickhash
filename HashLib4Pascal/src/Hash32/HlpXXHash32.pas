unit HlpXXHash32;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpConverters,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpNullable,
  HlpBits;

resourcestring
  SInvalidKeyLength = 'KeyLength Must Be Equal to %d';

type

  TXXHash32 = class sealed(THash, IHash32, IHashWithKey, ITransformBlock)

  strict private

    Fm_key, Fm_hash: UInt32;

  const
    CKEY = UInt32(0);

    PRIME32_1 = UInt32(2654435761);
    PRIME32_2 = UInt32(2246822519);
    PRIME32_3 = UInt32(3266489917);
    PRIME32_4 = UInt32(668265263);
    PRIME32_5 = UInt32(374761393);

    function GetKeyLength(): TNullableInteger;
    function GetKey: THashLibByteArray; inline;
    procedure SetKey(const value: THashLibByteArray); inline;

  type

    TXXH_State = record

    private

      Ftotal_len: UInt64;
      Fmemsize, Fv1, Fv2, Fv3, Fv4: UInt32;
      Fmemory: THashLibByteArray;

      function Clone(): TXXH_State; inline;

    end;

  strict private
    F_state: TXXH_State;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
    property KeyLength: TNullableInteger read GetKeyLength;
    property Key: THashLibByteArray read GetKey write SetKey;

  end;

implementation

{ TXXHash32.TXXH_State }

function TXXHash32.TXXH_State.Clone(): TXXH_State;
begin
  result := Default(TXXH_State);
  result.Ftotal_len := Ftotal_len;
  result.Fmemsize := Fmemsize;
  result.Fv1 := Fv1;
  result.Fv2 := Fv2;
  result.Fv3 := Fv3;
  result.Fv4 := Fv4;
  result.Fmemory := System.Copy(Fmemory);
end;

{ TXXHash32 }

function TXXHash32.Clone(): IHash;
var
  HashInstance: TXXHash32;
begin
  HashInstance := TXXHash32.Create();
  HashInstance.Fm_key := Fm_key;
  HashInstance.Fm_hash := Fm_hash;
  HashInstance.F_state := F_state.Clone();
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TXXHash32.Create;
begin
  Inherited Create(4, 16);
  Fm_key := CKEY;
  System.SetLength(F_state.Fmemory, 16);

end;

function TXXHash32.GetKey: THashLibByteArray;
begin
  result := TConverters.ReadUInt32AsBytesLE(Fm_key);
end;

function TXXHash32.GetKeyLength: TNullableInteger;
begin
  result := 4;
end;

procedure TXXHash32.Initialize;
begin
  Fm_hash := 0;
  F_state.Fv1 := Fm_key + PRIME32_1 + PRIME32_2;
  F_state.Fv2 := Fm_key + PRIME32_2;
  F_state.Fv3 := Fm_key + 0;
  F_state.Fv4 := Fm_key - PRIME32_1;
  F_state.Ftotal_len := 0;
  F_state.Fmemsize := 0;

end;

procedure TXXHash32.SetKey(const value: THashLibByteArray);
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

procedure TXXHash32.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  v1, v2, v3, v4: UInt32;
  ptrLimit, ptrEnd, ptrBuffer, ptrTemp, ptrMemory: PByte;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  ptrBuffer := PByte(a_data) + a_index;
  ptrMemory := PByte(F_state.Fmemory);
  F_state.Ftotal_len := F_state.Ftotal_len + UInt64(a_length);

  if ((F_state.Fmemsize + UInt32(a_length)) < UInt32(16)) then
  begin

    ptrTemp := PByte(F_state.Fmemory) + F_state.Fmemsize;

    System.Move(ptrBuffer^, ptrTemp^, a_length);

    F_state.Fmemsize := F_state.Fmemsize + UInt32(a_length);

    Exit;
  end;

  ptrEnd := ptrBuffer + UInt32(a_length);

  if F_state.Fmemsize > 0 then
  begin
    ptrTemp := PByte(F_state.Fmemory) + F_state.Fmemsize;
    System.Move(ptrBuffer^, ptrTemp^, 16 - F_state.Fmemsize);

    F_state.Fv1 := PRIME32_1 * TBits.RotateLeft32(F_state.Fv1 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(ptrMemory, 0), 13);
    F_state.Fv2 := PRIME32_1 * TBits.RotateLeft32(F_state.Fv2 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(ptrMemory, 4), 13);
    F_state.Fv3 := PRIME32_1 * TBits.RotateLeft32(F_state.Fv3 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(ptrMemory, 8), 13);
    F_state.Fv4 := PRIME32_1 * TBits.RotateLeft32(F_state.Fv4 + PRIME32_2 *
      TConverters.ReadBytesAsUInt32LE(ptrMemory, 12), 13);

    ptrBuffer := ptrBuffer + (16 - F_state.Fmemsize);
    F_state.Fmemsize := 0;
  end;

  if ptrBuffer <= (ptrEnd - 16) then
  begin
    v1 := F_state.Fv1;
    v2 := F_state.Fv2;
    v3 := F_state.Fv3;
    v4 := F_state.Fv4;

    ptrLimit := ptrEnd - 16;
    repeat

      v1 := PRIME32_1 * TBits.RotateLeft32
        (v1 + PRIME32_2 * TConverters.ReadBytesAsUInt32LE(ptrBuffer, 0), 13);
      v2 := PRIME32_1 * TBits.RotateLeft32
        (v2 + PRIME32_2 * TConverters.ReadBytesAsUInt32LE(ptrBuffer, 4), 13);
      v3 := PRIME32_1 * TBits.RotateLeft32
        (v3 + PRIME32_2 * TConverters.ReadBytesAsUInt32LE(ptrBuffer, 8), 13);
      v4 := PRIME32_1 * TBits.RotateLeft32
        (v4 + PRIME32_2 * TConverters.ReadBytesAsUInt32LE(ptrBuffer, 12), 13);
      System.Inc(ptrBuffer, 16);
    until not(ptrBuffer <= ptrLimit);

    F_state.Fv1 := v1;
    F_state.Fv2 := v2;
    F_state.Fv3 := v3;
    F_state.Fv4 := v4;
  end;

  if ptrBuffer < ptrEnd then
  begin
    ptrTemp := PByte(F_state.Fmemory);
    System.Move(ptrBuffer^, ptrTemp^, ptrEnd - ptrBuffer);
    F_state.Fmemsize := ptrEnd - ptrBuffer;
  end;

end;

function TXXHash32.TransformFinal: IHashResult;
var
  ptrEnd, ptrBuffer: PByte;
begin

  if F_state.Ftotal_len >= UInt64(16) then
    Fm_hash := TBits.RotateLeft32(F_state.Fv1, 1) +
      TBits.RotateLeft32(F_state.Fv2, 7) + TBits.RotateLeft32(F_state.Fv3, 12) +
      TBits.RotateLeft32(F_state.Fv4, 18)
  else
    Fm_hash := Fm_key + PRIME32_5;
  System.Inc(Fm_hash, F_state.Ftotal_len);

  ptrBuffer := PByte(F_state.Fmemory);

  ptrEnd := ptrBuffer + F_state.Fmemsize;
  while ((ptrBuffer + 4) <= ptrEnd) do
  begin
    Fm_hash := Fm_hash + TConverters.ReadBytesAsUInt32LE(ptrBuffer, 0) *
      PRIME32_3;
    Fm_hash := TBits.RotateLeft32(Fm_hash, 17) * PRIME32_4;
    System.Inc(ptrBuffer, 4);
  end;

  while ptrBuffer < ptrEnd do
  begin
    Fm_hash := Fm_hash + ptrBuffer^ * PRIME32_5;
    Fm_hash := TBits.RotateLeft32(Fm_hash, 11) * PRIME32_1;
    System.Inc(ptrBuffer);
  end;

  Fm_hash := Fm_hash xor (Fm_hash shr 15);
  Fm_hash := Fm_hash * PRIME32_2;
  Fm_hash := Fm_hash xor (Fm_hash shr 13);
  Fm_hash := Fm_hash * PRIME32_3;
  Fm_hash := Fm_hash xor (Fm_hash shr 16);

  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
