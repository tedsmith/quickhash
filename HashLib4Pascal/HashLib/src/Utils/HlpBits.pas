unit HlpBits;

{$I ..\Include\HashLib.inc}

interface

type
  TBits = class sealed(TObject)

  public

    class function ReverseBytesInt32(value: Int32): Int32; static; inline;
    class function ReverseBitsUInt8(value: UInt8): UInt8; static; inline;
    class function ReverseBytesUInt16(value: UInt16): UInt16; static; inline;
    class function ReverseBytesUInt32(value: UInt32): UInt32; static; inline;
    class function ReverseBytesUInt64(value: UInt64): UInt64; static; inline;

    /// <summary>
    /// Reverse a ByteArray.
    /// </summary>
    /// Implementation was found here <see cref="http://stackoverflow.com/a/12969282" />
    /// <param name="Source">Pointer to Input Array.</param>
    /// <param name="Dest">Pointer to Destination Array.</param>
    /// <param name="Size">Size of the Array to Reverse.</param>

    class procedure ReverseByteArray(Source, Dest: Pointer;
      Size: Int64); static;

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="value">Int32 value to compute 'Asr' on.</param>
    /// <param name="ShiftBits">Integer, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>

    class function Asr32(value: Int32; ShiftBits: Int32): Int32; static; inline;

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="value">Int64 value to compute 'Asr' on.</param>
    /// <param name="ShiftBits">Integer, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>
    /// Implementation was found here <see cref="https://github.com/Spelt/ZXing.Delphi/blob/master/Lib/Classes/Common/MathUtils.pas" />

    class function Asr64(value: Int64; ShiftBits: Int32): Int64; static; inline;

    class function RotateLeft32(a_value: UInt32; a_n: Int32): UInt32; overload;
      static; inline;
    class function RotateLeft64(a_value: UInt64; a_n: Int32): UInt64; overload;
      static; inline;
    class function RotateRight32(a_value: UInt32; a_n: Int32): UInt32; overload;
      static; inline;
    class function RotateRight64(a_value: UInt64; a_n: Int32): UInt64; overload;
      static; inline;

  end;

implementation

{ TBits }

class procedure TBits.ReverseByteArray(Source, Dest: Pointer; Size: Int64);
var
  ptr_src, ptr_dest: PByte;
begin
  ptr_src := PByte(Source);
  ptr_dest := PByte(Dest);
  System.Inc(ptr_dest, Size - 1);
  while Size > 0 do
  begin
    ptr_dest^ := ptr_src^;
    System.Inc(ptr_src);
    System.Dec(ptr_dest);
    System.Dec(Size);
  end;
end;

class function TBits.ReverseBytesInt32(value: Int32): Int32;
{$IFNDEF FPC}
var
  i1, i2, i3, i4: Int32;
{$ENDIF FPC}
begin
{$IFDEF FPC}
  result := SwapEndian(value);
{$ELSE}
  i1 := value and $FF;
  i2 := TBits.Asr32(value, 8) and $FF;
  i3 := TBits.Asr32(value, 16) and $FF;
  i4 := TBits.Asr32(value, 24) and $FF;

  result := (i1 shl 24) or (i2 shl 16) or (i3 shl 8) or (i4 shl 0);
{$ENDIF FPC}
end;

class function TBits.ReverseBitsUInt8(value: UInt8): UInt8;
begin
  value := ((value shr 1) and $55) or ((value shl 1) and $AA);
  value := ((value shr 2) and $33) or ((value shl 2) and $CC);
  value := ((value shr 4) and $0F) or ((value shl 4) and $F0);
  result := value;
end;

class function TBits.ReverseBytesUInt16(value: UInt16): UInt16;
begin
{$IFDEF FPC}
  result := SwapEndian(value);
{$ELSE}
  result := UInt16((value and UInt32($FF)) shl 8 or
    (value and UInt32($FF00)) shr 8);
{$ENDIF FPC}
end;

class function TBits.ReverseBytesUInt32(value: UInt32): UInt32;
begin
{$IFDEF FPC}
  result := SwapEndian(value);
{$ELSE}
  result := (value and UInt32($000000FF)) shl 24 or (value and UInt32($0000FF00)
    ) shl 8 or (value and UInt32($00FF0000)) shr 8 or
    (value and UInt32($FF000000)) shr 24;
{$ENDIF FPC}
end;

class function TBits.ReverseBytesUInt64(value: UInt64): UInt64;
begin
{$IFDEF FPC}
  result := SwapEndian(value);
{$ELSE}
  result := (value and UInt64($00000000000000FF)) shl 56 or
    (value and UInt64($000000000000FF00)) shl 40 or
    (value and UInt64($0000000000FF0000)) shl 24 or
    (value and UInt64($00000000FF000000)) shl 8 or
    (value and UInt64($000000FF00000000)) shr 8 or
    (value and UInt64($0000FF0000000000)) shr 24 or
    (value and UInt64($00FF000000000000)) shr 40 or
    (value and UInt64($FF00000000000000)) shr 56;
{$ENDIF FPC}
end;

class function TBits.Asr32(value: Int32; ShiftBits: Int32): Int32;

begin
{$IFDEF FPC}
  result := SarLongInt(value, ShiftBits);
{$ELSE}
  result := value shr ShiftBits;
  if (value and $80000000) > 0 then
    // if you don't want to cast ($FFFFFFFF) to an Int32,
    // simply replace it with (-1) to avoid range check error.
    result := result or (Int32($FFFFFFFF) shl (32 - ShiftBits));
{$ENDIF FPC}
end;

class function TBits.Asr64(value: Int64; ShiftBits: Int32): Int64;
begin
{$IFDEF FPC}
  result := SarInt64(value, ShiftBits);
{$ELSE}
  result := value shr ShiftBits;
  if (value and $8000000000000000) > 0 then
    result := result or ($FFFFFFFFFFFFFFFF shl (64 - ShiftBits));
{$ENDIF FPC}
end;

class function TBits.RotateLeft32(a_value: UInt32; a_n: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  result := RolDWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 31;

  result := (a_value shl a_n) or (a_value shr (32 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateLeft64(a_value: UInt64; a_n: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  result := RolQWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 63;

  result := (a_value shl a_n) or (a_value shr (64 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateRight32(a_value: UInt32; a_n: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  result := RorDWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 31;

  result := (a_value shr a_n) or (a_value shl (32 - a_n));
{$ENDIF FPC}
end;

class function TBits.RotateRight64(a_value: UInt64; a_n: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(a_n >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  result := RorQWord(a_value, a_n);
{$ELSE}
  a_n := a_n and 63;

  result := (a_value shr a_n) or (a_value shl (64 - a_n));
{$ENDIF FPC}
end;

end.
