unit HlpBits;

{$I ..\Include\HashLib.inc}

interface

type
  TBits = class sealed(TObject)

  public

    /// <summary>
    /// Reverse a ByteArray.
    /// </summary>
    /// Implementation was found here <see cref="http://stackoverflow.com/a/12969282" />
    /// <param name="ASource">Pointer to Source Array.</param>
    /// <param name="ADestination">Pointer to Destination Array.</param>
    /// <param name="ASize">Size of the Array to Reverse.</param>

    class procedure ReverseByteArray(ASource, ADestination: Pointer;
      ASize: Int64); static;

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="AValue">Int32 value to compute 'Asr' on.</param>
    /// <param name="AShiftBits">Byte, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>
    /// <remarks>
    /// Emulated Implementation was gotten from FreePascal sources
    /// </remarks>

    class function Asr32(AValue: Int32; AShiftBits: Byte): Int32;
      static; inline;

    /// <summary>
    /// Calculates Arithmetic shift right.
    /// </summary>
    /// <param name="AValue">Int64 value to compute 'Asr' on.</param>
    /// <param name="AShiftBits">Byte, number of bits to shift value to.</param>
    /// <returns>Shifted value.</returns>
    /// <remarks>
    /// Emulated Implementation was gotten from FreePascal sources
    /// </remarks>

    class function Asr64(AValue: Int64; AShiftBits: Byte): Int64;
      static; inline;

    class function RotateLeft8(AValue: Byte; ADistance: Int32): Byte;
      static; inline;
    class function RotateLeft32(AValue: UInt32; ADistance: Int32): UInt32;
      static; inline;
    class function RotateLeft64(AValue: UInt64; ADistance: Int32): UInt64;
      static; inline;
    class function RotateRight8(AValue: Byte; ADistance: Int32): Byte;
      static; inline;
    class function RotateRight32(AValue: UInt32; ADistance: Int32): UInt32;
      static; inline;
    class function RotateRight64(AValue: UInt64; ADistance: Int32): UInt64;
      static; inline;

    class function ReverseBytesInt32(AValue: Int32): Int32; static; inline;
    class function ReverseBitsUInt8(AValue: UInt8): UInt8; static; inline;
    class function ReverseBytesUInt16(AValue: UInt16): UInt16; static; inline;
    class function ReverseBytesUInt32(AValue: UInt32): UInt32; static; inline;
    class function ReverseBytesUInt64(AValue: UInt64): UInt64; static; inline;

  end;

implementation

{ TBits }

class procedure TBits.ReverseByteArray(ASource, ADestination: Pointer;
  ASize: Int64);
var
  LPtrSource, LPtrDestination: PByte;
begin
  LPtrSource := PByte(ASource);
  LPtrDestination := PByte(ADestination);
  System.Inc(LPtrDestination, ASize - 1);
  while ASize > 0 do
  begin
    LPtrDestination^ := LPtrSource^;
    System.Inc(LPtrSource);
    System.Dec(LPtrDestination);
    System.Dec(ASize);
  end;
end;

class function TBits.Asr32(AValue: Int32; AShiftBits: Byte): Int32;
begin
{$IFDEF FPC}
  Result := SarLongInt(AValue, AShiftBits);
{$ELSE}
  Result := Int32(UInt32(UInt32(UInt32(AValue) shr (AShiftBits and 31)) or
    (UInt32(Int32(UInt32(0 - UInt32(UInt32(AValue) shr 31)) and
    UInt32(Int32(0 - (Ord((AShiftBits and 31) <> 0) { and 1 } )))))
    shl (32 - (AShiftBits and 31)))));
{$ENDIF FPC}
end;

class function TBits.Asr64(AValue: Int64; AShiftBits: Byte): Int64;
begin
{$IFDEF FPC}
  Result := SarInt64(AValue, AShiftBits);
{$ELSE}
  Result := Int64(UInt64(UInt64(UInt64(AValue) shr (AShiftBits and 63)) or
    (UInt64(Int64(UInt64(0 - UInt64(UInt64(AValue) shr 63)) and
    UInt64(Int64(0 - (Ord((AShiftBits and 63) <> 0) { and 1 } )))))
    shl (64 - (AShiftBits and 63)))));
{$ENDIF FPC}
end;

class function TBits.RotateLeft8(AValue: Byte; ADistance: Int32): Byte;
begin
{$IFDEF DEBUG}
  System.Assert(ADistance >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RolByte(AValue, ADistance);
{$ELSE}
  ADistance := ADistance and 7;
  Result := (AValue shl ADistance) or (AValue shr (8 - ADistance));
{$ENDIF FPC}
end;

class function TBits.RotateLeft32(AValue: UInt32; ADistance: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(ADistance >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RolDWord(AValue, ADistance);
{$ELSE}
{$IFNDEF SHIFT_OVERFLOW_BUG_FIXED}
  ADistance := ADistance and 31;
{$ENDIF SHIFT_OVERFLOW_BUG_FIXED}
  Result := (AValue shl ADistance) or (AValue shr (32 - ADistance));
{$ENDIF FPC}
end;

class function TBits.RotateLeft64(AValue: UInt64; ADistance: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(ADistance >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RolQWord(AValue, ADistance);
{$ELSE}
{$IFNDEF SHIFT_OVERFLOW_BUG_FIXED}
  ADistance := ADistance and 63;
{$ENDIF SHIFT_OVERFLOW_BUG_FIXED}
  Result := (AValue shl ADistance) or (AValue shr (64 - ADistance));
{$ENDIF FPC}
end;

class function TBits.RotateRight8(AValue: Byte; ADistance: Int32): Byte;
begin
{$IFDEF DEBUG}
  System.Assert(ADistance >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RorByte(AValue, ADistance);
{$ELSE}
  ADistance := ADistance and 7;
  Result := (AValue shr ADistance) or (AValue shl (8 - ADistance));
{$ENDIF FPC}
end;

class function TBits.RotateRight32(AValue: UInt32; ADistance: Int32): UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(ADistance >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RorDWord(AValue, ADistance);
{$ELSE}
{$IFNDEF SHIFT_OVERFLOW_BUG_FIXED}
  ADistance := ADistance and 31;
{$ENDIF SHIFT_OVERFLOW_BUG_FIXED}
  Result := (AValue shr ADistance) or (AValue shl (32 - ADistance));
{$ENDIF FPC}
end;

class function TBits.RotateRight64(AValue: UInt64; ADistance: Int32): UInt64;
begin
{$IFDEF DEBUG}
  System.Assert(ADistance >= 0);
{$ENDIF DEBUG}
{$IFDEF FPC}
  Result := RorQWord(AValue, ADistance);
{$ELSE}
{$IFNDEF SHIFT_OVERFLOW_BUG_FIXED}
  ADistance := ADistance and 63;
{$ENDIF SHIFT_OVERFLOW_BUG_FIXED}
  Result := (AValue shr ADistance) or (AValue shl (64 - ADistance));
{$ENDIF FPC}
end;

class function TBits.ReverseBytesInt32(AValue: Int32): Int32;
{$IFNDEF FPC}
var
  i1, i2, i3, i4: Int32;
{$ENDIF FPC}
begin
{$IFDEF FPC}
  Result := SwapEndian(AValue);
{$ELSE}
  i1 := AValue and $FF;
  i2 := TBits.Asr32(AValue, 8) and $FF;
  i3 := TBits.Asr32(AValue, 16) and $FF;
  i4 := TBits.Asr32(AValue, 24) and $FF;

  Result := (i1 shl 24) or (i2 shl 16) or (i3 shl 8) or (i4 shl 0);
{$ENDIF FPC}
end;

class function TBits.ReverseBitsUInt8(AValue: UInt8): UInt8;
begin
  AValue := ((AValue shr 1) and $55) or ((AValue shl 1) and $AA);
  AValue := ((AValue shr 2) and $33) or ((AValue shl 2) and $CC);
  AValue := ((AValue shr 4) and $0F) or ((AValue shl 4) and $F0);
  Result := AValue;
end;

class function TBits.ReverseBytesUInt16(AValue: UInt16): UInt16;
begin
{$IFDEF FPC}
  Result := SwapEndian(AValue);
{$ELSE}
  Result := UInt16((AValue and UInt32($FF)) shl 8 or
    (AValue and UInt32($FF00)) shr 8);
{$ENDIF FPC}
  // Result := UInt16((AValue shr 8) + (AValue shl 8));
end;

class function TBits.ReverseBytesUInt32(AValue: UInt32): UInt32;
begin
{$IFDEF FPC}
  Result := SwapEndian(AValue);
{$ELSE}
  Result := ((AValue shl 24) and UInt32($FF000000)) or
    ((AValue shl 8) and UInt32($00FF0000)) or
    ((AValue shr 8) and UInt32($0000FF00)) or
    ((AValue shr 24) and UInt32($000000FF));
{$ENDIF FPC}
  // Result := RotateRight32(AValue and UInt32($00FF00FF), 8) +
  // RotateLeft32(AValue and UInt32($FF00FF00), 8);
end;

class function TBits.ReverseBytesUInt64(AValue: UInt64): UInt64;
begin
{$IFDEF FPC}
  Result := SwapEndian(AValue);
{$ELSE}
  Result := ((AValue shl 56) and UInt64($FF00000000000000)) or
    ((AValue shl 40) and UInt64($00FF000000000000)) or
    ((AValue shl 24) and UInt64($0000FF0000000000)) or
    ((AValue shl 8) and UInt64($000000FF00000000)) or
    ((AValue shr 8) and UInt64($00000000FF000000)) or
    ((AValue shr 24) and UInt64($0000000000FF0000)) or
    ((AValue shr 40) and UInt64($000000000000FF00)) or
    ((AValue shr 56) and UInt64($00000000000000FF));
{$ENDIF FPC}
  // Result := (UInt64(ReverseBytesUInt32(UInt32(AValue))) shl 32) +
  // ReverseBytesUInt32(UInt32(AValue shr 32));
end;

end.
