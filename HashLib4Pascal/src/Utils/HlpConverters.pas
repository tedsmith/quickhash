unit HlpConverters;

{$I ..\Include\HashLib.inc}

interface

uses
  Classes,
  StrUtils,
  SysUtils,
  HlpHashLibTypes,
  HlpBits,
  HlpBitConverter;

resourcestring
  SEncodingInstanceNil = 'Encoding Instance Cannot Be Nil';

type
  TConverters = class sealed(TObject)

  strict private

    class procedure swap_copy_str_to_u32(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32); static;

    class procedure swap_copy_str_to_u64(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32); static;

  public

    class function be2me_32(AInput: UInt32): UInt32; static; inline;

    class function be2me_64(AInput: UInt64): UInt64; static; inline;

    class function le2me_32(AInput: UInt32): UInt32; static; inline;

    class function le2me_64(AInput: UInt64): UInt64; static; inline;

    class procedure be32_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure le32_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure be64_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure le64_copy(ASource: Pointer; ASourceIndex: Int32;
      ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
      static; inline;

    class procedure ReadUInt32AsBytesLE(AInput: UInt32;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class procedure ReadUInt32AsBytesBE(AInput: UInt32;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class procedure ReadUInt64AsBytesLE(AInput: UInt64;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class procedure ReadUInt64AsBytesBE(AInput: UInt64;
      const AOutput: THashLibByteArray; AIndex: Int32); overload;
      static; inline;

    class function ReadPCardinalAsUInt32(AInput: PCardinal): UInt32;
      static; inline;

    class function ReadPUInt64AsUInt64(AInput: PUInt64): UInt64; static; inline;

    class function ReadPCardinalAsUInt32LE(AInput: PCardinal): UInt32;
      static; inline;

    class function ReadPUInt64AsUInt64LE(AInput: PUInt64): UInt64;
      static; inline;

    class function ReadPCardinalAsUInt32BE(AInput: PCardinal): UInt32;
      static; inline;

    class function ReadPUInt64AsUInt64BE(AInput: PUInt64): UInt64;
      static; inline;

    class function ReadBytesAsUInt32LE(AInput: PByte; AIndex: Int32): UInt32;
      static; inline;

    class function ReadBytesAsUInt64LE(AInput: PByte; AIndex: Int32): UInt64;
      static; inline;

    class function ReadBytesAsUInt32BE(AInput: PByte; AIndex: Int32): UInt32;
      static; inline;

    class function ReadBytesAsUInt64BE(AInput: PByte; AIndex: Int32): UInt64;
      static; inline;

    class function ReadUInt32AsBytesLE(AInput: UInt32): THashLibByteArray;
      overload; static; inline;

    class function ReadUInt64AsBytesLE(AInput: UInt64): THashLibByteArray;
      overload; static; inline;

    class function ConvertStringToBytes(const AInput: String;
      const AEncoding: TEncoding): THashLibByteArray; overload; static;

    class function ConvertBytesToString(const AInput: THashLibByteArray;
      const AEncoding: TEncoding): String; overload; static;

    class function ConvertHexStringToBytes(const AInput: String)
      : THashLibByteArray; static; inline;

    class function ConvertBytesToHexString(const AInput: THashLibByteArray;
      AGroup: Boolean): String; static;

  end;

implementation

{ TConverters }

class procedure TConverters.swap_copy_str_to_u32(ASource: Pointer;
  ASourceIndex: Int32; ADestination: Pointer; ADestinationIndex: Int32;
  ASize: Int32);
var
  LPtrSourceStart, LPtrDestinationStart, LPtrSourceEnd: PCardinal;
  LPtrByteSourceStart: PByte;
  LLength: Int32;
begin
  // if all pointers and length are 32-bits aligned
  if ((Int32(PByte(ADestination) - PByte(0)) or (PByte(ASource) - PByte(0)) or
    ASourceIndex or ADestinationIndex or ASize) and 3) = 0 then
  begin
    // copy memory as 32-bit words
    LPtrSourceStart := PCardinal(PByte(ASource) + ASourceIndex);
    LPtrSourceEnd := PCardinal((PByte(ASource) + ASourceIndex) + ASize);
    LPtrDestinationStart := PCardinal(PByte(ADestination) + ADestinationIndex);
    while LPtrSourceStart < LPtrSourceEnd do
    begin
      LPtrDestinationStart^ := TBits.ReverseBytesUInt32(LPtrSourceStart^);
      System.Inc(LPtrDestinationStart);
      System.Inc(LPtrSourceStart);
    end;
  end
  else
  begin
    LPtrByteSourceStart := (PByte(ASource) + ASourceIndex);
    LLength := ASize + ADestinationIndex;
    while ADestinationIndex < LLength do
    begin
      PByte(ADestination)[ADestinationIndex xor 3] := LPtrByteSourceStart^;
      System.Inc(LPtrByteSourceStart);
      System.Inc(ADestinationIndex);
    end;
  end;
end;

class procedure TConverters.swap_copy_str_to_u64(ASource: Pointer;
  ASourceIndex: Int32; ADestination: Pointer; ADestinationIndex: Int32;
  ASize: Int32);
var
  LPtrSourceStart, LPtrDestinationStart, LPtrSourceEnd: PUInt64;
  LPtrByteSourceStart: PByte;
  LLength: Int32;
begin
  // if all pointers and length are 64-bits aligned
  if ((Int32(PByte(ADestination) - PByte(0)) or (PByte(ASource) - PByte(0)) or
    ASourceIndex or ADestinationIndex or ASize) and 7) = 0 then
  begin
    // copy aligned memory block as 64-bit integers
    LPtrSourceStart := PUInt64(PByte(ASource) + ASourceIndex);
    LPtrSourceEnd := PUInt64((PByte(ASource) + ASourceIndex) + ASize);
    LPtrDestinationStart := PUInt64(PByte(ADestination) + ADestinationIndex);
    while LPtrSourceStart < LPtrSourceEnd do
    begin
      LPtrDestinationStart^ := TBits.ReverseBytesUInt64(LPtrSourceStart^);
      System.Inc(LPtrDestinationStart);
      System.Inc(LPtrSourceStart);
    end;
  end
  else
  begin
    LPtrByteSourceStart := (PByte(ASource) + ASourceIndex);
    LLength := ASize + ADestinationIndex;
    while ADestinationIndex < LLength do
    begin
      PByte(ADestination)[ADestinationIndex xor 7] := LPtrByteSourceStart^;
      System.Inc(LPtrByteSourceStart);
      System.Inc(ADestinationIndex);
    end;
  end;
end;

class function TConverters.be2me_32(AInput: UInt32): UInt32;
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  result := TBits.ReverseBytesUInt32(AInput);
{$ELSE}
  result := AInput;
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class function TConverters.be2me_64(AInput: UInt64): UInt64;
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  result := TBits.ReverseBytesUInt64(AInput);
{$ELSE}
  result := AInput;
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.be32_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  swap_copy_str_to_u32(ASource, ASourceIndex, ADestination,
    ADestinationIndex, ASize)
{$ELSE}
  System.Move(Pointer(PByte(ASource) + ASourceIndex)^,
    Pointer(PByte(ADestination) + ADestinationIndex)^, ASize);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.be64_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  swap_copy_str_to_u64(ASource, ASourceIndex, ADestination,
    ADestinationIndex, ASize)
{$ELSE}
  System.Move(Pointer(PByte(ASource) + ASourceIndex)^,
    Pointer(PByte(ADestination) + ADestinationIndex)^, ASize);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class function TConverters.le2me_32(AInput: UInt32): UInt32;
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  result := AInput;
{$ELSE}
  result := TBits.ReverseBytesUInt32(AInput);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class function TConverters.le2me_64(AInput: UInt64): UInt64;
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  result := AInput;
{$ELSE}
  result := TBits.ReverseBytesUInt64(AInput);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.le32_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  System.Move((PByte(ASource) + ASourceIndex)^,
    (PByte(ADestination) + ADestinationIndex)^, ASize)
{$ELSE}
  swap_copy_str_to_u32(ASource, ASourceIndex, ADestination,
    ADestinationIndex, ASize);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.le64_copy(ASource: Pointer; ASourceIndex: Int32;
  ADestination: Pointer; ADestinationIndex: Int32; ASize: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  System.Move((PByte(ASource) + ASourceIndex)^,
    (PByte(ADestination) + ADestinationIndex)^, ASize)
{$ELSE}
  swap_copy_str_to_u64(ASource, ASourceIndex, ADestination,
    ADestinationIndex, ASize);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.ReadUInt32AsBytesLE(AInput: UInt32;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
{$IFDEF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput, AOutput[AIndex], System.SizeOf(UInt32));
{$ELSE}
  PCardinal(PByte(AOutput) + AIndex)^ := AInput;
{$ENDIF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
{$ELSE}
  AOutput[AIndex] := Byte(AInput);
  AOutput[AIndex + 1] := Byte(AInput shr 8);
  AOutput[AIndex + 2] := Byte(AInput shr 16);
  AOutput[AIndex + 3] := Byte(AInput shr 24);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.ReadUInt32AsBytesBE(AInput: UInt32;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  AOutput[AIndex] := Byte(AInput shr 24);
  AOutput[AIndex + 1] := Byte(AInput shr 16);
  AOutput[AIndex + 2] := Byte(AInput shr 8);
  AOutput[AIndex + 3] := Byte(AInput);
{$ELSE}
{$IFDEF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput, AOutput[AIndex], System.SizeOf(UInt32));
{$ELSE}
  PCardinal(PByte(AOutput) + AIndex)^ := AInput;
{$ENDIF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.ReadUInt64AsBytesLE(AInput: UInt64;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
{$IFDEF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput, AOutput[AIndex], System.SizeOf(UInt64));
{$ELSE}
  PUInt64(PByte(AOutput) + AIndex)^ := AInput;
{$ENDIF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
{$ELSE}
  AOutput[AIndex] := Byte(AInput);
  AOutput[AIndex + 1] := Byte(AInput shr 8);
  AOutput[AIndex + 2] := Byte(AInput shr 16);
  AOutput[AIndex + 3] := Byte(AInput shr 24);
  AOutput[AIndex + 4] := Byte(AInput shr 32);
  AOutput[AIndex + 5] := Byte(AInput shr 40);
  AOutput[AIndex + 6] := Byte(AInput shr 48);
  AOutput[AIndex + 7] := Byte(AInput shr 56);
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class procedure TConverters.ReadUInt64AsBytesBE(AInput: UInt64;
  const AOutput: THashLibByteArray; AIndex: Int32);
begin
{$IFDEF HASHLIB_LITTLE_ENDIAN}
  AOutput[AIndex] := Byte(AInput shr 56);
  AOutput[AIndex + 1] := Byte(AInput shr 48);
  AOutput[AIndex + 2] := Byte(AInput shr 40);
  AOutput[AIndex + 3] := Byte(AInput shr 32);
  AOutput[AIndex + 4] := Byte(AInput shr 24);
  AOutput[AIndex + 5] := Byte(AInput shr 16);
  AOutput[AIndex + 6] := Byte(AInput shr 8);
  AOutput[AIndex + 7] := Byte(AInput);
{$ELSE}
{$IFDEF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput, AOutput[AIndex], System.SizeOf(UInt64));
{$ELSE}
  PUInt64(PByte(AOutput) + AIndex)^ := AInput;
{$ENDIF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
{$ENDIF HASHLIB_LITTLE_ENDIAN}
end;

class function TConverters.ReadPCardinalAsUInt32(AInput: PCardinal): UInt32;
begin
{$IFDEF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput^, result, System.SizeOf(UInt32));
{$ELSE}
  result := AInput^;
{$ENDIF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
end;

class function TConverters.ReadPUInt64AsUInt64(AInput: PUInt64): UInt64;
begin
{$IFDEF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
  System.Move(AInput^, result, System.SizeOf(UInt64));
{$ELSE}
  result := AInput^;
{$ENDIF HASHLIB_REQUIRES_PROPER_ALIGNMENT}
end;

class function TConverters.ReadPCardinalAsUInt32LE(AInput: PCardinal): UInt32;
begin
  result := le2me_32(ReadPCardinalAsUInt32(AInput));
end;

class function TConverters.ReadPUInt64AsUInt64LE(AInput: PUInt64): UInt64;
begin
  result := le2me_64(ReadPUInt64AsUInt64(AInput));
end;

class function TConverters.ReadPCardinalAsUInt32BE(AInput: PCardinal): UInt32;
begin
  result := be2me_32(ReadPCardinalAsUInt32(AInput));
end;

class function TConverters.ReadPUInt64AsUInt64BE(AInput: PUInt64): UInt64;
begin
  result := be2me_64(ReadPUInt64AsUInt64(AInput));
end;

class function TConverters.ReadBytesAsUInt32LE(AInput: PByte;
  AIndex: Int32): UInt32;
begin
  result := ReadPCardinalAsUInt32LE(PCardinal(AInput + AIndex));
  // while this below is slower, it's portable
  // result := (UInt32(AInput[AIndex])) or (UInt32(AInput[AIndex + 1]) shl 8) or
  // (UInt32(AInput[AIndex + 2]) shl 16) or (UInt32(AInput[AIndex + 3]) shl 24);
end;

class function TConverters.ReadBytesAsUInt64LE(AInput: PByte;
  AIndex: Int32): UInt64;
begin
  result := ReadPUInt64AsUInt64LE(PUInt64(AInput + AIndex));
  // while this below is slower, it's portable
  // result := (UInt64(AInput[AIndex])) or (UInt64(AInput[AIndex + 1]) shl 8) or
  // (UInt64(AInput[AIndex + 2]) shl 16) or (UInt64(AInput[AIndex + 3]) shl 24)
  // or (UInt64(AInput[AIndex + 4]) shl 32) or
  // (UInt64(AInput[AIndex + 5]) shl 40) or (UInt64(AInput[AIndex + 6]) shl 48)
  // or (UInt64(AInput[AIndex + 7]) shl 56);
end;

class function TConverters.ReadBytesAsUInt32BE(AInput: PByte;
  AIndex: Int32): UInt32;
begin
  result := ReadPCardinalAsUInt32BE(PCardinal(AInput + AIndex));
  // while this below is slower, it's portable
  // result := (UInt32(AInput[AIndex]) shl 24) or
  // (UInt32(AInput[AIndex + 1]) shl 16) or (UInt32(AInput[AIndex + 2]) shl 8) or
  // (UInt32(AInput[AIndex + 3]));
end;

class function TConverters.ReadBytesAsUInt64BE(AInput: PByte;
  AIndex: Int32): UInt64;
begin
  result := ReadPUInt64AsUInt64BE(PUInt64(AInput + AIndex));
  // while this below is slower, it's portable
  // result := (UInt64(AInput[AIndex]) shl 56) or
  // (UInt64(AInput[AIndex + 1]) shl 48) or (UInt64(AInput[AIndex + 2]) shl 40)
  // or (UInt64(AInput[AIndex + 3]) shl 32) or
  // (UInt64(AInput[AIndex + 4]) shl 24) or (UInt64(AInput[AIndex + 5]) shl 16)
  // or (UInt64(AInput[AIndex + 6]) shl 8) or (UInt64(AInput[AIndex + 7]));
end;

class function TConverters.ReadUInt32AsBytesLE(AInput: UInt32)
  : THashLibByteArray;
begin
  System.SetLength(result, System.SizeOf(UInt32));
  TConverters.ReadUInt32AsBytesLE(AInput, result, 0);
end;

class function TConverters.ReadUInt64AsBytesLE(AInput: UInt64)
  : THashLibByteArray;
begin
  System.SetLength(result, System.SizeOf(UInt64));
  TConverters.ReadUInt64AsBytesLE(AInput, result, 0);
end;

class function TConverters.ConvertBytesToHexString(const AInput
  : THashLibByteArray; AGroup: Boolean): String;
begin
  result := UpperCase(TBitConverter.ToString(AInput));

  if System.length(AInput) = 1 then
  begin
    Exit;
  end;

  if (AGroup) then
  begin
    Exit;
  end;

  result := StringReplace(result, '-', '', [rfIgnoreCase, rfReplaceAll]);
end;

class function TConverters.ConvertHexStringToBytes(const AInput: String)
  : THashLibByteArray;
var
  LInput: String;
begin
  LInput := AInput;
  LInput := StringReplace(LInput, '-', '', [rfIgnoreCase, rfReplaceAll]);

{$IFDEF DEBUG}
  System.Assert(System.length(LInput) and 1 = 0);
{$ENDIF DEBUG}
  System.SetLength(result, System.length(LInput) shr 1);

{$IFNDEF NEXTGEN}
  HexToBin(PChar(LInput), @result[0], System.length(result));
{$ELSE}
  HexToBin(PChar(LInput), 0, result, 0, System.length(LInput));
{$ENDIF !NEXTGEN}
end;

class function TConverters.ConvertStringToBytes(const AInput: String;
  const AEncoding: TEncoding): THashLibByteArray;
begin
  if AEncoding = Nil then
  begin
    raise EArgumentNilHashLibException.CreateRes(@SEncodingInstanceNil);
  end;

{$IFDEF FPC}
  result := AEncoding.GetBytes(UnicodeString(AInput));
{$ELSE}
  result := AEncoding.GetBytes(AInput);
{$ENDIF FPC}
end;

class function TConverters.ConvertBytesToString(const AInput: THashLibByteArray;
  const AEncoding: TEncoding): String;
begin
  if AEncoding = Nil then
  begin
    raise EArgumentNilHashLibException.CreateRes(@SEncodingInstanceNil);
  end;

{$IFDEF FPC}
  result := String(AEncoding.GetString(AInput));
{$ELSE}
  result := AEncoding.GetString(AInput);
{$ENDIF FPC}
end;

end.
