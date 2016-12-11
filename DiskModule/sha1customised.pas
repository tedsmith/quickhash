{
 This is a customised version of the FPC 'sha1' library.
 Two functions are different. 'SHA1Transform' and 'SHA1File'.

 The new SHA1File which forces FPC to use the Windows API 'CreateFile' function differently.
 

 The new SHA1Transform function is courtesy of EngKin from the FPC Lazarus forums.
 http://forum.lazarus.freepascal.org/index.php/topic,25081.msg151774.html#msg151774
 It is a much faster implementation of the necessaries and makes hashing around 40% quicker
 Thanks, EngKin!!
 TODO : Introduce a patch for the main FPC library

 I have created this unit as a seperate unit because one day, FPC will better support
 Unicode and hopefully the faster transform functions, at which stage I hope to
 be able to just switch it back to using the proper and maintained  unit(s).
}

unit sha1Customised;
{$mode objfpc}{$h+}

interface

{$ifdef windows}
// We add these uses clauses to allow conversion of Unicode on Windows.
// The default FPC libraries do not contain these clauses.
uses
   LazUTF8, Windows, SysUtils;
{$endif}

type
  TSHA1Digest = array[0..19] of Byte;

  TSHA1Context = record
    State: array[0..4] of Cardinal;
    Buffer: array[0..63] of Byte;
    BufCnt: PtrUInt;   { in current block, i.e. in range of 0..63 }
    Length: QWord;     { total count of bytes processed }
  end;

{ core }
procedure SHA1Init(out ctx: TSHA1Context);
procedure SHA1Transform(var ctx: TSHA1Context; Buf: Pointer);               // Fast, Freepascal version
//procedure SHA1Transform(var ctx: TSHA1Context; const Buf: Pointer);assembler; // Faster version, in assembly!
procedure SHA1Update(var ctx: TSHA1Context; const Buf; BufLen: PtrUInt);
procedure SHA1Final(var ctx: TSHA1Context; out Digest: TSHA1Digest);

{ auxiliary }
function SHA1String(const S: String): TSHA1Digest;
function SHA1Buffer(const Buf; BufLen: PtrUInt): TSHA1Digest;
function SHA1File(const Filename: String; const Bufsize: PtrUInt = 1024): TSHA1Digest;

{ helpers }
function SHA1Print(const Digest: TSHA1Digest): String;
function SHA1Match(const Digest1, Digest2: TSHA1Digest): Boolean;

implementation

// inverts the bytes of (Count div 4) cardinals from source to target.
procedure Invert(Source, Dest: Pointer; Count: PtrUInt);
var
  S: PByte;
  T: PCardinal;
  I: PtrUInt;
begin
  S := Source;
  T := Dest;
  for I := 1 to (Count div 4) do
  begin
    T^ := S[3] or (S[2] shl 8) or (S[1] shl 16) or (S[0] shl 24);
    inc(S,4);
    inc(T);
  end;
end;

procedure SHA1Init(out ctx: TSHA1Context);
begin
  FillChar(ctx, sizeof(TSHA1Context), 0);
  ctx.State[0] := $67452301;
  ctx.State[1] := $efcdab89;
  ctx.State[2] := $98badcfe;
  ctx.State[3] := $10325476;
  ctx.State[4] := $c3d2e1f0;
end;

const
  K20 = $5A827999;
  K40 = $6ED9EBA1;
  K60 = $8F1BBCDC;
  K80 = $CA62C1D6;

{ Customised version of SHA1Transform, in assembly, is here.
 The default Freepascal function (as of 2.6.4 at least)is much slower and
 is commented below. There is also the enhanced Pascal version below.
 A faster assembly coded version is available by uncommenting the two lines below
 and then commenting out the original and enhanced Pascal SHA1 transform routines below.
}
// {$ASMMODE Intel}                                // Uncomment this for assembly version
// {$I SHA1TransformEnhancedAssemblyCodei386.inc}  // Uncomment this for assembly version


// Enhanced SHA1Transform procedure, written in Freepascal but faster than the default version
procedure SHA1Transform(var ctx: TSHA1Context; Buf: Pointer);
var
  A, B, C, D, E, T: Cardinal;
  Data: array[0..15] of Cardinal;
begin
  A := ctx.State[0];
  B := ctx.State[1];
  C := ctx.State[2];
  D := ctx.State[3];
  E := ctx.State[4];
  Invert(Buf, @Data, 64);
{$push}
{$r-,q-}

{$I SHA1TransformEnhanced.pas}  // <----- This file contains the enhanced repeat loops - used for ease of maintenance though not as fast as assembly version of course

  Inc(ctx.State[0], A);
  Inc(ctx.State[1], B);
  Inc(ctx.State[2], C);
  Inc(ctx.State[3], D);
  Inc(ctx.State[4], E);
{$pop}
  Inc(ctx.Length,64);
end;

// The original native FPC SHA1Transorm procedure. Comment out procedures above
// to use the native FPC 2.6.4 SHA1Transform procedure below.

{procedure SHA1Transform(var ctx: TSHA1Context; Buf: Pointer);
var
  A, B, C, D, E, T: Cardinal;
  Data: array[0..15] of Cardinal;
  i: Integer;
begin
  A := ctx.State[0];
  B := ctx.State[1];
  C := ctx.State[2];
  D := ctx.State[3];
  E := ctx.State[4];
  Invert(Buf, @Data, 64);
{$push}
{$r-,q-}
  i := 0;
  repeat
    T := (B and C) or (not B and D) + K20 + E;
    E := D;
    D := C;
    C := rordword(B, 2);
    B := A;
    A := T + roldword(A, 5) + Data[i and 15];
    Data[i and 15] := roldword(Data[i and 15] xor Data[(i+2) and 15] xor Data[(i+8) and 15] xor Data[(i+13) and 15], 1);
    Inc(i);
  until i > 19;

  repeat
    T := (B xor C xor D) + K40 + E;
    E := D;
    D := C;
    C := rordword(B, 2);
    B := A;
    A := T + roldword(A, 5) + Data[i and 15];
    Data[i and 15] := roldword(Data[i and 15] xor Data[(i+2) and 15] xor Data[(i+8) and 15] xor Data[(i+13) and 15], 1);
    Inc(i);
  until i > 39;

  repeat
    T := (B and C) or (B and D) or (C and D) + K60 + E;
    E := D;
    D := C;
    C := rordword(B, 2);
    B := A;
    A := T + roldword(A, 5) + Data[i and 15];
    Data[i and 15] := roldword(Data[i and 15] xor Data[(i+2) and 15] xor Data[(i+8) and 15] xor Data[(i+13) and 15], 1);
    Inc(i);
  until i > 59;

  repeat
    T := (B xor C xor D) + K80 + E;
    E := D;
    D := C;
    C := rordword(B, 2);
    B := A;
    A := T + roldword(A, 5) + Data[i and 15];
    Data[i and 15] := roldword(Data[i and 15] xor Data[(i+2) and 15] xor Data[(i+8) and 15] xor Data[(i+13) and 15], 1);
    Inc(i);
  until i > 79;

  Inc(ctx.State[0], A);
  Inc(ctx.State[1], B);
  Inc(ctx.State[2], C);
  Inc(ctx.State[3], D);
  Inc(ctx.State[4], E);
{$pop}
  Inc(ctx.Length,64);
end;
}

procedure SHA1Update(var ctx: TSHA1Context; const Buf; BufLen: PtrUInt);
var
  Src: PByte;
  Num: PtrUInt;
begin
  if BufLen = 0 then
    Exit;

  Src := @Buf;
  Num := 0;

  // 1. Transform existing data in buffer
  if ctx.BufCnt > 0 then
  begin
    // 1.1 Try to fill buffer up to block size
    Num := 64 - ctx.BufCnt;
    if Num > BufLen then
      Num := BufLen;

    Move(Src^, ctx.Buffer[ctx.BufCnt], Num);
    Inc(ctx.BufCnt, Num);
    Inc(Src, Num);

    // 1.2 If buffer is filled, transform it
    if ctx.BufCnt = 64 then
    begin
      SHA1Transform(ctx, @ctx.Buffer);
      ctx.BufCnt := 0;
    end;
  end;

  // 2. Transform input data in 64-byte blocks
  Num := BufLen - Num;
  while Num >= 64 do
  begin
    SHA1Transform(ctx, Src);
    Inc(Src, 64);
    Dec(Num, 64);
  end;

  // 3. If there's less than 64 bytes left, add it to buffer
  if Num > 0 then
  begin
    ctx.BufCnt := Num;
    Move(Src^, ctx.Buffer, Num);
  end;
end;

const
  PADDING: array[0..63] of Byte =
    ($80,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
    );

procedure SHA1Final(var ctx: TSHA1Context; out Digest: TSHA1Digest);
var
  Length: QWord;
  Pads: Cardinal;
begin
  // 1. Compute length of the whole stream in bits
  Length := 8 * (ctx.Length + ctx.BufCnt);

  // 2. Append padding bits
  if ctx.BufCnt >= 56 then
    Pads := 120 - ctx.BufCnt
  else
    Pads := 56 - ctx.BufCnt;
  SHA1Update(ctx, PADDING, Pads);

  // 3. Append length of the stream (8 bytes)
  Length := NtoBE(Length);
  SHA1Update(ctx, Length, 8);

  // 4. Invert state to digest
  Invert(@ctx.State, @Digest, 20);
  FillChar(ctx, sizeof(TSHA1Context), 0);
end;

function SHA1String(const S: String): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, PChar(S)^, length(S));
  SHA1Final(Context, Result);
end;

function SHA1Buffer(const Buf; BufLen: PtrUInt): TSHA1Digest;
var
  Context: TSHA1Context;
begin
  SHA1Init(Context);
  SHA1Update(Context, buf, buflen);
  SHA1Final(Context, Result);
end;

// Customised version of SHA1File. It enables FPC to deal with the ANSI Code Pages
// used by Windows that are not Unicode\UTF8 transferable. The main change
// is in the ifdef statement that creates a different handle to the Windows file to default.

function SHA1File(const Filename: String; const Bufsize: PtrUInt): TSHA1Digest;
var
  F: File;
  Buf: Pchar;
  Context: TSHA1Context;
  Count: Cardinal;
  ofm: Longint;
  {$ifdef windows}
  ws: UnicodeString;
  security : TSecurityAttributes;
  {$endif windows}
begin
  SHA1Init(Context);
  Assign(F, Filename);
  {$push}{$i-}
  ofm := FileMode;
  FileMode := 0;
  Reset(F, 1);
  {$pop}


  {$ifdef windows}
  if IOResult <> 0 then
  begin
    ws := UTF8ToUTF16(FileName);
    security.nLength := Sizeof(TSecurityAttributes);
    security.bInheritHandle := true;
    security.lpSecurityDescriptor := nil;
    filerec(f).handle:=CreateFileW(@ws[1],GENERIC_READ, file_Share_Read, @security, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if filerec(f).handle = INVALID_HANDLE_VALUE then RaiseLastOSError;
  end;
  {$endif windows}

  if IOResult = 0 then
  begin
    GetMem(Buf, BufSize);
    repeat
      BlockRead(F, Buf^, Bufsize, Count);
      if Count > 0 then
        SHA1Update(Context, Buf^, Count);
    until Count < BufSize;
    FreeMem(Buf, BufSize);
    Close(F);
  end;

  SHA1Final(Context, Result);
  FileMode := ofm;
end;

const
  HexTbl: array[0..15] of char='0123456789abcdef';     // lowercase

function SHA1Print(const Digest: TSHA1Digest): String;
var
  I: Integer;
  P: PChar;
begin
  SetLength(Result, 40);
  P := Pointer(Result);
  for I := 0 to 19 do
  begin
    P[0] := HexTbl[(Digest[i] shr 4) and 15];
    P[1] := HexTbl[Digest[i] and 15];
    Inc(P,2);
  end;
end;

function SHA1Match(const Digest1, Digest2: TSHA1Digest): Boolean;
var
  A: array[0..4] of Cardinal absolute Digest1;
  B: array[0..4] of Cardinal absolute Digest2;
begin
  Result := (A[0] = B[0]) and (A[1] = B[1]) and (A[2] = B[2]) and (A[3] = B[3]) and (A[4] = B[4]);
end;

end.
