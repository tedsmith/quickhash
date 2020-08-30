unit HlpRIPEMD;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpMDBase,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo;

type
  TRIPEMD = class sealed(TMDBase, ITransformBlock)

  strict private
    class function P1(a, b, c: UInt32): UInt32; static; inline;
    class function P2(a, b, c: UInt32): UInt32; static; inline;
    class function P3(a, b, c: UInt32): UInt32; static; inline;

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TRIPEMD }

function TRIPEMD.Clone(): IHash;
var
  LHashInstance: TRIPEMD;
begin
  LHashInstance := TRIPEMD.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRIPEMD.Create;
begin
  Inherited Create(4, 16);
end;

class function TRIPEMD.P1(a, b, c: UInt32): UInt32;
begin
  result := (a and b) or (not a and c);
end;

class function TRIPEMD.P2(a, b, c: UInt32): UInt32;
begin
  result := (a and b) or (a and c) or (b and c);
end;

class function TRIPEMD.P3(a, b, c: UInt32): UInt32;
begin
  result := a xor b xor c;
end;

procedure TRIPEMD.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
var
  a, b, c, d, aa, bb, cc, dd: UInt32;
  LData: array [0 .. 15] of UInt32;
begin
  TConverters.le32_copy(AData, AIndex, @(LData[0]), 0, ADataLength);

  a := FState[0];
  b := FState[1];
  c := FState[2];
  d := FState[3];
  aa := a;
  bb := b;
  cc := c;
  dd := d;

  a := TBits.RotateLeft32(P1(b, c, d) + a + LData[0], 11);
  d := TBits.RotateLeft32(P1(a, b, c) + d + LData[1], 14);
  c := TBits.RotateLeft32(P1(d, a, b) + c + LData[2], 15);
  b := TBits.RotateLeft32(P1(c, d, a) + b + LData[3], 12);
  a := TBits.RotateLeft32(P1(b, c, d) + a + LData[4], 5);
  d := TBits.RotateLeft32(P1(a, b, c) + d + LData[5], 8);
  c := TBits.RotateLeft32(P1(d, a, b) + c + LData[6], 7);
  b := TBits.RotateLeft32(P1(c, d, a) + b + LData[7], 9);
  a := TBits.RotateLeft32(P1(b, c, d) + a + LData[8], 11);
  d := TBits.RotateLeft32(P1(a, b, c) + d + LData[9], 13);
  c := TBits.RotateLeft32(P1(d, a, b) + c + LData[10], 14);
  b := TBits.RotateLeft32(P1(c, d, a) + b + LData[11], 15);
  a := TBits.RotateLeft32(P1(b, c, d) + a + LData[12], 6);
  d := TBits.RotateLeft32(P1(a, b, c) + d + LData[13], 7);
  c := TBits.RotateLeft32(P1(d, a, b) + c + LData[14], 9);
  b := TBits.RotateLeft32(P1(c, d, a) + b + LData[15], 8);

  a := TBits.RotateLeft32(P2(b, c, d) + a + LData[7] + C2, 7);
  d := TBits.RotateLeft32(P2(a, b, c) + d + LData[4] + C2, 6);
  c := TBits.RotateLeft32(P2(d, a, b) + c + LData[13] + C2, 8);
  b := TBits.RotateLeft32(P2(c, d, a) + b + LData[1] + C2, 13);
  a := TBits.RotateLeft32(P2(b, c, d) + a + LData[10] + C2, 11);
  d := TBits.RotateLeft32(P2(a, b, c) + d + LData[6] + C2, 9);
  c := TBits.RotateLeft32(P2(d, a, b) + c + LData[15] + C2, 7);
  b := TBits.RotateLeft32(P2(c, d, a) + b + LData[3] + C2, 15);
  a := TBits.RotateLeft32(P2(b, c, d) + a + LData[12] + C2, 7);
  d := TBits.RotateLeft32(P2(a, b, c) + d + LData[0] + C2, 12);
  c := TBits.RotateLeft32(P2(d, a, b) + c + LData[9] + C2, 15);
  b := TBits.RotateLeft32(P2(c, d, a) + b + LData[5] + C2, 9);
  a := TBits.RotateLeft32(P2(b, c, d) + a + LData[14] + C2, 7);
  d := TBits.RotateLeft32(P2(a, b, c) + d + LData[2] + C2, 11);
  c := TBits.RotateLeft32(P2(d, a, b) + c + LData[11] + C2, 13);
  b := TBits.RotateLeft32(P2(c, d, a) + b + LData[8] + C2, 12);

  a := TBits.RotateLeft32(P3(b, c, d) + a + LData[3] + C4, 11);
  d := TBits.RotateLeft32(P3(a, b, c) + d + LData[10] + C4, 13);
  c := TBits.RotateLeft32(P3(d, a, b) + c + LData[2] + C4, 14);
  b := TBits.RotateLeft32(P3(c, d, a) + b + LData[4] + C4, 7);
  a := TBits.RotateLeft32(P3(b, c, d) + a + LData[9] + C4, 14);
  d := TBits.RotateLeft32(P3(a, b, c) + d + LData[15] + C4, 9);
  c := TBits.RotateLeft32(P3(d, a, b) + c + LData[8] + C4, 13);
  b := TBits.RotateLeft32(P3(c, d, a) + b + LData[1] + C4, 15);
  a := TBits.RotateLeft32(P3(b, c, d) + a + LData[14] + C4, 6);
  d := TBits.RotateLeft32(P3(a, b, c) + d + LData[7] + C4, 8);
  c := TBits.RotateLeft32(P3(d, a, b) + c + LData[0] + C4, 13);
  b := TBits.RotateLeft32(P3(c, d, a) + b + LData[6] + C4, 6);
  a := TBits.RotateLeft32(P3(b, c, d) + a + LData[11] + C4, 12);
  d := TBits.RotateLeft32(P3(a, b, c) + d + LData[13] + C4, 5);
  c := TBits.RotateLeft32(P3(d, a, b) + c + LData[5] + C4, 7);
  b := TBits.RotateLeft32(P3(c, d, a) + b + LData[12] + C4, 5);

  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + LData[0] + C1, 11);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + LData[1] + C1, 14);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + LData[2] + C1, 15);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + LData[3] + C1, 12);
  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + LData[4] + C1, 5);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + LData[5] + C1, 8);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + LData[6] + C1, 7);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + LData[7] + C1, 9);
  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + LData[8] + C1, 11);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + LData[9] + C1, 13);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + LData[10] + C1, 14);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + LData[11] + C1, 15);
  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + LData[12] + C1, 6);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + LData[13] + C1, 7);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + LData[14] + C1, 9);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + LData[15] + C1, 8);

  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + LData[7], 7);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + LData[4], 6);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + LData[13], 8);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + LData[1], 13);
  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + LData[10], 11);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + LData[6], 9);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + LData[15], 7);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + LData[3], 15);
  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + LData[12], 7);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + LData[0], 12);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + LData[9], 15);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + LData[5], 9);
  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + LData[14], 7);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + LData[2], 11);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + LData[11], 13);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + LData[8], 12);

  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + LData[3] + C3, 11);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + LData[10] + C3, 13);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + LData[2] + C3, 14);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + LData[4] + C3, 7);
  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + LData[9] + C3, 14);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + LData[15] + C3, 9);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + LData[8] + C3, 13);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + LData[1] + C3, 15);
  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + LData[14] + C3, 6);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + LData[7] + C3, 8);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + LData[0] + C3, 13);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + LData[6] + C3, 6);
  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + LData[11] + C3, 12);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + LData[13] + C3, 5);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + LData[5] + C3, 7);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + LData[12] + C3, 5);

  cc := cc + FState[0] + b;
  FState[0] := FState[1] + c + dd;
  FState[1] := FState[2] + d + aa;
  FState[2] := FState[3] + a + bb;
  FState[3] := cc;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
