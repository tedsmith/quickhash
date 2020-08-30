unit HlpRIPEMD128;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpMDBase,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpBits,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo;

type
  TRIPEMD128 = class sealed(TMDBase, ITransformBlock)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TRIPEMD128 }

function TRIPEMD128.Clone(): IHash;
var
  LHashInstance: TRIPEMD128;
begin
  LHashInstance := TRIPEMD128.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRIPEMD128.Create;
begin
  Inherited Create(4, 16);
end;

procedure TRIPEMD128.TransformBlock(AData: PByte; ADataLength: Int32;
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

  a := a + (LData[0] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 11);
  d := d + (LData[1] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 14);
  c := c + (LData[2] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 15);
  b := b + (LData[3] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 12);
  a := a + (LData[4] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 5);
  d := d + (LData[5] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 8);
  c := c + (LData[6] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 7);
  b := b + (LData[7] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 9);
  a := a + (LData[8] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 11);
  d := d + (LData[9] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 13);
  c := c + (LData[10] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 14);
  b := b + (LData[11] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[12] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 6);
  d := d + (LData[13] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[14] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 9);
  b := b + (LData[15] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 8);

  a := a + (LData[7] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 7);
  d := d + (LData[4] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 6);
  c := c + (LData[13] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 8);
  b := b + (LData[1] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (LData[10] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 11);
  d := d + (LData[6] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[15] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 7);
  b := b + (LData[3] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[12] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 7);
  d := d + (LData[0] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 12);
  c := c + (LData[9] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 15);
  b := b + (LData[5] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 9);
  a := a + (LData[2] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 11);
  d := d + (LData[14] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[11] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 13);
  b := b + (LData[8] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 12);

  a := a + (LData[3] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 11);
  d := d + (LData[10] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 13);
  c := c + (LData[14] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 6);
  b := b + (LData[4] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 7);
  a := a + (LData[9] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 14);
  d := d + (LData[15] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[8] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 13);
  b := b + (LData[1] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[2] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 14);
  d := d + (LData[7] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 8);
  c := c + (LData[0] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 13);
  b := b + (LData[6] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 6);
  a := a + (LData[13] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 5);
  d := d + (LData[11] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 12);
  c := c + (LData[5] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 7);
  b := b + (LData[12] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 5);

  a := a + (LData[1] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 11);
  d := d + (LData[9] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 12);
  c := c + (LData[11] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 14);
  b := b + (LData[10] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[0] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 14);
  d := d + (LData[8] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 15);
  c := c + (LData[12] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (LData[4] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 8);
  a := a + (LData[13] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 9);
  d := d + (LData[3] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 14);
  c := c + (LData[7] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 5);
  b := b + (LData[15] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 6);
  a := a + (LData[14] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 8);
  d := d + (LData[5] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 6);
  c := c + (LData[6] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 5);
  b := b + (LData[2] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 12);

  aa := aa + (LData[5] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 8);
  dd := dd + (LData[14] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 9);
  cc := cc + (LData[7] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (LData[0] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (LData[9] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 13);
  dd := dd + (LData[2] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 15);
  cc := cc + (LData[11] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (LData[4] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 5);
  aa := aa + (LData[13] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 7);
  dd := dd + (LData[6] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (LData[15] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 8);
  bb := bb + (LData[8] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (LData[1] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 14);
  dd := dd + (LData[10] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 14);
  cc := cc + (LData[3] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (LData[12] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 6);

  aa := aa + (LData[6] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 9);
  dd := dd + (LData[11] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (LData[3] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (LData[7] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 7);
  aa := aa + (LData[0] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 12);
  dd := dd + (LData[13] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 8);
  cc := cc + (LData[5] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (LData[10] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (LData[14] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 7);
  dd := dd + (LData[15] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (LData[8] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (LData[12] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 7);
  aa := aa + (LData[4] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 6);
  dd := dd + (LData[9] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 15);
  cc := cc + (LData[1] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 13);
  bb := bb + (LData[2] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 11);

  aa := aa + (LData[15] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 9);
  dd := dd + (LData[5] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (LData[1] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (LData[3] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (LData[7] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 8);
  dd := dd + (LData[14] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 6);
  cc := cc + (LData[6] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 6);
  bb := bb + (LData[9] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 14);
  aa := aa + (LData[11] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 12);
  dd := dd + (LData[8] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (LData[12] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 5);
  bb := bb + (LData[2] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 14);
  aa := aa + (LData[10] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 13);
  dd := dd + (LData[0] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (LData[4] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 7);
  bb := bb + (LData[13] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 5);

  aa := aa + (LData[8] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 15);
  dd := dd + (LData[6] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 5);
  cc := cc + (LData[4] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 8);
  bb := bb + (LData[1] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (LData[3] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 14);
  dd := dd + (LData[11] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 14);
  cc := cc + (LData[15] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 6);
  bb := bb + (LData[0] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 14);
  aa := aa + (LData[5] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 6);
  dd := dd + (LData[12] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 9);
  cc := cc + (LData[2] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (LData[13] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 9);
  aa := aa + (LData[9] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 12);
  dd := dd + (LData[7] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 5);
  cc := cc + (LData[10] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (LData[14] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 8);

  dd := dd + c + FState[1];
  FState[1] := FState[2] + d + aa;
  FState[2] := FState[3] + a + bb;
  FState[3] := FState[0] + b + cc;
  FState[0] := dd;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
