unit HlpRIPEMD256;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpMDBase,
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo;

type
  TRIPEMD256 = class sealed(TMDBase, ITransformBlock)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TRIPEMD256 }

function TRIPEMD256.Clone(): IHash;
var
  LHashInstance: TRIPEMD256;
begin
  LHashInstance := TRIPEMD256.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRIPEMD256.Create;
begin
  Inherited Create(8, 32);
end;

procedure TRIPEMD256.Initialize;
begin
  FState[4] := $76543210;
  FState[5] := $FEDCBA98;
  FState[6] := $89ABCDEF;
  FState[7] := $01234567;
  Inherited Initialize();
end;

procedure TRIPEMD256.TransformBlock(AData: PByte; ADataLength: Int32;
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
  aa := FState[4];
  bb := FState[5];
  cc := FState[6];
  dd := FState[7];

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

  aa := aa + (LData[7] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 7);
  d := d + (LData[4] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 6);
  c := c + (LData[13] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 8);
  b := b + (LData[1] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 13);
  aa := aa + (LData[10] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (LData[6] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[15] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 7);
  b := b + (LData[3] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 15);
  aa := aa + (LData[12] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 7);
  d := d + (LData[0] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 12);
  c := c + (LData[9] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 15);
  b := b + (LData[5] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 9);
  aa := aa + (LData[2] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (LData[14] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[11] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 13);
  b := b + (LData[8] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 12);

  a := a + (LData[6] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 9);
  dd := dd + (LData[11] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (LData[3] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (LData[7] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 7);
  a := a + (LData[0] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 12);
  dd := dd + (LData[13] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 8);
  cc := cc + (LData[5] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (LData[10] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 11);
  a := a + (LData[14] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 7);
  dd := dd + (LData[15] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (LData[8] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (LData[12] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 7);
  a := a + (LData[4] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 6);
  dd := dd + (LData[9] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 15);
  cc := cc + (LData[1] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 13);
  bb := bb + (LData[2] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 11);

  aa := aa + (LData[3] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (LData[10] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 13);
  c := c + (LData[14] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 6);
  bb := bb + (LData[4] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 7);
  aa := aa + (LData[9] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 14);
  d := d + (LData[15] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[8] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 13);
  bb := bb + (LData[1] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 15);
  aa := aa + (LData[2] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 14);
  d := d + (LData[7] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 8);
  c := c + (LData[0] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 13);
  bb := bb + (LData[6] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 6);
  aa := aa + (LData[13] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 5);
  d := d + (LData[11] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 12);
  c := c + (LData[5] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 7);
  bb := bb + (LData[12] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 5);

  a := a + (LData[15] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 9);
  dd := dd + (LData[5] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (LData[1] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 15);
  b := b + (LData[3] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 11);
  a := a + (LData[7] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 8);
  dd := dd + (LData[14] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 6);
  cc := cc + (LData[6] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 6);
  b := b + (LData[9] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 14);
  a := a + (LData[11] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 12);
  dd := dd + (LData[8] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (LData[12] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 5);
  b := b + (LData[2] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 14);
  a := a + (LData[10] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 13);
  dd := dd + (LData[0] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (LData[4] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 7);
  b := b + (LData[13] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 5);

  aa := aa + (LData[1] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (LData[9] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 12);
  cc := cc + (LData[11] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 14);
  bb := bb + (LData[10] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 15);
  aa := aa + (LData[0] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 14);
  d := d + (LData[8] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 15);
  cc := cc + (LData[12] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (LData[4] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 8);
  aa := aa + (LData[13] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 9);
  d := d + (LData[3] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 14);
  cc := cc + (LData[7] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 5);
  bb := bb + (LData[15] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 6);
  aa := aa + (LData[14] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 8);
  d := d + (LData[5] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 6);
  cc := cc + (LData[6] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 5);
  bb := bb + (LData[2] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 12);

  a := a + (LData[8] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 15);
  dd := dd + (LData[6] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 5);
  c := c + (LData[4] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 8);
  b := b + (LData[1] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 11);
  a := a + (LData[3] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 14);
  dd := dd + (LData[11] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 14);
  c := c + (LData[15] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 6);
  b := b + (LData[0] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 14);
  a := a + (LData[5] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 6);
  dd := dd + (LData[12] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 9);
  c := c + (LData[2] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 12);
  b := b + (LData[13] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 9);
  a := a + (LData[9] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 12);
  dd := dd + (LData[7] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 5);
  c := c + (LData[10] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 15);
  b := b + (LData[14] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 8);

  FState[0] := FState[0] + aa;
  FState[1] := FState[1] + bb;
  FState[2] := FState[2] + cc;
  FState[3] := FState[3] + dd;
  FState[4] := FState[4] + a;
  FState[5] := FState[5] + b;
  FState[6] := FState[6] + c;
  FState[7] := FState[7] + d;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
