unit HlpRIPEMD128;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpMDBase,
{$IFDEF DELPHI}
  HlpBitConverter,
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
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TRIPEMD128 }

function TRIPEMD128.Clone(): IHash;
var
  HashInstance: TRIPEMD128;
begin
  HashInstance := TRIPEMD128.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRIPEMD128.Create;
begin
  Inherited Create(4, 16);
end;

procedure TRIPEMD128.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  a, b, c, d, aa, bb, cc, dd: UInt32;
  data: array [0 .. 15] of UInt32;
begin

  TConverters.le32_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  a := Fm_state[0];
  b := Fm_state[1];
  c := Fm_state[2];
  d := Fm_state[3];
  aa := a;
  bb := b;
  cc := c;
  dd := d;

  a := a + (data[0] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 11);
  d := d + (data[1] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 14);
  c := c + (data[2] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 15);
  b := b + (data[3] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 12);
  a := a + (data[4] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 5);
  d := d + (data[5] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 8);
  c := c + (data[6] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 7);
  b := b + (data[7] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 9);
  a := a + (data[8] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 11);
  d := d + (data[9] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 13);
  c := c + (data[10] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 14);
  b := b + (data[11] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[12] + (b xor c xor d));
  a := TBits.RotateLeft32(a, 6);
  d := d + (data[13] + (a xor b xor c));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[14] + (d xor a xor b));
  c := TBits.RotateLeft32(c, 9);
  b := b + (data[15] + (c xor d xor a));
  b := TBits.RotateLeft32(b, 8);

  a := a + (data[7] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 7);
  d := d + (data[4] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 6);
  c := c + (data[13] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 8);
  b := b + (data[1] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (data[10] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 11);
  d := d + (data[6] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[15] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 7);
  b := b + (data[3] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[12] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 7);
  d := d + (data[0] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 12);
  c := c + (data[9] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 15);
  b := b + (data[5] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 9);
  a := a + (data[2] + C2 + ((b and c) or (not b and d)));
  a := TBits.RotateLeft32(a, 11);
  d := d + (data[14] + C2 + ((a and b) or (not a and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[11] + C2 + ((d and a) or (not d and b)));
  c := TBits.RotateLeft32(c, 13);
  b := b + (data[8] + C2 + ((c and d) or (not c and a)));
  b := TBits.RotateLeft32(b, 12);

  a := a + (data[3] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 11);
  d := d + (data[10] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 13);
  c := c + (data[14] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 6);
  b := b + (data[4] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 7);
  a := a + (data[9] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 14);
  d := d + (data[15] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[8] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 13);
  b := b + (data[1] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[2] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 14);
  d := d + (data[7] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 8);
  c := c + (data[0] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 13);
  b := b + (data[6] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 6);
  a := a + (data[13] + C4 + ((b or not c) xor d));
  a := TBits.RotateLeft32(a, 5);
  d := d + (data[11] + C4 + ((a or not b) xor c));
  d := TBits.RotateLeft32(d, 12);
  c := c + (data[5] + C4 + ((d or not a) xor b));
  c := TBits.RotateLeft32(c, 7);
  b := b + (data[12] + C4 + ((c or not d) xor a));
  b := TBits.RotateLeft32(b, 5);

  a := a + (data[1] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 11);
  d := d + (data[9] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 12);
  c := c + (data[11] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 14);
  b := b + (data[10] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[0] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 14);
  d := d + (data[8] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 15);
  c := c + (data[12] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (data[4] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 8);
  a := a + (data[13] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 9);
  d := d + (data[3] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 14);
  c := c + (data[7] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 5);
  b := b + (data[15] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 6);
  a := a + (data[14] + C6 + ((b and d) or (c and not d)));
  a := TBits.RotateLeft32(a, 8);
  d := d + (data[5] + C6 + ((a and c) or (b and not c)));
  d := TBits.RotateLeft32(d, 6);
  c := c + (data[6] + C6 + ((d and b) or (a and not b)));
  c := TBits.RotateLeft32(c, 5);
  b := b + (data[2] + C6 + ((c and a) or (d and not a)));
  b := TBits.RotateLeft32(b, 12);

  aa := aa + (data[5] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 8);
  dd := dd + (data[14] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 9);
  cc := cc + (data[7] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (data[0] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (data[9] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 13);
  dd := dd + (data[2] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 15);
  cc := cc + (data[11] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (data[4] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 5);
  aa := aa + (data[13] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 7);
  dd := dd + (data[6] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (data[15] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 8);
  bb := bb + (data[8] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (data[1] + C1 + ((bb and dd) or (cc and not dd)));
  aa := TBits.RotateLeft32(aa, 14);
  dd := dd + (data[10] + C1 + ((aa and cc) or (bb and not cc)));
  dd := TBits.RotateLeft32(dd, 14);
  cc := cc + (data[3] + C1 + ((dd and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (data[12] + C1 + ((cc and aa) or (dd and not aa)));
  bb := TBits.RotateLeft32(bb, 6);

  aa := aa + (data[6] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 9);
  dd := dd + (data[11] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (data[3] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (data[7] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 7);
  aa := aa + (data[0] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 12);
  dd := dd + (data[13] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 8);
  cc := cc + (data[5] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (data[10] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (data[14] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 7);
  dd := dd + (data[15] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (data[8] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (data[12] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 7);
  aa := aa + (data[4] + C3 + ((bb or not cc) xor dd));
  aa := TBits.RotateLeft32(aa, 6);
  dd := dd + (data[9] + C3 + ((aa or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 15);
  cc := cc + (data[1] + C3 + ((dd or not aa) xor bb));
  cc := TBits.RotateLeft32(cc, 13);
  bb := bb + (data[2] + C3 + ((cc or not dd) xor aa));
  bb := TBits.RotateLeft32(bb, 11);

  aa := aa + (data[15] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 9);
  dd := dd + (data[5] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (data[1] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (data[3] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (data[7] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 8);
  dd := dd + (data[14] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 6);
  cc := cc + (data[6] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 6);
  bb := bb + (data[9] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 14);
  aa := aa + (data[11] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 12);
  dd := dd + (data[8] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (data[12] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 5);
  bb := bb + (data[2] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 14);
  aa := aa + (data[10] + C5 + ((bb and cc) or (not bb and dd)));
  aa := TBits.RotateLeft32(aa, 13);
  dd := dd + (data[0] + C5 + ((aa and bb) or (not aa and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (data[4] + C5 + ((dd and aa) or (not dd and bb)));
  cc := TBits.RotateLeft32(cc, 7);
  bb := bb + (data[13] + C5 + ((cc and dd) or (not cc and aa)));
  bb := TBits.RotateLeft32(bb, 5);

  aa := aa + (data[8] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 15);
  dd := dd + (data[6] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 5);
  cc := cc + (data[4] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 8);
  bb := bb + (data[1] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 11);
  aa := aa + (data[3] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 14);
  dd := dd + (data[11] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 14);
  cc := cc + (data[15] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 6);
  bb := bb + (data[0] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 14);
  aa := aa + (data[5] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 6);
  dd := dd + (data[12] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 9);
  cc := cc + (data[2] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (data[13] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 9);
  aa := aa + (data[9] + (bb xor cc xor dd));
  aa := TBits.RotateLeft32(aa, 12);
  dd := dd + (data[7] + (aa xor bb xor cc));
  dd := TBits.RotateLeft32(dd, 5);
  cc := cc + (data[10] + (dd xor aa xor bb));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (data[14] + (cc xor dd xor aa));
  bb := TBits.RotateLeft32(bb, 8);

  dd := dd + c + Fm_state[1];
  Fm_state[1] := Fm_state[2] + d + aa;
  Fm_state[2] := Fm_state[3] + a + bb;
  Fm_state[3] := Fm_state[0] + b + cc;
  Fm_state[0] := dd;

  System.FillChar(data, System.SizeOf(data), UInt32(0));

end;

end.
