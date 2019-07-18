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
  HlpBitConverter,
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo;

type
  TRIPEMD256 = class sealed(TMDBase, ITransformBlock)

  strict protected
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TRIPEMD256 }

function TRIPEMD256.Clone(): IHash;
var
  HashInstance: TRIPEMD256;
begin
  HashInstance := TRIPEMD256.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRIPEMD256.Create;
begin
  Inherited Create(8, 32);
end;

procedure TRIPEMD256.Initialize;
begin
  Fm_state[4] := $76543210;
  Fm_state[5] := $FEDCBA98;
  Fm_state[6] := $89ABCDEF;
  Fm_state[7] := $01234567;

  Inherited Initialize();

end;

procedure TRIPEMD256.TransformBlock(a_data: PByte; a_data_length: Int32;
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
  aa := Fm_state[4];
  bb := Fm_state[5];
  cc := Fm_state[6];
  dd := Fm_state[7];

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

  aa := aa + (data[7] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 7);
  d := d + (data[4] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 6);
  c := c + (data[13] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 8);
  b := b + (data[1] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 13);
  aa := aa + (data[10] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (data[6] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[15] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 7);
  b := b + (data[3] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 15);
  aa := aa + (data[12] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 7);
  d := d + (data[0] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 12);
  c := c + (data[9] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 15);
  b := b + (data[5] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 9);
  aa := aa + (data[2] + C2 + ((b and c) or (not b and d)));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (data[14] + C2 + ((aa and b) or (not aa and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[11] + C2 + ((d and aa) or (not d and b)));
  c := TBits.RotateLeft32(c, 13);
  b := b + (data[8] + C2 + ((c and d) or (not c and aa)));
  b := TBits.RotateLeft32(b, 12);

  a := a + (data[6] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 9);
  dd := dd + (data[11] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (data[3] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 15);
  bb := bb + (data[7] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 7);
  a := a + (data[0] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 12);
  dd := dd + (data[13] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 8);
  cc := cc + (data[5] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (data[10] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 11);
  a := a + (data[14] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 7);
  dd := dd + (data[15] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (data[8] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 12);
  bb := bb + (data[12] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 7);
  a := a + (data[4] + C3 + ((bb or not cc) xor dd));
  a := TBits.RotateLeft32(a, 6);
  dd := dd + (data[9] + C3 + ((a or not bb) xor cc));
  dd := TBits.RotateLeft32(dd, 15);
  cc := cc + (data[1] + C3 + ((dd or not a) xor bb));
  cc := TBits.RotateLeft32(cc, 13);
  bb := bb + (data[2] + C3 + ((cc or not dd) xor a));
  bb := TBits.RotateLeft32(bb, 11);

  aa := aa + (data[3] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (data[10] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 13);
  c := c + (data[14] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 6);
  bb := bb + (data[4] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 7);
  aa := aa + (data[9] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 14);
  d := d + (data[15] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[8] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 13);
  bb := bb + (data[1] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 15);
  aa := aa + (data[2] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 14);
  d := d + (data[7] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 8);
  c := c + (data[0] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 13);
  bb := bb + (data[6] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 6);
  aa := aa + (data[13] + C4 + ((bb or not c) xor d));
  aa := TBits.RotateLeft32(aa, 5);
  d := d + (data[11] + C4 + ((aa or not bb) xor c));
  d := TBits.RotateLeft32(d, 12);
  c := c + (data[5] + C4 + ((d or not aa) xor bb));
  c := TBits.RotateLeft32(c, 7);
  bb := bb + (data[12] + C4 + ((c or not d) xor aa));
  bb := TBits.RotateLeft32(bb, 5);

  a := a + (data[15] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 9);
  dd := dd + (data[5] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 7);
  cc := cc + (data[1] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 15);
  b := b + (data[3] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 11);
  a := a + (data[7] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 8);
  dd := dd + (data[14] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 6);
  cc := cc + (data[6] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 6);
  b := b + (data[9] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 14);
  a := a + (data[11] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 12);
  dd := dd + (data[8] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (data[12] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 5);
  b := b + (data[2] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 14);
  a := a + (data[10] + C5 + ((b and cc) or (not b and dd)));
  a := TBits.RotateLeft32(a, 13);
  dd := dd + (data[0] + C5 + ((a and b) or (not a and cc)));
  dd := TBits.RotateLeft32(dd, 13);
  cc := cc + (data[4] + C5 + ((dd and a) or (not dd and b)));
  cc := TBits.RotateLeft32(cc, 7);
  b := b + (data[13] + C5 + ((cc and dd) or (not cc and a)));
  b := TBits.RotateLeft32(b, 5);

  aa := aa + (data[1] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 11);
  d := d + (data[9] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 12);
  cc := cc + (data[11] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 14);
  bb := bb + (data[10] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 15);
  aa := aa + (data[0] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 14);
  d := d + (data[8] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 15);
  cc := cc + (data[12] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 9);
  bb := bb + (data[4] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 8);
  aa := aa + (data[13] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 9);
  d := d + (data[3] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 14);
  cc := cc + (data[7] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 5);
  bb := bb + (data[15] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 6);
  aa := aa + (data[14] + C6 + ((bb and d) or (cc and not d)));
  aa := TBits.RotateLeft32(aa, 8);
  d := d + (data[5] + C6 + ((aa and cc) or (bb and not cc)));
  d := TBits.RotateLeft32(d, 6);
  cc := cc + (data[6] + C6 + ((d and bb) or (aa and not bb)));
  cc := TBits.RotateLeft32(cc, 5);
  bb := bb + (data[2] + C6 + ((cc and aa) or (d and not aa)));
  bb := TBits.RotateLeft32(bb, 12);

  a := a + (data[8] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 15);
  dd := dd + (data[6] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 5);
  c := c + (data[4] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 8);
  b := b + (data[1] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 11);
  a := a + (data[3] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 14);
  dd := dd + (data[11] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 14);
  c := c + (data[15] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 6);
  b := b + (data[0] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 14);
  a := a + (data[5] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 6);
  dd := dd + (data[12] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 9);
  c := c + (data[2] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 12);
  b := b + (data[13] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 9);
  a := a + (data[9] + (b xor c xor dd));
  a := TBits.RotateLeft32(a, 12);
  dd := dd + (data[7] + (a xor b xor c));
  dd := TBits.RotateLeft32(dd, 5);
  c := c + (data[10] + (dd xor a xor b));
  c := TBits.RotateLeft32(c, 15);
  b := b + (data[14] + (c xor dd xor a));
  b := TBits.RotateLeft32(b, 8);

  Fm_state[0] := Fm_state[0] + aa;
  Fm_state[1] := Fm_state[1] + bb;
  Fm_state[2] := Fm_state[2] + cc;
  Fm_state[3] := Fm_state[3] + dd;
  Fm_state[4] := Fm_state[4] + a;
  Fm_state[5] := Fm_state[5] + b;
  Fm_state[6] := Fm_state[6] + c;
  Fm_state[7] := Fm_state[7] + d;

  System.FillChar(data, System.SizeOf(data), UInt32(0));

end;

end.
