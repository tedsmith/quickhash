unit HlpRIPEMD;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpBits,
{$IFDEF DELPHI}
  HlpBitConverter,
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
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TRIPEMD }

function TRIPEMD.Clone(): IHash;
var
  HashInstance: TRIPEMD;
begin
  HashInstance := TRIPEMD.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
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

procedure TRIPEMD.TransformBlock(a_data: PByte; a_data_length: Int32;
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

  a := TBits.RotateLeft32(P1(b, c, d) + a + data[0], 11);
  d := TBits.RotateLeft32(P1(a, b, c) + d + data[1], 14);
  c := TBits.RotateLeft32(P1(d, a, b) + c + data[2], 15);
  b := TBits.RotateLeft32(P1(c, d, a) + b + data[3], 12);
  a := TBits.RotateLeft32(P1(b, c, d) + a + data[4], 5);
  d := TBits.RotateLeft32(P1(a, b, c) + d + data[5], 8);
  c := TBits.RotateLeft32(P1(d, a, b) + c + data[6], 7);
  b := TBits.RotateLeft32(P1(c, d, a) + b + data[7], 9);
  a := TBits.RotateLeft32(P1(b, c, d) + a + data[8], 11);
  d := TBits.RotateLeft32(P1(a, b, c) + d + data[9], 13);
  c := TBits.RotateLeft32(P1(d, a, b) + c + data[10], 14);
  b := TBits.RotateLeft32(P1(c, d, a) + b + data[11], 15);
  a := TBits.RotateLeft32(P1(b, c, d) + a + data[12], 6);
  d := TBits.RotateLeft32(P1(a, b, c) + d + data[13], 7);
  c := TBits.RotateLeft32(P1(d, a, b) + c + data[14], 9);
  b := TBits.RotateLeft32(P1(c, d, a) + b + data[15], 8);

  a := TBits.RotateLeft32(P2(b, c, d) + a + data[7] + C2, 7);
  d := TBits.RotateLeft32(P2(a, b, c) + d + data[4] + C2, 6);
  c := TBits.RotateLeft32(P2(d, a, b) + c + data[13] + C2, 8);
  b := TBits.RotateLeft32(P2(c, d, a) + b + data[1] + C2, 13);
  a := TBits.RotateLeft32(P2(b, c, d) + a + data[10] + C2, 11);
  d := TBits.RotateLeft32(P2(a, b, c) + d + data[6] + C2, 9);
  c := TBits.RotateLeft32(P2(d, a, b) + c + data[15] + C2, 7);
  b := TBits.RotateLeft32(P2(c, d, a) + b + data[3] + C2, 15);
  a := TBits.RotateLeft32(P2(b, c, d) + a + data[12] + C2, 7);
  d := TBits.RotateLeft32(P2(a, b, c) + d + data[0] + C2, 12);
  c := TBits.RotateLeft32(P2(d, a, b) + c + data[9] + C2, 15);
  b := TBits.RotateLeft32(P2(c, d, a) + b + data[5] + C2, 9);
  a := TBits.RotateLeft32(P2(b, c, d) + a + data[14] + C2, 7);
  d := TBits.RotateLeft32(P2(a, b, c) + d + data[2] + C2, 11);
  c := TBits.RotateLeft32(P2(d, a, b) + c + data[11] + C2, 13);
  b := TBits.RotateLeft32(P2(c, d, a) + b + data[8] + C2, 12);

  a := TBits.RotateLeft32(P3(b, c, d) + a + data[3] + C4, 11);
  d := TBits.RotateLeft32(P3(a, b, c) + d + data[10] + C4, 13);
  c := TBits.RotateLeft32(P3(d, a, b) + c + data[2] + C4, 14);
  b := TBits.RotateLeft32(P3(c, d, a) + b + data[4] + C4, 7);
  a := TBits.RotateLeft32(P3(b, c, d) + a + data[9] + C4, 14);
  d := TBits.RotateLeft32(P3(a, b, c) + d + data[15] + C4, 9);
  c := TBits.RotateLeft32(P3(d, a, b) + c + data[8] + C4, 13);
  b := TBits.RotateLeft32(P3(c, d, a) + b + data[1] + C4, 15);
  a := TBits.RotateLeft32(P3(b, c, d) + a + data[14] + C4, 6);
  d := TBits.RotateLeft32(P3(a, b, c) + d + data[7] + C4, 8);
  c := TBits.RotateLeft32(P3(d, a, b) + c + data[0] + C4, 13);
  b := TBits.RotateLeft32(P3(c, d, a) + b + data[6] + C4, 6);
  a := TBits.RotateLeft32(P3(b, c, d) + a + data[11] + C4, 12);
  d := TBits.RotateLeft32(P3(a, b, c) + d + data[13] + C4, 5);
  c := TBits.RotateLeft32(P3(d, a, b) + c + data[5] + C4, 7);
  b := TBits.RotateLeft32(P3(c, d, a) + b + data[12] + C4, 5);

  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + data[0] + C1, 11);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + data[1] + C1, 14);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + data[2] + C1, 15);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + data[3] + C1, 12);
  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + data[4] + C1, 5);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + data[5] + C1, 8);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + data[6] + C1, 7);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + data[7] + C1, 9);
  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + data[8] + C1, 11);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + data[9] + C1, 13);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + data[10] + C1, 14);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + data[11] + C1, 15);
  aa := TBits.RotateLeft32(P1(bb, cc, dd) + aa + data[12] + C1, 6);
  dd := TBits.RotateLeft32(P1(aa, bb, cc) + dd + data[13] + C1, 7);
  cc := TBits.RotateLeft32(P1(dd, aa, bb) + cc + data[14] + C1, 9);
  bb := TBits.RotateLeft32(P1(cc, dd, aa) + bb + data[15] + C1, 8);

  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + data[7], 7);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + data[4], 6);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + data[13], 8);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + data[1], 13);
  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + data[10], 11);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + data[6], 9);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + data[15], 7);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + data[3], 15);
  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + data[12], 7);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + data[0], 12);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + data[9], 15);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + data[5], 9);
  aa := TBits.RotateLeft32(P2(bb, cc, dd) + aa + data[14], 7);
  dd := TBits.RotateLeft32(P2(aa, bb, cc) + dd + data[2], 11);
  cc := TBits.RotateLeft32(P2(dd, aa, bb) + cc + data[11], 13);
  bb := TBits.RotateLeft32(P2(cc, dd, aa) + bb + data[8], 12);

  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + data[3] + C3, 11);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + data[10] + C3, 13);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + data[2] + C3, 14);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + data[4] + C3, 7);
  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + data[9] + C3, 14);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + data[15] + C3, 9);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + data[8] + C3, 13);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + data[1] + C3, 15);
  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + data[14] + C3, 6);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + data[7] + C3, 8);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + data[0] + C3, 13);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + data[6] + C3, 6);
  aa := TBits.RotateLeft32(P3(bb, cc, dd) + aa + data[11] + C3, 12);
  dd := TBits.RotateLeft32(P3(aa, bb, cc) + dd + data[13] + C3, 5);
  cc := TBits.RotateLeft32(P3(dd, aa, bb) + cc + data[5] + C3, 7);
  bb := TBits.RotateLeft32(P3(cc, dd, aa) + bb + data[12] + C3, 5);

  cc := cc + Fm_state[0] + b;
  Fm_state[0] := Fm_state[1] + c + dd;
  Fm_state[1] := Fm_state[2] + d + aa;
  Fm_state[2] := Fm_state[3] + a + bb;
  Fm_state[3] := cc;

  System.FillChar(data, System.SizeOf(data), UInt32(0));

end;

end.
