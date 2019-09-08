unit HlpMD4;

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
  TMD4 = class sealed(TMDBase, ITransformBlock)

  strict protected
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TMD4 }

function TMD4.Clone(): IHash;
var
  HashInstance: TMD4;
begin
  HashInstance := TMD4.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMD4.Create;
begin
  Inherited Create(4, 16);
end;

procedure TMD4.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  a, b, c, d: UInt32;
  data: array [0 .. 15] of UInt32;
begin

  TConverters.le32_copy(a_data, a_index, @(data[0]), 0, a_data_length);

  a := Fm_state[0];
  b := Fm_state[1];
  c := Fm_state[2];
  d := Fm_state[3];

  a := a + (data[0] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[1] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[2] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[3] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);
  a := a + (data[4] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[5] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[6] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[7] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);
  a := a + (data[8] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[9] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[10] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[11] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);
  a := a + (data[12] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[13] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (data[14] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[15] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);

  a := a + (data[0] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[4] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (data[8] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (data[12] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (data[1] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[5] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (data[9] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (data[13] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (data[2] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[6] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (data[10] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (data[14] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (data[3] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[7] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (data[11] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (data[15] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);

  a := a + (data[0] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[8] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[4] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[12] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[2] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[10] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[6] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[14] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[1] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[9] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[5] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[13] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (data[3] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (data[11] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (data[7] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (data[15] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);

  Fm_state[0] := Fm_state[0] + a;
  Fm_state[1] := Fm_state[1] + b;
  Fm_state[2] := Fm_state[2] + c;
  Fm_state[3] := Fm_state[3] + d;

  System.FillChar(data, System.SizeOf(data), UInt32(0));
end;

end.
