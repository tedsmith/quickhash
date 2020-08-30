unit HlpMD4;

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
  TMD4 = class sealed(TMDBase, ITransformBlock)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TMD4 }

function TMD4.Clone(): IHash;
var
  LHashInstance: TMD4;
begin
  LHashInstance := TMD4.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMD4.Create;
begin
  Inherited Create(4, 16);
end;

procedure TMD4.TransformBlock(AData: PByte; ADataLength: Int32; AIndex: Int32);
var
  a, b, c, d: UInt32;
  LData: array [0 .. 15] of UInt32;
begin
  TConverters.le32_copy(AData, AIndex, @(LData[0]), 0, ADataLength);

  a := FState[0];
  b := FState[1];
  c := FState[2];
  d := FState[3];

  a := a + (LData[0] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[1] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[2] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[3] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);
  a := a + (LData[4] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[5] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[6] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[7] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);
  a := a + (LData[8] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[9] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[10] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[11] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);
  a := a + (LData[12] + ((b and c) or ((not b) and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[13] + ((a and b) or ((not a) and c)));
  d := TBits.RotateLeft32(d, 7);
  c := c + (LData[14] + ((d and a) or ((not d) and b)));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[15] + ((c and d) or ((not c) and a)));
  b := TBits.RotateLeft32(b, 19);

  a := a + (LData[0] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[4] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (LData[8] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (LData[12] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (LData[1] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[5] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (LData[9] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (LData[13] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (LData[2] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[6] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (LData[10] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (LData[14] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);
  a := a + (LData[3] + C2 + ((b and (c or d)) or (c and d)));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[7] + C2 + ((a and (b or c)) or (b and c)));
  d := TBits.RotateLeft32(d, 5);
  c := c + (LData[11] + C2 + ((d and (a or b)) or (a and b)));
  c := TBits.RotateLeft32(c, 9);
  b := b + (LData[15] + C2 + ((c and (d or a)) or (d and a)));
  b := TBits.RotateLeft32(b, 13);

  a := a + (LData[0] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[8] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[4] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[12] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[2] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[10] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[6] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[14] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[1] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[9] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[5] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[13] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);
  a := a + (LData[3] + C4 + (b xor c xor d));
  a := TBits.RotateLeft32(a, 3);
  d := d + (LData[11] + C4 + (a xor b xor c));
  d := TBits.RotateLeft32(d, 9);
  c := c + (LData[7] + C4 + (d xor a xor b));
  c := TBits.RotateLeft32(c, 11);
  b := b + (LData[15] + C4 + (c xor d xor a));
  b := TBits.RotateLeft32(b, 15);

  FState[0] := FState[0] + a;
  FState[1] := FState[1] + b;
  FState[2] := FState[2] + c;
  FState[3] := FState[3] + d;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
