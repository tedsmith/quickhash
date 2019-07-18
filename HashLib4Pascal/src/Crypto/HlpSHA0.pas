unit HlpSHA0;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpBitConverter,
  HlpHash,
{$ENDIF DELPHI}
  HlpHashLibTypes,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpHashCryptoNotBuildIn;

type
  TSHA0 = class(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict protected

    Fm_state: THashLibUInt32Array;

{$REGION 'Consts'}

  const

    C1 = UInt32($5A827999);
    C2 = UInt32($6ED9EBA1);
    C3 = UInt32($8F1BBCDC);
    C4 = UInt32($CA62C1D6);

{$ENDREGION}
    procedure Finish(); override;
    procedure Expand(a_data: PCardinal); virtual;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TSHA0 }

function TSHA0.Clone(): IHash;
var
  HashInstance: TSHA0;
begin
  HashInstance := TSHA0.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA0.Create;
begin
  Inherited Create(20, 64);
  System.SetLength(Fm_state, 5);
end;

procedure TSHA0.Expand(a_data: PCardinal);
{$IFNDEF USE_UNROLLED_VARIANT}
var
  j: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
begin

{$IFDEF USE_UNROLLED_VARIANT}
  a_data[16] := ((a_data[16 - 3] xor a_data[16 - 8]) xor a_data[16 - 14])
    xor a_data[0];
  a_data[17] := ((a_data[17 - 3] xor a_data[17 - 8]) xor a_data[17 - 14])
    xor a_data[17 - 16];
  a_data[18] := ((a_data[18 - 3] xor a_data[18 - 8]) xor a_data[18 - 14])
    xor a_data[18 - 16];
  a_data[19] := ((a_data[19 - 3] xor a_data[19 - 8]) xor a_data[19 - 14])
    xor a_data[19 - 16];
  a_data[20] := ((a_data[20 - 3] xor a_data[20 - 8]) xor a_data[20 - 14])
    xor a_data[20 - 16];
  a_data[21] := ((a_data[21 - 3] xor a_data[21 - 8]) xor a_data[21 - 14])
    xor a_data[21 - 16];
  a_data[22] := ((a_data[22 - 3] xor a_data[22 - 8]) xor a_data[22 - 14])
    xor a_data[22 - 16];
  a_data[23] := ((a_data[23 - 3] xor a_data[23 - 8]) xor a_data[23 - 14])
    xor a_data[23 - 16];
  a_data[24] := ((a_data[24 - 3] xor a_data[24 - 8]) xor a_data[24 - 14])
    xor a_data[24 - 16];
  a_data[25] := ((a_data[25 - 3] xor a_data[25 - 8]) xor a_data[25 - 14])
    xor a_data[25 - 16];
  a_data[26] := ((a_data[26 - 3] xor a_data[26 - 8]) xor a_data[26 - 14])
    xor a_data[26 - 16];
  a_data[27] := ((a_data[27 - 3] xor a_data[27 - 8]) xor a_data[27 - 14])
    xor a_data[27 - 16];
  a_data[28] := ((a_data[28 - 3] xor a_data[28 - 8]) xor a_data[28 - 14])
    xor a_data[28 - 16];
  a_data[29] := ((a_data[29 - 3] xor a_data[29 - 8]) xor a_data[29 - 14])
    xor a_data[29 - 16];
  a_data[30] := ((a_data[30 - 3] xor a_data[30 - 8]) xor a_data[30 - 14])
    xor a_data[30 - 16];
  a_data[31] := ((a_data[31 - 3] xor a_data[31 - 8]) xor a_data[31 - 14])
    xor a_data[31 - 16];
  a_data[32] := ((a_data[32 - 3] xor a_data[32 - 8]) xor a_data[32 - 14])
    xor a_data[32 - 16];
  a_data[33] := ((a_data[33 - 3] xor a_data[33 - 8]) xor a_data[33 - 14])
    xor a_data[33 - 16];
  a_data[34] := ((a_data[34 - 3] xor a_data[34 - 8]) xor a_data[34 - 14])
    xor a_data[34 - 16];
  a_data[35] := ((a_data[35 - 3] xor a_data[35 - 8]) xor a_data[35 - 14])
    xor a_data[35 - 16];
  a_data[36] := ((a_data[36 - 3] xor a_data[36 - 8]) xor a_data[36 - 14])
    xor a_data[36 - 16];
  a_data[37] := ((a_data[37 - 3] xor a_data[37 - 8]) xor a_data[37 - 14])
    xor a_data[37 - 16];
  a_data[38] := ((a_data[38 - 3] xor a_data[38 - 8]) xor a_data[38 - 14])
    xor a_data[38 - 16];
  a_data[39] := ((a_data[39 - 3] xor a_data[39 - 8]) xor a_data[39 - 14])
    xor a_data[39 - 16];
  a_data[40] := ((a_data[40 - 3] xor a_data[40 - 8]) xor a_data[40 - 14])
    xor a_data[40 - 16];
  a_data[41] := ((a_data[41 - 3] xor a_data[41 - 8]) xor a_data[41 - 14])
    xor a_data[41 - 16];
  a_data[42] := ((a_data[42 - 3] xor a_data[42 - 8]) xor a_data[42 - 14])
    xor a_data[42 - 16];
  a_data[43] := ((a_data[43 - 3] xor a_data[43 - 8]) xor a_data[43 - 14])
    xor a_data[43 - 16];
  a_data[44] := ((a_data[44 - 3] xor a_data[44 - 8]) xor a_data[44 - 14])
    xor a_data[44 - 16];
  a_data[45] := ((a_data[45 - 3] xor a_data[45 - 8]) xor a_data[45 - 14])
    xor a_data[45 - 16];
  a_data[46] := ((a_data[46 - 3] xor a_data[46 - 8]) xor a_data[46 - 14])
    xor a_data[46 - 16];
  a_data[47] := ((a_data[47 - 3] xor a_data[47 - 8]) xor a_data[47 - 14])
    xor a_data[47 - 16];
  a_data[48] := ((a_data[48 - 3] xor a_data[48 - 8]) xor a_data[48 - 14])
    xor a_data[48 - 16];
  a_data[49] := ((a_data[49 - 3] xor a_data[49 - 8]) xor a_data[49 - 14])
    xor a_data[49 - 16];
  a_data[50] := ((a_data[50 - 3] xor a_data[50 - 8]) xor a_data[50 - 14])
    xor a_data[50 - 16];
  a_data[51] := ((a_data[51 - 3] xor a_data[51 - 8]) xor a_data[51 - 14])
    xor a_data[51 - 16];
  a_data[52] := ((a_data[52 - 3] xor a_data[52 - 8]) xor a_data[52 - 14])
    xor a_data[52 - 16];
  a_data[53] := ((a_data[53 - 3] xor a_data[53 - 8]) xor a_data[53 - 14])
    xor a_data[53 - 16];
  a_data[54] := ((a_data[54 - 3] xor a_data[54 - 8]) xor a_data[54 - 14])
    xor a_data[54 - 16];
  a_data[55] := ((a_data[55 - 3] xor a_data[55 - 8]) xor a_data[55 - 14])
    xor a_data[55 - 16];
  a_data[56] := ((a_data[56 - 3] xor a_data[56 - 8]) xor a_data[56 - 14])
    xor a_data[56 - 16];
  a_data[57] := ((a_data[57 - 3] xor a_data[57 - 8]) xor a_data[57 - 14])
    xor a_data[57 - 16];
  a_data[58] := ((a_data[58 - 3] xor a_data[58 - 8]) xor a_data[58 - 14])
    xor a_data[58 - 16];
  a_data[59] := ((a_data[59 - 3] xor a_data[59 - 8]) xor a_data[59 - 14])
    xor a_data[59 - 16];
  a_data[60] := ((a_data[60 - 3] xor a_data[60 - 8]) xor a_data[60 - 14])
    xor a_data[60 - 16];
  a_data[61] := ((a_data[61 - 3] xor a_data[61 - 8]) xor a_data[61 - 14])
    xor a_data[61 - 16];
  a_data[62] := ((a_data[62 - 3] xor a_data[62 - 8]) xor a_data[62 - 14])
    xor a_data[62 - 16];
  a_data[63] := ((a_data[63 - 3] xor a_data[63 - 8]) xor a_data[63 - 14])
    xor a_data[63 - 16];
  a_data[64] := ((a_data[64 - 3] xor a_data[64 - 8]) xor a_data[64 - 14])
    xor a_data[64 - 16];
  a_data[65] := ((a_data[65 - 3] xor a_data[65 - 8]) xor a_data[65 - 14])
    xor a_data[65 - 16];
  a_data[66] := ((a_data[66 - 3] xor a_data[66 - 8]) xor a_data[66 - 14])
    xor a_data[66 - 16];
  a_data[67] := ((a_data[67 - 3] xor a_data[67 - 8]) xor a_data[67 - 14])
    xor a_data[67 - 16];
  a_data[68] := ((a_data[68 - 3] xor a_data[68 - 8]) xor a_data[68 - 14])
    xor a_data[68 - 16];
  a_data[69] := ((a_data[69 - 3] xor a_data[69 - 8]) xor a_data[69 - 14])
    xor a_data[69 - 16];
  a_data[70] := ((a_data[70 - 3] xor a_data[70 - 8]) xor a_data[70 - 14])
    xor a_data[70 - 16];
  a_data[71] := ((a_data[71 - 3] xor a_data[71 - 8]) xor a_data[71 - 14])
    xor a_data[71 - 16];
  a_data[72] := ((a_data[72 - 3] xor a_data[72 - 8]) xor a_data[72 - 14])
    xor a_data[72 - 16];
  a_data[73] := ((a_data[73 - 3] xor a_data[73 - 8]) xor a_data[73 - 14])
    xor a_data[73 - 16];
  a_data[74] := ((a_data[74 - 3] xor a_data[74 - 8]) xor a_data[74 - 14])
    xor a_data[74 - 16];
  a_data[75] := ((a_data[75 - 3] xor a_data[75 - 8]) xor a_data[75 - 14])
    xor a_data[75 - 16];
  a_data[76] := ((a_data[76 - 3] xor a_data[76 - 8]) xor a_data[76 - 14])
    xor a_data[76 - 16];
  a_data[77] := ((a_data[77 - 3] xor a_data[77 - 8]) xor a_data[77 - 14])
    xor a_data[77 - 16];
  a_data[78] := ((a_data[78 - 3] xor a_data[78 - 8]) xor a_data[78 - 14])
    xor a_data[78 - 16];
  a_data[79] := ((a_data[79 - 3] xor a_data[79 - 8]) xor a_data[79 - 14])
    xor a_data[79 - 16];

{$ELSE}
  for j := 16 to 79 do
  begin
    a_data[j] := ((a_data[j - 3] xor a_data[j - 8]) xor a_data[j - 14])
      xor a_data[j - 16];
  end;
{$ENDIF USE_UNROLLED_VARIANT}
end;

procedure TSHA0.Finish;
var
  bits: UInt64;
  padindex: Int32;
  pad: THashLibByteArray;
begin
  bits := Fm_processed_bytes * 8;
  if (Fm_buffer.Pos < 56) then

    padindex := (56 - Fm_buffer.Pos)
  else
    padindex := (120 - Fm_buffer.Pos);

  System.SetLength(pad, padindex + 8);

  pad[0] := $80;

  bits := TConverters.be2me_64(bits);

  TConverters.ReadUInt64AsBytesLE(bits, pad, padindex);

  padindex := padindex + 8;

  TransformBytes(pad, 0, padindex);

end;

function TSHA0.GetResult: THashLibByteArray;
begin

  System.SetLength(result, 5 * System.SizeOf(UInt32));
  TConverters.be32_copy(PCardinal(Fm_state), 0, PByte(result), 0,
    System.Length(result));

end;

procedure TSHA0.Initialize;
begin

  Fm_state[0] := $67452301;
  Fm_state[1] := $EFCDAB89;
  Fm_state[2] := $98BADCFE;
  Fm_state[3] := $10325476;
  Fm_state[4] := $C3D2E1F0;

  Inherited Initialize();

end;

procedure TSHA0.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  A, B, C, D, E: UInt32;
  data: array [0 .. 79] of UInt32;
  ptr_data: PCardinal;
begin

  ptr_data := @(data[0]);

  TConverters.be32_copy(a_data, a_index, ptr_data, 0, a_data_length);

  Expand(ptr_data);

  A := Fm_state[0];
  B := Fm_state[1];
  C := Fm_state[2];
  D := Fm_state[3];
  E := Fm_state[4];

  E := (data[0] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[1] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[2] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[3] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[4] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[5] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[6] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[7] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[8] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[9] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[10] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[11] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[12] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[13] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[14] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[15] + C1 + TBits.RotateLeft32(A, 5) +
    (D xor (B and (C xor D)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[16] + C1 + TBits.RotateLeft32(E, 5) +
    (C xor (A and (B xor C)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[17] + C1 + TBits.RotateLeft32(D, 5) +
    (B xor (E and (A xor B)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[18] + C1 + TBits.RotateLeft32(C, 5) +
    (A xor (D and (E xor A)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[19] + C1 + TBits.RotateLeft32(B, 5) +
    (E xor (C and (D xor E)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[20] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[21] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[22] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[23] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[24] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[25] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[26] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[27] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[28] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[29] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[30] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[31] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[32] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[33] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[34] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[35] + C2 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[36] + C2 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[37] + C2 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[38] + C2 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[39] + C2 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[40] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[41] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[42] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[43] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[44] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[45] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[46] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[47] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[48] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[49] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[50] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[51] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[52] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[53] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[54] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[55] + C3 + TBits.RotateLeft32(A, 5) +
    ((B and C) or (D and (B or C)))) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[56] + C3 + TBits.RotateLeft32(E, 5) +
    ((A and B) or (C and (A or B)))) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[57] + C3 + TBits.RotateLeft32(D, 5) +
    ((E and A) or (B and (E or A)))) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[58] + C3 + TBits.RotateLeft32(C, 5) +
    ((D and E) or (A and (D or E)))) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[59] + C3 + TBits.RotateLeft32(B, 5) +
    ((C and D) or (E and (C or D)))) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[60] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[61] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[62] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[63] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[64] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[65] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[66] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[67] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[68] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[69] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[70] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[71] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[72] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[73] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[74] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);
  E := (data[75] + C4 + TBits.RotateLeft32(A, 5) + (B xor C xor D)) + E;

  B := TBits.RotateLeft32(B, 30);
  D := (data[76] + C4 + TBits.RotateLeft32(E, 5) + (A xor B xor C)) + D;

  A := TBits.RotateLeft32(A, 30);
  C := (data[77] + C4 + TBits.RotateLeft32(D, 5) + (E xor A xor B)) + C;

  E := TBits.RotateLeft32(E, 30);
  B := (data[78] + C4 + TBits.RotateLeft32(C, 5) + (D xor E xor A)) + B;

  D := TBits.RotateLeft32(D, 30);
  A := (data[79] + C4 + TBits.RotateLeft32(B, 5) + (C xor D xor E)) + A;

  C := TBits.RotateLeft32(C, 30);

  Fm_state[0] := Fm_state[0] + A;
  Fm_state[1] := Fm_state[1] + B;
  Fm_state[2] := Fm_state[2] + C;
  Fm_state[3] := Fm_state[3] + D;
  Fm_state[4] := Fm_state[4] + E;

  System.FillChar(data, System.SizeOf(data), UInt32(0));

end;

end.
