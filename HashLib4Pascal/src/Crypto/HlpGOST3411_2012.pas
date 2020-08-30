unit HlpGOST3411_2012;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpBits,
  HlpArrayUtils,
  HlpHashLibTypes;

type
  TGOST3411_2012 = class abstract(THash, ICryptoNotBuildIn, ITransformBlock)
  strict private
  class var
    FZero: THashLibByteArray;
    FC: THashLibMatrixByteArray;
    FT: THashLibMatrixUInt64Array;

    procedure InternalUpdate(AInput: Byte); inline;
    procedure xor512(const A, B: THashLibByteArray); inline;
    procedure E(const K, M: THashLibByteArray);
    procedure F(const V: THashLibByteArray);
    procedure GN(const AH, AN, AM: THashLibByteArray); inline;
    procedure AddMod512(const A: THashLibByteArray; Num: Int32); overload;
    procedure AddMod512(const A, B: THashLibByteArray); overload;
    procedure Reverse(const ASource, ADestination: THashLibByteArray);

    class constructor GOST3411_2012();

  strict protected

  var
    FIV, FN, FSigma, FKi, FM, FH, FTemp, FBlock: THashLibByteArray;

    FBOff: Int32;

    constructor Create(AHashSize: Int32; const IV: THashLibByteArray);

  public
    procedure Initialize; override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ADataLength: Int32); override;
    function TransformFinal: IHashResult; override;

  end;

type
  TGOST3411_2012_256 = class sealed(TGOST3411_2012)

  strict private
    class var

      FIV_256: THashLibByteArray;

    class constructor TGOST3411_2012_256();
  public
    constructor Create();
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;
  end;

type
  TGOST3411_2012_512 = class sealed(TGOST3411_2012)

  strict private
    class var

      FIV_512: THashLibByteArray;

    class constructor TGOST3411_2012_512();
  public
    constructor Create();
    function Clone(): IHash; override;
  end;

implementation

{ TGOST3411_2012Base }

procedure TGOST3411_2012.xor512(const A, B: THashLibByteArray);
var
  i: Int32;
begin
  for i := 0 to System.Pred(64) do
  begin
    A[i] := A[i] xor B[i];
  end;
end;

procedure TGOST3411_2012.AddMod512(const A: THashLibByteArray; Num: Int32);
var
  c, i: Int32;
begin
  c := (A[63] and $FF) + (Num and $FF);
  A[63] := Byte(c);

  c := (A[62] and $FF) + ((TBits.Asr32(Num, 8)) and $FF) + (TBits.Asr32(c, 8));
  A[62] := Byte(c);

  i := 61;

  while ((i >= 0) and (c > 0)) do
  begin
    c := (A[i] and $FF) + (TBits.Asr32(c, 8));
    A[i] := Byte(c);
    System.Dec(i);
  end;

end;

procedure TGOST3411_2012.AddMod512(const A, B: THashLibByteArray);
var
  i, c: Int32;
begin
  c := 0;
  i := 63;

  while i >= 0 do
  begin
    c := Int32(A[i] and $FF) + Int32(B[i] and $FF) + (TBits.Asr32(c, 8));
    A[i] := Byte(c);
    System.Dec(i);
  end;
end;

constructor TGOST3411_2012.Create(AHashSize: Int32;
  const IV: THashLibByteArray);
begin
  Inherited Create(AHashSize, 64);
  System.SetLength(FIV, 64);
  System.SetLength(FN, 64);
  System.SetLength(FSigma, 64);
  System.SetLength(FKi, 64);
  System.SetLength(FM, 64);
  System.SetLength(FH, 64);

  // Temporary buffers
  System.SetLength(FTemp, 64);
  System.SetLength(FBlock, 64);

  FBOff := 64;

  System.Move(IV[0], FIV[0], 64 * System.SizeOf(Byte));
  System.Move(IV[0], FH[0], 64 * System.SizeOf(Byte));
end;

procedure TGOST3411_2012.E(const K, M: THashLibByteArray);
var
  i: Int32;
begin
  System.Move(K[0], FKi[0], 64 * System.SizeOf(Byte));
  xor512(K, M);
  F(K);
  for i := 0 to System.Pred(11) do
  begin
    xor512(FKi, FC[i]);
    F(FKi);
    xor512(K, FKi);
    F(K);
  end;
  xor512(FKi, FC[11]);
  F(FKi);
  xor512(K, FKi);
end;

procedure TGOST3411_2012.F(const V: THashLibByteArray);
var
  res: array [0 .. 7] of UInt64;
  r: UInt64;
begin

  r := 0;
  r := r xor (FT[0][(V[56] and $FF)]);
  r := r xor (FT[1][(V[48] and $FF)]);
  r := r xor (FT[2][(V[40] and $FF)]);
  r := r xor (FT[3][(V[32] and $FF)]);
  r := r xor (FT[4][(V[24] and $FF)]);
  r := r xor (FT[5][(V[16] and $FF)]);
  r := r xor (FT[6][(V[8] and $FF)]);
  r := r xor (FT[7][(V[0] and $FF)]);
  res[0] := r;

  r := 0;
  r := r xor (FT[0][(V[57] and $FF)]);
  r := r xor (FT[1][(V[49] and $FF)]);
  r := r xor (FT[2][(V[41] and $FF)]);
  r := r xor (FT[3][(V[33] and $FF)]);
  r := r xor (FT[4][(V[25] and $FF)]);
  r := r xor (FT[5][(V[17] and $FF)]);
  r := r xor (FT[6][(V[9] and $FF)]);
  r := r xor (FT[7][(V[1] and $FF)]);
  res[1] := r;

  r := 0;
  r := r xor (FT[0][(V[58] and $FF)]);
  r := r xor (FT[1][(V[50] and $FF)]);
  r := r xor (FT[2][(V[42] and $FF)]);
  r := r xor (FT[3][(V[34] and $FF)]);
  r := r xor (FT[4][(V[26] and $FF)]);
  r := r xor (FT[5][(V[18] and $FF)]);
  r := r xor (FT[6][(V[10] and $FF)]);
  r := r xor (FT[7][(V[2] and $FF)]);
  res[2] := r;

  r := 0;
  r := r xor (FT[0][(V[59] and $FF)]);
  r := r xor (FT[1][(V[51] and $FF)]);
  r := r xor (FT[2][(V[43] and $FF)]);
  r := r xor (FT[3][(V[35] and $FF)]);
  r := r xor (FT[4][(V[27] and $FF)]);
  r := r xor (FT[5][(V[19] and $FF)]);
  r := r xor (FT[6][(V[11] and $FF)]);
  r := r xor (FT[7][(V[3] and $FF)]);
  res[3] := r;

  r := 0;
  r := r xor (FT[0][(V[60] and $FF)]);
  r := r xor (FT[1][(V[52] and $FF)]);
  r := r xor (FT[2][(V[44] and $FF)]);
  r := r xor (FT[3][(V[36] and $FF)]);
  r := r xor (FT[4][(V[28] and $FF)]);
  r := r xor (FT[5][(V[20] and $FF)]);
  r := r xor (FT[6][(V[12] and $FF)]);
  r := r xor (FT[7][(V[4] and $FF)]);
  res[4] := r;

  r := 0;
  r := r xor (FT[0][(V[61] and $FF)]);
  r := r xor (FT[1][(V[53] and $FF)]);
  r := r xor (FT[2][(V[45] and $FF)]);
  r := r xor (FT[3][(V[37] and $FF)]);
  r := r xor (FT[4][(V[29] and $FF)]);
  r := r xor (FT[5][(V[21] and $FF)]);
  r := r xor (FT[6][(V[13] and $FF)]);
  r := r xor (FT[7][(V[5] and $FF)]);
  res[5] := r;

  r := 0;
  r := r xor (FT[0][(V[62] and $FF)]);
  r := r xor (FT[1][(V[54] and $FF)]);
  r := r xor (FT[2][(V[46] and $FF)]);
  r := r xor (FT[3][(V[38] and $FF)]);
  r := r xor (FT[4][(V[30] and $FF)]);
  r := r xor (FT[5][(V[22] and $FF)]);
  r := r xor (FT[6][(V[14] and $FF)]);
  r := r xor (FT[7][(V[6] and $FF)]);
  res[6] := r;

  r := 0;
  r := r xor (FT[0][(V[63] and $FF)]);
  r := r xor (FT[1][(V[55] and $FF)]);
  r := r xor (FT[2][(V[47] and $FF)]);
  r := r xor (FT[3][(V[39] and $FF)]);
  r := r xor (FT[4][(V[31] and $FF)]);
  r := r xor (FT[5][(V[23] and $FF)]);
  r := r xor (FT[6][(V[15] and $FF)]);
  r := r xor (FT[7][(V[7] and $FF)]);
  res[7] := r;

  r := res[0];
  V[7] := Byte(r shr 56);
  V[6] := Byte(r shr 48);
  V[5] := Byte(r shr 40);
  V[4] := Byte(r shr 32);
  V[3] := Byte(r shr 24);
  V[2] := Byte(r shr 16);
  V[1] := Byte(r shr 8);
  V[0] := Byte(r);

  r := res[1];
  V[15] := Byte(r shr 56);
  V[14] := Byte(r shr 48);
  V[13] := Byte(r shr 40);
  V[12] := Byte(r shr 32);
  V[11] := Byte(r shr 24);
  V[10] := Byte(r shr 16);
  V[9] := Byte(r shr 8);
  V[8] := Byte(r);

  r := res[2];
  V[23] := Byte(r shr 56);
  V[22] := Byte(r shr 48);
  V[21] := Byte(r shr 40);
  V[20] := Byte(r shr 32);
  V[19] := Byte(r shr 24);
  V[18] := Byte(r shr 16);
  V[17] := Byte(r shr 8);
  V[16] := Byte(r);

  r := res[3];
  V[31] := Byte(r shr 56);
  V[30] := Byte(r shr 48);
  V[29] := Byte(r shr 40);
  V[28] := Byte(r shr 32);
  V[27] := Byte(r shr 24);
  V[26] := Byte(r shr 16);
  V[25] := Byte(r shr 8);
  V[24] := Byte(r);

  r := res[4];
  V[39] := Byte(r shr 56);
  V[38] := Byte(r shr 48);
  V[37] := Byte(r shr 40);
  V[36] := Byte(r shr 32);
  V[35] := Byte(r shr 24);
  V[34] := Byte(r shr 16);
  V[33] := Byte(r shr 8);
  V[32] := Byte(r);

  r := res[5];
  V[47] := Byte(r shr 56);
  V[46] := Byte(r shr 48);
  V[45] := Byte(r shr 40);
  V[44] := Byte(r shr 32);
  V[43] := Byte(r shr 24);
  V[42] := Byte(r shr 16);
  V[41] := Byte(r shr 8);
  V[40] := Byte(r);

  r := res[6];
  V[55] := Byte(r shr 56);
  V[54] := Byte(r shr 48);
  V[53] := Byte(r shr 40);
  V[52] := Byte(r shr 32);
  V[51] := Byte(r shr 24);
  V[50] := Byte(r shr 16);
  V[49] := Byte(r shr 8);
  V[48] := Byte(r);

  r := res[7];
  V[63] := Byte(r shr 56);
  V[62] := Byte(r shr 48);
  V[61] := Byte(r shr 40);
  V[60] := Byte(r shr 32);
  V[59] := Byte(r shr 24);
  V[58] := Byte(r shr 16);
  V[57] := Byte(r shr 8);
  V[56] := Byte(r);

  System.FillChar(res, System.SizeOf(res), UInt64(0));
end;

class constructor TGOST3411_2012.GOST3411_2012;
begin
{$REGION 'Consts'}
  FC := THashLibMatrixByteArray.Create(THashLibByteArray.Create(Byte($B1),
    Byte($08), Byte($5B), Byte($DA), Byte($1E), Byte($CA), Byte($DA), Byte($E9),
    Byte($EB), Byte($CB), Byte($2F), Byte($81), Byte($C0), Byte($65), Byte($7C),
    Byte($1F), Byte($2F), Byte($6A), Byte($76), Byte($43), Byte($2E), Byte($45),
    Byte($D0), Byte($16), Byte($71), Byte($4E), Byte($B8), Byte($8D), Byte($75),
    Byte($85), Byte($C4), Byte($FC), Byte($4B), Byte($7C), Byte($E0), Byte($91),
    Byte($92), Byte($67), Byte($69), Byte($01), Byte($A2), Byte($42), Byte($2A),
    Byte($08), Byte($A4), Byte($60), Byte($D3), Byte($15), Byte($05), Byte($76),
    Byte($74), Byte($36), Byte($CC), Byte($74), Byte($4D), Byte($23), Byte($DD),
    Byte($80), Byte($65), Byte($59), Byte($F2), Byte($A6), Byte($45),
    Byte($07)),

    THashLibByteArray.Create(Byte($6F), Byte($A3), Byte($B5), Byte($8A),
    Byte($A9), Byte($9D), Byte($2F), Byte($1A), Byte($4F), Byte($E3), Byte($9D),
    Byte($46), Byte($0F), Byte($70), Byte($B5), Byte($D7), Byte($F3), Byte($FE),
    Byte($EA), Byte($72), Byte($0A), Byte($23), Byte($2B), Byte($98), Byte($61),
    Byte($D5), Byte($5E), Byte($0F), Byte($16), Byte($B5), Byte($01), Byte($31),
    Byte($9A), Byte($B5), Byte($17), Byte($6B), Byte($12), Byte($D6), Byte($99),
    Byte($58), Byte($5C), Byte($B5), Byte($61), Byte($C2), Byte($DB), Byte($0A),
    Byte($A7), Byte($CA), Byte($55), Byte($DD), Byte($A2), Byte($1B), Byte($D7),
    Byte($CB), Byte($CD), Byte($56), Byte($E6), Byte($79), Byte($04), Byte($70),
    Byte($21), Byte($B1), Byte($9B), Byte($B7)),
    THashLibByteArray.Create(Byte($F5), Byte($74), Byte($DC), Byte($AC),
    Byte($2B), Byte($CE), Byte($2F), Byte($C7), Byte($0A), Byte($39), Byte($FC),
    Byte($28), Byte($6A), Byte($3D), Byte($84), Byte($35), Byte($06), Byte($F1),
    Byte($5E), Byte($5F), Byte($52), Byte($9C), Byte($1F), Byte($8B), Byte($F2),
    Byte($EA), Byte($75), Byte($14), Byte($B1), Byte($29), Byte($7B), Byte($7B),
    Byte($D3), Byte($E2), Byte($0F), Byte($E4), Byte($90), Byte($35), Byte($9E),
    Byte($B1), Byte($C1), Byte($C9), Byte($3A), Byte($37), Byte($60), Byte($62),
    Byte($DB), Byte($09), Byte($C2), Byte($B6), Byte($F4), Byte($43), Byte($86),
    Byte($7A), Byte($DB), Byte($31), Byte($99), Byte($1E), Byte($96), Byte($F5),
    Byte($0A), Byte($BA), Byte($0A), Byte($B2)),
    THashLibByteArray.Create(Byte($EF), Byte($1F), Byte($DF), Byte($B3),
    Byte($E8), Byte($15), Byte($66), Byte($D2), Byte($F9), Byte($48), Byte($E1),
    Byte($A0), Byte($5D), Byte($71), Byte($E4), Byte($DD), Byte($48), Byte($8E),
    Byte($85), Byte($7E), Byte($33), Byte($5C), Byte($3C), Byte($7D), Byte($9D),
    Byte($72), Byte($1C), Byte($AD), Byte($68), Byte($5E), Byte($35), Byte($3F),
    Byte($A9), Byte($D7), Byte($2C), Byte($82), Byte($ED), Byte($03), Byte($D6),
    Byte($75), Byte($D8), Byte($B7), Byte($13), Byte($33), Byte($93), Byte($52),
    Byte($03), Byte($BE), Byte($34), Byte($53), Byte($EA), Byte($A1), Byte($93),
    Byte($E8), Byte($37), Byte($F1), Byte($22), Byte($0C), Byte($BE), Byte($BC),
    Byte($84), Byte($E3), Byte($D1), Byte($2E)),
    THashLibByteArray.Create(Byte($4B), Byte($EA), Byte($6B), Byte($AC),
    Byte($AD), Byte($47), Byte($47), Byte($99), Byte($9A), Byte($3F), Byte($41),
    Byte($0C), Byte($6C), Byte($A9), Byte($23), Byte($63), Byte($7F), Byte($15),
    Byte($1C), Byte($1F), Byte($16), Byte($86), Byte($10), Byte($4A), Byte($35),
    Byte($9E), Byte($35), Byte($D7), Byte($80), Byte($0F), Byte($FF), Byte($BD),
    Byte($BF), Byte($CD), Byte($17), Byte($47), Byte($25), Byte($3A), Byte($F5),
    Byte($A3), Byte($DF), Byte($FF), Byte($00), Byte($B7), Byte($23), Byte($27),
    Byte($1A), Byte($16), Byte($7A), Byte($56), Byte($A2), Byte($7E), Byte($A9),
    Byte($EA), Byte($63), Byte($F5), Byte($60), Byte($17), Byte($58), Byte($FD),
    Byte($7C), Byte($6C), Byte($FE), Byte($57)),
    THashLibByteArray.Create(Byte($AE), Byte($4F), Byte($AE), Byte($AE),
    Byte($1D), Byte($3A), Byte($D3), Byte($D9), Byte($6F), Byte($A4), Byte($C3),
    Byte($3B), Byte($7A), Byte($30), Byte($39), Byte($C0), Byte($2D), Byte($66),
    Byte($C4), Byte($F9), Byte($51), Byte($42), Byte($A4), Byte($6C), Byte($18),
    Byte($7F), Byte($9A), Byte($B4), Byte($9A), Byte($F0), Byte($8E), Byte($C6),
    Byte($CF), Byte($FA), Byte($A6), Byte($B7), Byte($1C), Byte($9A), Byte($B7),
    Byte($B4), Byte($0A), Byte($F2), Byte($1F), Byte($66), Byte($C2), Byte($BE),
    Byte($C6), Byte($B6), Byte($BF), Byte($71), Byte($C5), Byte($72), Byte($36),
    Byte($90), Byte($4F), Byte($35), Byte($FA), Byte($68), Byte($40), Byte($7A),
    Byte($46), Byte($64), Byte($7D), Byte($6E)),
    THashLibByteArray.Create(Byte($F4), Byte($C7), Byte($0E), Byte($16),
    Byte($EE), Byte($AA), Byte($C5), Byte($EC), Byte($51), Byte($AC), Byte($86),
    Byte($FE), Byte($BF), Byte($24), Byte($09), Byte($54), Byte($39), Byte($9E),
    Byte($C6), Byte($C7), Byte($E6), Byte($BF), Byte($87), Byte($C9), Byte($D3),
    Byte($47), Byte($3E), Byte($33), Byte($19), Byte($7A), Byte($93), Byte($C9),
    Byte($09), Byte($92), Byte($AB), Byte($C5), Byte($2D), Byte($82), Byte($2C),
    Byte($37), Byte($06), Byte($47), Byte($69), Byte($83), Byte($28), Byte($4A),
    Byte($05), Byte($04), Byte($35), Byte($17), Byte($45), Byte($4C), Byte($A2),
    Byte($3C), Byte($4A), Byte($F3), Byte($88), Byte($86), Byte($56), Byte($4D),
    Byte($3A), Byte($14), Byte($D4), Byte($93)),
    THashLibByteArray.Create(Byte($9B), Byte($1F), Byte($5B), Byte($42),
    Byte($4D), Byte($93), Byte($C9), Byte($A7), Byte($03), Byte($E7), Byte($AA),
    Byte($02), Byte($0C), Byte($6E), Byte($41), Byte($41), Byte($4E), Byte($B7),
    Byte($F8), Byte($71), Byte($9C), Byte($36), Byte($DE), Byte($1E), Byte($89),
    Byte($B4), Byte($44), Byte($3B), Byte($4D), Byte($DB), Byte($C4), Byte($9A),
    Byte($F4), Byte($89), Byte($2B), Byte($CB), Byte($92), Byte($9B), Byte($06),
    Byte($90), Byte($69), Byte($D1), Byte($8D), Byte($2B), Byte($D1), Byte($A5),
    Byte($C4), Byte($2F), Byte($36), Byte($AC), Byte($C2), Byte($35), Byte($59),
    Byte($51), Byte($A8), Byte($D9), Byte($A4), Byte($7F), Byte($0D), Byte($D4),
    Byte($BF), Byte($02), Byte($E7), Byte($1E)),
    THashLibByteArray.Create(Byte($37), Byte($8F), Byte($5A), Byte($54),
    Byte($16), Byte($31), Byte($22), Byte($9B), Byte($94), Byte($4C), Byte($9A),
    Byte($D8), Byte($EC), Byte($16), Byte($5F), Byte($DE), Byte($3A), Byte($7D),
    Byte($3A), Byte($1B), Byte($25), Byte($89), Byte($42), Byte($24), Byte($3C),
    Byte($D9), Byte($55), Byte($B7), Byte($E0), Byte($0D), Byte($09), Byte($84),
    Byte($80), Byte($0A), Byte($44), Byte($0B), Byte($DB), Byte($B2), Byte($CE),
    Byte($B1), Byte($7B), Byte($2B), Byte($8A), Byte($9A), Byte($A6), Byte($07),
    Byte($9C), Byte($54), Byte($0E), Byte($38), Byte($DC), Byte($92), Byte($CB),
    Byte($1F), Byte($2A), Byte($60), Byte($72), Byte($61), Byte($44), Byte($51),
    Byte($83), Byte($23), Byte($5A), Byte($DB)),
    THashLibByteArray.Create(Byte($AB), Byte($BE), Byte($DE), Byte($A6),
    Byte($80), Byte($05), Byte($6F), Byte($52), Byte($38), Byte($2A), Byte($E5),
    Byte($48), Byte($B2), Byte($E4), Byte($F3), Byte($F3), Byte($89), Byte($41),
    Byte($E7), Byte($1C), Byte($FF), Byte($8A), Byte($78), Byte($DB), Byte($1F),
    Byte($FF), Byte($E1), Byte($8A), Byte($1B), Byte($33), Byte($61), Byte($03),
    Byte($9F), Byte($E7), Byte($67), Byte($02), Byte($AF), Byte($69), Byte($33),
    Byte($4B), Byte($7A), Byte($1E), Byte($6C), Byte($30), Byte($3B), Byte($76),
    Byte($52), Byte($F4), Byte($36), Byte($98), Byte($FA), Byte($D1), Byte($15),
    Byte($3B), Byte($B6), Byte($C3), Byte($74), Byte($B4), Byte($C7), Byte($FB),
    Byte($98), Byte($45), Byte($9C), Byte($ED)),
    THashLibByteArray.Create(Byte($7B), Byte($CD), Byte($9E), Byte($D0),
    Byte($EF), Byte($C8), Byte($89), Byte($FB), Byte($30), Byte($02), Byte($C6),
    Byte($CD), Byte($63), Byte($5A), Byte($FE), Byte($94), Byte($D8), Byte($FA),
    Byte($6B), Byte($BB), Byte($EB), Byte($AB), Byte($07), Byte($61), Byte($20),
    Byte($01), Byte($80), Byte($21), Byte($14), Byte($84), Byte($66), Byte($79),
    Byte($8A), Byte($1D), Byte($71), Byte($EF), Byte($EA), Byte($48), Byte($B9),
    Byte($CA), Byte($EF), Byte($BA), Byte($CD), Byte($1D), Byte($7D), Byte($47),
    Byte($6E), Byte($98), Byte($DE), Byte($A2), Byte($59), Byte($4A), Byte($C0),
    Byte($6F), Byte($D8), Byte($5D), Byte($6B), Byte($CA), Byte($A4), Byte($CD),
    Byte($81), Byte($F3), Byte($2D), Byte($1B)),
    THashLibByteArray.Create(Byte($37), Byte($8E), Byte($E7), Byte($67),
    Byte($F1), Byte($16), Byte($31), Byte($BA), Byte($D2), Byte($13), Byte($80),
    Byte($B0), Byte($04), Byte($49), Byte($B1), Byte($7A), Byte($CD), Byte($A4),
    Byte($3C), Byte($32), Byte($BC), Byte($DF), Byte($1D), Byte($77), Byte($F8),
    Byte($20), Byte($12), Byte($D4), Byte($30), Byte($21), Byte($9F), Byte($9B),
    Byte($5D), Byte($80), Byte($EF), Byte($9D), Byte($18), Byte($91), Byte($CC),
    Byte($86), Byte($E7), Byte($1D), Byte($A4), Byte($AA), Byte($88), Byte($E1),
    Byte($28), Byte($52), Byte($FA), Byte($F4), Byte($17), Byte($D5), Byte($D9),
    Byte($B2), Byte($1B), Byte($99), Byte($48), Byte($BC), Byte($92), Byte($4A),
    Byte($F1), Byte($1B), Byte($D7), Byte($20)));

  FZero := THashLibByteArray.Create($00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);

  FT := THashLibMatrixUInt64Array.Create
    (THashLibUInt64Array.Create(UInt64($E6F87E5C5B711FD0),
    UInt64($258377800924FA16), UInt64($C849E07E852EA4A8),
    UInt64($5B4686A18F06C16A), UInt64($0B32E9A2D77B416E),
    UInt64($ABDA37A467815C66), UInt64($F61796A81A686676),
    UInt64($F5DC0B706391954B), UInt64($4862F38DB7E64BF1),
    UInt64($FF5C629A68BD85C5), UInt64($CB827DA6FCD75795),
    UInt64($66D36DAF69B9F089), UInt64($356C9F74483D83B0),
    UInt64($7CBCECB1238C99A1), UInt64($36A702AC31C4708D),
    UInt64($9EB6A8D02FBCDFD6), UInt64($8B19FA51E5B3AE37),
    UInt64($9CCFB5408A127D0B), UInt64($BC0C78B508208F5A),
    UInt64($E533E3842288ECED), UInt64($CEC2C7D377C15FD2),
    UInt64($EC7817B6505D0F5E), UInt64($B94CC2C08336871D),
    UInt64($8C205DB4CB0B04AD), UInt64($763C855B28A0892F),
    UInt64($588D1B79F6FF3257), UInt64($3FECF69E4311933E),
    UInt64($0FC0D39F803A18C9), UInt64($EE010A26F5F3AD83),
    UInt64($10EFE8F4411979A6), UInt64($5DCDA10C7DE93A10),
    UInt64($4A1BEE1D1248E92C), UInt64($53BFF2DB21847339),
    UInt64($B4F50CCFA6A23D09), UInt64($5FB4BC9CD84798CD),
    UInt64($E88A2D8B071C56F9), UInt64($7F7771695A756A9C),
    UInt64($C5F02E71A0BA1EBC), UInt64($A663F9AB4215E672),
    UInt64($2EB19E22DE5FBB78), UInt64($0DB9CE0F2594BA14),
    UInt64($82520E6397664D84), UInt64($2F031E6A0208EA98),
    UInt64($5C7F2144A1BE6BF0), UInt64($7A37CB1CD16362DB),
    UInt64($83E08E2B4B311C64), UInt64($CF70479BAB960E32),
    UInt64($856BA986B9DEE71E), UInt64($B5478C877AF56CE9),
    UInt64($B8FE42885F61D6FD), UInt64($1BDD0156966238C8),
    UInt64($622157923EF8A92E), UInt64($FC97FF42114476F8),
    UInt64($9D7D350856452CEB), UInt64($4C90C9B0E0A71256),
    UInt64($2308502DFBCB016C), UInt64($2D7A03FAA7A64845),
    UInt64($F46E8B38BFC6C4AB), UInt64($BDBEF8FDD477DEBA),
    UInt64($3AAC4CEBC8079B79), UInt64($F09CB105E8879D0C),
    UInt64($27FA6A10AC8A58CB), UInt64($8960E7C1401D0CEA),
    UInt64($1A6F811E4A356928), UInt64($90C4FB0773D196FF),
    UInt64($43501A2F609D0A9F), UInt64($F7A516E0C63F3796),
    UInt64($1CE4A6B3B8DA9252), UInt64($1324752C38E08A9B),
    UInt64($A5A864733BEC154F), UInt64($2BF124575549B33F),
    UInt64($D766DB15440DC5C7), UInt64($A7D179E39E42B792),
    UInt64($DADF151A61997FD3), UInt64($86A0345EC0271423),
    UInt64($38D5517B6DA939A4), UInt64($6518F077104003B4),
    UInt64($02791D90A5AEA2DD), UInt64($88D267899C4A5D0A),
    UInt64($930F66DF0A2865C2), UInt64($4EE9D4204509B08B),
    UInt64($325538916685292A), UInt64($412907BFC533A842),
    UInt64($B27E2B62544DC673), UInt64($6C5304456295E007),
    UInt64($5AF406E95351908A), UInt64($1F2F3B6BC123616F),
    UInt64($C37B09DC5255E5C6), UInt64($3967D133B1FE6844),
    UInt64($298839C7F0E711E2), UInt64($409B87F71964F9A2),
    UInt64($E938ADC3DB4B0719), UInt64($0C0B4E47F9C3EBF4),
    UInt64($5534D576D36B8843), UInt64($4610A05AEB8B02D8),
    UInt64($20C3CDF58232F251), UInt64($6DE1840DBEC2B1E7),
    UInt64($A0E8DE06B0FA1D08), UInt64($7B854B540D34333B),
    UInt64($42E29A67BCCA5B7F), UInt64($D8A6088AC437DD0E),
    UInt64($C63BB3A9D943ED81), UInt64($21714DBD5E65A3B1),
    UInt64($6761EDE7B5EEA169), UInt64($2431F7C8D573ABF6),
    UInt64($D51FC685E1A3671A), UInt64($5E063CD40410C92D),
    UInt64($283AB98F2CB04002), UInt64($8FEBC06CB2F2F790),
    UInt64($17D64F116FA1D33C), UInt64($E07359F1A99EE4AA),
    UInt64($784ED68C74CDC006), UInt64($6E2A19D5C73B42DA),
    UInt64($8712B4161C7045C3), UInt64($371582E4ED93216D),
    UInt64($ACE390414939F6FC), UInt64($7EC5F12186223B7C),
    UInt64($C0B094042BAC16FB), UInt64($F9D745379A527EBF),
    UInt64($737C3F2EA3B68168), UInt64($33E7B8D9BAD278CA),
    UInt64($A9A32A34C22FFEBB), UInt64($E48163CCFEDFBD0D),
    UInt64($8E5940246EA5A670), UInt64($51C6EF4B842AD1E4),
    UInt64($22BAD065279C508C), UInt64($D91488C218608CEE),
    UInt64($319EA5491F7CDA17), UInt64($D394E128134C9C60),
    UInt64($094BF43272D5E3B3), UInt64($9BF612A5A4AAD791),
    UInt64($CCBBDA43D26FFD0F), UInt64($34DE1F3C946AD250),
    UInt64($4F5B5468995EE16B), UInt64($DF9FAF6FEA8F7794),
    UInt64($2648EA5870DD092B), UInt64($BFC7E56D71D97C67),
    UInt64($DDE6B2FF4F21D549), UInt64($3C276B463AE86003),
    UInt64($91767B4FAF86C71F), UInt64($68A13E7835D4B9A0),
    UInt64($B68C115F030C9FD4), UInt64($141DD2C916582001),
    UInt64($983D8F7DDD5324AC), UInt64($64AA703FCC175254),
    UInt64($C2C989948E02B426), UInt64($3E5E76D69F46C2DE),
    UInt64($50746F03587D8004), UInt64($45DB3D829272F1E5),
    UInt64($60584A029B560BF3), UInt64($FBAE58A73FFCDC62),
    UInt64($A15A5E4E6CAD4CE8), UInt64($4BA96E55CE1FB8CC),
    UInt64($08F9747AAE82B253), UInt64($C102144CF7FB471B),
    UInt64($9F042898F3EB8E36), UInt64($068B27ADF2EFFB7A),
    UInt64($EDCA97FE8C0A5EBE), UInt64($778E0513F4F7D8CF),
    UInt64($302C2501C32B8BF7), UInt64($8D92DDFC175C554D),
    UInt64($F865C57F46052F5F), UInt64($EAF3301BA2B2F424),
    UInt64($AA68B7ECBBD60D86), UInt64($998F0F350104754C),
    UInt64($0000000000000000), UInt64($F12E314D34D0CCEC),
    UInt64($710522BE061823B5), UInt64($AF280D9930C005C1),
    UInt64($97FD5CE25D693C65), UInt64($19A41CC633CC9A15),
    UInt64($95844172F8C79EB8), UInt64($DC5432B7937684A9),
    UInt64($9436C13A2490CF58), UInt64($802B13F332C8EF59),
    UInt64($C442AE397CED4F5C), UInt64($FA1CD8EFE3AB8D82),
    UInt64($F2E5AC954D293FD1), UInt64($6AD823E8907A1B7D),
    UInt64($4D2249F83CF043B6), UInt64($03CB9DD879F9F33D),
    UInt64($DE2D2F2736D82674), UInt64($2A43A41F891EE2DF),
    UInt64($6F98999D1B6C133A), UInt64($D4AD46CD3DF436FA),
    UInt64($BB35DF50269825C0), UInt64($964FDCAA813E6D85),
    UInt64($EB41B0537EE5A5C4), UInt64($0540BA758B160847),
    UInt64($A41AE43BE7BB44AF), UInt64($E3B8C429D0671797),
    UInt64($819993BBEE9FBEB9), UInt64($AE9A8DD1EC975421),
    UInt64($F3572CDD917E6E31), UInt64($6393D7DAE2AFF8CE),
    UInt64($47A2201237DC5338), UInt64($A32343DEC903EE35),
    UInt64($79FC56C4A89A91E6), UInt64($01B28048DC5751E0),
    UInt64($1296F564E4B7DB7B), UInt64($75F7188351597A12),
    UInt64($DB6D9552BDCE2E33), UInt64($1E9DBB231D74308F),
    UInt64($520D7293FDD322D9), UInt64($E20A44610C304677),
    UInt64($FEEEE2D2B4EAD425), UInt64($CA30FDEE20800675),
    UInt64($61EACA4A47015A13), UInt64($E74AFE1487264E30),
    UInt64($2CC883B27BF119A5), UInt64($1664CF59B3F682DC),
    UInt64($A811AA7C1E78AF5B), UInt64($1D5626FB648DC3B2),
    UInt64($B73E9117DF5BCE34), UInt64($D05F7CF06AB56F5D),
    UInt64($FD257F0ACD132718), UInt64($574DC8E676C52A9E),
    UInt64($0739A7E52EB8AA9A), UInt64($5486553E0F3CD9A3),
    UInt64($56FF48AEAA927B7E), UInt64($BE756525AD8E2D87),
    UInt64($7D0E6CF9FFDBC841), UInt64($3B1ECCA31450CA99),
    UInt64($6913BE30E983E840), UInt64($AD511009956EA71C),
    UInt64($B1B5B6BA2DB4354E), UInt64($4469BDCA4E25A005),
    UInt64($15AF5281CA0F71E1), UInt64($744598CB8D0E2BF2),
    UInt64($593F9B312AA863B7), UInt64($EFB38A6E29A4FC63),
    UInt64($6B6AA3A04C2D4A9D), UInt64($3D95EB0EE6BF31E3),
    UInt64($A291C3961554BFD5), UInt64($18169C8EEF9BCBF5),
    UInt64($115D68BC9D4E2846), UInt64($BA875F18FACF7420),
    UInt64($D1EDFCB8B6E23EBD), UInt64($B00736F2F1E364AE),
    UInt64($84D929CE6589B6FE), UInt64($70B7A2F6DA4F7255),
    UInt64($0E7253D75C6D4929), UInt64($04F23A3D574159A7),
    UInt64($0A8069EA0B2C108E), UInt64($49D073C56BB11A11),
    UInt64($8AAB7A1939E4FFD7), UInt64($CD095A0B0E38ACEF),
    UInt64($C9FB60365979F548), UInt64($92BDE697D67F3422),
    UInt64($C78933E10514BC61), UInt64($E1C1D9B975C9B54A),
    UInt64($D2266160CF1BCD80), UInt64($9A4492ED78FD8671),
    UInt64($B3CCAB2A881A9793), UInt64($72CEBF667FE1D088),
    UInt64($D6D45B5D985A9427)),
    THashLibUInt64Array.Create(UInt64($C811A8058C3F55DE),
    UInt64($65F5B43196B50619), UInt64($F74F96B1D6706E43),
    UInt64($859D1E8BCB43D336), UInt64($5AAB8A85CCFA3D84),
    UInt64($F9C7BF99C295FCFD), UInt64($A21FD5A1DE4B630F),
    UInt64($CDB3EF763B8B456D), UInt64($803F59F87CF7C385),
    UInt64($B27C73BE5F31913C), UInt64($98E3AC6633B04821),
    UInt64($BF61674C26B8F818), UInt64($0FFBC995C4C130C8),
    UInt64($AAA0862010761A98), UInt64($6057F342210116AA),
    UInt64($F63C760C0654CC35), UInt64($2DDB45CC667D9042),
    UInt64($BCF45A964BD40382), UInt64($68E8A0C3EF3C6F3D),
    UInt64($A7BD92D269FF73BC), UInt64($290AE20201ED2287),
    UInt64($B7DE34CDE885818F), UInt64($D901EEA7DD61059B),
    UInt64($D6FA273219A03553), UInt64($D56F1AE874CCCEC9),
    UInt64($EA31245C2E83F554), UInt64($7034555DA07BE499),
    UInt64($CE26D2AC56E7BEF7), UInt64($FD161857A5054E38),
    UInt64($6A0E7DA4527436D1), UInt64($5BD86A381CDE9FF2),
    UInt64($CAF7756231770C32), UInt64($B09AAED9E279C8D0),
    UInt64($5DEF1091C60674DB), UInt64($111046A2515E5045),
    UInt64($23536CE4729802FC), UInt64($C50CBCF7F5B63CFA),
    UInt64($73A16887CD171F03), UInt64($7D2941AFD9F28DBD),
    UInt64($3F5E3EB45A4F3B9D), UInt64($84EEFE361B677140),
    UInt64($3DB8E3D3E7076271), UInt64($1A3A28F9F20FD248),
    UInt64($7EBC7C75B49E7627), UInt64($74E5F293C7EB565C),
    UInt64($18DCF59E4F478BA4), UInt64($0C6EF44FA9ADCB52),
    UInt64($C699812D98DAC760), UInt64($788B06DC6E469D0E),
    UInt64($FC65F8EA7521EC4E), UInt64($30A5F7219E8E0B55),
    UInt64($2BEC3F65BCA57B6B), UInt64($DDD04969BAF1B75E),
    UInt64($99904CDBE394EA57), UInt64($14B201D1E6EA40F6),
    UInt64($BBB0C08241284ADD), UInt64($50F20463BF8F1DFF),
    UInt64($E8D7F93B93CBACB8), UInt64($4D8CB68E477C86E8),
    UInt64($C1DD1B3992268E3F), UInt64($7C5AA11209D62FCB),
    UInt64($2F3D98ABDB35C9AE), UInt64($671369562BFD5FF5),
    UInt64($15C1E16C36CEE280), UInt64($1D7EB2EDF8F39B17),
    UInt64($DA94D37DB00DFE01), UInt64($877BC3EC760B8ADA),
    UInt64($CB8495DFE153AE44), UInt64($05A24773B7B410B3),
    UInt64($12857B783C32ABDF), UInt64($8EB770D06812513B),
    UInt64($536739B9D2E3E665), UInt64($584D57E271B26468),
    UInt64($D789C78FC9849725), UInt64($A935BBFA7D1AE102),
    UInt64($8B1537A3DFA64188), UInt64($D0CD5D9BC378DE7A),
    UInt64($4AC82C9A4D80CFB7), UInt64($42777F1B83BDB620),
    UInt64($72D2883A1D33BD75), UInt64($5E7A2D4BAB6A8F41),
    UInt64($F4DAAB6BBB1C95D9), UInt64($905CFFE7FD8D31B6),
    UInt64($83AA6422119B381F), UInt64($C0AEFB8442022C49),
    UInt64($A0F908C663033AE3), UInt64($A428AF0804938826),
    UInt64($ADE41C341A8A53C7), UInt64($AE7121EE77E6A85D),
    UInt64($C47F5C4A25929E8C), UInt64($B538E9AA55CDD863),
    UInt64($06377AA9DAD8EB29), UInt64($A18AE87BB3279895),
    UInt64($6EDFDA6A35E48414), UInt64($6B7D9D19825094A7),
    UInt64($D41CFA55A4E86CBF), UInt64($E5CAEDC9EA42C59C),
    UInt64($A36C351C0E6FC179), UInt64($5181E4DE6FABBF89),
    UInt64($FFF0C530184D17D4), UInt64($9D41EB1584045892),
    UInt64($1C0D525028D73961), UInt64($F178EC180CA8856A),
    UInt64($9A0571018EF811CD), UInt64($4091A27C3EF5EFCC),
    UInt64($19AF15239F6329D2), UInt64($347450EFF91EB990),
    UInt64($E11B4A078DD27759), UInt64($B9561DE5FC601331),
    UInt64($912F1F5A2DA993C0), UInt64($1654DCB65BA2191A),
    UInt64($3E2DDE098A6B99EB), UInt64($8A66D71E0F82E3FE),
    UInt64($8C51ADB7D55A08D7), UInt64($4533E50F8941FF7F),
    UInt64($02E6DD67BD4859EC), UInt64($E068AABA5DF6D52F),
    UInt64($C24826E3FF4A75A5), UInt64($6C39070D88ACDDF8),
    UInt64($6486548C4691A46F), UInt64($D1BEBD26135C7C0C),
    UInt64($B30F93038F15334A), UInt64($82D9849FC1BF9A69),
    UInt64($9C320BA85420FAE4), UInt64($FA528243AFF90767),
    UInt64($9ED4D6CFE968A308), UInt64($B825FD582C44B147),
    UInt64($9B7691BC5EDCB3BB), UInt64($C7EA619048FE6516),
    UInt64($1063A61F817AF233), UInt64($47D538683409A693),
    UInt64($63C2CE984C6DED30), UInt64($2A9FDFD86C81D91D),
    UInt64($7B1E3B06032A6694), UInt64($666089EBFBD9FD83),
    UInt64($0A598EE67375207B), UInt64($07449A140AFC495F),
    UInt64($2CA8A571B6593234), UInt64($1F986F8A45BBC2FB),
    UInt64($381AA4A050B372C2), UInt64($5423A3ADD81FAF3A),
    UInt64($17273C0B8B86BB6C), UInt64($FE83258DC869B5A2),
    UInt64($287902BFD1C980F1), UInt64($F5A94BD66B3837AF),
    UInt64($88800A79B2CABA12), UInt64($55504310083B0D4C),
    UInt64($DF36940E07B9EEB2), UInt64($04D1A7CE6790B2C5),
    UInt64($612413FFF125B4DC), UInt64($26F12B97C52C124F),
    UInt64($86082351A62F28AC), UInt64($EF93632F9937E5E7),
    UInt64($3507B052293A1BE6), UInt64($E72C30AE570A9C70),
    UInt64($D3586041AE1425E0), UInt64($DE4574B3D79D4CC4),
    UInt64($92BA228040C5685A), UInt64($F00B0CA5DC8C271C),
    UInt64($BE1287F1F69C5A6E), UInt64($F39E317FB1E0DC86),
    UInt64($495D114020EC342D), UInt64($699B407E3F18CD4B),
    UInt64($DCA3A9D46AD51528), UInt64($0D1D14F279896924),
    UInt64($0000000000000000), UInt64($593EB75FA196C61E),
    UInt64($2E4E78160B116BD8), UInt64($6D4AE7B058887F8E),
    UInt64($E65FD013872E3E06), UInt64($7A6DDBBBD30EC4E2),
    UInt64($AC97FC89CAAEF1B1), UInt64($09CCB33C1E19DBE1),
    UInt64($89F3EAC462EE1864), UInt64($7770CF49AA87ADC6),
    UInt64($56C57ECA6557F6D6), UInt64($03953DDA6D6CFB9A),
    UInt64($36928D884456E07C), UInt64($1EEB8F37959F608D),
    UInt64($31D6179C4EAAA923), UInt64($6FAC3AD7E5C02662),
    UInt64($43049FA653991456), UInt64($ABD3669DC052B8EE),
    UInt64($AF02C153A7C20A2B), UInt64($3CCB036E3723C007),
    UInt64($93C9C23D90E1CA2C), UInt64($C33BC65E2F6ED7D3),
    UInt64($4CFF56339758249E), UInt64($B1E94E64325D6AA6),
    UInt64($37E16D359472420A), UInt64($79F8E661BE623F78),
    UInt64($5214D90402C74413), UInt64($482EF1FDF0C8965B),
    UInt64($13F69BC5EC1609A9), UInt64($0E88292814E592BE),
    UInt64($4E198B542A107D72), UInt64($CCC00FCBEBAFE71B),
    UInt64($1B49C844222B703E), UInt64($2564164DA840E9D5),
    UInt64($20C6513E1FF4F966), UInt64($BAC3203F910CE8AB),
    UInt64($F2EDD1C261C47EF0), UInt64($814CB945ACD361F3),
    UInt64($95FEB8944A392105), UInt64($5C9CF02C1622D6AD),
    UInt64($971865F3F77178E9), UInt64($BD87BA2B9BF0A1F4),
    UInt64($444005B259655D09), UInt64($ED75BE48247FBC0B),
    UInt64($7596122E17CFF42A), UInt64($B44B091785E97A15),
    UInt64($966B854E2755DA9F), UInt64($EEE0839249134791),
    UInt64($32432A4623C652B9), UInt64($A8465B47AD3E4374),
    UInt64($F8B45F2412B15E8B), UInt64($2417F6F078644BA3),
    UInt64($FB2162FE7FDDA511), UInt64($4BBBCC279DA46DC1),
    UInt64($0173E0BDD024A276), UInt64($22208C59A2BCA08A),
    UInt64($8FC4906DB836F34D), UInt64($E4B90D743A6667EA),
    UInt64($7147B5E0705F46EF), UInt64($2782CB2A1508B039),
    UInt64($EC065EF5F45B1E7D), UInt64($21B5B183CFD05B10),
    UInt64($DBE733C060295C77), UInt64($9FA73672394C017E),
    UInt64($CF55321186C31C81), UInt64($D8720E1A0D45A7ED),
    UInt64($3B8F997A3DDF8958), UInt64($3AFC79C7EDFB2B2E),
    UInt64($E9A4198643EF0ECE), UInt64($5F09CDF67B4E2D37),
    UInt64($4F6A6BE9FA34DF04), UInt64($B6ADD47038A123F9),
    UInt64($8D224D0A057EAAA1), UInt64($C96248B85C1BF7A8),
    UInt64($E3FD9760309A2EB5), UInt64($0B2A6E5BA351820D),
    UInt64($EB42C4E1FEA75722), UInt64($948D58299A1D8373),
    UInt64($7FCF9CC864BAD451), UInt64($A55B4FB5D4B72A50),
    UInt64($08BF5381CE3D7997), UInt64($46A6D8D5E42D04E5),
    UInt64($D22B80FC7E308796), UInt64($57B69E77B57354A0),
    UInt64($3969441D8097D0B4), UInt64($3330CAFBF3E2F0CF),
    UInt64($E28E77DDE0BE8CC3), UInt64($62B12E259C494F46),
    UInt64($A6CE726FB9DBD1CA), UInt64($41E242C1EED14DBA),
    UInt64($76032FF47AA30FB0)),
    THashLibUInt64Array.Create(UInt64($45B268A93ACDE4CC),
    UInt64($AF7F0BE884549D08), UInt64($048354B3C1468263),
    UInt64($925435C2C80EFED2), UInt64($EE4E37F27FDFFBA7),
    UInt64($167A33920C60F14D), UInt64($FB123B52EA03E584),
    UInt64($4A0CAB53FDBB9007), UInt64($9DEAF6380F788A19),
    UInt64($CB48EC558F0CB32A), UInt64($B59DC4B2D6FEF7E0),
    UInt64($DCDBCA22F4F3ECB6), UInt64($11DF5813549A9C40),
    UInt64($E33FDEDF568ACED3), UInt64($A0C1C8124322E9C3),
    UInt64($07A56B8158FA6D0D), UInt64($77279579B1E1F3DD),
    UInt64($D9B18B74422AC004), UInt64($B8EC2D9FFFABC294),
    UInt64($F4ACF8A82D75914F), UInt64($7BBF69B1EF2B6878),
    UInt64($C4F62FAF487AC7E1), UInt64($76CE809CC67E5D0C),
    UInt64($6711D88F92E4C14C), UInt64($627B99D9243DEDFE),
    UInt64($234AA5C3DFB68B51), UInt64($909B1F15262DBF6D),
    UInt64($4F66EA054B62BCB5), UInt64($1AE2CF5A52AA6AE8),
    UInt64($BEA053FBD0CE0148), UInt64($ED6808C0E66314C9),
    UInt64($43FE16CD15A82710), UInt64($CD049231A06970F6),
    UInt64($E7BC8A6C97CC4CB0), UInt64($337CE835FCB3B9C0),
    UInt64($65DEF2587CC780F3), UInt64($52214EDE4132BB50),
    UInt64($95F15E4390F493DF), UInt64($870839625DD2E0F1),
    UInt64($41313C1AFB8B66AF), UInt64($91720AF051B211BC),
    UInt64($477D427ED4EEA573), UInt64($2E3B4CEEF6E3BE25),
    UInt64($82627834EB0BCC43), UInt64($9C03E3DD78E724C8),
    UInt64($2877328AD9867DF9), UInt64($14B51945E243B0F2),
    UInt64($574B0F88F7EB97E2), UInt64($88B6FA989AA4943A),
    UInt64($19C4F068CB168586), UInt64($50EE6409AF11FAEF),
    UInt64($7DF317D5C04EABA4), UInt64($7A567C5498B4C6A9),
    UInt64($B6BBFB804F42188E), UInt64($3CC22BCF3BC5CD0B),
    UInt64($D04336EAAA397713), UInt64($F02FAC1BEC33132C),
    UInt64($2506DBA7F0D3488D), UInt64($D7E65D6BF2C31A1E),
    UInt64($5EB9B2161FF820F5), UInt64($842E0650C46E0F9F),
    UInt64($716BEB1D9E843001), UInt64($A933758CAB315ED4),
    UInt64($3FE414FDA2792265), UInt64($27C9F1701EF00932),
    UInt64($73A4C1CA70A771BE), UInt64($94184BA6E76B3D0E),
    UInt64($40D829FF8C14C87E), UInt64($0FBEC3FAC77674CB),
    UInt64($3616A9634A6A9572), UInt64($8F139119C25EF937),
    UInt64($F545ED4D5AEA3F9E), UInt64($E802499650BA387B),
    UInt64($6437E7BD0B582E22), UInt64($E6559F89E053E261),
    UInt64($80AD52E305288DFC), UInt64($6DC55A23E34B9935),
    UInt64($DE14E0F51AD0AD09), UInt64($C6390578A659865E),
    UInt64($96D7617109487CB1), UInt64($E2D6CB3A21156002),
    UInt64($01E915E5779FAED1), UInt64($ADB0213F6A77DCB7),
    UInt64($9880B76EB9A1A6AB), UInt64($5D9F8D248644CF9B),
    UInt64($FD5E4536C5662658), UInt64($F1C6B9FE9BACBDFD),
    UInt64($EACD6341BE9979C4), UInt64($EFA7221708405576),
    UInt64($510771ECD88E543E), UInt64($C2BA51CB671F043D),
    UInt64($0AD482AC71AF5879), UInt64($FE787A045CDAC936),
    UInt64($B238AF338E049AED), UInt64($BD866CC94972EE26),
    UInt64($615DA6EBBD810290), UInt64($3295FDD08B2C1711),
    UInt64($F834046073BF0AEA), UInt64($F3099329758FFC42),
    UInt64($1CAEB13E7DCFA934), UInt64($BA2307481188832B),
    UInt64($24EFCE42874CE65C), UInt64($0E57D61FB0E9DA1A),
    UInt64($B3D1BAD6F99B343C), UInt64($C0757B1C893C4582),
    UInt64($2B510DB8403A9297), UInt64($5C7698C1F1DB614A),
    UInt64($3E0D0118D5E68CB4), UInt64($D60F488E855CB4CF),
    UInt64($AE961E0DF3CB33D9), UInt64($3A8E55AB14A00ED7),
    UInt64($42170328623789C1), UInt64($838B6DD19C946292),
    UInt64($895FEF7DED3B3AEB), UInt64($CFCBB8E64E4A3149),
    UInt64($064C7E642F65C3DC), UInt64($3D2B3E2A4C5A63DA),
    UInt64($5BD3F340A9210C47), UInt64($B474D157A1615931),
    UInt64($AC5934DA1DE87266), UInt64($6EE365117AF7765B),
    UInt64($C86ED36716B05C44), UInt64($9BA6885C201D49C5),
    UInt64($B905387A88346C45), UInt64($131072C4BAB9DDFF),
    UInt64($BF49461EA751AF99), UInt64($D52977BC1CE05BA1),
    UInt64($B0F785E46027DB52), UInt64($546D30BA6E57788C),
    UInt64($305AD707650F56AE), UInt64($C987C682612FF295),
    UInt64($A5AB8944F5FBC571), UInt64($7ED528E759F244CA),
    UInt64($8DDCBBCE2C7DB888), UInt64($AA154ABE328DB1BA),
    UInt64($1E619BE993ECE88B), UInt64($09F2BD9EE813B717),
    UInt64($7401AA4B285D1CB3), UInt64($21858F143195CAEE),
    UInt64($48C381841398D1B8), UInt64($FCB750D3B2F98889),
    UInt64($39A86A998D1CE1B9), UInt64($1F888E0CE473465A),
    UInt64($7899568376978716), UInt64($02CF2AD7EE2341BF),
    UInt64($85C713B5B3F1A14E), UInt64($FF916FE12B4567E7),
    UInt64($7C1A0230B7D10575), UInt64($0C98FCC85ECA9BA5),
    UInt64($A3E7F720DA9E06AD), UInt64($6A6031A2BBB1F438),
    UInt64($973E74947ED7D260), UInt64($2CF4663918C0FF9A),
    UInt64($5F50A7F368678E24), UInt64($34D983B4A449D4CD),
    UInt64($68AF1B755592B587), UInt64($7F3C3D022E6DEA1B),
    UInt64($ABFC5F5B45121F6B), UInt64($0D71E92D29553574),
    UInt64($DFFDF5106D4F03D8), UInt64($081BA87B9F8C19C6),
    UInt64($DB7EA1A3AC0981BB), UInt64($BBCA12AD66172DFA),
    UInt64($79704366010829C7), UInt64($179326777BFF5F9C),
    UInt64($0000000000000000), UInt64($EB2476A4C906D715),
    UInt64($724DD42F0738DF6F), UInt64($B752EE6538DDB65F),
    UInt64($37FFBC863DF53BA3), UInt64($8EFA84FCB5C157E6),
    UInt64($E9EB5C73272596AA), UInt64($1B0BDABF2535C439),
    UInt64($86E12C872A4D4E20), UInt64($9969A28BCE3E087A),
    UInt64($FAFB2EB79D9C4B55), UInt64($056A4156B6D92CB2),
    UInt64($5A3AE6A5DEBEA296), UInt64($22A3B026A8292580),
    UInt64($53C85B3B36AD1581), UInt64($B11E900117B87583),
    UInt64($C51F3A4A3FE56930), UInt64($E019E1EDCF3621BD),
    UInt64($EC811D2591FCBA18), UInt64($445B7D4C4D524A1D),
    UInt64($A8DA6069DCAEF005), UInt64($58F5CC72309DE329),
    UInt64($D4C062596B7FF570), UInt64($CE22AD0339D59F98),
    UInt64($591CD99747024DF8), UInt64($8B90C5AA03187B54),
    UInt64($F663D27FC356D0F0), UInt64($D8589E9135B56ED5),
    UInt64($35309651D3D67A1C), UInt64($12F96721CD26732E),
    UInt64($D28C1C3D441A36AC), UInt64($492A946164077F69),
    UInt64($2D1D73DC6F5F514B), UInt64($6F0A70F40D68D88A),
    UInt64($60B4B30ECA1EAC41), UInt64($D36509D83385987D),
    UInt64($0B3D97490630F6A8), UInt64($9ECCC90A96C46577),
    UInt64($A20EE2C5AD01A87C), UInt64($E49AB55E0E70A3DE),
    UInt64($A4429CA182646BA0), UInt64($DA97B446DB962F6A),
    UInt64($CCED87D4D7F6DE27), UInt64($2AB8185D37A53C46),
    UInt64($9F25DCEFE15BCBA6), UInt64($C19C6EF9FEA3EB53),
    UInt64($A764A3931BD884CE), UInt64($2FD2590B817C10F4),
    UInt64($56A21A6D80743933), UInt64($E573A0BB79EF0D0F),
    UInt64($155C0CA095DC1E23), UInt64($6C2C4FC694D437E4),
    UInt64($10364DF623053291), UInt64($DD32DFC7836C4267),
    UInt64($03263F3299BCEF6E), UInt64($66F8CD6AE57B6F9D),
    UInt64($8C35AE2B5BE21659), UInt64($31B3C2E21290F87F),
    UInt64($93BD2027BF915003), UInt64($69460E90220D1B56),
    UInt64($299E276FAE19D328), UInt64($63928C3C53A2432F),
    UInt64($7082FEF8E91B9ED0), UInt64($BC6F792C3EED40F7),
    UInt64($4C40D537D2DE53DB), UInt64($75E8BFAE5FC2B262),
    UInt64($4DA9C0D2A541FD0A), UInt64($4E8FFFE03CFD1264),
    UInt64($2620E495696FA7E3), UInt64($E1F0F408B8A98F6C),
    UInt64($D1AA230FDDA6D9C2), UInt64($C7D0109DD1C6288F),
    UInt64($8A79D04F7487D585), UInt64($4694579BA3710BA2),
    UInt64($38417F7CFA834F68), UInt64($1D47A4DB0A5007E5),
    UInt64($206C9AF1460A643F), UInt64($A128DDF734BD4712),
    UInt64($8144470672B7232D), UInt64($F2E086CC02105293),
    UInt64($182DE58DBC892B57), UInt64($CAA1F9B0F8931DFB),
    UInt64($6B892447CC2E5AE9), UInt64($F9DD11850420A43B),
    UInt64($4BE5BEB68A243ED6), UInt64($5584255F19C8D65D),
    UInt64($3B67404E633FA006), UInt64($A68DB6766C472A1F),
    UInt64($F78AC79AB4C97E21), UInt64($C353442E1080AAEC),
    UInt64($9A4F9DB95782E714)),
    THashLibUInt64Array.Create(UInt64($05BA7BC82C9B3220),
    UInt64($31A54665F8B65E4F), UInt64($B1B651F77547F4D4),
    UInt64($8BFA0D857BA46682), UInt64($85A96C5AA16A98BB),
    UInt64($990FAEF908EB79C9), UInt64($A15E37A247F4A62D),
    UInt64($76857DCD5D27741E), UInt64($F8C50B800A1820BC),
    UInt64($BE65DCB201F7A2B4), UInt64($666D1B986F9426E7),
    UInt64($4CC921BF53C4E648), UInt64($95410A0F93D9CA42),
    UInt64($20CDCCAA647BA4EF), UInt64($429A4060890A1871),
    UInt64($0C4EA4F69B32B38B), UInt64($CCDA362DDE354CD3),
    UInt64($96DC23BC7C5B2FA9), UInt64($C309BB68AA851AB3),
    UInt64($D26131A73648E013), UInt64($021DC52941FC4DB2),
    UInt64($CD5ADAB7704BE48A), UInt64($A77965D984ED71E6),
    UInt64($32386FD61734BBA4), UInt64($E82D6DD538AB7245),
    UInt64($5C2147EA6177B4B1), UInt64($5DA1AB70CF091CE8),
    UInt64($AC907FCE72B8BDFF), UInt64($57C85DFD972278A8),
    UInt64($A4E44C6A6B6F940D), UInt64($3851995B4F1FDFE4),
    UInt64($62578CCAED71BC9E), UInt64($D9882BB0C01D2C0A),
    UInt64($917B9D5D113C503B), UInt64($A2C31E11A87643C6),
    UInt64($E463C923A399C1CE), UInt64($F71686C57EA876DC),
    UInt64($87B4A973E096D509), UInt64($AF0D567D9D3A5814),
    UInt64($B40C2A3F59DCC6F4), UInt64($3602F88495D121DD),
    UInt64($D3E1DD3D9836484A), UInt64($F945E71AA46688E5),
    UInt64($7518547EB2A591F5), UInt64($9366587450C01D89),
    UInt64($9EA81018658C065B), UInt64($4F54080CBC4603A3),
    UInt64($2D0384C65137BF3D), UInt64($DC325078EC861E2A),
    UInt64($EA30A8FC79573FF7), UInt64($214D2030CA050CB6),
    UInt64($65F0322B8016C30C), UInt64($69BE96DD1B247087),
    UInt64($DB95EE9981E161B8), UInt64($D1FC1814D9CA05F8),
    UInt64($820ED2BBCC0DE729), UInt64($63D76050430F14C7),
    UInt64($3BCCB0E8A09D3A0F), UInt64($8E40764D573F54A2),
    UInt64($39D175C1E16177BD), UInt64($12F5A37C734F1F4B),
    UInt64($AB37C12F1FDFC26D), UInt64($5648B167395CD0F1),
    UInt64($6C04ED1537BF42A7), UInt64($ED97161D14304065),
    UInt64($7D6C67DAAB72B807), UInt64($EC17FA87BA4EE83C),
    UInt64($DFAF79CB0304FBC1), UInt64($733F060571BC463E),
    UInt64($78D61C1287E98A27), UInt64($D07CF48E77B4ADA1),
    UInt64($B9C262536C90DD26), UInt64($E2449B5860801605),
    UInt64($8FC09AD7F941FCFB), UInt64($FAD8CEA94BE46D0E),
    UInt64($A343F28B0608EB9F), UInt64($9B126BD04917347B),
    UInt64($9A92874AE7699C22), UInt64($1B017C42C4E69EE0),
    UInt64($3A4C5C720EE39256), UInt64($4B6E9F5E3EA399DA),
    UInt64($6BA353F45AD83D35), UInt64($E7FEE0904C1B2425),
    UInt64($22D009832587E95D), UInt64($842980C00F1430E2),
    UInt64($C6B3C0A0861E2893), UInt64($087433A419D729F2),
    UInt64($341F3DADD42D6C6F), UInt64($EE0A3FAEFBB2A58E),
    UInt64($4AEE73C490DD3183), UInt64($AAB72DB5B1A16A34),
    UInt64($A92A04065E238FDF), UInt64($7B4B35A1686B6FCC),
    UInt64($6A23BF6EF4A6956C), UInt64($191CB96B851AD352),
    UInt64($55D598D4D6DE351A), UInt64($C9604DE5F2AE7EF3),
    UInt64($1CA6C2A3A981E172), UInt64($DE2F9551AD7A5398),
    UInt64($3025AAFF56C8F616), UInt64($15521D9D1E2860D9),
    UInt64($506FE31CFA45073A), UInt64($189C55F12B647B0B),
    UInt64($0180EC9AAE7EA859), UInt64($7CEC8B40050C105E),
    UInt64($2350E5198BF94104), UInt64($EF8AD33455CC0DD7),
    UInt64($07A7BEE16D677F92), UInt64($E5E325B90DE76997),
    UInt64($5A061591A26E637A), UInt64($B611EF1618208B46),
    UInt64($09F4DF3EB7A981AB), UInt64($1EBB078AE87DACC0),
    UInt64($B791038CB65E231F), UInt64($0FD38D4574B05660),
    UInt64($67EDF702C1EA8EBE), UInt64($BA5F4BE0831238CD),
    UInt64($E3C477C2CEFEBE5C), UInt64($0DCE486C354C1BD2),
    UInt64($8C5DB36416C31910), UInt64($26EA9ED1A7627324),
    UInt64($039D29B3EF82E5EB), UInt64($9F28FC82CBF2AE02),
    UInt64($A8AAE89CF05D2786), UInt64($431AACFA2774B028),
    UInt64($CF471F9E31B7A938), UInt64($581BD0B8E3922EC8),
    UInt64($BC78199B400BEF06), UInt64($90FB71C7BF42F862),
    UInt64($1F3BEB1046030499), UInt64($683E7A47B55AD8DE),
    UInt64($988F4263A695D190), UInt64($D808C72A6E638453),
    UInt64($0627527BC319D7CB), UInt64($EBB04466D72997AE),
    UInt64($E67E0C0AE2658C7C), UInt64($14D2F107B056C880),
    UInt64($7122C32C30400B8C), UInt64($8A7AE11FD5DACEDB),
    UInt64($A0DEDB38E98A0E74), UInt64($AD109354DCC615A6),
    UInt64($0BE91A17F655CC19), UInt64($8DDD5FFEB8BDB149),
    UInt64($BFE53028AF890AED), UInt64($D65BA6F5B4AD7A6A),
    UInt64($7956F0882997227E), UInt64($10E8665532B352F9),
    UInt64($0E5361DFDACEFE39), UInt64($CEC7F3049FC90161),
    UInt64($FF62B561677F5F2E), UInt64($975CCF26D22587F0),
    UInt64($51EF0F86543BAF63), UInt64($2F1E41EF10CBF28F),
    UInt64($52722635BBB94A88), UInt64($AE8DBAE73344F04D),
    UInt64($410769D36688FD9A), UInt64($B3AB94DE34BBB966),
    UInt64($801317928DF1AA9B), UInt64($A564A0F0C5113C54),
    UInt64($F131D4BEBDB1A117), UInt64($7F71A2F3EA8EF5B5),
    UInt64($40878549C8F655C3), UInt64($7EF14E6944F05DEC),
    UInt64($D44663DCF55137D8), UInt64($F2ACFD0D523344FC),
    UInt64($0000000000000000), UInt64($5FBC6E598EF5515A),
    UInt64($16CF342EF1AA8532), UInt64($B036BD6DDB395C8D),
    UInt64($13754FE6DD31B712), UInt64($BBDFA77A2D6C9094),
    UInt64($89E7C8AC3A582B30), UInt64($3C6B0E09CDFA459D),
    UInt64($C4AE0589C7E26521), UInt64($49735A777F5FD468),
    UInt64($CAFD64561D2C9B18), UInt64($DA1502032F9FC9E1),
    UInt64($8867243694268369), UInt64($3782141E3BAF8984),
    UInt64($9CB5D53124704BE9), UInt64($D7DB4A6F1AD3D233),
    UInt64($A6F989432A93D9BF), UInt64($9D3539AB8A0EE3B0),
    UInt64($53F2CAAF15C7E2D1), UInt64($6E19283C76430F15),
    UInt64($3DEBE2936384EDC4), UInt64($5E3C82C3208BF903),
    UInt64($33B8834CB94A13FD), UInt64($6470DEB12E686B55),
    UInt64($359FD1377A53C436), UInt64($61CAA57902F35975),
    UInt64($043A975282E59A79), UInt64($FD7F70482683129C),
    UInt64($C52EE913699CCD78), UInt64($28B9FF0E7DAC8D1D),
    UInt64($5455744E78A09D43), UInt64($CB7D88CCB3523341),
    UInt64($44BD121B4A13CFBA), UInt64($4D49CD25FDBA4E11),
    UInt64($3E76CB208C06082F), UInt64($3FF627BA2278A076),
    UInt64($C28957F204FBB2EA), UInt64($453DFE81E46D67E3),
    UInt64($94C1E6953DA7621B), UInt64($2C83685CFF491764),
    UInt64($F32C1197FC4DECA5), UInt64($2B24D6BD922E68F6),
    UInt64($B22B78449AC5113F), UInt64($48F3B6EDD1217C31),
    UInt64($2E9EAD75BEB55AD6), UInt64($174FD8B45FD42D6B),
    UInt64($4ED4E4961238ABFA), UInt64($92E6B4EEFEBEB5D0),
    UInt64($46A0D7320BEF8208), UInt64($47203BA8A5912A51),
    UInt64($24F75BF8E69E3E96), UInt64($F0B1382413CF094E),
    UInt64($FEE259FBC901F777), UInt64($276A724B091CDB7D),
    UInt64($BDF8F501EE75475F), UInt64($599B3C224DEC8691),
    UInt64($6D84018F99C1EAFE), UInt64($7498B8E41CDB39AC),
    UInt64($E0595E71217C5BB7), UInt64($2AA43A273C50C0AF),
    UInt64($F50B43EC3F543B6E), UInt64($838E3E2162734F70),
    UInt64($C09492DB4507FF58), UInt64($72BFEA9FDFC2EE67),
    UInt64($11688ACF9CCDFAA0), UInt64($1A8190D86A9836B9),
    UInt64($7ACBD93BC615C795), UInt64($C7332C3A286080CA),
    UInt64($863445E94EE87D50), UInt64($F6966A5FD0D6DE85),
    UInt64($E9AD814F96D5DA1C), UInt64($70A22FB69E3EA3D5),
    UInt64($0A69F68D582B6440), UInt64($B8428EC9C2EE757F),
    UInt64($604A49E3AC8DF12C), UInt64($5B86F90B0C10CB23),
    UInt64($E1D9B2EB8F02F3EE), UInt64($29391394D3D22544),
    UInt64($C8E0A17F5CD0D6AA), UInt64($B58CC6A5F7A26EAD),
    UInt64($8193FB08238F02C2), UInt64($D5C68F465B2F9F81),
    UInt64($FCFF9CD288FDBAC5), UInt64($77059157F359DC47),
    UInt64($1D262E3907FF492B), UInt64($FB582233E59AC557),
    UInt64($DDB2BCE242F8B673), UInt64($2577B76248E096CF),
    UInt64($6F99C4A6D83DA74C), UInt64($C1147E41EB795701),
    UInt64($F48BAF76912A9337)),
    THashLibUInt64Array.Create(UInt64($3EF29D249B2C0A19),
    UInt64($E9E16322B6F8622F), UInt64($5536994047757F7A),
    UInt64($9F4D56D5A47B0B33), UInt64($822567466AA1174C),
    UInt64($B8F5057DEB082FB2), UInt64($CC48C10BF4475F53),
    UInt64($373088D4275DEC3A), UInt64($968F4325180AED10),
    UInt64($173D232CF7016151), UInt64($AE4ED09F946FCC13),
    UInt64($FD4B4741C4539873), UInt64($1B5B3F0DD9933765),
    UInt64($2FFCB0967B644052), UInt64($E02376D20A89840C),
    UInt64($A3AE3A70329B18D7), UInt64($419CBD2335DE8526),
    UInt64($FAFEBF115B7C3199), UInt64($0397074F85AA9B0D),
    UInt64($C58AD4FB4836B970), UInt64($BEC60BE3FC4104A8),
    UInt64($1EFF36DC4B708772), UInt64($131FDC33ED8453B6),
    UInt64($0844E33E341764D3), UInt64($0FF11B6EAB38CD39),
    UInt64($64351F0A7761B85A), UInt64($3B5694F509CFBA0E),
    UInt64($30857084B87245D0), UInt64($47AFB3BD2297AE3C),
    UInt64($F2BA5C2F6F6B554A), UInt64($74BDC4761F4F70E1),
    UInt64($CFDFC64471EDC45E), UInt64($E610784C1DC0AF16),
    UInt64($7ACA29D63C113F28), UInt64($2DED411776A859AF),
    UInt64($AC5F211E99A3D5EE), UInt64($D484F949A87EF33B),
    UInt64($3CE36CA596E013E4), UInt64($D120F0983A9D432C),
    UInt64($6BC40464DC597563), UInt64($69D5F5E5D1956C9E),
    UInt64($9AE95F043698BB24), UInt64($C9ECC8DA66A4EF44),
    UInt64($D69508C8A5B2EAC6), UInt64($C40C2235C0503B80),
    UInt64($38C193BA8C652103), UInt64($1CEEC75D46BC9E8F),
    UInt64($D331011937515AD1), UInt64($D8E2E56886ECA50F),
    UInt64($B137108D5779C991), UInt64($709F3B6905CA4206),
    UInt64($4FEB50831680CAEF), UInt64($EC456AF3241BD238),
    UInt64($58D673AFE181ABBE), UInt64($242F54E7CAD9BF8C),
    UInt64($0211F1810DCC19FD), UInt64($90BC4DBB0F43C60A),
    UInt64($9518446A9DA0761D), UInt64($A1BFCBF13F57012A),
    UInt64($2BDE4F8961E172B5), UInt64($27B853A84F732481),
    UInt64($B0B1E643DF1F4B61), UInt64($18CC38425C39AC68),
    UInt64($D2B7F7D7BF37D821), UInt64($3103864A3014C720),
    UInt64($14AA246372ABFA5C), UInt64($6E600DB54EBAC574),
    UInt64($394765740403A3F3), UInt64($09C215F0BC71E623),
    UInt64($2A58B947E987F045), UInt64($7B4CDF18B477BDD8),
    UInt64($9709B5EB906C6FE0), UInt64($73083C268060D90B),
    UInt64($FEDC400E41F9037E), UInt64($284948C6E44BE9B8),
    UInt64($728ECAE808065BFB), UInt64($06330E9E17492B1A),
    UInt64($5950856169E7294E), UInt64($BAE4F4FCE6C4364F),
    UInt64($CA7BCF95E30E7449), UInt64($7D7FD186A33E96C2),
    UInt64($52836110D85AD690), UInt64($4DFAA1021B4CD312),
    UInt64($913ABB75872544FA), UInt64($DD46ECB9140F1518),
    UInt64($3D659A6B1E869114), UInt64($C23F2CABD719109A),
    UInt64($D713FE062DD46836), UInt64($D0A60656B2FBC1DC),
    UInt64($221C5A79DD909496), UInt64($EFD26DBCA1B14935),
    UInt64($0E77EDA0235E4FC9), UInt64($CBFD395B6B68F6B9),
    UInt64($0DE0EAEFA6F4D4C4), UInt64($0422FF1F1A8532E7),
    UInt64($F969B85EDED6AA94), UInt64($7F6E2007AEF28F3F),
    UInt64($3AD0623B81A938FE), UInt64($6624EE8B7AADA1A7),
    UInt64($B682E8DDC856607B), UInt64($A78CC56F281E2A30),
    UInt64($C79B257A45FAA08D), UInt64($5B4174E0642B30B3),
    UInt64($5F638BFF7EAE0254), UInt64($4BC9AF9C0C05F808),
    UInt64($CE59308AF98B46AE), UInt64($8FC58DA9CC55C388),
    UInt64($803496C7676D0EB1), UInt64($F33CAAE1E70DD7BA),
    UInt64($BB6202326EA2B4BF), UInt64($D5020F87201871CB),
    UInt64($9D5CA754A9B712CE), UInt64($841669D87DE83C56),
    UInt64($8A6184785EB6739F), UInt64($420BBA6CB0741E2B),
    UInt64($F12D5B60EAC1CE47), UInt64($76AC35F71283691C),
    UInt64($2C6BB7D9FECEDB5F), UInt64($FCCDB18F4C351A83),
    UInt64($1F79C012C3160582), UInt64($F0ABADAE62A74CB7),
    UInt64($E1A5801C82EF06FC), UInt64($67A21845F2CB2357),
    UInt64($5114665F5DF04D9D), UInt64($BF40FD2D74278658),
    UInt64($A0393D3FB73183DA), UInt64($05A409D192E3B017),
    UInt64($A9FB28CF0B4065F9), UInt64($25A9A22942BF3D7C),
    UInt64($DB75E22703463E02), UInt64($B326E10C5AB5D06C),
    UInt64($E7968E8295A62DE6), UInt64($B973F3B3636EAD42),
    UInt64($DF571D3819C30CE5), UInt64($EE549B7229D7CBC5),
    UInt64($12992AFD65E2D146), UInt64($F8EF4E9056B02864),
    UInt64($B7041E134030E28B), UInt64($C02EDD2ADAD50967),
    UInt64($932B4AF48AE95D07), UInt64($6FE6FB7BC6DC4784),
    UInt64($239AACB755F61666), UInt64($401A4BEDBDB807D6),
    UInt64($485EA8D389AF6305), UInt64($A41BC220ADB4B13D),
    UInt64($753B32B89729F211), UInt64($997E584BB3322029),
    UInt64($1D683193CEDA1C7F), UInt64($FF5AB6C0C99F818E),
    UInt64($16BBD5E27F67E3A1), UInt64($A59D34EE25D233CD),
    UInt64($98F8AE853B54A2D9), UInt64($6DF70AFACB105E79),
    UInt64($795D2E99B9BBA425), UInt64($8E437B6744334178),
    UInt64($0186F6CE886682F0), UInt64($EBF092A3BB347BD2),
    UInt64($BCD7FA62F18D1D55), UInt64($ADD9D7D011C5571E),
    UInt64($0BD3E471B1BDFFDE), UInt64($AA6C2F808EEAFEF4),
    UInt64($5EE57D31F6C880A4), UInt64($F50FA47FF044FCA0),
    UInt64($1ADDC9C351F5B595), UInt64($EA76646D3352F922),
    UInt64($0000000000000000), UInt64($85909F16F58EBEA6),
    UInt64($46294573AAF12CCC), UInt64($0A5512BF39DB7D2E),
    UInt64($78DBD85731DD26D5), UInt64($29CFBE086C2D6B48),
    UInt64($218B5D36583A0F9B), UInt64($152CD2ADFACD78AC),
    UInt64($83A39188E2C795BC), UInt64($C3B9DA655F7F926A),
    UInt64($9ECBA01B2C1D89C3), UInt64($07B5F8509F2FA9EA),
    UInt64($7EE8D6C926940DCF), UInt64($36B67E1AAF3B6ECA),
    UInt64($86079859702425AB), UInt64($FB7849DFD31AB369),
    UInt64($4C7C57CC932A51E2), UInt64($D96413A60E8A27FF),
    UInt64($263EA566C715A671), UInt64($6C71FC344376DC89),
    UInt64($4A4F595284637AF8), UInt64($DAF314E98B20BCF2),
    UInt64($572768C14AB96687), UInt64($1088DB7C682EC8BB),
    UInt64($887075F9537A6A62), UInt64($2E7A4658F302C2A2),
    UInt64($619116DBE582084D), UInt64($A87DDE018326E709),
    UInt64($DCC01A779C6997E8), UInt64($EDC39C3DAC7D50C8),
    UInt64($A60A33A1A078A8C0), UInt64($C1A82BE452B38B97),
    UInt64($3F746BEA134A88E9), UInt64($A228CCBEBAFD9A27),
    UInt64($ABEAD94E068C7C04), UInt64($F48952B178227E50),
    UInt64($5CF48CB0FB049959), UInt64($6017E0156DE48ABD),
    UInt64($4438B4F2A73D3531), UInt64($8C528AE649FF5885),
    UInt64($B515EF924DFCFB76), UInt64($0C661C212E925634),
    UInt64($B493195CC59A7986), UInt64($9CDA519A21D1903E),
    UInt64($32948105B5BE5C2D), UInt64($194ACE8CD45F2E98),
    UInt64($438D4CA238129CDB), UInt64($9B6FA9CABEFE39D4),
    UInt64($81B26009EF0B8C41), UInt64($DED1EBF691A58E15),
    UInt64($4E6DA64D9EE6481F), UInt64($54B06F8ECF13FD8A),
    UInt64($49D85E1D01C9E1F5), UInt64($AFC826511C094EE3),
    UInt64($F698A33075EE67AD), UInt64($5AC7822EEC4DB243),
    UInt64($8DD47C28C199DA75), UInt64($89F68337DB1CE892),
    UInt64($CDCE37C57C21DDA3), UInt64($530597DE503C5460),
    UInt64($6A42F2AA543FF793), UInt64($5D727A7E73621BA9),
    UInt64($E232875307459DF1), UInt64($56A19E0FC2DFE477),
    UInt64($C61DD3B4CD9C227D), UInt64($E5877F03986A341B),
    UInt64($949EB2A415C6F4ED), UInt64($6206119460289340),
    UInt64($6380E75AE84E11B0), UInt64($8BE772B6D6D0F16F),
    UInt64($50929091D596CF6D), UInt64($E86795EC3E9EE0DF),
    UInt64($7CF927482B581432), UInt64($C86A3E14EEC26DB4),
    UInt64($7119CDA78DACC0F6), UInt64($E40189CD100CB6EB),
    UInt64($92ADBC3A028FDFF7), UInt64($B2A017C2D2D3529C),
    UInt64($200DABF8D05C8D6B), UInt64($34A78F9BA2F77737),
    UInt64($E3B4719D8F231F01), UInt64($45BE423C2F5BB7C1),
    UInt64($F71E55FEFD88E55D), UInt64($6853032B59F3EE6E),
    UInt64($65B3E9C4FF073AAA), UInt64($772AC3399AE5EBEC),
    UInt64($87816E97F842A75B), UInt64($110E2DB2E0484A4B),
    UInt64($331277CB3DD8DEDD), UInt64($BD510CAC79EB9FA5),
    UInt64($352179552A91F5C7)),
    THashLibUInt64Array.Create(UInt64($8AB0A96846E06A6D),
    UInt64($43C7E80B4BF0B33A), UInt64($08C9B3546B161EE5),
    UInt64($39F1C235EBA990BE), UInt64($C1BEF2376606C7B2),
    UInt64($2C209233614569AA), UInt64($EB01523B6FC3289A),
    UInt64($946953AB935ACEDD), UInt64($272838F63E13340E),
    UInt64($8B0455ECA12BA052), UInt64($77A1B2C4978FF8A2),
    UInt64($A55122CA13E54086), UInt64($2276135862D3F1CD),
    UInt64($DB8DDFDE08B76CFE), UInt64($5D1E12C89E4A178A),
    UInt64($0E56816B03969867), UInt64($EE5F79953303ED59),
    UInt64($AFED748BAB78D71D), UInt64($6D929F2DF93E53EE),
    UInt64($F5D8A8F8BA798C2A), UInt64($F619B1698E39CF6B),
    UInt64($95DDAF2F749104E2), UInt64($EC2A9C80E0886427),
    UInt64($CE5C8FD8825B95EA), UInt64($C4E0D9993AC60271),
    UInt64($4699C3A5173076F9), UInt64($3D1B151F50A29F42),
    UInt64($9ED505EA2BC75946), UInt64($34665ACFDC7F4B98),
    UInt64($61B1FB53292342F7), UInt64($C721C0080E864130),
    UInt64($8693CD1696FD7B74), UInt64($872731927136B14B),
    UInt64($D3446C8A63A1721B), UInt64($669A35E8A6680E4A),
    UInt64($CAB658F239509A16), UInt64($A4E5DE4EF42E8AB9),
    UInt64($37A7435EE83F08D9), UInt64($134E6239E26C7F96),
    UInt64($82791A3C2DF67488), UInt64($3F6EF00A8329163C),
    UInt64($8E5A7E42FDEB6591), UInt64($5CAAEE4C7981DDB5),
    UInt64($19F234785AF1E80D), UInt64($255DDDE3ED98BD70),
    UInt64($50898A32A99CCCAC), UInt64($28CA4519DA4E6656),
    UInt64($AE59880F4CB31D22), UInt64($0D9798FA37D6DB26),
    UInt64($32F968F0B4FFCD1A), UInt64($A00F09644F258545),
    UInt64($FA3AD5175E24DE72), UInt64($F46C547C5DB24615),
    UInt64($713E80FBFF0F7E20), UInt64($7843CF2B73D2AAFA),
    UInt64($BD17EA36AEDF62B4), UInt64($FD111BACD16F92CF),
    UInt64($4ABAA7DBC72D67E0), UInt64($B3416B5DAD49FAD3),
    UInt64($BCA316B24914A88B), UInt64($15D150068AECF914),
    UInt64($E27C1DEBE31EFC40), UInt64($4FE48C759BEDA223),
    UInt64($7EDCFD141B522C78), UInt64($4E5070F17C26681C),
    UInt64($E696CAC15815F3BC), UInt64($35D2A64B3BB481A7),
    UInt64($800CFF29FE7DFDF6), UInt64($1ED9FAC3D5BAA4B0),
    UInt64($6C2663A91EF599D1), UInt64($03C1199134404341),
    UInt64($F7AD4DED69F20554), UInt64($CD9D9649B61BD6AB),
    UInt64($C8C3BDE7EADB1368), UInt64($D131899FB02AFB65),
    UInt64($1D18E352E1FAE7F1), UInt64($DA39235AEF7CA6C1),
    UInt64($A1BBF5E0A8EE4F7A), UInt64($91377805CF9A0B1E),
    UInt64($3138716180BF8E5B), UInt64($D9F83ACBDB3CE580),
    UInt64($0275E515D38B897E), UInt64($472D3F21F0FBBCC6),
    UInt64($2D946EB7868EA395), UInt64($BA3C248D21942E09),
    UInt64($E7223645BFDE3983), UInt64($FF64FEB902E41BB1),
    UInt64($C97741630D10D957), UInt64($C3CB1722B58D4ECC),
    UInt64($A27AEC719CAE0C3B), UInt64($99FECB51A48C15FB),
    UInt64($1465AC826D27332B), UInt64($E1BD047AD75EBF01),
    UInt64($79F733AF941960C5), UInt64($672EC96C41A3C475),
    UInt64($C27FEBA6524684F3), UInt64($64EFD0FD75E38734),
    UInt64($ED9E60040743AE18), UInt64($FB8E2993B9EF144D),
    UInt64($38453EB10C625A81), UInt64($6978480742355C12),
    UInt64($48CF42CE14A6EE9E), UInt64($1CAC1FD606312DCE),
    UInt64($7B82D6BA4792E9BB), UInt64($9D141C7B1F871A07),
    UInt64($5616B80DC11C4A2E), UInt64($B849C198F21FA777),
    UInt64($7CA91801C8D9A506), UInt64($B1348E487EC273AD),
    UInt64($41B20D1E987B3A44), UInt64($7460AB55A3CFBBE3),
    UInt64($84E628034576F20A), UInt64($1B87D16D897A6173),
    UInt64($0FE27DEFE45D5258), UInt64($83CDE6B8CA3DBEB7),
    UInt64($0C23647ED01D1119), UInt64($7A362A3EA0592384),
    UInt64($B61F40F3F1893F10), UInt64($75D457D1440471DC),
    UInt64($4558DA34237035B8), UInt64($DCA6116587FC2043),
    UInt64($8D9B67D3C9AB26D0), UInt64($2B0B5C88EE0E2517),
    UInt64($6FE77A382AB5DA90), UInt64($269CC472D9D8FE31),
    UInt64($63C41E46FAA8CB89), UInt64($B7ABBC771642F52F),
    UInt64($7D1DE4852F126F39), UInt64($A8C6BA3024339BA0),
    UInt64($600507D7CEE888C8), UInt64($8FEE82C61A20AFAE),
    UInt64($57A2448926D78011), UInt64($FCA5E72836A458F0),
    UInt64($072BCEBB8F4B4CBD), UInt64($497BBE4AF36D24A1),
    UInt64($3CAFE99BB769557D), UInt64($12FA9EBD05A7B5A9),
    UInt64($E8C04BAA5B836BDB), UInt64($4273148FAC3B7905),
    UInt64($908384812851C121), UInt64($E557D3506C55B0FD),
    UInt64($72FF996ACB4F3D61), UInt64($3EDA0C8E64E2DC03),
    UInt64($F0868356E6B949E9), UInt64($04EAD72ABB0B0FFC),
    UInt64($17A4B5135967706A), UInt64($E3C8E16F04D5367F),
    UInt64($F84F30028DAF570C), UInt64($1846C8FCBD3A2232),
    UInt64($5B8120F7F6CA9108), UInt64($D46FA231ECEA3EA6),
    UInt64($334D947453340725), UInt64($58403966C28AD249),
    UInt64($BED6F3A79A9F21F5), UInt64($68CCB483A5FE962D),
    UInt64($D085751B57E1315A), UInt64($FED0023DE52FD18E),
    UInt64($4B0E5B5F20E6ADDF), UInt64($1A332DE96EB1AB4C),
    UInt64($A3CE10F57B65C604), UInt64($108F7BA8D62C3CD7),
    UInt64($AB07A3A11073D8E1), UInt64($6B0DAD1291BED56C),
    UInt64($F2F366433532C097), UInt64($2E557726B2CEE0D4),
    UInt64($0000000000000000), UInt64($CB02A476DE9B5029),
    UInt64($E4E32FD48B9E7AC2), UInt64($734B65EE2C84F75E),
    UInt64($6E5386BCCD7E10AF), UInt64($01B4FC84E7CBCA3F),
    UInt64($CFE8735C65905FD5), UInt64($3613BFDA0FF4C2E6),
    UInt64($113B872C31E7F6E8), UInt64($2FE18BA255052AEB),
    UInt64($E974B72EBC48A1E4), UInt64($0ABC5641B89D979B),
    UInt64($B46AA5E62202B66E), UInt64($44EC26B0C4BBFF87),
    UInt64($A6903B5B27A503C7), UInt64($7F680190FC99E647),
    UInt64($97A84A3AA71A8D9C), UInt64($DD12EDE16037EA7C),
    UInt64($C554251DDD0DC84E), UInt64($88C54C7D956BE313),
    UInt64($4D91696048662B5D), UInt64($B08072CC9909B992),
    UInt64($B5DE5962C5C97C51), UInt64($81B803AD19B637C9),
    UInt64($B2F597D94A8230EC), UInt64($0B08AAC55F565DA4),
    UInt64($F1327FD2017283D6), UInt64($AD98919E78F35E63),
    UInt64($6AB9519676751F53), UInt64($24E921670A53774F),
    UInt64($B9FD3D1C15D46D48), UInt64($92F66194FBDA485F),
    UInt64($5A35DC7311015B37), UInt64($DED3F4705477A93D),
    UInt64($C00A0EB381CD0D8D), UInt64($BB88D809C65FE436),
    UInt64($16104997BEACBA55), UInt64($21B70AC95693B28C),
    UInt64($59F4C5E225411876), UInt64($D5DB5EB50B21F499),
    UInt64($55D7A19CF55C096F), UInt64($A97246B4C3F8519F),
    UInt64($8552D487A2BD3835), UInt64($54635D181297C350),
    UInt64($23C2EFDC85183BF2), UInt64($9F61F96ECC0C9379),
    UInt64($534893A39DDC8FED), UInt64($5EDF0B59AA0A54CB),
    UInt64($AC2C6D1A9F38945C), UInt64($D7AEBBA0D8AA7DE7),
    UInt64($2ABFA00C09C5EF28), UInt64($D84CC64F3CF72FBF),
    UInt64($2003F64DB15878B3), UInt64($A724C7DFC06EC9F8),
    UInt64($069F323F68808682), UInt64($CC296ACD51D01C94),
    UInt64($055E2BAE5CC0C5C3), UInt64($6270E2C21D6301B6),
    UInt64($3B842720382219C0), UInt64($D2F0900E846AB824),
    UInt64($52FC6F277A1745D2), UInt64($C6953C8CE94D8B0F),
    UInt64($E009F8FE3095753E), UInt64($655B2C7992284D0B),
    UInt64($984A37D54347DFC4), UInt64($EAB5AEBF8808E2A5),
    UInt64($9A3FD2C090CC56BA), UInt64($9CA0E0FFF84CD038),
    UInt64($4C2595E4AFADE162), UInt64($DF6708F4B3BC6302),
    UInt64($BF620F237D54EBCA), UInt64($93429D101C118260),
    UInt64($097D4FD08CDDD4DA), UInt64($8C2F9B572E60ECEF),
    UInt64($708A7C7F18C4B41F), UInt64($3A30DBA4DFE9D3FF),
    UInt64($4006F19A7FB0F07B), UInt64($5F6BF7DD4DC19EF4),
    UInt64($1F6D064732716E8F), UInt64($F9FBCC866A649D33),
    UInt64($308C8DE567744464), UInt64($8971B0F972A0292C),
    UInt64($D61A47243F61B7D8), UInt64($EFEB8511D4C82766),
    UInt64($961CB6BE40D147A3), UInt64($AAB35F25F7B812DE),
    UInt64($76154E407044329D), UInt64($513D76B64E570693),
    UInt64($F3479AC7D2F90AA8), UInt64($9B8B2E4477079C85),
    UInt64($297EB99D3D85AC69)),
    THashLibUInt64Array.Create(UInt64($7E37E62DFC7D40C3),
    UInt64($776F25A4EE939E5B), UInt64($E045C850DD8FB5AD),
    UInt64($86ED5BA711FF1952), UInt64($E91D0BD9CF616B35),
    UInt64($37E0AB256E408FFB), UInt64($9607F6C031025A7A),
    UInt64($0B02F5E116D23C9D), UInt64($F3D8486BFB50650C),
    UInt64($621CFF27C40875F5), UInt64($7D40CB71FA5FD34A),
    UInt64($6DAA6616DAA29062), UInt64($9F5F354923EC84E2),
    UInt64($EC847C3DC507C3B3), UInt64($025A3668043CE205),
    UInt64($A8BF9E6C4DAC0B19), UInt64($FA808BE2E9BEBB94),
    UInt64($B5B99C5277C74FA3), UInt64($78D9BC95F0397BCC),
    UInt64($E332E50CDBAD2624), UInt64($C74FCE129332797E),
    UInt64($1729ECEB2EA709AB), UInt64($C2D6B9F69954D1F8),
    UInt64($5D898CBFBAB8551A), UInt64($859A76FB17DD8ADB),
    UInt64($1BE85886362F7FB5), UInt64($F6413F8FF136CD8A),
    UInt64($D3110FA5BBB7E35C), UInt64($0A2FEED514CC4D11),
    UInt64($E83010EDCD7F1AB9), UInt64($A1E75DE55F42D581),
    UInt64($EEDE4A55C13B21B6), UInt64($F2F5535FF94E1480),
    UInt64($0CC1B46D1888761E), UInt64($BCE15FDB6529913B),
    UInt64($2D25E8975A7181C2), UInt64($71817F1CE2D7A554),
    UInt64($2E52C5CB5C53124B), UInt64($F9F7A6BEEF9C281D),
    UInt64($9E722E7D21F2F56E), UInt64($CE170D9B81DCA7E6),
    UInt64($0E9B82051CB4941B), UInt64($1E712F623C49D733),
    UInt64($21E45CFA42F9F7DC), UInt64($CB8E7A7F8BBA0F60),
    UInt64($8E98831A010FB646), UInt64($474CCF0D8E895B23),
    UInt64($A99285584FB27A95), UInt64($8CC2B57205335443),
    UInt64($42D5B8E984EFF3A5), UInt64($012D1B34021E718C),
    UInt64($57A6626AAE74180B), UInt64($FF19FC06E3D81312),
    UInt64($35BA9D4D6A7C6DFE), UInt64($C9D44C178F86ED65),
    UInt64($506523E6A02E5288), UInt64($03772D5C06229389),
    UInt64($8B01F4FE0B691EC0), UInt64($F8DABD8AED825991),
    UInt64($4C4E3AEC985B67BE), UInt64($B10DF0827FBF96A9),
    UInt64($6A69279AD4F8DAE1), UInt64($E78689DCD3D5FF2E),
    UInt64($812E1A2B1FA553D1), UInt64($FBAD90D6EBA0CA18),
    UInt64($1AC543B234310E39), UInt64($1604F7DF2CB97827),
    UInt64($A6241C6951189F02), UInt64($753513CCEAAF7C5E),
    UInt64($64F2A59FC84C4EFA), UInt64($247D2B1E489F5F5A),
    UInt64($DB64D718AB474C48), UInt64($79F4A7A1F2270A40),
    UInt64($1573DA832A9BEBAE), UInt64($3497867968621C72),
    UInt64($514838D2A2302304), UInt64($F0AF6537FD72F685),
    UInt64($1D06023E3A6B44BA), UInt64($678588C3CE6EDD73),
    UInt64($66A893F7CC70ACFF), UInt64($D4D24E29B5EDA9DF),
    UInt64($3856321470EA6A6C), UInt64($07C3418C0E5A4A83),
    UInt64($2BCBB22F5635BACD), UInt64($04B46CD00878D90A),
    UInt64($06EE5AB80C443B0F), UInt64($3B211F4876C8F9E5),
    UInt64($0958C38912EEDE98), UInt64($D14B39CDBF8B0159),
    UInt64($397B292072F41BE0), UInt64($87C0409313E168DE),
    UInt64($AD26E98847CAA39F), UInt64($4E140C849C6785BB),
    UInt64($D5FF551DB7F3D853), UInt64($A0CA46D15D5CA40D),
    UInt64($CD6020C787FE346F), UInt64($84B76DCF15C3FB57),
    UInt64($DEFDA0FCA121E4CE), UInt64($4B8D7B6096012D3D),
    UInt64($9AC642AD298A2C64), UInt64($0875D8BD10F0AF14),
    UInt64($B357C6EA7B8374AC), UInt64($4D6321D89A451632),
    UInt64($EDA96709C719B23F), UInt64($F76C24BBF328BC06),
    UInt64($C662D526912C08F2), UInt64($3CE25EC47892B366),
    UInt64($B978283F6F4F39BD), UInt64($C08C8F9E9D6833FD),
    UInt64($4F3917B09E79F437), UInt64($593DE06FB2C08C10),
    UInt64($D6887841B1D14BDA), UInt64($19B26EEE32139DB0),
    UInt64($B494876675D93E2F), UInt64($825937771987C058),
    UInt64($90E9AC783D466175), UInt64($F1827E03FF6C8709),
    UInt64($945DC0A8353EB87F), UInt64($4516F9658AB5B926),
    UInt64($3F9573987EB020EF), UInt64($B855330B6D514831),
    UInt64($2AE6A91B542BCB41), UInt64($6331E413C6160479),
    UInt64($408F8E8180D311A0), UInt64($EFF35161C325503A),
    UInt64($D06622F9BD9570D5), UInt64($8876D9A20D4B8D49),
    UInt64($A5533135573A0C8B), UInt64($E168D364DF91C421),
    UInt64($F41B09E7F50A2F8F), UInt64($12B09B0F24C1A12D),
    UInt64($DA49CC2CA9593DC4), UInt64($1F5C34563E57A6BF),
    UInt64($54D14F36A8568B82), UInt64($AF7CDFE043F6419A),
    UInt64($EA6A2685C943F8BC), UInt64($E5DCBFB4D7E91D2B),
    UInt64($B27ADDDE799D0520), UInt64($6B443CAED6E6AB6D),
    UInt64($7BAE91C9F61BE845), UInt64($3EB868AC7CAE5163),
    UInt64($11C7B65322E332A4), UInt64($D23C1491B9A992D0),
    UInt64($8FB5982E0311C7CA), UInt64($70AC6428E0C9D4D8),
    UInt64($895BC2960F55FCC5), UInt64($76423E90EC8DEFD7),
    UInt64($6FF0507EDE9E7267), UInt64($3DCF45F07A8CC2EA),
    UInt64($4AA06054941F5CB1), UInt64($5810FB5BB0DEFD9C),
    UInt64($5EFEA1E3BC9AC693), UInt64($6EDD4B4ADC8003EB),
    UInt64($741808F8E8B10DD2), UInt64($145EC1B728859A22),
    UInt64($28BC9F7350172944), UInt64($270A06424EBDCCD3),
    UInt64($972AEDF4331C2BF6), UInt64($059977E40A66A886),
    UInt64($2550302A4A812ED6), UInt64($DD8A8DA0A7037747),
    UInt64($C515F87A970E9B7B), UInt64($3023EAA9601AC578),
    UInt64($B7E3AA3A73FBADA6), UInt64($0FB699311EAAE597),
    UInt64($0000000000000000), UInt64($310EF19D6204B4F4),
    UInt64($229371A644DB6455), UInt64($0DECAF591A960792),
    UInt64($5CA4978BB8A62496), UInt64($1C2B190A38753536),
    UInt64($41A295B582CD602C), UInt64($3279DCC16426277D),
    UInt64($C1A194AA9F764271), UInt64($139D803B26DFD0A1),
    UInt64($AE51C4D441E83016), UInt64($D813FA44AD65DFC1),
    UInt64($AC0BF2BC45D4D213), UInt64($23BE6A9246C515D9),
    UInt64($49D74D08923DCF38), UInt64($9D05032127D066E7),
    UInt64($2F7FDEFF5E4D63C7), UInt64($A47E2A0155247D07),
    UInt64($99B16FF12FA8BFED), UInt64($4661D4398C972AAF),
    UInt64($DFD0BBC8A33F9542), UInt64($DCA79694A51D06CB),
    UInt64($B020EBB67DA1E725), UInt64($BA0F0563696DAA34),
    UInt64($E4F1A480D5F76CA7), UInt64($C438E34E9510EAF7),
    UInt64($939E81243B64F2FC), UInt64($8DEFAE46072D25CF),
    UInt64($2C08F3A3586FF04E), UInt64($D7A56375B3CF3A56),
    UInt64($20C947CE40E78650), UInt64($43F8A3DD86F18229),
    UInt64($568B795EAC6A6987), UInt64($8003011F1DBB225D),
    UInt64($F53612D3F7145E03), UInt64($189F75DA300DEC3C),
    UInt64($9570DB9C3720C9F3), UInt64($BB221E576B73DBB8),
    UInt64($72F65240E4F536DD), UInt64($443BE25188ABC8AA),
    UInt64($E21FFE38D9B357A8), UInt64($FD43CA6EE7E4F117),
    UInt64($CAA3614B89A47EEC), UInt64($FE34E732E1C6629E),
    UInt64($83742C431B99B1D4), UInt64($CF3A16AF83C2D66A),
    UInt64($AAE5A8044990E91C), UInt64($26271D764CA3BD5F),
    UInt64($91C4B74C3F5810F9), UInt64($7C6DD045F841A2C6),
    UInt64($7F1AFD19FE63314F), UInt64($C8F957238D989CE9),
    UInt64($A709075D5306EE8E), UInt64($55FC5402AA48FA0E),
    UInt64($48FA563C9023BEB4), UInt64($65DFBEABCA523F76),
    UInt64($6C877D22D8BCE1EE), UInt64($CC4D3BF385E045E3),
    UInt64($BEBB69B36115733E), UInt64($10EAAD6720FD4328),
    UInt64($B6CEB10E71E5DC2A), UInt64($BDCC44EF6737E0B7),
    UInt64($523F158EA412B08D), UInt64($989C74C52DB6CE61),
    UInt64($9BEB59992B945DE8), UInt64($8A2CEFCA09776F4C),
    UInt64($A3BD6B8D5B7E3784), UInt64($EB473DB1CB5D8930),
    UInt64($C3FBA2C29B4AA074), UInt64($9C28181525CE176B),
    UInt64($683311F2D0C438E4), UInt64($5FD3BAD7BE84B71F),
    UInt64($FC6ED15AE5FA809B), UInt64($36CDB0116C5EFE77),
    UInt64($29918447520958C8), UInt64($A29070B959604608),
    UInt64($53120EBAA60CC101), UInt64($3A0C047C74D68869),
    UInt64($691E0AC6D2DA4968), UInt64($73DB4974E6EB4751),
    UInt64($7A838AFDF40599C9), UInt64($5A4ACD33B4E21F99),
    UInt64($6046C94FC03497F0), UInt64($E6AB92E8D1CB8EA2),
    UInt64($3354C7F5663856F1), UInt64($D93EE170AF7BAE4D),
    UInt64($616BD27BC22AE67C), UInt64($92B39A10397A8370),
    UInt64($ABC8B3304B8E9890), UInt64($BF967287630B02B2),
    UInt64($5B67D607B6FC6E15)),
    THashLibUInt64Array.Create(UInt64($D031C397CE553FE6),
    UInt64($16BA5B01B006B525), UInt64($A89BADE6296E70C8),
    UInt64($6A1F525D77D3435B), UInt64($6E103570573DFA0B),
    UInt64($660EFB2A17FC95AB), UInt64($76327A9E97634BF6),
    UInt64($4BAD9D6462458BF5), UInt64($F1830CAEDBC3F748),
    UInt64($C5C8F542669131FF), UInt64($95044A1CDC48B0CB),
    UInt64($892962DF3CF8B866), UInt64($B0B9E208E930C135),
    UInt64($A14FB3F0611A767C), UInt64($8D2605F21C160136),
    UInt64($D6B71922FECC549E), UInt64($37089438A5907D8B),
    UInt64($0B5DA38E5803D49C), UInt64($5A5BCC9CEA6F3CBC),
    UInt64($EDAE246D3B73FFE5), UInt64($D2B87E0FDE22EDCE),
    UInt64($5E54ABB1CA8185EC), UInt64($1DE7F88FE80561B9),
    UInt64($AD5E1A870135A08C), UInt64($2F2ADBD665CECC76),
    UInt64($5780B5A782F58358), UInt64($3EDC8A2EEDE47B3F),
    UInt64($C9D95C3506BEE70F), UInt64($83BE111D6C4E05EE),
    UInt64($A603B90959367410), UInt64($103C81B4809FDE5D),
    UInt64($2C69B6027D0C774A), UInt64($399080D7D5C87953),
    UInt64($09D41E16487406B4), UInt64($CDD63B1826505E5F),
    UInt64($F99DC2F49B0298E8), UInt64($9CD0540A943CB67F),
    UInt64($BCA84B7F891F17C5), UInt64($723D1DB3B78DF2A6),
    UInt64($78AA6E71E73B4F2E), UInt64($1433E699A071670D),
    UInt64($84F21BE454620782), UInt64($98DF3327B4D20F2F),
    UInt64($F049DCE2D3769E5C), UInt64($DB6C60199656EB7A),
    UInt64($648746B2078B4783), UInt64($32CD23598DCBADCF),
    UInt64($1EA4955BF0C7DA85), UInt64($E9A143401B9D46B5),
    UInt64($FD92A5D9BBEC21B8), UInt64($C8138C790E0B8E1B),
    UInt64($2EE00B9A6D7BA562), UInt64($F85712B893B7F1FC),
    UInt64($EB28FED80BEA949D), UInt64($564A65EB8A40EA4C),
    UInt64($6C9988E8474A2823), UInt64($4535898B121D8F2D),
    UInt64($ABD8C03231ACCBF4), UInt64($BA2E91CAB9867CBD),
    UInt64($7960BE3DEF8E263A), UInt64($0C11A977602FD6F0),
    UInt64($CB50E1AD16C93527), UInt64($EAE22E94035FFD89),
    UInt64($2866D12F5DE2CE1A), UInt64($FF1B1841AB9BF390),
    UInt64($9F9339DE8CFE0D43), UInt64($964727C8C48A0BF7),
    UInt64($524502C6AAAE531C), UInt64($9B9C5EF3AC10B413),
    UInt64($4FA2FA4942AB32A5), UInt64($3F165A62E551122B),
    UInt64($C74148DA76E6E3D7), UInt64($924840E5E464B2A7),
    UInt64($D372AE43D69784DA), UInt64($233B72A105E11A86),
    UInt64($A48A04914941A638), UInt64($B4B68525C9DE7865),
    UInt64($DDEABAACA6CF8002), UInt64($0A9773C250B6BD88),
    UInt64($C284FFBB5EBD3393), UInt64($8BA0DF472C8F6A4E),
    UInt64($2AEF6CB74D951C32), UInt64($427983722A318D41),
    UInt64($73F7CDFFBF389BB2), UInt64($074C0AF9382C026C),
    UInt64($8A6A0F0B243A035A), UInt64($6FDAE53C5F88931F),
    UInt64($C68B98967E538AC3), UInt64($44FF59C71AA8E639),
    UInt64($E2FCE0CE439E9229), UInt64($A20CDE2479D8CD40),
    UInt64($19E89FA2C8EBD8E9), UInt64($F446BBCFF398270C),
    UInt64($43B3533E2284E455), UInt64($D82F0DCD8E945046),
    UInt64($51066F12B26CE820), UInt64($E73957AF6BC5426D),
    UInt64($081ECE5A40C16FA0), UInt64($3B193D4FC5BFAB7B),
    UInt64($7FE66488DF174D42), UInt64($0E9814EF705804D8),
    UInt64($8137AC857C39D7C6), UInt64($B1733244E185A821),
    UInt64($695C3F896F11F867), UInt64($F6CF0657E3EFF524),
    UInt64($1AABF276D02963D5), UInt64($2DA3664E75B91E5E),
    UInt64($0289BD981077D228), UInt64($90C1FD7DF413608F),
    UInt64($3C5537B6FD93A917), UInt64($AA12107E3919A2E0),
    UInt64($0686DAB530996B78), UInt64($DAA6B0559EE3826E),
    UInt64($C34E2FF756085A87), UInt64($6D5358A44FFF4137),
    UInt64($FC587595B35948AC), UInt64($7CA5095CC7D5F67E),
    UInt64($FB147F6C8B754AC0), UInt64($BFEB26AB91DDACF9),
    UInt64($6896EFC567A49173), UInt64($CA9A31E11E7C5C33),
    UInt64($BBE44186B13315A9), UInt64($0DDB793B689ABFE4),
    UInt64($70B4A02BA7FA208E), UInt64($E47A3A7B7307F951),
    UInt64($8CECD5BE14A36822), UInt64($EEED49B923B144D9),
    UInt64($17708B4DB8B3DC31), UInt64($6088219F2765FED3),
    UInt64($B3FA8FDCF1F27A09), UInt64($910B2D31FCA6099B),
    UInt64($0F52C4A378ED6DCC), UInt64($50CCBF5EBAD98134),
    UInt64($6BD582117F662A4F), UInt64($94CE9A50D4FDD9DF),
    UInt64($2B25BCFB45207526), UInt64($67C42B661F49FCBF),
    UInt64($492420FC723259DD), UInt64($03436DD418C2BB3C),
    UInt64($1F6E4517F872B391), UInt64($A08563BC69AF1F68),
    UInt64($D43EA4BAEEBB86B6), UInt64($01CAD04C08B56914),
    UInt64($AC94CACB0980C998), UInt64($54C3D8739A373864),
    UInt64($26FEC5C02DBACAC2), UInt64($DEA9D778BE0D3B3E),
    UInt64($040F672D20EEB950), UInt64($E5B0EA377BB29045),
    UInt64($F30AB136CBB42560), UInt64($62019C0737122CFB),
    UInt64($E86B930C13282FA1), UInt64($CC1CEB542EE5374B),
    UInt64($538FD28AA21B3A08), UInt64($1B61223AD89C0AC1),
    UInt64($36C24474AD25149F), UInt64($7A23D3E9F74C9D06),
    UInt64($BE21F6E79968C5ED), UInt64($CF5F868036278C77),
    UInt64($F705D61BEB5A9C30), UInt64($4D2B47D152DCE08D),
    UInt64($5F9E7BFDC234ECF8), UInt64($247778583DCD18EA),
    UInt64($867BA67C4415D5AA), UInt64($4CE1979D5A698999),
    UInt64($0000000000000000), UInt64($EC64F42133C696F1),
    UInt64($B57C5569C16B1171), UInt64($C1C7926F467F88AF),
    UInt64($654D96FE0F3E2E97), UInt64($15F936D5A8C40E19),
    UInt64($B8A72C52A9F1AE95), UInt64($A9517DAA21DB19DC),
    UInt64($58D27104FA18EE94), UInt64($5918A148F2AD8780),
    UInt64($5CDD1629DAF657C4), UInt64($8274C15164FB6CFA),
    UInt64($D1FB13DBC6E056F2), UInt64($7D6FD910CF609F6A),
    UInt64($B63F38BDD9A9AA4D), UInt64($3D9FE7FAF526C003),
    UInt64($74BBC706871499DE), UInt64($DF630734B6B8522A),
    UInt64($3AD3ED03CD0AC26F), UInt64($FADEAF2083C023D4),
    UInt64($C00D42234ECAE1BB), UInt64($8538CBA85CD76E96),
    UInt64($C402250E6E2458EB), UInt64($47BC3413026A5D05),
    UInt64($AFD7A71F114272A4), UInt64($978DF784CC3F62E3),
    UInt64($B96DFC1EA144C781), UInt64($21B2CF391596C8AE),
    UInt64($318E4E8D950916F3), UInt64($CE9556CC3E92E563),
    UInt64($385A509BDD7D1047), UInt64($358129A0B5E7AFA3),
    UInt64($E6F387E363702B79), UInt64($E0755D5653E94001),
    UInt64($7BE903A5FFF9F412), UInt64($12B53C2C90E80C75),
    UInt64($3307F315857EC4DB), UInt64($8FAFB86A0C61D31E),
    UInt64($D9E5DD8186213952), UInt64($77F8AAD29FD622E2),
    UInt64($25BDA814357871FE), UInt64($7571174A8FA1F0CA),
    UInt64($137FEC60985D6561), UInt64($30449EC19DBC7FE7),
    UInt64($A540D4DD41F4CF2C), UInt64($DC206AE0AE7AE916),
    UInt64($5B911CD0E2DA55A8), UInt64($B2305F90F947131D),
    UInt64($344BF9ECBD52C6B7), UInt64($5D17C665D2433ED0),
    UInt64($18224FEEC05EB1FD), UInt64($9E59E992844B6457),
    UInt64($9A568EBFA4A5DD07), UInt64($A3C60E68716DA454),
    UInt64($7E2CB4C4D7A22456), UInt64($87B176304CA0BCBE),
    UInt64($413AEEA632F3367D), UInt64($9915E36BBC67663B),
    UInt64($40F03EEA3A465F69), UInt64($1C2D28C3E0B008AD),
    UInt64($4E682A054A1E5BB1), UInt64($05C5B761285BD044),
    UInt64($E1BF8D1A5B5C2915), UInt64($F2C0617AC3014C74),
    UInt64($B7F5E8F1D11CC359), UInt64($63CB4C4B3FA745EF),
    UInt64($9D1A84469C89DF6B), UInt64($E33630824B2BFB3D),
    UInt64($D5F474F6E60EEFA2), UInt64($F58C6B83FB2D4E18),
    UInt64($4676E45F0ADF3411), UInt64($20781F751D23A1BA),
    UInt64($BD629B3381AA7ED1), UInt64($AE1D775319F71BB0),
    UInt64($FED1C80DA32E9A84), UInt64($5509083F92825170),
    UInt64($29AC01635557A70E), UInt64($A7C9694551831D04),
    UInt64($8E65682604D4BA0A), UInt64($11F651F8882AB749),
    UInt64($D77DC96EF6793D8A), UInt64($EF2799F52B042DCD),
    UInt64($48EEF0B07A8730C9), UInt64($22F1A2ED0D547392),
    UInt64($6142F1D32FD097C7), UInt64($4A674D286AF0E2E1),
    UInt64($80FD7CC9748CBED2), UInt64($717E7067AF4F499A),
    UInt64($938290A9ECD1DBB3), UInt64($88E3B293344DD172),
    UInt64($2734158C250FA3D6)));

{$ENDREGION}
end;

procedure TGOST3411_2012.GN(const AH, AN, AM: THashLibByteArray);
begin
  System.Move(AH[0], FTemp[0], 64 * System.SizeOf(Byte));

  xor512(AH, AN);
  F(AH);

  E(AH, AM);
  xor512(AH, FTemp);
  xor512(AH, AM);
end;

procedure TGOST3411_2012.Initialize;
begin
  FBOff := 64;
  TArrayUtils.ZeroFill(FN);
  TArrayUtils.ZeroFill(FSigma);

  System.Move(FIV[0], FH[0], 64 * System.SizeOf(Byte));

  TArrayUtils.ZeroFill(FBlock);
end;

procedure TGOST3411_2012.InternalUpdate(AInput: Byte);
begin
  System.Dec(FBOff);
  FBlock[FBOff] := AInput;
  if (FBOff = 0) then
  begin
    GN(FH, FN, FBlock);
    AddMod512(FN, 512);
    AddMod512(FSigma, FBlock);
    FBOff := 64;
  end;
end;

procedure TGOST3411_2012.Reverse(const ASource, ADestination
  : THashLibByteArray);
var
  len, i: Int32;
begin
  len := System.Length(ASource);
  for i := 0 to System.Pred(len) do
  begin
    ADestination[len - 1 - i] := ASource[i];
  end;
end;

procedure TGOST3411_2012.TransformBytes(const AData: THashLibByteArray;
  AIndex, ADataLength: Int32);
begin
  while ((FBOff <> 64) and (ADataLength > 0)) do
  begin
    InternalUpdate(AData[AIndex]);
    System.Inc(AIndex);
    System.Dec(ADataLength);
  end;
  while (ADataLength >= 64) do
  begin
    System.Move(AData[AIndex], FTemp[0], 64 * System.SizeOf(Byte));
    Reverse(FTemp, FBlock);
    GN(FH, FN, FBlock);
    AddMod512(FN, 512);
    AddMod512(FSigma, FBlock);

    ADataLength := ADataLength - 64;
    AIndex := AIndex + 64;
  end;
  while (ADataLength > 0) do
  begin
    InternalUpdate(AData[AIndex]);
    System.Inc(AIndex);
    System.Dec(ADataLength);
  end;

end;

function TGOST3411_2012.TransformFinal: IHashResult;
var
  tempRes: THashLibByteArray;
  lenM, i: Int32;
begin
  lenM := 64 - FBOff;

  // At this point it is certain that lenM is smaller than 64
  i := 0;
  while i <> (64 - lenM) do
  begin
    FM[i] := 0;
    System.Inc(i);
  end;

  FM[63 - lenM] := 1;

  if (FBOff <> 64) then
  begin
    System.Move(FBlock[FBOff], FM[64 - lenM], lenM * System.SizeOf(Byte));
  end;

  GN(FH, FN, FM);
  AddMod512(FN, lenM * 8);
  AddMod512(FSigma, FM);
  GN(FH, FZero, FN);
  GN(FH, FZero, FSigma);

  Reverse(FH, FTemp);

  System.SetLength(tempRes, 64);
  System.Move(FTemp[0], tempRes[0], 64 * System.SizeOf(Byte));

  result := THashResult.Create(tempRes);

  Initialize();
end;

{ TGOST3411_2012_256 }

function TGOST3411_2012_256.Clone(): IHash;
var
  LHashInstance: TGOST3411_2012_256;
begin
  LHashInstance := TGOST3411_2012_256.Create();
  LHashInstance.FIV := System.Copy(FIV);
  LHashInstance.FN := System.Copy(FN);
  LHashInstance.FSigma := System.Copy(FSigma);
  LHashInstance.FKi := System.Copy(FKi);
  LHashInstance.FM := System.Copy(FM);
  LHashInstance.FH := System.Copy(FH);
  LHashInstance.FTemp := System.Copy(FTemp);
  LHashInstance.FBlock := System.Copy(FBlock);
  LHashInstance.FBOff := FBOff;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TGOST3411_2012_256.Create();
begin
  inherited Create(32, FIV_256);
end;

class constructor TGOST3411_2012_256.TGOST3411_2012_256;
begin
  FIV_256 := THashLibByteArray.Create($01, $01, $01, $01, $01, $01, $01, $01,
    $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01,
    $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01,
    $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01,
    $01, $01, $01, $01, $01, $01, $01, $01, $01, $01, $01);
end;

function TGOST3411_2012_256.TransformFinal: IHashResult;
var
  LOutput, LTempRes: THashLibByteArray;
begin
  LOutput := inherited TransformFinal().GetBytes;
  System.SetLength(LTempRes, HashSize);
  System.Move(LOutput[32], LTempRes[0], 32 * System.SizeOf(Byte));
  result := THashResult.Create(LTempRes);
end;

{ TGOST3411_2012_512 }

function TGOST3411_2012_512.Clone(): IHash;
var
  LHashInstance: TGOST3411_2012_512;
begin
  LHashInstance := TGOST3411_2012_512.Create();
  LHashInstance.FIV := System.Copy(FIV);
  LHashInstance.FN := System.Copy(FN);
  LHashInstance.FSigma := System.Copy(FSigma);
  LHashInstance.FKi := System.Copy(FKi);
  LHashInstance.FM := System.Copy(FM);
  LHashInstance.FH := System.Copy(FH);
  LHashInstance.FTemp := System.Copy(FTemp);
  LHashInstance.FBlock := System.Copy(FBlock);
  LHashInstance.FBOff := FBOff;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TGOST3411_2012_512.Create();
begin
  inherited Create(64, FIV_512);
end;

class constructor TGOST3411_2012_512.TGOST3411_2012_512;
begin
  FIV_512 := THashLibByteArray.Create($00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00,
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00);
end;

end.
