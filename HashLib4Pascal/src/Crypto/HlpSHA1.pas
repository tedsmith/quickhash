unit HlpSHA1;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpBits,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpSHA0,
  HlpIHash;

type
  TSHA1 = class sealed(TSHA0)

  strict protected
    procedure Expand(a_data: PCardinal); override;

  public
    // Not really needed because there is an Intristic default constructor always
    // called for classes if none is defined by the developer but I just put it
    // for readability reasons.
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TSHA1 }

function TSHA1.Clone(): IHash;
var
  HashInstance: TSHA1;
begin
  HashInstance := TSHA1.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TSHA1.Create;
begin
  Inherited Create();
end;

procedure TSHA1.Expand(a_data: PCardinal);
var
{$IFNDEF USE_UNROLLED_VARIANT}
  i: Int32;
{$ENDIF USE_UNROLLED_VARIANT}
  T: UInt32;
begin

{$IFDEF USE_UNROLLED_VARIANT}
  T := a_data[16 - 3] xor a_data[16 - 8] xor a_data[16 - 14] xor a_data[0];
  a_data[16] := TBits.RotateLeft32(T, 1);
  T := a_data[17 - 3] xor a_data[17 - 8] xor a_data[17 - 14] xor a_data
    [17 - 16];
  a_data[17] := TBits.RotateLeft32(T, 1);
  T := a_data[18 - 3] xor a_data[18 - 8] xor a_data[18 - 14] xor a_data
    [18 - 16];
  a_data[18] := TBits.RotateLeft32(T, 1);
  T := a_data[19 - 3] xor a_data[19 - 8] xor a_data[19 - 14] xor a_data
    [19 - 16];
  a_data[19] := TBits.RotateLeft32(T, 1);
  T := a_data[20 - 3] xor a_data[20 - 8] xor a_data[20 - 14] xor a_data
    [20 - 16];
  a_data[20] := TBits.RotateLeft32(T, 1);
  T := a_data[21 - 3] xor a_data[21 - 8] xor a_data[21 - 14] xor a_data
    [21 - 16];
  a_data[21] := TBits.RotateLeft32(T, 1);
  T := a_data[22 - 3] xor a_data[22 - 8] xor a_data[22 - 14] xor a_data
    [22 - 16];
  a_data[22] := TBits.RotateLeft32(T, 1);
  T := a_data[23 - 3] xor a_data[23 - 8] xor a_data[23 - 14] xor a_data
    [23 - 16];
  a_data[23] := TBits.RotateLeft32(T, 1);
  T := a_data[24 - 3] xor a_data[24 - 8] xor a_data[24 - 14] xor a_data
    [24 - 16];
  a_data[24] := TBits.RotateLeft32(T, 1);
  T := a_data[25 - 3] xor a_data[25 - 8] xor a_data[25 - 14] xor a_data
    [25 - 16];
  a_data[25] := TBits.RotateLeft32(T, 1);
  T := a_data[26 - 3] xor a_data[26 - 8] xor a_data[26 - 14] xor a_data
    [26 - 16];
  a_data[26] := TBits.RotateLeft32(T, 1);
  T := a_data[27 - 3] xor a_data[27 - 8] xor a_data[27 - 14] xor a_data
    [27 - 16];
  a_data[27] := TBits.RotateLeft32(T, 1);
  T := a_data[28 - 3] xor a_data[28 - 8] xor a_data[28 - 14] xor a_data
    [28 - 16];
  a_data[28] := TBits.RotateLeft32(T, 1);
  T := a_data[29 - 3] xor a_data[29 - 8] xor a_data[29 - 14] xor a_data
    [29 - 16];
  a_data[29] := TBits.RotateLeft32(T, 1);
  T := a_data[30 - 3] xor a_data[30 - 8] xor a_data[30 - 14] xor a_data
    [30 - 16];
  a_data[30] := TBits.RotateLeft32(T, 1);
  T := a_data[31 - 3] xor a_data[31 - 8] xor a_data[31 - 14] xor a_data
    [31 - 16];
  a_data[31] := TBits.RotateLeft32(T, 1);
  T := a_data[32 - 3] xor a_data[32 - 8] xor a_data[32 - 14] xor a_data
    [32 - 16];
  a_data[32] := TBits.RotateLeft32(T, 1);
  T := a_data[33 - 3] xor a_data[33 - 8] xor a_data[33 - 14] xor a_data
    [33 - 16];
  a_data[33] := TBits.RotateLeft32(T, 1);
  T := a_data[34 - 3] xor a_data[34 - 8] xor a_data[34 - 14] xor a_data
    [34 - 16];
  a_data[34] := TBits.RotateLeft32(T, 1);
  T := a_data[35 - 3] xor a_data[35 - 8] xor a_data[35 - 14] xor a_data
    [35 - 16];
  a_data[35] := TBits.RotateLeft32(T, 1);
  T := a_data[36 - 3] xor a_data[36 - 8] xor a_data[36 - 14] xor a_data
    [36 - 16];
  a_data[36] := TBits.RotateLeft32(T, 1);
  T := a_data[37 - 3] xor a_data[37 - 8] xor a_data[37 - 14] xor a_data
    [37 - 16];
  a_data[37] := TBits.RotateLeft32(T, 1);
  T := a_data[38 - 3] xor a_data[38 - 8] xor a_data[38 - 14] xor a_data
    [38 - 16];
  a_data[38] := TBits.RotateLeft32(T, 1);
  T := a_data[39 - 3] xor a_data[39 - 8] xor a_data[39 - 14] xor a_data
    [39 - 16];
  a_data[39] := TBits.RotateLeft32(T, 1);
  T := a_data[40 - 3] xor a_data[40 - 8] xor a_data[40 - 14] xor a_data
    [40 - 16];
  a_data[40] := TBits.RotateLeft32(T, 1);
  T := a_data[41 - 3] xor a_data[41 - 8] xor a_data[41 - 14] xor a_data
    [41 - 16];
  a_data[41] := TBits.RotateLeft32(T, 1);
  T := a_data[42 - 3] xor a_data[42 - 8] xor a_data[42 - 14] xor a_data
    [42 - 16];
  a_data[42] := TBits.RotateLeft32(T, 1);
  T := a_data[43 - 3] xor a_data[43 - 8] xor a_data[43 - 14] xor a_data
    [43 - 16];
  a_data[43] := TBits.RotateLeft32(T, 1);
  T := a_data[44 - 3] xor a_data[44 - 8] xor a_data[44 - 14] xor a_data
    [44 - 16];
  a_data[44] := TBits.RotateLeft32(T, 1);
  T := a_data[45 - 3] xor a_data[45 - 8] xor a_data[45 - 14] xor a_data
    [45 - 16];
  a_data[45] := TBits.RotateLeft32(T, 1);
  T := a_data[46 - 3] xor a_data[46 - 8] xor a_data[46 - 14] xor a_data
    [46 - 16];
  a_data[46] := TBits.RotateLeft32(T, 1);
  T := a_data[47 - 3] xor a_data[47 - 8] xor a_data[47 - 14] xor a_data
    [47 - 16];
  a_data[47] := TBits.RotateLeft32(T, 1);
  T := a_data[48 - 3] xor a_data[48 - 8] xor a_data[48 - 14] xor a_data
    [48 - 16];
  a_data[48] := TBits.RotateLeft32(T, 1);
  T := a_data[49 - 3] xor a_data[49 - 8] xor a_data[49 - 14] xor a_data
    [49 - 16];
  a_data[49] := TBits.RotateLeft32(T, 1);
  T := a_data[50 - 3] xor a_data[50 - 8] xor a_data[50 - 14] xor a_data
    [50 - 16];
  a_data[50] := TBits.RotateLeft32(T, 1);
  T := a_data[51 - 3] xor a_data[51 - 8] xor a_data[51 - 14] xor a_data
    [51 - 16];
  a_data[51] := TBits.RotateLeft32(T, 1);
  T := a_data[52 - 3] xor a_data[52 - 8] xor a_data[52 - 14] xor a_data
    [52 - 16];
  a_data[52] := TBits.RotateLeft32(T, 1);
  T := a_data[53 - 3] xor a_data[53 - 8] xor a_data[53 - 14] xor a_data
    [53 - 16];
  a_data[53] := TBits.RotateLeft32(T, 1);
  T := a_data[54 - 3] xor a_data[54 - 8] xor a_data[54 - 14] xor a_data
    [54 - 16];
  a_data[54] := TBits.RotateLeft32(T, 1);
  T := a_data[55 - 3] xor a_data[55 - 8] xor a_data[55 - 14] xor a_data
    [55 - 16];
  a_data[55] := TBits.RotateLeft32(T, 1);
  T := a_data[56 - 3] xor a_data[56 - 8] xor a_data[56 - 14] xor a_data
    [56 - 16];
  a_data[56] := TBits.RotateLeft32(T, 1);
  T := a_data[57 - 3] xor a_data[57 - 8] xor a_data[57 - 14] xor a_data
    [57 - 16];
  a_data[57] := TBits.RotateLeft32(T, 1);
  T := a_data[58 - 3] xor a_data[58 - 8] xor a_data[58 - 14] xor a_data
    [58 - 16];
  a_data[58] := TBits.RotateLeft32(T, 1);
  T := a_data[59 - 3] xor a_data[59 - 8] xor a_data[59 - 14] xor a_data
    [59 - 16];
  a_data[59] := TBits.RotateLeft32(T, 1);
  T := a_data[60 - 3] xor a_data[60 - 8] xor a_data[60 - 14] xor a_data
    [60 - 16];
  a_data[60] := TBits.RotateLeft32(T, 1);
  T := a_data[61 - 3] xor a_data[61 - 8] xor a_data[61 - 14] xor a_data
    [61 - 16];
  a_data[61] := TBits.RotateLeft32(T, 1);
  T := a_data[62 - 3] xor a_data[62 - 8] xor a_data[62 - 14] xor a_data
    [62 - 16];
  a_data[62] := TBits.RotateLeft32(T, 1);
  T := a_data[63 - 3] xor a_data[63 - 8] xor a_data[63 - 14] xor a_data
    [63 - 16];
  a_data[63] := TBits.RotateLeft32(T, 1);
  T := a_data[64 - 3] xor a_data[64 - 8] xor a_data[64 - 14] xor a_data
    [64 - 16];
  a_data[64] := TBits.RotateLeft32(T, 1);
  T := a_data[65 - 3] xor a_data[65 - 8] xor a_data[65 - 14] xor a_data
    [65 - 16];
  a_data[65] := TBits.RotateLeft32(T, 1);
  T := a_data[66 - 3] xor a_data[66 - 8] xor a_data[66 - 14] xor a_data
    [66 - 16];
  a_data[66] := TBits.RotateLeft32(T, 1);
  T := a_data[67 - 3] xor a_data[67 - 8] xor a_data[67 - 14] xor a_data
    [67 - 16];
  a_data[67] := TBits.RotateLeft32(T, 1);
  T := a_data[68 - 3] xor a_data[68 - 8] xor a_data[68 - 14] xor a_data
    [68 - 16];
  a_data[68] := TBits.RotateLeft32(T, 1);
  T := a_data[69 - 3] xor a_data[69 - 8] xor a_data[69 - 14] xor a_data
    [69 - 16];
  a_data[69] := TBits.RotateLeft32(T, 1);
  T := a_data[70 - 3] xor a_data[70 - 8] xor a_data[70 - 14] xor a_data
    [70 - 16];
  a_data[70] := TBits.RotateLeft32(T, 1);
  T := a_data[71 - 3] xor a_data[71 - 8] xor a_data[71 - 14] xor a_data
    [71 - 16];
  a_data[71] := TBits.RotateLeft32(T, 1);
  T := a_data[72 - 3] xor a_data[72 - 8] xor a_data[72 - 14] xor a_data
    [72 - 16];
  a_data[72] := TBits.RotateLeft32(T, 1);
  T := a_data[73 - 3] xor a_data[73 - 8] xor a_data[73 - 14] xor a_data
    [73 - 16];
  a_data[73] := TBits.RotateLeft32(T, 1);
  T := a_data[74 - 3] xor a_data[74 - 8] xor a_data[74 - 14] xor a_data
    [74 - 16];
  a_data[74] := TBits.RotateLeft32(T, 1);
  T := a_data[75 - 3] xor a_data[75 - 8] xor a_data[75 - 14] xor a_data
    [75 - 16];
  a_data[75] := TBits.RotateLeft32(T, 1);
  T := a_data[76 - 3] xor a_data[76 - 8] xor a_data[76 - 14] xor a_data
    [76 - 16];
  a_data[76] := TBits.RotateLeft32(T, 1);
  T := a_data[77 - 3] xor a_data[77 - 8] xor a_data[77 - 14] xor a_data
    [77 - 16];
  a_data[77] := TBits.RotateLeft32(T, 1);
  T := a_data[78 - 3] xor a_data[78 - 8] xor a_data[78 - 14] xor a_data
    [78 - 16];
  a_data[78] := TBits.RotateLeft32(T, 1);
  T := a_data[79 - 3] xor a_data[79 - 8] xor a_data[79 - 14] xor a_data
    [79 - 16];
  a_data[79] := TBits.RotateLeft32(T, 1);

{$ELSE}
  for i := 16 to 79 do
  begin
    T := a_data[i - 3] xor a_data[i - 8] xor a_data[i - 14] xor a_data[i - 16];
    a_data[i] := TBits.RotateLeft32(T, 1);
  end;
{$ENDIF USE_UNROLLED_VARIANT}
end;

end.
