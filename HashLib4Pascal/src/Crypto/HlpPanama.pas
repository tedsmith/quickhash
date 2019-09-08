unit HlpPanama;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpBits,
{$IFDEF DELPHI}
  HlpBitConverter,
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashCryptoNotBuildIn;

type
  TPanama = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private

    Fm_state, Ftheta, Fgamma, Fpi: THashLibUInt32Array;

    Fm_stages: THashLibMatrixUInt32Array;

    Fm_tap: Int32;

    procedure GPT(a_theta: PCardinal);

  const

  strict protected
    procedure Finish(); override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;
    function GetResult(): THashLibByteArray; override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TPanama }

function TPanama.Clone(): IHash;
var
  HashInstance: TPanama;
  Idx: Int32;
begin
  HashInstance := TPanama.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Ftheta := System.Copy(Ftheta);
  HashInstance.Fgamma := System.Copy(Fgamma);
  HashInstance.Fpi := System.Copy(Fpi);
  // since System.Copy() does not support jagged arrays (multidimensional dynamic arrays, we improvise)
  for Idx := System.Low(Fm_stages) to System.High(Fm_stages) do
  begin
    HashInstance.Fm_stages[Idx] := System.Copy(Fm_stages[Idx]);
  end;
  HashInstance.Fm_tap := Fm_tap;
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TPanama.Create;
var
  i: Int32;
begin
  Inherited Create(32, 32);
  System.SetLength(Fm_state, 17);

  System.SetLength(Fm_stages, 32);
  i := 0;
  while i <= System.High(Fm_stages) do
  begin
    System.SetLength(Fm_stages[i], 8);
    System.Inc(i);
  end;

  System.SetLength(Ftheta, 17);

  System.SetLength(Fgamma, 17);

  System.SetLength(Fpi, 17);

end;

procedure TPanama.Finish;
var
  padding_size, i, tap4, tap16, tap25: Int32;
  pad: THashLibByteArray;
  theta: THashLibUInt32Array;
  ptr_theta: PCardinal;
begin

  padding_size := 32 - ((Int32(Fm_processed_bytes)) and 31);

  System.SetLength(pad, padding_size);

  pad[0] := $01;
  TransformBytes(pad, 0, padding_size);

  System.SetLength(theta, 17);

  ptr_theta := PCardinal(theta);

  i := 0;
  while i < 32 do
  begin
    tap4 := (Fm_tap + 4) and $1F;
    tap16 := (Fm_tap + 16) and $1F;

    Fm_tap := (Fm_tap - 1) and $1F;
    tap25 := (Fm_tap + 25) and $1F;

    GPT(ptr_theta);

    Fm_stages[tap25, 0] := Fm_stages[tap25, 0] xor Fm_stages[Fm_tap, 2];
    Fm_stages[tap25, 1] := Fm_stages[tap25, 1] xor Fm_stages[Fm_tap, 3];
    Fm_stages[tap25, 2] := Fm_stages[tap25, 2] xor Fm_stages[Fm_tap, 4];
    Fm_stages[tap25, 3] := Fm_stages[tap25, 3] xor Fm_stages[Fm_tap, 5];
    Fm_stages[tap25, 4] := Fm_stages[tap25, 4] xor Fm_stages[Fm_tap, 6];
    Fm_stages[tap25, 5] := Fm_stages[tap25, 5] xor Fm_stages[Fm_tap, 7];
    Fm_stages[tap25, 6] := Fm_stages[tap25, 6] xor Fm_stages[Fm_tap, 0];
    Fm_stages[tap25, 7] := Fm_stages[tap25, 7] xor Fm_stages[Fm_tap, 1];
    Fm_stages[Fm_tap, 0] := Fm_stages[Fm_tap, 0] xor Fm_state[1];
    Fm_stages[Fm_tap, 1] := Fm_stages[Fm_tap, 1] xor Fm_state[2];
    Fm_stages[Fm_tap, 2] := Fm_stages[Fm_tap, 2] xor Fm_state[3];
    Fm_stages[Fm_tap, 3] := Fm_stages[Fm_tap, 3] xor Fm_state[4];
    Fm_stages[Fm_tap, 4] := Fm_stages[Fm_tap, 4] xor Fm_state[5];
    Fm_stages[Fm_tap, 5] := Fm_stages[Fm_tap, 5] xor Fm_state[6];
    Fm_stages[Fm_tap, 6] := Fm_stages[Fm_tap, 6] xor Fm_state[7];
    Fm_stages[Fm_tap, 7] := Fm_stages[Fm_tap, 7] xor Fm_state[8];

    Fm_state[0] := theta[0] xor $01;
    Fm_state[1] := theta[1] xor Fm_stages[tap4, 0];
    Fm_state[2] := theta[2] xor Fm_stages[tap4, 1];
    Fm_state[3] := theta[3] xor Fm_stages[tap4, 2];
    Fm_state[4] := theta[4] xor Fm_stages[tap4, 3];
    Fm_state[5] := theta[5] xor Fm_stages[tap4, 4];
    Fm_state[6] := theta[6] xor Fm_stages[tap4, 5];
    Fm_state[7] := theta[7] xor Fm_stages[tap4, 6];
    Fm_state[8] := theta[8] xor Fm_stages[tap4, 7];
    Fm_state[9] := theta[9] xor Fm_stages[tap16, 0];
    Fm_state[10] := theta[10] xor Fm_stages[tap16, 1];
    Fm_state[11] := theta[11] xor Fm_stages[tap16, 2];
    Fm_state[12] := theta[12] xor Fm_stages[tap16, 3];
    Fm_state[13] := theta[13] xor Fm_stages[tap16, 4];
    Fm_state[14] := theta[14] xor Fm_stages[tap16, 5];
    Fm_state[15] := theta[15] xor Fm_stages[tap16, 6];
    Fm_state[16] := theta[16] xor Fm_stages[tap16, 7];

    System.Inc(i);
  end;

end;

function TPanama.GetResult: THashLibByteArray;
begin

  System.SetLength(result, 8 * System.SizeOf(UInt32));

  TConverters.le32_copy(PCardinal(Fm_state), 9 * System.SizeOf(UInt32),
    PByte(result), 0, System.Length(result));

end;

procedure TPanama.GPT(a_theta: PCardinal);
begin

  Fgamma[0] := Fm_state[0] xor (Fm_state[1] or not Fm_state[2]);
  Fgamma[1] := Fm_state[1] xor (Fm_state[2] or not Fm_state[3]);
  Fgamma[2] := Fm_state[2] xor (Fm_state[3] or not Fm_state[4]);
  Fgamma[3] := Fm_state[3] xor (Fm_state[4] or not Fm_state[5]);
  Fgamma[4] := Fm_state[4] xor (Fm_state[5] or not Fm_state[6]);
  Fgamma[5] := Fm_state[5] xor (Fm_state[6] or not Fm_state[7]);
  Fgamma[6] := Fm_state[6] xor (Fm_state[7] or not Fm_state[8]);
  Fgamma[7] := Fm_state[7] xor (Fm_state[8] or not Fm_state[9]);
  Fgamma[8] := Fm_state[8] xor (Fm_state[9] or not Fm_state[10]);
  Fgamma[9] := Fm_state[9] xor (Fm_state[10] or not Fm_state[11]);
  Fgamma[10] := Fm_state[10] xor (Fm_state[11] or not Fm_state[12]);
  Fgamma[11] := Fm_state[11] xor (Fm_state[12] or not Fm_state[13]);
  Fgamma[12] := Fm_state[12] xor (Fm_state[13] or not Fm_state[14]);
  Fgamma[13] := Fm_state[13] xor (Fm_state[14] or not Fm_state[15]);
  Fgamma[14] := Fm_state[14] xor (Fm_state[15] or not Fm_state[16]);
  Fgamma[15] := Fm_state[15] xor (Fm_state[16] or not Fm_state[0]);
  Fgamma[16] := Fm_state[16] xor (Fm_state[0] or not Fm_state[1]);

  Fpi[0] := Fgamma[0];
  Fpi[1] := TBits.RotateLeft32(Fgamma[7], 1);
  Fpi[2] := TBits.RotateLeft32(Fgamma[14], 3);
  Fpi[3] := TBits.RotateLeft32(Fgamma[4], 6);
  Fpi[4] := TBits.RotateLeft32(Fgamma[11], 10);
  Fpi[5] := TBits.RotateLeft32(Fgamma[1], 15);
  Fpi[6] := TBits.RotateLeft32(Fgamma[8], 21);
  Fpi[7] := TBits.RotateLeft32(Fgamma[15], 28);
  Fpi[8] := TBits.RotateLeft32(Fgamma[5], 4);
  Fpi[9] := TBits.RotateLeft32(Fgamma[12], 13);
  Fpi[10] := TBits.RotateLeft32(Fgamma[2], 23);
  Fpi[11] := TBits.RotateLeft32(Fgamma[9], 2);
  Fpi[12] := TBits.RotateLeft32(Fgamma[16], 14);
  Fpi[13] := TBits.RotateLeft32(Fgamma[6], 27);
  Fpi[14] := TBits.RotateLeft32(Fgamma[13], 9);
  Fpi[15] := TBits.RotateLeft32(Fgamma[3], 24);
  Fpi[16] := TBits.RotateLeft32(Fgamma[10], 8);

  a_theta[0] := Fpi[0] xor Fpi[1] xor Fpi[4];
  a_theta[1] := Fpi[1] xor Fpi[2] xor Fpi[5];
  a_theta[2] := Fpi[2] xor Fpi[3] xor Fpi[6];
  a_theta[3] := Fpi[3] xor Fpi[4] xor Fpi[7];
  a_theta[4] := Fpi[4] xor Fpi[5] xor Fpi[8];
  a_theta[5] := Fpi[5] xor Fpi[6] xor Fpi[9];
  a_theta[6] := Fpi[6] xor Fpi[7] xor Fpi[10];
  a_theta[7] := Fpi[7] xor Fpi[8] xor Fpi[11];
  a_theta[8] := Fpi[8] xor Fpi[9] xor Fpi[12];
  a_theta[9] := Fpi[9] xor Fpi[10] xor Fpi[13];
  a_theta[10] := Fpi[10] xor Fpi[11] xor Fpi[14];
  a_theta[11] := Fpi[11] xor Fpi[12] xor Fpi[15];
  a_theta[12] := Fpi[12] xor Fpi[13] xor Fpi[16];
  a_theta[13] := Fpi[13] xor Fpi[14] xor Fpi[0];
  a_theta[14] := Fpi[14] xor Fpi[15] xor Fpi[1];
  a_theta[15] := Fpi[15] xor Fpi[16] xor Fpi[2];
  a_theta[16] := Fpi[16] xor Fpi[0] xor Fpi[3];

end;

procedure TPanama.Initialize;
var
  i: Int32;
begin

  TArrayUtils.ZeroFill(Fm_state);
  TArrayUtils.ZeroFill(Ftheta);
  TArrayUtils.ZeroFill(Fgamma);
  TArrayUtils.ZeroFill(Fpi);

  Fm_tap := 0;

  for i := System.Low(Fm_stages) to System.High(Fm_stages) do
  begin
    TArrayUtils.ZeroFill(Fm_stages[i]);
  end;

  Inherited Initialize();

end;

procedure TPanama.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  work_buffer: array [0 .. 7] of UInt32;
  tap16, tap25: Int32;
begin

  TConverters.le32_copy(a_data, a_index, @(work_buffer[0]), 0, a_data_length);

  tap16 := (Fm_tap + 16) and $1F;

  Fm_tap := (Fm_tap - 1) and $1F;
  tap25 := (Fm_tap + 25) and $1F;

  GPT(PCardinal(Ftheta));

  Fm_stages[tap25, 0] := Fm_stages[tap25, 0] xor Fm_stages[Fm_tap, 2];
  Fm_stages[tap25, 1] := Fm_stages[tap25, 1] xor Fm_stages[Fm_tap, 3];
  Fm_stages[tap25, 2] := Fm_stages[tap25, 2] xor Fm_stages[Fm_tap, 4];
  Fm_stages[tap25, 3] := Fm_stages[tap25, 3] xor Fm_stages[Fm_tap, 5];
  Fm_stages[tap25, 4] := Fm_stages[tap25, 4] xor Fm_stages[Fm_tap, 6];
  Fm_stages[tap25, 5] := Fm_stages[tap25, 5] xor Fm_stages[Fm_tap, 7];
  Fm_stages[tap25, 6] := Fm_stages[tap25, 6] xor Fm_stages[Fm_tap, 0];
  Fm_stages[tap25, 7] := Fm_stages[tap25, 7] xor Fm_stages[Fm_tap, 1];
  Fm_stages[Fm_tap, 0] := Fm_stages[Fm_tap, 0] xor work_buffer[0];
  Fm_stages[Fm_tap, 1] := Fm_stages[Fm_tap, 1] xor work_buffer[1];
  Fm_stages[Fm_tap, 2] := Fm_stages[Fm_tap, 2] xor work_buffer[2];
  Fm_stages[Fm_tap, 3] := Fm_stages[Fm_tap, 3] xor work_buffer[3];
  Fm_stages[Fm_tap, 4] := Fm_stages[Fm_tap, 4] xor work_buffer[4];
  Fm_stages[Fm_tap, 5] := Fm_stages[Fm_tap, 5] xor work_buffer[5];
  Fm_stages[Fm_tap, 6] := Fm_stages[Fm_tap, 6] xor work_buffer[6];
  Fm_stages[Fm_tap, 7] := Fm_stages[Fm_tap, 7] xor work_buffer[7];

  Fm_state[0] := Ftheta[0] xor $01;
  Fm_state[1] := Ftheta[1] xor work_buffer[0];
  Fm_state[2] := Ftheta[2] xor work_buffer[1];
  Fm_state[3] := Ftheta[3] xor work_buffer[2];
  Fm_state[4] := Ftheta[4] xor work_buffer[3];
  Fm_state[5] := Ftheta[5] xor work_buffer[4];
  Fm_state[6] := Ftheta[6] xor work_buffer[5];
  Fm_state[7] := Ftheta[7] xor work_buffer[6];
  Fm_state[8] := Ftheta[8] xor work_buffer[7];
  Fm_state[9] := Ftheta[9] xor Fm_stages[tap16, 0];
  Fm_state[10] := Ftheta[10] xor Fm_stages[tap16, 1];
  Fm_state[11] := Ftheta[11] xor Fm_stages[tap16, 2];
  Fm_state[12] := Ftheta[12] xor Fm_stages[tap16, 3];
  Fm_state[13] := Ftheta[13] xor Fm_stages[tap16, 4];
  Fm_state[14] := Ftheta[14] xor Fm_stages[tap16, 5];
  Fm_state[15] := Ftheta[15] xor Fm_stages[tap16, 6];
  Fm_state[16] := Ftheta[16] xor Fm_stages[tap16, 7];

  System.FillChar(work_buffer, System.SizeOf(work_buffer), UInt32(0));

end;

end.
