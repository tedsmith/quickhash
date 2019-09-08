unit HlpMD2;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashCryptoNotBuildIn;

type
  TMD2 = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
    Fm_state, Fm_checksum: THashLibByteArray;

{$REGION 'Consts'}

  const

    s_pi: array [0 .. 255] of Byte = (41, 46, 67, 201, 162, 216, 124, 1, 61, 54,
      84, 161, 236, 240, 6, 19,

      98, 167, 5, 243, 192, 199, 115, 140, 152, 147, 43, 217, 188, 76, 130, 202,

      30, 155, 87, 60, 253, 212, 224, 22, 103, 66, 111, 24, 138, 23, 229, 18,

      190, 78, 196, 214, 218, 158, 222, 73, 160, 251, 245, 142, 187,
      47, 238, 122,

      169, 104, 121, 145, 21, 178, 7, 63, 148, 194, 16, 137, 11, 34, 95, 33,

      128, 127, 93, 154, 90, 144, 50, 39, 53, 62, 204, 231, 191, 247, 151, 3,

      255, 25, 48, 179, 72, 165, 181, 209, 215, 94, 146, 42, 172, 86, 170, 198,

      79, 184, 56, 210, 150, 164, 125, 182, 118, 252, 107, 226, 156,
      116, 4, 241,

      69, 157, 112, 89, 100, 113, 135, 32, 134, 91, 207, 101, 230, 45, 168, 2,

      27, 96, 37, 173, 174, 176, 185, 246, 28, 70, 97, 105, 52, 64, 126, 15,

      85, 71, 163, 35, 221, 81, 175, 58, 195, 92, 249, 206, 186, 197, 234, 38,

      44, 83, 13, 110, 133, 40, 132, 9, 211, 223, 205, 244, 65, 129, 77, 82,

      106, 220, 55, 200, 108, 193, 171, 250, 36, 225, 123, 8, 12, 189, 177, 74,

      120, 136, 149, 139, 227, 99, 232, 109, 233, 203, 213, 254, 59, 0, 29, 57,

      242, 239, 183, 14, 102, 88, 208, 228, 166, 119, 114, 248, 235,
      117, 75, 10,

      49, 68, 80, 180, 143, 237, 31, 26, 219, 153, 141, 51, 159, 17, 131, 20);

{$ENDREGION}
  strict protected
    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(a_data: PByte; a_data_length: Int32;
      a_index: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TMD2 }

function TMD2.Clone(): IHash;
var
  HashInstance: TMD2;
begin
  HashInstance := TMD2.Create();
  HashInstance.Fm_state := System.Copy(Fm_state);
  HashInstance.Fm_checksum := System.Copy(Fm_checksum);
  HashInstance.Fm_buffer := Fm_buffer.Clone();
  HashInstance.Fm_processed_bytes := Fm_processed_bytes;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMD2.Create;
begin
  Inherited Create(16, 16);
  System.SetLength(Fm_state, 16);
  System.SetLength(Fm_checksum, 16);

end;

procedure TMD2.Finish;
var
  padLen, i: Int32;
  pad: THashLibByteArray;
begin
  padLen := 16 - Fm_buffer.Pos;
  System.SetLength(pad, padLen);

  i := 0;
  while i < padLen do
  begin
    pad[i] := Byte(padLen);
    System.Inc(i);
  end;

  TransformBytes(pad, 0, padLen);
  TransformBytes(Fm_checksum, 0, 16);
end;

function TMD2.GetResult: THashLibByteArray;
begin
  result := System.Copy(Fm_state);
end;

procedure TMD2.Initialize;
begin

  TArrayUtils.ZeroFill(Fm_state);
  TArrayUtils.ZeroFill(Fm_checksum);

  Inherited Initialize();

end;

procedure TMD2.TransformBlock(a_data: PByte; a_data_length: Int32;
  a_index: Int32);
var
  i, j: Int32;
  t: UInt32;
  temp: array [0 .. 47] of Byte;

begin

  System.Move(Fm_state[0], temp[0], a_data_length);

  System.Move(a_data[a_index], temp[a_data_length], a_data_length);

  for i := 0 to 15 do
  begin
    temp[i + 32] := Byte(Fm_state[i] xor a_data[i + a_index]);
  end;

  t := 0;

  for i := 0 to 17 do
  begin

    for j := 0 to 47 do
    begin
      temp[j] := Byte(temp[j] xor s_pi[t]);
      t := temp[j];
    end;

    t := Byte(t + UInt32(i));
  end;

  System.Move(temp[0], Fm_state[0], 16);

  t := Fm_checksum[15];

  for i := 0 to 15 do
  begin

    Fm_checksum[i] := Fm_checksum[i] xor (s_pi[a_data[i + a_index] xor t]);
    t := Fm_checksum[i];
  end;

  System.FillChar(temp, System.SizeOf(temp), Byte(0));

end;

end.
