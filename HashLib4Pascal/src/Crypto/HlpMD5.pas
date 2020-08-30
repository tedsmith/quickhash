unit HlpMD5;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpBits,
  HlpMDBase,
{$IFDEF DELPHI}
  HlpHashBuffer,
  HlpHash,
{$ENDIF DELPHI}
  HlpIHash,
  HlpConverters,
  HlpIHashInfo;

type
  TMD5 = class sealed(TMDBase, ITransformBlock)

  strict protected
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    function Clone(): IHash; override;

  end;

implementation

{ TMD5 }

function TMD5.Clone(): IHash;
var
  LHashInstance: TMD5;
begin
  LHashInstance := TMD5.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TMD5.Create;
begin
  Inherited Create(4, 16);
end;

procedure TMD5.TransformBlock(AData: PByte; ADataLength: Int32; AIndex: Int32);
var
  A, B, C, D: UInt32;
  LData: array [0 .. 15] of UInt32;
begin
  TConverters.le32_copy(AData, AIndex, @(LData[0]), 0, ADataLength);

  A := FState[0];
  B := FState[1];
  C := FState[2];
  D := FState[3];

  A := LData[0] + $D76AA478 + A + ((B and C) or (not B and D));
  A := TBits.RotateLeft32(A, 7) + B;
  D := LData[1] + $E8C7B756 + D + ((A and B) or (not A and C));
  D := TBits.RotateLeft32(D, 12) + A;
  C := LData[2] + $242070DB + C + ((D and A) or (not D and B));
  C := TBits.RotateLeft32(C, 17) + D;
  B := LData[3] + $C1BDCEEE + B + ((C and D) or (not C and A));
  B := TBits.RotateLeft32(B, 22) + C;
  A := LData[4] + $F57C0FAF + A + ((B and C) or (not B and D));
  A := TBits.RotateLeft32(A, 7) + B;
  D := LData[5] + $4787C62A + D + ((A and B) or (not A and C));
  D := TBits.RotateLeft32(D, 12) + A;
  C := LData[6] + $A8304613 + C + ((D and A) or (not D and B));
  C := TBits.RotateLeft32(C, 17) + D;
  B := LData[7] + $FD469501 + B + ((C and D) or (not C and A));
  B := TBits.RotateLeft32(B, 22) + C;
  A := LData[8] + $698098D8 + A + ((B and C) or (not B and D));
  A := TBits.RotateLeft32(A, 7) + B;
  D := LData[9] + $8B44F7AF + D + ((A and B) or (not A and C));
  D := TBits.RotateLeft32(D, 12) + A;
  C := LData[10] + $FFFF5BB1 + C + ((D and A) or (not D and B));
  C := TBits.RotateLeft32(C, 17) + D;
  B := LData[11] + $895CD7BE + B + ((C and D) or (not C and A));
  B := TBits.RotateLeft32(B, 22) + C;
  A := LData[12] + $6B901122 + A + ((B and C) or (not B and D));
  A := TBits.RotateLeft32(A, 7) + B;
  D := LData[13] + $FD987193 + D + ((A and B) or (not A and C));
  D := TBits.RotateLeft32(D, 12) + A;
  C := LData[14] + $A679438E + C + ((D and A) or (not D and B));
  C := TBits.RotateLeft32(C, 17) + D;
  B := LData[15] + $49B40821 + B + ((C and D) or (not C and A));
  B := TBits.RotateLeft32(B, 22) + C;

  A := LData[1] + $F61E2562 + A + ((B and D) or (C and not D));
  A := TBits.RotateLeft32(A, 5) + B;
  D := LData[6] + $C040B340 + D + ((A and C) or (B and not C));
  D := TBits.RotateLeft32(D, 9) + A;
  C := LData[11] + $265E5A51 + C + ((D and B) or (A and not B));
  C := TBits.RotateLeft32(C, 14) + D;
  B := LData[0] + $E9B6C7AA + B + ((C and A) or (D and not A));
  B := TBits.RotateLeft32(B, 20) + C;
  A := LData[5] + $D62F105D + A + ((B and D) or (C and not D));
  A := TBits.RotateLeft32(A, 5) + B;
  D := LData[10] + $2441453 + D + ((A and C) or (B and not C));
  D := TBits.RotateLeft32(D, 9) + A;
  C := LData[15] + $D8A1E681 + C + ((D and B) or (A and not B));
  C := TBits.RotateLeft32(C, 14) + D;
  B := LData[4] + $E7D3FBC8 + B + ((C and A) or (D and not A));
  B := TBits.RotateLeft32(B, 20) + C;
  A := LData[9] + $21E1CDE6 + A + ((B and D) or (C and not D));
  A := TBits.RotateLeft32(A, 5) + B;
  D := LData[14] + $C33707D6 + D + ((A and C) or (B and not C));
  D := TBits.RotateLeft32(D, 9) + A;
  C := LData[3] + $F4D50D87 + C + ((D and B) or (A and not B));
  C := TBits.RotateLeft32(C, 14) + D;
  B := LData[8] + $455A14ED + B + ((C and A) or (D and not A));
  B := TBits.RotateLeft32(B, 20) + C;
  A := LData[13] + $A9E3E905 + A + ((B and D) or (C and not D));
  A := TBits.RotateLeft32(A, 5) + B;
  D := LData[2] + $FCEFA3F8 + D + ((A and C) or (B and not C));
  D := TBits.RotateLeft32(D, 9) + A;
  C := LData[7] + $676F02D9 + C + ((D and B) or (A and not B));
  C := TBits.RotateLeft32(C, 14) + D;
  B := LData[12] + $8D2A4C8A + B + ((C and A) or (D and not A));
  B := TBits.RotateLeft32(B, 20) + C;

  A := LData[5] + $FFFA3942 + A + (B xor C xor D);
  A := TBits.RotateLeft32(A, 4) + B;
  D := LData[8] + $8771F681 + D + (A xor B xor C);
  D := TBits.RotateLeft32(D, 11) + A;
  C := LData[11] + $6D9D6122 + C + (D xor A xor B);
  C := TBits.RotateLeft32(C, 16) + D;
  B := LData[14] + $FDE5380C + B + (C xor D xor A);
  B := TBits.RotateLeft32(B, 23) + C;
  A := LData[1] + $A4BEEA44 + A + (B xor C xor D);
  A := TBits.RotateLeft32(A, 4) + B;
  D := LData[4] + $4BDECFA9 + D + (A xor B xor C);
  D := TBits.RotateLeft32(D, 11) + A;
  C := LData[7] + $F6BB4B60 + C + (D xor A xor B);
  C := TBits.RotateLeft32(C, 16) + D;
  B := LData[10] + $BEBFBC70 + B + (C xor D xor A);
  B := TBits.RotateLeft32(B, 23) + C;
  A := LData[13] + $289B7EC6 + A + (B xor C xor D);
  A := TBits.RotateLeft32(A, 4) + B;
  D := LData[0] + $EAA127FA + D + (A xor B xor C);
  D := TBits.RotateLeft32(D, 11) + A;
  C := LData[3] + $D4EF3085 + C + (D xor A xor B);
  C := TBits.RotateLeft32(C, 16) + D;
  B := LData[6] + $4881D05 + B + (C xor D xor A);
  B := TBits.RotateLeft32(B, 23) + C;
  A := LData[9] + $D9D4D039 + A + (B xor C xor D);
  A := TBits.RotateLeft32(A, 4) + B;
  D := LData[12] + $E6DB99E5 + D + (A xor B xor C);
  D := TBits.RotateLeft32(D, 11) + A;
  C := LData[15] + $1FA27CF8 + C + (D xor A xor B);
  C := TBits.RotateLeft32(C, 16) + D;
  B := LData[2] + $C4AC5665 + B + (C xor D xor A);
  B := TBits.RotateLeft32(B, 23) + C;

  A := LData[0] + $F4292244 + A + (C xor (B or not D));
  A := TBits.RotateLeft32(A, 6) + B;
  D := LData[7] + $432AFF97 + D + (B xor (A or not C));
  D := TBits.RotateLeft32(D, 10) + A;
  C := LData[14] + $AB9423A7 + C + (A xor (D or not B));
  C := TBits.RotateLeft32(C, 15) + D;
  B := LData[5] + $FC93A039 + B + (D xor (C or not A));
  B := TBits.RotateLeft32(B, 21) + C;
  A := LData[12] + $655B59C3 + A + (C xor (B or not D));
  A := TBits.RotateLeft32(A, 6) + B;
  D := LData[3] + $8F0CCC92 + D + (B xor (A or not C));
  D := TBits.RotateLeft32(D, 10) + A;
  C := LData[10] + $FFEFF47D + C + (A xor (D or not B));
  C := TBits.RotateLeft32(C, 15) + D;
  B := LData[1] + $85845DD1 + B + (D xor (C or not A));
  B := TBits.RotateLeft32(B, 21) + C;
  A := LData[8] + $6FA87E4F + A + (C xor (B or not D));
  A := TBits.RotateLeft32(A, 6) + B;
  D := LData[15] + $FE2CE6E0 + D + (B xor (A or not C));
  D := TBits.RotateLeft32(D, 10) + A;
  C := LData[6] + $A3014314 + C + (A xor (D or not B));
  C := TBits.RotateLeft32(C, 15) + D;
  B := LData[13] + $4E0811A1 + B + (D xor (C or not A));
  B := TBits.RotateLeft32(B, 21) + C;
  A := LData[4] + $F7537E82 + A + (C xor (B or not D));
  A := TBits.RotateLeft32(A, 6) + B;
  D := LData[11] + $BD3AF235 + D + (B xor (A or not C));
  D := TBits.RotateLeft32(D, 10) + A;
  C := LData[2] + $2AD7D2BB + C + (A xor (D or not B));
  C := TBits.RotateLeft32(C, 15) + D;
  B := LData[9] + $EB86D391 + B + (D xor (C or not A));
  B := TBits.RotateLeft32(B, 21) + C;

  FState[0] := FState[0] + A;
  FState[1] := FState[1] + B;
  FState[2] := FState[2] + C;
  FState[3] := FState[3] + D;

  System.FillChar(LData, System.SizeOf(LData), UInt32(0));
end;

end.
