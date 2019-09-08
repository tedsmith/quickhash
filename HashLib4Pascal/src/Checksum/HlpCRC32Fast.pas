unit HlpCRC32Fast;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

type

  TCRC32Fast = class(THash, IChecksum, IHash32, ITransformBlock)

  strict protected
  var
    FCurrentCRC: UInt32;

    procedure LocalCRCCompute(const ACRCTable: THashLibUInt32Array;
      const AData: THashLibByteArray; AIndex, ALength: Int32);

    class function Init_CRC_Table(APolynomial: UInt32)
      : THashLibUInt32Array; static;

  public

    constructor Create();

    procedure Initialize(); override;
    function TransformFinal(): IHashResult; override;

  end;

  TCRC32_PKZIP = class sealed(TCRC32Fast)

  strict private

  const
    // Polynomial Reversed
    CRC32_PKZIP_Polynomial = UInt32($EDB88320);
    class var

      FCRC32_PKZIP_Table: THashLibUInt32Array;

    class constructor CRC32_PKZIP();

  public
    constructor Create();
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function Clone(): IHash; override;

  end;

  TCRC32_CASTAGNOLI = class sealed(TCRC32Fast)

  strict private

  const
    CRC32_CASTAGNOLI_Polynomial = UInt32($82F63B78); // Polynomial Reversed
    class var

      FCRC32_CASTAGNOLI_Table: THashLibUInt32Array;

    class constructor CRC32_CASTAGNOLI();

  public
    constructor Create();
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function Clone(): IHash; override;

  end;

implementation

{ TCRC32Fast }

class function TCRC32Fast.Init_CRC_Table(APolynomial: UInt32)
  : THashLibUInt32Array;
var
  LIdx, LJIdx, LKIdx: Int32;
  LRes: UInt32;
begin
  System.SetLength(Result, 16 * 256);
  for LIdx := 0 to System.Pred(256) do
  begin
    LRes := LIdx;
    for LJIdx := 0 to System.Pred(16) do
    begin
      LKIdx := 0;
      while LKIdx < System.Pred(9) do
      begin
        { *
          // branched variant
          if (LRes and 1) = 1 then
          begin
          LRes := APolynomial xor (LRes shr 1)
          end
          else
          begin
          LRes := LRes shr 1;
          end;
          * }
        { *
          // branchless variant
          LRes := (LRes shr 1) xor (LRes and 1) * APolynomial;
          * }
        // faster branchless variant
        LRes := (LRes shr 1) xor (-Int32(LRes and 1) and APolynomial);
        Result[(LJIdx * 256) + LIdx] := LRes;
        System.Inc(LKIdx);
      end;
    end;
  end;
end;

procedure TCRC32Fast.LocalCRCCompute(const ACRCTable: THashLibUInt32Array;
  const AData: THashLibByteArray; AIndex, ALength: Int32);
var
  LCRC, LA, LB, LC, LD: UInt32;
  LCRCTable: THashLibUInt32Array;
begin
  LCRC := not FCurrentCRC; // LCRC := System.High(UInt32) xor FCurrentCRC;
  LCRCTable := ACRCTable;
  while ALength >= 16 do
  begin

    LA := LCRCTable[(3 * 256) + AData[AIndex + 12]] xor LCRCTable
      [(2 * 256) + AData[AIndex + 13]] xor LCRCTable
      [(1 * 256) + AData[AIndex + 14]] xor LCRCTable
      [(0 * 256) + AData[AIndex + 15]];

    LB := LCRCTable[(7 * 256) + AData[AIndex + 8]] xor LCRCTable
      [(6 * 256) + AData[AIndex + 9]] xor LCRCTable
      [(5 * 256) + AData[AIndex + 10]] xor LCRCTable
      [(4 * 256) + AData[AIndex + 11]];

    LC := LCRCTable[(11 * 256) + AData[AIndex + 4]] xor LCRCTable
      [(10 * 256) + AData[AIndex + 5]] xor LCRCTable
      [(9 * 256) + AData[AIndex + 6]] xor LCRCTable
      [(8 * 256) + AData[AIndex + 7]];

    LD := LCRCTable[(15 * 256) + ((LCRC and $FF) xor AData[AIndex])
      ] xor LCRCTable[(14 * 256) + (((LCRC shr 8) and $FF) xor AData[AIndex + 1]
      )] xor LCRCTable[(13 * 256) + (((LCRC shr 16) and $FF) xor AData
      [AIndex + 2])] xor LCRCTable
      [(12 * 256) + ((LCRC shr 24) xor AData[AIndex + 3])];

    LCRC := LD xor LC xor LB xor LA;
    System.Inc(AIndex, 16);
    System.Dec(ALength, 16);
  end;

  System.Dec(ALength);
  while (ALength >= 0) do
  begin
    LCRC := LCRCTable[Byte(LCRC xor AData[AIndex])] xor (LCRC shr 8);
    System.Inc(AIndex);
    System.Dec(ALength);
  end;

  FCurrentCRC := not LCRC; // FCurrentCRC := LCRC xor System.High(UInt32);
end;

constructor TCRC32Fast.Create();
begin
  Inherited Create(4, 1);
end;

procedure TCRC32Fast.Initialize;
begin
  FCurrentCRC := 0;
end;

function TCRC32Fast.TransformFinal: IHashResult;
begin
  Result := THashResult.Create(FCurrentCRC);
  Initialize();
end;

{ TCRC32_PKZIP }

function TCRC32_PKZIP.Clone(): IHash;
var
  HashInstance: TCRC32_PKZIP;
begin
  HashInstance := TCRC32_PKZIP.Create();
  HashInstance.FCurrentCRC := FCurrentCRC;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TCRC32_PKZIP.Create;
begin
  Inherited Create();
end;

procedure TCRC32_PKZIP.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
  LocalCRCCompute(FCRC32_PKZIP_Table, a_data, a_index, a_length);
end;

class constructor TCRC32_PKZIP.CRC32_PKZIP();
begin
  FCRC32_PKZIP_Table := Init_CRC_Table(CRC32_PKZIP_Polynomial);
end;

{ TCRC32_CASTAGNOLI }

function TCRC32_CASTAGNOLI.Clone(): IHash;
var
  HashInstance: TCRC32_CASTAGNOLI;
begin
  HashInstance := TCRC32_CASTAGNOLI.Create();
  HashInstance.FCurrentCRC := FCurrentCRC;
  Result := HashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TCRC32_CASTAGNOLI.Create;
begin
  Inherited Create();
end;

procedure TCRC32_CASTAGNOLI.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
begin
  LocalCRCCompute(FCRC32_CASTAGNOLI_Table, a_data, a_index, a_length);
end;

class constructor TCRC32_CASTAGNOLI.CRC32_CASTAGNOLI();
begin
  FCRC32_CASTAGNOLI_Table := Init_CRC_Table(CRC32_CASTAGNOLI_Polynomial);
end;

end.

