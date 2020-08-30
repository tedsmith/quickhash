unit HlpCRC32Fast;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpConverters;

type

  TCRC32Fast = class(THash, IChecksum, IHash32, ITransformBlock)

  strict protected
  var
    FCurrentCRC: UInt32;

    procedure LocalCRCCompute(const ACRCTable: THashLibMatrixUInt32Array;
      const AData: THashLibByteArray; AIndex, ALength: Int32);

    class function Init_CRC_Table(APolynomial: UInt32)
      : THashLibMatrixUInt32Array; static;

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

      FCRC32_PKZIP_Table: THashLibMatrixUInt32Array;

    class constructor CRC32_PKZIP();

  public
    constructor Create();
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function Clone(): IHash; override;

  end;

  TCRC32_CASTAGNOLI = class sealed(TCRC32Fast)

  strict private

  const
    CRC32_CASTAGNOLI_Polynomial = UInt32($82F63B78); // Polynomial Reversed
    class var

      FCRC32_CASTAGNOLI_Table: THashLibMatrixUInt32Array;

    class constructor CRC32_CASTAGNOLI();

  public
    constructor Create();
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function Clone(): IHash; override;

  end;

implementation

{ TCRC32Fast }

class function TCRC32Fast.Init_CRC_Table(APolynomial: UInt32)
  : THashLibMatrixUInt32Array;
var
  LIdx, LJIdx, LKIdx: Int32;
  LRes: UInt32;
begin
  System.SetLength(Result, 16);
  for LIdx := System.Low(Result) to System.High(Result) do
  begin
    System.SetLength(Result[LIdx], 256);
  end;
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
        Result[LJIdx][LIdx] := LRes;
        System.Inc(LKIdx);
      end;
    end;
  end;
end;

procedure TCRC32Fast.LocalCRCCompute(const ACRCTable: THashLibMatrixUInt32Array;
  const AData: THashLibByteArray; AIndex, ALength: Int32);
const
  Unroll = Int32(4);
  BytesAtOnce = Int32(16 * Unroll);
var
  LCRC, LOne, LTwo, LThree, LFour: UInt32;
  LCRCTable: THashLibMatrixUInt32Array;
  LCurrent: PCardinal;
  LUnrolling: Int32;
  LCurrentPtr: PByte;
begin
  LCRC := not FCurrentCRC; // LCRC := System.High(UInt32) xor FCurrentCRC;
  LCRCTable := ACRCTable;
  LCurrent := PCardinal(PByte(AData) + AIndex);
  while ALength >= BytesAtOnce do
  begin
    LUnrolling := 0;

    while LUnrolling < Unroll do
    begin
      LOne := TConverters.ReadPCardinalAsUInt32(LCurrent)
        xor TConverters.le2me_32(LCRC);
      System.Inc(LCurrent);
      LTwo := TConverters.ReadPCardinalAsUInt32(LCurrent);
      System.Inc(LCurrent);
      LThree := TConverters.ReadPCardinalAsUInt32(LCurrent);
      System.Inc(LCurrent);
      LFour := TConverters.ReadPCardinalAsUInt32(LCurrent);
      System.Inc(LCurrent);

{$IFDEF HASHLIB_LITTLE_ENDIAN}
      LCRC := LCRCTable[0][(LFour shr 24) and $FF] xor LCRCTable[1]
        [(LFour shr 16) and $FF] xor LCRCTable[2][(LFour shr 8) and $FF]
        xor LCRCTable[3][LFour and $FF] xor LCRCTable[4]
        [(LThree shr 24) and $FF] xor LCRCTable[5][(LThree shr 16) and $FF]
        xor LCRCTable[6][(LThree shr 8) and $FF] xor LCRCTable[7]
        [LThree and $FF] xor LCRCTable[8][(LTwo shr 24) and $FF] xor LCRCTable
        [9][(LTwo shr 16) and $FF] xor LCRCTable[10][(LTwo shr 8) and $FF]
        xor LCRCTable[11][LTwo and $FF] xor LCRCTable[12][(LOne shr 24) and $FF]
        xor LCRCTable[13][(LOne shr 16) and $FF] xor LCRCTable[14]
        [(LOne shr 8) and $FF] xor LCRCTable[15][LOne and $FF];
{$ELSE}
      LCRC := LCRCTable[0][LFour and $FF] xor LCRCTable[1]
        [(LFour shr 8) and $FF] xor LCRCTable[2][(LFour shr 16) and $FF]
        xor LCRCTable[3][(LFour shr 24) and $FF] xor LCRCTable[4]
        [LThree and $FF] xor LCRCTable[5][(LThree shr 8) and $FF] xor LCRCTable
        [6][(LThree shr 16) and $FF] xor LCRCTable[7][(LThree shr 24) and $FF]
        xor LCRCTable[8][LTwo and $FF] xor LCRCTable[9][(LTwo shr 8) and $FF]
        xor LCRCTable[10][(LTwo shr 16) and $FF] xor LCRCTable[11]
        [(LTwo shr 24) and $FF] xor LCRCTable[12][LOne and $FF] xor LCRCTable
        [13][(LOne shr 8) and $FF] xor LCRCTable[14][(LOne shr 16) and $FF]
        xor LCRCTable[15][(LOne shr 24) and $FF];
{$ENDIF HASHLIB_LITTLE_ENDIAN}
      System.Inc(LUnrolling);
    end;

    System.Dec(ALength, BytesAtOnce);
  end;

  LCurrentPtr := PByte(LCurrent);
  // remaining 1 to 63 bytes (standard algorithm)
  while (ALength <> 0) do
  begin
    LCRC := (LCRC shr 8) xor LCRCTable[0][(LCRC and $FF) xor LCurrentPtr^];
    System.Inc(LCurrentPtr);
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
var
  LBufferBytes: THashLibByteArray;
begin
  System.SetLength(LBufferBytes, HashSize);
  TConverters.ReadUInt32AsBytesBE(FCurrentCRC, LBufferBytes, 0);

  Result := THashResult.Create(LBufferBytes);
  Initialize();
end;

{ TCRC32_PKZIP }

function TCRC32_PKZIP.Clone(): IHash;
var
  LHashInstance: TCRC32_PKZIP;
begin
  LHashInstance := TCRC32_PKZIP.Create();
  LHashInstance.FCurrentCRC := FCurrentCRC;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TCRC32_PKZIP.Create;
begin
  Inherited Create();
end;

procedure TCRC32_PKZIP.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  LocalCRCCompute(FCRC32_PKZIP_Table, AData, AIndex, ALength);
end;

class constructor TCRC32_PKZIP.CRC32_PKZIP();
begin
  FCRC32_PKZIP_Table := Init_CRC_Table(CRC32_PKZIP_Polynomial);
end;

{ TCRC32_CASTAGNOLI }

function TCRC32_CASTAGNOLI.Clone(): IHash;
var
  LHashInstance: TCRC32_CASTAGNOLI;
begin
  LHashInstance := TCRC32_CASTAGNOLI.Create();
  LHashInstance.FCurrentCRC := FCurrentCRC;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TCRC32_CASTAGNOLI.Create;
begin
  Inherited Create();
end;

procedure TCRC32_CASTAGNOLI.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  LocalCRCCompute(FCRC32_CASTAGNOLI_Table, AData, AIndex, ALength);
end;

class constructor TCRC32_CASTAGNOLI.CRC32_CASTAGNOLI();
begin
  FCRC32_CASTAGNOLI_Table := Init_CRC_Table(CRC32_CASTAGNOLI_Polynomial);
end;

end.
