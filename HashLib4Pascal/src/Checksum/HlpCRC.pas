unit HlpCRC;

// A vast majority if not all of the parameters for these CRC standards
// were gotten from http://reveng.sourceforge.net/crc-catalogue/.

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
  TypInfo,
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult,
  HlpICRC;

resourcestring
  SUnSupportedCRCType = 'UnSupported CRC Type: "%s"';
  SWidthOutOfRange = 'Width Must be Between 3 and 64. "%d"';

{$REGION 'CRC Standards'}

type
  /// <summary>
  /// Enum of all defined and implemented CRC standards.
  /// </summary>
  TCRCStandard = (

    /// <summary>
    /// CRC standard named "CRC3_GSM".
    /// </summary>
    CRC3_GSM,

    /// <summary>
    /// CRC standard named "CRC3_ROHC".
    /// </summary>
    CRC3_ROHC,

    /// <summary>
    /// CRC standard named "CRC4_INTERLAKEN".
    /// </summary>
    CRC4_INTERLAKEN,

    /// <summary>
    /// CRC standard named "CRC4_ITU".
    /// </summary>
    CRC4_ITU,

    /// <summary>
    /// CRC standard named "CRC5_EPC".
    /// </summary>
    CRC5_EPC,

    /// <summary>
    /// CRC standard named "CRC5_ITU".
    /// </summary>
    CRC5_ITU,

    /// <summary>
    /// CRC standard named "CRC5_USB".
    /// </summary>
    CRC5_USB,

    /// <summary>
    /// CRC standard named "CRC6_CDMA2000A".
    /// </summary>
    CRC6_CDMA2000A,

    /// <summary>
    /// CRC standard named "CRC6_CDMA2000B".
    /// </summary>
    CRC6_CDMA2000B,

    /// <summary>
    /// CRC standard named "CRC6_DARC".
    /// </summary>
    CRC6_DARC,

    /// <summary>
    /// CRC standard named "CRC6_GSM".
    /// </summary>
    CRC6_GSM,

    /// <summary>
    /// CRC standard named "CRC6_ITU".
    /// </summary>
    CRC6_ITU,

    /// <summary>
    /// CRC standard named "CRC7".
    /// </summary>
    CRC7,

    /// <summary>
    /// CRC standard named "CRC7_ROHC".
    /// </summary>
    CRC7_ROHC,

    /// <summary>
    /// CRC standard named "CRC7_UMTS".
    /// </summary>
    CRC7_UMTS,

    /// <summary>
    /// CRC standard named "CRC8".
    /// </summary>
    CRC8,

    /// <summary>
    /// CRC standard named "CRC8_AUTOSAR".
    /// </summary>
    CRC8_AUTOSAR,

    /// <summary>
    /// CRC standard named "CRC8_BLUETOOTH".
    /// </summary>
    CRC8_BLUETOOTH,

    /// <summary>
    /// CRC standard named "CRC8_CDMA2000".
    /// </summary>
    CRC8_CDMA2000,

    /// <summary>
    /// CRC standard named "CRC8_DARC".
    /// </summary>
    CRC8_DARC,

    /// <summary>
    /// CRC standard named "CRC8_DVBS2".
    /// </summary>
    CRC8_DVBS2,

    /// <summary>
    /// CRC standard named "CRC8_EBU".
    /// </summary>
    CRC8_EBU,

    /// <summary>
    /// CRC standard named "CRC8_GSMA".
    /// </summary>
    CRC8_GSMA,

    /// <summary>
    /// CRC standard named "CRC8_GSMB".
    /// </summary>
    CRC8_GSMB,

    /// <summary>
    /// CRC standard named "CRC8_ICODE".
    /// </summary>
    CRC8_ICODE,

    /// <summary>
    /// CRC standard named "CRC8_ITU".
    /// </summary>
    CRC8_ITU,

    /// <summary>
    /// CRC standard named "CRC8_LTE".
    /// </summary>
    CRC8_LTE,

    /// <summary>
    /// CRC standard named "CRC8_MAXIM".
    /// </summary>
    CRC8_MAXIM,

    /// <summary>
    /// CRC standard named "CRC8_OPENSAFETY".
    /// </summary>
    CRC8_OPENSAFETY,

    /// <summary>
    /// CRC standard named "CRC8_ROHC".
    /// </summary>
    CRC8_ROHC,

    /// <summary>
    /// CRC standard named "CRC8_SAEJ1850".
    /// </summary>
    CRC8_SAEJ1850,

    /// <summary>
    /// CRC standard named "CRC8_WCDMA".
    /// </summary>
    CRC8_WCDMA,

    /// <summary>
    /// CRC standard named "CRC8_MIFAREMAD".
    /// </summary>
    CRC8_MIFAREMAD,

    /// <summary>
    /// CRC standard named "CRC8_NRSC5".
    /// </summary>
    CRC8_NRSC5,

    /// <summary>
    /// CRC standard named "CRC10".
    /// </summary>
    CRC10,

    /// <summary>
    /// CRC standard named "CRC10_CDMA2000".
    /// </summary>
    CRC10_CDMA2000,

    /// <summary>
    /// CRC standard named "CRC10_GSM".
    /// </summary>
    CRC10_GSM,

    /// <summary>
    /// CRC standard named "CRC11".
    /// </summary>
    CRC11,

    /// <summary>
    /// CRC standard named "CRC11_UMTS".
    /// </summary>
    CRC11_UMTS,

    /// <summary>
    /// CRC standard named "CRC12_CDMA2000".
    /// </summary>
    CRC12_CDMA2000,

    /// <summary>
    /// CRC standard named "CRC12_DECT".
    /// </summary>
    CRC12_DECT,

    /// <summary>
    /// CRC standard named "CRC12_GSM".
    /// </summary>
    CRC12_GSM,

    /// <summary>
    /// CRC standard named "CRC12_UMTS".
    /// </summary>
    CRC12_UMTS,

    /// <summary>
    /// CRC standard named "CRC13_BBC".
    /// </summary>
    CRC13_BBC,

    /// <summary>
    /// CRC standard named "CRC14_DARC".
    /// </summary>
    CRC14_DARC,

    /// <summary>
    /// CRC standard named "CRC14_GSM".
    /// </summary>
    CRC14_GSM,

    /// <summary>
    /// CRC standard named "CRC15".
    /// </summary>
    CRC15,

    /// <summary>
    /// CRC standard named "CRC15_MPT1327".
    /// </summary>
    CRC15_MPT1327,

    /// <summary>
    /// CRC standard named "ARC".
    /// </summary>
    ARC,

    /// <summary>
    /// CRC standard named "CRC16_AUGCCITT".
    /// </summary>
    CRC16_AUGCCITT,

    /// <summary>
    /// CRC standard named "CRC16_BUYPASS".
    /// </summary>
    CRC16_BUYPASS,

    /// <summary>
    /// CRC standard named "CRC16_CCITTFALSE".
    /// </summary>
    CRC16_CCITTFALSE,

    /// <summary>
    /// CRC standard named "CRC16_CDMA2000".
    /// </summary>
    CRC16_CDMA2000,

    /// <summary>
    /// CRC standard named "CRC16_CMS".
    /// </summary>
    CRC16_CMS,

    /// <summary>
    /// CRC standard named "CRC16_DDS110".
    /// </summary>
    CRC16_DDS110,

    /// <summary>
    /// CRC standard named "CRC16_DECTR".
    /// </summary>
    CRC16_DECTR,

    /// <summary>
    /// CRC standard named "CRC16_DECTX".
    /// </summary>
    CRC16_DECTX,

    /// <summary>
    /// CRC standard named "CRC16_DNP".
    /// </summary>
    CRC16_DNP,

    /// <summary>
    /// CRC standard named "CRC16_EN13757".
    /// </summary>
    CRC16_EN13757,

    /// <summary>
    /// CRC standard named "CRC16_GENIBUS".
    /// </summary>
    CRC16_GENIBUS,

    /// <summary>
    /// CRC standard named "CRC16_GSM".
    /// </summary>
    CRC16_GSM,

    /// <summary>
    /// CRC standard named "CRC16_LJ1200".
    /// </summary>
    CRC16_LJ1200,

    /// <summary>
    /// CRC standard named "CRC16_MAXIM".
    /// </summary>
    CRC16_MAXIM,

    /// <summary>
    /// CRC standard named "CRC16_MCRF4XX".
    /// </summary>
    CRC16_MCRF4XX,

    /// <summary>
    /// CRC standard named "CRC16_OPENSAFETYA".
    /// </summary>
    CRC16_OPENSAFETYA,

    /// <summary>
    /// CRC standard named "CRC16_OPENSAFETYB".
    /// </summary>
    CRC16_OPENSAFETYB,

    /// <summary>
    /// CRC standard named "CRC16_PROFIBUS".
    /// </summary>
    CRC16_PROFIBUS,

    /// <summary>
    /// CRC standard named "CRC16_RIELLO".
    /// </summary>
    CRC16_RIELLO,

    /// <summary>
    /// CRC standard named "CRC16_T10DIF".
    /// </summary>
    CRC16_T10DIF,

    /// <summary>
    /// CRC standard named "CRC16_TELEDISK".
    /// </summary>
    CRC16_TELEDISK,

    /// <summary>
    /// CRC standard named "CRC16_TMS37157".
    /// </summary>
    CRC16_TMS37157,

    /// <summary>
    /// CRC standard named "CRC16_USB".
    /// </summary>
    CRC16_USB,

    /// <summary>
    /// CRC standard named "CRCA".
    /// </summary>
    CRCA,

    /// <summary>
    /// CRC standard named "KERMIT".
    /// </summary>
    KERMIT,

    /// <summary>
    /// CRC standard named "MODBUS".
    /// </summary>
    MODBUS,

    /// <summary>
    /// CRC standard named "X25".
    /// </summary>
    X25,

    /// <summary>
    /// CRC standard named "XMODEM".
    /// </summary>
    XMODEM,

    /// <summary>
    /// CRC standard named "CRC16_NRSC5".
    /// </summary>
    CRC16_NRSC5,

    /// <summary>
    /// CRC standard named "CRC17_CANFD".
    /// </summary>
    CRC17_CANFD,

    /// <summary>
    /// CRC standard named "CRC21_CANFD".
    /// </summary>
    CRC21_CANFD,

    /// <summary>
    /// CRC standard named "CRC24".
    /// </summary>
    CRC24,

    /// <summary>
    /// CRC standard named "CRC24_BLE".
    /// </summary>
    CRC24_BLE,

    /// <summary>
    /// CRC standard named "CRC24_FLEXRAYA".
    /// </summary>
    CRC24_FLEXRAYA,

    /// <summary>
    /// CRC standard named "CRC24_FLEXRAYB".
    /// </summary>
    CRC24_FLEXRAYB,

    /// <summary>
    /// CRC standard named "CRC24_INTERLAKEN".
    /// </summary>
    CRC24_INTERLAKEN,

    /// <summary>
    /// CRC standard named "CRC24_LTEA".
    /// </summary>
    CRC24_LTEA,

    /// <summary>
    /// CRC standard named "CRC24_LTEB".
    /// </summary>
    CRC24_LTEB,

    /// <summary>
    /// CRC standard named "CRC24_OS9".
    /// </summary>
    CRC24_OS9,

    /// <summary>
    /// CRC standard named "CRC30_CDMA".
    /// </summary>
    CRC30_CDMA,

    /// <summary>
    /// CRC standard named "CRC31_PHILIPS".
    /// </summary>
    CRC31_PHILIPS,

    /// <summary>
    /// CRC standard named "CRC32".
    /// </summary>
    CRC32,

    /// <summary>
    /// CRC standard named "CRC32_AUTOSAR".
    /// </summary>
    CRC32_AUTOSAR,

    /// <summary>
    /// CRC standard named "CRC32_BZIP2".
    /// </summary>
    CRC32_BZIP2,

    /// <summary>
    /// CRC standard named "CRC32C".
    /// </summary>
    CRC32C,

    /// <summary>
    /// CRC standard named "CRC32D".
    /// </summary>
    CRC32D,

    /// <summary>
    /// CRC standard named "CRC32_MPEG2".
    /// </summary>
    CRC32_MPEG2,

    /// <summary>
    /// CRC standard named "CRC32_POSIX".
    /// </summary>
    CRC32_POSIX,

    /// <summary>
    /// CRC standard named "CRC32Q".
    /// </summary>
    CRC32Q,

    /// <summary>
    /// CRC standard named "JAMCRC".
    /// </summary>
    JAMCRC,

    /// <summary>
    /// CRC standard named "XFER".
    /// </summary>
    XFER,

    /// <summary>
    /// CRC standard named "CRC32_CDROMEDC".
    /// </summary>
    CRC32_CDROMEDC,

    /// <summary>
    /// CRC standard named "CRC40_GSM".
    /// </summary>
    CRC40_GSM,

    /// <summary>
    /// CRC standard named "CRC64".
    /// </summary>
    CRC64,

    /// <summary>
    /// CRC standard named "CRC64_GOISO".
    /// </summary>
    CRC64_GOISO,

    /// <summary>
    /// CRC standard named "CRC64_WE".
    /// </summary>
    CRC64_WE,

    /// <summary>
    /// CRC standard named "CRC64_XZ".
    /// </summary>
    CRC64_XZ,

    /// <summary>
    /// CRC standard named "CRC64_1B".
    /// </summary>
    CRC64_1B,

    /// <summary>
    /// CRC standard named "CRC64_Jones".
    /// </summary>
    CRC64_Jones);

{$ENDREGION}

type
  TCRC = class sealed(THash, IChecksum, ICRC, ITransformBlock)

  strict private
  var
    FNames: THashLibStringArray;
    FWidth: Int32;
    FPolynomial, FInitialValue, FOutputXor, FCheckValue, FCRCMask,
      FCRCHighBitMask, FHash: UInt64;
    FIsInputReflected, FIsOutputReflected, FIsTableGenerated: Boolean;

    FCRCTable: THashLibUInt64Array;

  const
    Delta = Int32(7);

    function GetNames: THashLibStringArray; inline;
    procedure SetNames(const AValue: THashLibStringArray); inline;
    function GetWidth: Int32; inline;
    procedure SetWidth(AValue: Int32); inline;
    function GetPolynomial: UInt64; inline;
    procedure SetPolynomial(AValue: UInt64); inline;
    function GetInitialValue: UInt64; inline;
    procedure SetInitialValue(AValue: UInt64); inline;
    function GetIsInputReflected: Boolean; inline;
    procedure SetIsInputReflected(AValue: Boolean); inline;
    function GetIsOutputReflected: Boolean; inline;
    procedure SetIsOutputReflected(AValue: Boolean); inline;
    function GetOutputXor: UInt64; inline;
    procedure SetOutputXor(AValue: UInt64); inline;
    function GetCheckValue: UInt64; inline;
    procedure SetCheckValue(AValue: UInt64); inline;

    procedure GenerateTable();
    // tables work only for CRCs with width > 7
    procedure CalculateCRCbyTable(AData: PByte; ADataLength, AIndex: Int32);
    // fast bit by bit algorithm without augmented zero bytes.
    // does not use lookup table, suited for polynomial orders between 1...32.
    procedure CalculateCRCdirect(AData: PByte; ADataLength, AIndex: Int32);

    // reflects the lower 'width' LBits of 'value'
    class function Reflect(AValue: UInt64; AWidth: Int32): UInt64; static;

    property Names: THashLibStringArray read GetNames write SetNames;
    property Width: Int32 read GetWidth write SetWidth;
    property Polynomial: UInt64 read GetPolynomial write SetPolynomial;
    property InitialValue: UInt64 read GetInitialValue write SetInitialValue;
    property IsInputReflected: Boolean read GetIsInputReflected
      write SetIsInputReflected;
    property IsOutputReflected: Boolean read GetIsOutputReflected
      write SetIsOutputReflected;
    property OutputXor: UInt64 read GetOutputXor write SetOutputXor;
    property CheckValue: UInt64 read GetCheckValue write SetCheckValue;

  strict protected
    function GetName: String; override;

  public

    constructor Create(AWidth: Int32; APolynomial, AInitial: UInt64;
      AIsInputReflected, AIsOutputReflected: Boolean;
      AOutputXor, ACheckValue: UInt64; const ANames: THashLibStringArray);

    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;

    function Clone(): IHash; override;

    class function CreateCRCObject(AValue: TCRCStandard): ICRC; static;

  end;

implementation

{ TCRC }

function TCRC.GetCheckValue: UInt64;
begin
  result := FCheckValue;
end;

function TCRC.GetInitialValue: UInt64;
begin
  result := FInitialValue;
end;

function TCRC.GetNames: THashLibStringArray;
begin
  result := FNames;
end;

function TCRC.GetPolynomial: UInt64;
begin
  result := FPolynomial;
end;

function TCRC.GetIsInputReflected: Boolean;
begin
  result := FIsInputReflected;
end;

function TCRC.GetIsOutputReflected: Boolean;
begin
  result := FIsOutputReflected;
end;

function TCRC.GetWidth: Int32;
begin
  result := FWidth;
end;

function TCRC.GetOutputXor: UInt64;
begin
  result := FOutputXor;
end;

procedure TCRC.SetCheckValue(AValue: UInt64);
begin
  FCheckValue := AValue;
end;

procedure TCRC.SetInitialValue(AValue: UInt64);
begin
  FInitialValue := AValue;
end;

procedure TCRC.SetNames(const AValue: THashLibStringArray);
begin
  FNames := AValue;
end;

procedure TCRC.SetPolynomial(AValue: UInt64);
begin
  FPolynomial := AValue;
end;

procedure TCRC.SetIsInputReflected(AValue: Boolean);
begin
  FIsInputReflected := AValue;
end;

procedure TCRC.SetIsOutputReflected(AValue: Boolean);
begin
  FIsOutputReflected := AValue;
end;

procedure TCRC.SetWidth(AValue: Int32);
begin
  FWidth := AValue;
end;

procedure TCRC.SetOutputXor(AValue: UInt64);
begin
  FOutputXor := AValue;
end;

function TCRC.GetName: String;
begin
  result := Format('T%s', [(Self as ICRC).Names[0]]);
end;

procedure TCRC.CalculateCRCbyTable(AData: PByte; ADataLength, AIndex: Int32);
var
  LLength, LIndex: Int32;
  LTemp: UInt64;
  LCRCTable: THashLibUInt64Array;
begin
  LLength := ADataLength;
  LIndex := AIndex;
  LTemp := FHash;
  LCRCTable := FCRCTable;

  if (IsInputReflected) then
  begin
    while LLength > 0 do
    begin
      LTemp := (LTemp shr 8) xor LCRCTable[Byte(LTemp xor AData[LIndex])];
      System.Inc(LIndex);
      System.Dec(LLength);
    end;
  end
  else
  begin
    while LLength > 0 do
    begin
      LTemp := (LTemp shl 8) xor LCRCTable
        [Byte((LTemp shr (Width - 8)) xor AData[LIndex])];
      System.Inc(LIndex);
      System.Dec(LLength);
    end;
  end;

  FHash := LTemp;
end;

procedure TCRC.CalculateCRCdirect(AData: PByte; ADataLength, AIndex: Int32);
var
  LLength, LIdx: Int32;
  LTemp, LBit, LJdx, LHash: UInt64;
begin

  LLength := ADataLength;
  LIdx := AIndex;
  while LLength > 0 do
  begin
    LTemp := UInt64(AData[LIdx]);
    if (IsInputReflected) then
    begin
      LTemp := Reflect(LTemp, 8);
    end;

    LJdx := $80;
    LHash := FHash;
    while LJdx > 0 do
    begin
      LBit := LHash and FCRCHighBitMask;
      LHash := LHash shl 1;
      if ((LTemp and LJdx) > 0) then
        LBit := LBit xor FCRCHighBitMask;
      if (LBit > 0) then
        LHash := LHash xor Polynomial;
      LJdx := LJdx shr 1;
    end;
    FHash := LHash;
    System.Inc(LIdx);
    System.Dec(LLength);
  end;

end;

function TCRC.Clone(): IHash;
var
  LHashInstance: TCRC;
begin
  LHashInstance := TCRC.Create(Width, Polynomial, InitialValue,
    IsInputReflected, IsOutputReflected, OutputXor, CheckValue,
    System.Copy(Names));
  LHashInstance.FCRCMask := FCRCMask;
  LHashInstance.FCRCHighBitMask := FCRCHighBitMask;
  LHashInstance.FHash := FHash;
  LHashInstance.FIsTableGenerated := FIsTableGenerated;
  LHashInstance.FCRCTable := System.Copy(FCRCTable);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TCRC.Create(AWidth: Int32; APolynomial, AInitial: UInt64;
  AIsInputReflected, AIsOutputReflected: Boolean;
  AOutputXor, ACheckValue: UInt64; const ANames: THashLibStringArray);
begin

  if not(AWidth in [3 .. 64]) then
  begin
    raise EArgumentOutOfRangeHashLibException.CreateResFmt(@SWidthOutOfRange,
      [AWidth]);
  end;

  FIsTableGenerated := False;

  Inherited Create(-1, -1); // Dummy State

  case AWidth of
    0 .. 7:
      begin
        Self.HashSize := 1;
        Self.BlockSize := 1;
      end;

    8 .. 16:
      begin
        Self.HashSize := 2;
        Self.BlockSize := 1;
      end;

    17 .. 39:
      begin
        Self.HashSize := 4;
        Self.BlockSize := 1;
      end;

  else
    begin
      Self.HashSize := 8;
      Self.BlockSize := 1;
    end;

  end;

  Names := ANames;
  Width := AWidth;
  Polynomial := APolynomial;
  InitialValue := AInitial;
  IsInputReflected := AIsInputReflected;
  IsOutputReflected := AIsOutputReflected;
  OutputXor := AOutputXor;
  CheckValue := ACheckValue;

end;

{$REGION 'CRC Standards Implementation'}

class function TCRC.CreateCRCObject(AValue: TCRCStandard): ICRC;
begin
  case AValue of

    TCRCStandard.CRC3_GSM:
      result := TCRC.Create(3, $3, $0, False, False, $7, $4,
        THashLibStringArray.Create('CRC-3/GSM'));

    TCRCStandard.CRC3_ROHC:
      result := TCRC.Create(3, $3, $7, True, True, $0, $6,
        THashLibStringArray.Create('CRC-3/ROHC'));

    TCRCStandard.CRC4_INTERLAKEN:
      result := TCRC.Create(4, $3, $F, False, False, $F, $B,
        THashLibStringArray.Create('CRC-4/INTERLAKEN'));

    TCRCStandard.CRC4_ITU:
      result := TCRC.Create(4, $3, $0, True, True, $0, $7,
        THashLibStringArray.Create('CRC-4/ITU', 'CRC-4/G-704'));

    TCRCStandard.CRC5_EPC:
      result := TCRC.Create(5, $9, $9, False, False, $00, $00,
        THashLibStringArray.Create('CRC-5/EPC', 'CRC-5/EPC-C1G2'));

    TCRCStandard.CRC5_ITU:
      result := TCRC.Create(5, $15, $00, True, True, $00, $07,
        THashLibStringArray.Create('CRC-5/ITU', 'CRC-5/G-704'));

    TCRCStandard.CRC5_USB:
      result := TCRC.Create(5, $05, $1F, True, True, $1F, $19,
        THashLibStringArray.Create('CRC-5/USB'));

    TCRCStandard.CRC6_CDMA2000A:
      result := TCRC.Create(6, $27, $3F, False, False, $00, $0D,
        THashLibStringArray.Create('CRC-6/CDMA2000-A'));

    TCRCStandard.CRC6_CDMA2000B:
      result := TCRC.Create(6, $07, $3F, False, False, $00, $3B,
        THashLibStringArray.Create('CRC-6/CDMA2000-B'));

    TCRCStandard.CRC6_DARC:
      result := TCRC.Create(6, $19, $00, True, True, $00, $26,
        THashLibStringArray.Create('CRC-6/DARC'));

    TCRCStandard.CRC6_GSM:
      result := TCRC.Create(6, $2F, $00, False, False, $3F, $13,
        THashLibStringArray.Create('CRC-6/GSM'));

    TCRCStandard.CRC6_ITU:
      result := TCRC.Create(6, $03, $00, True, True, $00, $06,
        THashLibStringArray.Create('CRC-6/ITU', 'CRC-6/G-704'));

    TCRCStandard.CRC7:
      result := TCRC.Create(7, $09, $00, False, False, $00, $75,
        THashLibStringArray.Create('CRC-7', 'CRC-7/MMC'));

    TCRCStandard.CRC7_ROHC:
      result := TCRC.Create(7, $4F, $7F, True, True, $00, $53,
        THashLibStringArray.Create('CRC-7/ROHC'));

    TCRCStandard.CRC7_UMTS:
      result := TCRC.Create(7, $45, $00, False, False, $00, $61,
        THashLibStringArray.Create('CRC-7/UMTS'));

    TCRCStandard.CRC8:
      result := TCRC.Create(8, $07, $00, False, False, $00, $F4,
        THashLibStringArray.Create('CRC-8', 'CRC-8/SMBUS'));

    TCRCStandard.CRC8_AUTOSAR:
      result := TCRC.Create(8, $2F, $FF, False, False, $FF, $DF,
        THashLibStringArray.Create('CRC-8/AUTOSAR'));

    TCRCStandard.CRC8_BLUETOOTH:
      result := TCRC.Create(8, $A7, $00, True, True, $00, $26,
        THashLibStringArray.Create('CRC-8/BLUETOOTH'));

    TCRCStandard.CRC8_CDMA2000:
      result := TCRC.Create(8, $9B, $FF, False, False, $00, $DA,
        THashLibStringArray.Create('CRC-8/CDMA2000'));

    TCRCStandard.CRC8_DARC:
      result := TCRC.Create(8, $39, $00, True, True, $00, $15,
        THashLibStringArray.Create('CRC-8/DARC'));

    TCRCStandard.CRC8_DVBS2:
      result := TCRC.Create(8, $D5, $00, False, False, $00, $BC,
        THashLibStringArray.Create('CRC-8/DVB-S2'));

    TCRCStandard.CRC8_EBU:
      result := TCRC.Create(8, $1D, $FF, True, True, $00, $97,
        THashLibStringArray.Create('CRC-8/EBU', 'CRC-8/AES',
        'CRC-8/TECH-3250'));

    TCRCStandard.CRC8_GSMA:
      result := TCRC.Create(8, $1D, $00, False, False, $00, $37,
        THashLibStringArray.Create('CRC-8/GSM-A'));

    TCRCStandard.CRC8_GSMB:
      result := TCRC.Create(8, $49, $00, False, False, $FF, $94,
        THashLibStringArray.Create('CRC-8/GSM-B'));

    TCRCStandard.CRC8_ICODE:
      result := TCRC.Create(8, $1D, $FD, False, False, $00, $7E,
        THashLibStringArray.Create('CRC-8/I-CODE'));

    TCRCStandard.CRC8_ITU:
      result := TCRC.Create(8, $07, $00, False, False, $55, $A1,
        THashLibStringArray.Create('CRC-8/ITU', 'CRC-8/I-432-1'));

    TCRCStandard.CRC8_LTE:
      result := TCRC.Create(8, $9B, $00, False, False, $00, $EA,
        THashLibStringArray.Create('CRC-8/LTE'));

    TCRCStandard.CRC8_MAXIM:
      result := TCRC.Create(8, $31, $00, True, True, $00, $A1,
        THashLibStringArray.Create('CRC-8/MAXIM', 'DOW-CRC',
        'CRC-8/MAXIM-DOW'));

    TCRCStandard.CRC8_OPENSAFETY:
      result := TCRC.Create(8, $2F, $00, False, False, $00, $3E,
        THashLibStringArray.Create('CRC-8/OPENSAFETY'));

    TCRCStandard.CRC8_ROHC:
      result := TCRC.Create(8, $07, $FF, True, True, $00, $D0,
        THashLibStringArray.Create('CRC-8/ROHC'));

    TCRCStandard.CRC8_SAEJ1850:
      result := TCRC.Create(8, $1D, $FF, False, False, $FF, $4B,
        THashLibStringArray.Create('CRC-8/SAE-J1850'));

    TCRCStandard.CRC8_WCDMA:
      result := TCRC.Create(8, $9B, $00, True, True, $00, $25,
        THashLibStringArray.Create('CRC-8/WCDMA'));

    TCRCStandard.CRC8_MIFAREMAD:
      result := TCRC.Create(8, $1D, $C7, False, False, $00, $99,
        THashLibStringArray.Create('CRC-8/MIFARE-MAD'));

    TCRCStandard.CRC8_NRSC5:
      result := TCRC.Create(8, $31, $FF, False, False, $00, $F7,
        THashLibStringArray.Create('CRC-8/NRSC-5'));

    TCRCStandard.CRC10:
      result := TCRC.Create(10, $233, $000, False, False, $000, $199,
        THashLibStringArray.Create('CRC-10', 'CRC-10/ATM', 'CRC-10/I-610'));

    TCRCStandard.CRC10_CDMA2000:
      result := TCRC.Create(10, $3D9, $3FF, False, False, $000, $233,
        THashLibStringArray.Create('CRC-10/CDMA2000'));

    TCRCStandard.CRC10_GSM:
      result := TCRC.Create(10, $175, $000, False, False, $3FF, $12A,
        THashLibStringArray.Create('CRC-10/GSM'));

    TCRCStandard.CRC11:
      result := TCRC.Create(11, $385, $01A, False, False, $000, $5A3,
        THashLibStringArray.Create('CRC-11', 'CRC-11/FLEXRAY'));

    TCRCStandard.CRC11_UMTS:
      result := TCRC.Create(11, $307, $000, False, False, $000, $061,
        THashLibStringArray.Create('CRC-11/UMTS'));

    TCRCStandard.CRC12_CDMA2000:
      result := TCRC.Create(12, $F13, $FFF, False, False, $000, $D4D,
        THashLibStringArray.Create('CRC-12/CDMA2000'));

    TCRCStandard.CRC12_DECT:
      result := TCRC.Create(12, $80F, $000, False, False, $000, $F5B,
        THashLibStringArray.Create('CRC-12/DECT', 'X-CRC-12'));

    TCRCStandard.CRC12_GSM:
      result := TCRC.Create(12, $D31, $000, False, False, $FFF, $B34,
        THashLibStringArray.Create('CRC-12/GSM'));

    TCRCStandard.CRC12_UMTS:
      result := TCRC.Create(12, $80F, $000, False, True, $000, $DAF,
        THashLibStringArray.Create('CRC-12/UMTS', 'CRC-12/3GPP'));

    TCRCStandard.CRC13_BBC:
      result := TCRC.Create(13, $1CF5, $0000, False, False, $0000, $04FA,
        THashLibStringArray.Create('CRC-13/BBC'));

    TCRCStandard.CRC14_DARC:
      result := TCRC.Create(14, $0805, $0000, True, True, $0000, $082D,
        THashLibStringArray.Create('CRC-14/DARC'));

    TCRCStandard.CRC14_GSM:
      result := TCRC.Create(14, $202D, $0000, False, False, $3FFF, $30AE,
        THashLibStringArray.Create('CRC-14/GSM'));

    TCRCStandard.CRC15:
      result := TCRC.Create(15, $4599, $0000, False, False, $0000, $059E,
        THashLibStringArray.Create('CRC-15', 'CRC-15/CAN'));

    TCRCStandard.CRC15_MPT1327:
      result := TCRC.Create(15, $6815, $0000, False, False, $0001, $2566,
        THashLibStringArray.Create('CRC-15/MPT1327'));

    TCRCStandard.ARC:
      result := TCRC.Create(16, $8005, $0000, True, True, $0000, $BB3D,
        THashLibStringArray.Create('CRC-16', 'ARC', 'CRC-IBM', 'CRC-16/ARC',
        'CRC-16/LHA'));

    TCRCStandard.CRC16_AUGCCITT:
      result := TCRC.Create(16, $1021, $1D0F, False, False, $0000, $E5CC,
        THashLibStringArray.Create('CRC-16/AUG-CCITT', 'CRC-16/SPI-FUJITSU'));

    TCRCStandard.CRC16_BUYPASS:
      result := TCRC.Create(16, $8005, $0000, False, False, $0000, $FEE8,
        THashLibStringArray.Create('CRC-16/BUYPASS', 'CRC-16/VERIFONE',
        'CRC-16/UMTS'));

    TCRCStandard.CRC16_CCITTFALSE:
      result := TCRC.Create(16, $1021, $FFFF, False, False, $0000, $29B1,
        THashLibStringArray.Create('CRC-16/CCITT-FALSE', 'CRC-16/AUTOSAR',
        'CRC-16/IBM-3740'));

    TCRCStandard.CRC16_CDMA2000:
      result := TCRC.Create(16, $C867, $FFFF, False, False, $0000, $4C06,
        THashLibStringArray.Create('CRC-16/CDMA2000'));

    TCRCStandard.CRC16_CMS:
      result := TCRC.Create(16, $8005, $FFFF, False, False, $0000, $AEE7,
        THashLibStringArray.Create('CRC-16/CMS'));

    TCRCStandard.CRC16_DDS110:
      result := TCRC.Create(16, $8005, $800D, False, False, $0000, $9ECF,
        THashLibStringArray.Create('CRC-16/DDS-110'));

    TCRCStandard.CRC16_DECTR:
      result := TCRC.Create(16, $0589, $0000, False, False, $0001, $007E,
        THashLibStringArray.Create('CRC-16/DECT-R', 'R-CRC-16'));

    TCRCStandard.CRC16_DECTX:
      result := TCRC.Create(16, $0589, $0000, False, False, $0000, $007F,
        THashLibStringArray.Create('CRC-16/DECT-X', 'X-CRC-16'));

    TCRCStandard.CRC16_DNP:
      result := TCRC.Create(16, $3D65, $0000, True, True, $FFFF, $EA82,
        THashLibStringArray.Create('CRC-16/DNP'));

    TCRCStandard.CRC16_EN13757:
      result := TCRC.Create(16, $3D65, $0000, False, False, $FFFF, $C2B7,
        THashLibStringArray.Create('CRC-16/EN13757'));

    TCRCStandard.CRC16_GENIBUS:
      result := TCRC.Create(16, $1021, $FFFF, False, False, $FFFF, $D64E,
        THashLibStringArray.Create('CRC-16/GENIBUS', 'CRC-16/EPC',
        'CRC-16/I-CODE', 'CRC-16/DARC', 'CRC-16/EPC-C1G2'));

    TCRCStandard.CRC16_GSM:
      result := TCRC.Create(16, $1021, $0000, False, False, $FFFF, $CE3C,
        THashLibStringArray.Create('CRC-16/GSM'));

    TCRCStandard.CRC16_LJ1200:
      result := TCRC.Create(16, $6F63, $0000, False, False, $0000, $BDF4,
        THashLibStringArray.Create('CRC-16/LJ1200'));

    TCRCStandard.CRC16_MAXIM:
      result := TCRC.Create(16, $8005, $0000, True, True, $FFFF, $44C2,
        THashLibStringArray.Create('CRC-16/MAXIM', 'CRC-16/MAXIM-DOW'));

    TCRCStandard.CRC16_MCRF4XX:
      result := TCRC.Create(16, $1021, $FFFF, True, True, $0000, $6F91,
        THashLibStringArray.Create('CRC-16/MCRF4XX'));

    TCRCStandard.CRC16_OPENSAFETYA:
      result := TCRC.Create(16, $5935, $0000, False, False, $0000, $5D38,
        THashLibStringArray.Create('CRC-16/OPENSAFETY-A'));

    TCRCStandard.CRC16_OPENSAFETYB:
      result := TCRC.Create(16, $755B, $0000, False, False, $0000, $20FE,
        THashLibStringArray.Create('CRC-16/OPENSAFETY-B'));

    TCRCStandard.CRC16_PROFIBUS:
      result := TCRC.Create(16, $1DCF, $FFFF, False, False, $FFFF, $A819,
        THashLibStringArray.Create('CRC-16/PROFIBUS', 'CRC-16/IEC-61158-2'));

    TCRCStandard.CRC16_RIELLO:
      result := TCRC.Create(16, $1021, $B2AA, True, True, $0000, $63D0,
        THashLibStringArray.Create('CRC-16/RIELLO'));

    TCRCStandard.CRC16_T10DIF:
      result := TCRC.Create(16, $8BB7, $0000, False, False, $0000, $D0DB,
        THashLibStringArray.Create('CRC-16/T10-DIF'));

    TCRCStandard.CRC16_TELEDISK:
      result := TCRC.Create(16, $A097, $0000, False, False, $0000, $0FB3,
        THashLibStringArray.Create('CRC-16/TELEDISK'));

    TCRCStandard.CRC16_TMS37157:
      result := TCRC.Create(16, $1021, $89EC, True, True, $0000, $26B1,
        THashLibStringArray.Create('CRC-16/TMS37157'));

    TCRCStandard.CRC16_USB:
      result := TCRC.Create(16, $8005, $FFFF, True, True, $FFFF, $B4C8,
        THashLibStringArray.Create('CRC-16/USB'));

    TCRCStandard.CRCA:
      result := TCRC.Create(16, $1021, $C6C6, True, True, $0000, $BF05,
        THashLibStringArray.Create('CRC-A', 'CRC-16/ISO-IEC-14443-3-A'));

    TCRCStandard.KERMIT:
      result := TCRC.Create(16, $1021, $0000, True, True, $0000, $2189,
        THashLibStringArray.Create('KERMIT', 'CRC-16/CCITT',
        'CRC-16/CCITT-TRUE', 'CRC-CCITT', 'CRC-16/KERMIT', 'CRC-16/V-41-LSB'));

    TCRCStandard.MODBUS:
      result := TCRC.Create(16, $8005, $FFFF, True, True, $0000, $4B37,
        THashLibStringArray.Create('MODBUS', 'CRC-16/MODBUS'));

    TCRCStandard.X25:
      result := TCRC.Create(16, $1021, $FFFF, True, True, $FFFF, $906E,
        THashLibStringArray.Create('X-25', 'CRC-16/IBM-SDLC', 'CRC-16/ISO-HDLC',
        'CRC-16/ISO-IEC-14443-3-B', 'CRC-B', 'CRC-16/X-25'));

    TCRCStandard.XMODEM:
      result := TCRC.Create(16, $1021, $0000, False, False, $0000, $31C3,
        THashLibStringArray.Create('XMODEM', 'ZMODEM', 'CRC-16/ACORN',
        'CRC-16/XMODEM', 'CRC-16/V-41-MSB'));

    TCRCStandard.CRC16_NRSC5:
      result := TCRC.Create(16, $080B, $FFFF, True, True, $0000, $A066,
        THashLibStringArray.Create('CRC-16/NRSC-5'));

    TCRCStandard.CRC17_CANFD:
      result := TCRC.Create(17, $1685B, $00000, False, False, $00000, $04F03,
        THashLibStringArray.Create('CRC-17/CAN-FD'));

    TCRCStandard.CRC21_CANFD:
      result := TCRC.Create(21, $102899, $00000, False, False, $00000, $0ED841,
        THashLibStringArray.Create('CRC-21/CAN-FD'));

    TCRCStandard.CRC24:
      result := TCRC.Create(24, $864CFB, $B704CE, False, False, $000000,
        $21CF02, THashLibStringArray.Create('CRC-24', 'CRC-24/OPENPGP'));

    TCRCStandard.CRC24_BLE:
      result := TCRC.Create(24, $00065B, $555555, True, True, $000000, $C25A56,
        THashLibStringArray.Create('CRC-24/BLE'));

    TCRCStandard.CRC24_FLEXRAYA:
      result := TCRC.Create(24, $5D6DCB, $FEDCBA, False, False, $000000,
        $7979BD, THashLibStringArray.Create('CRC-24/FLEXRAY-A'));

    TCRCStandard.CRC24_FLEXRAYB:
      result := TCRC.Create(24, $5D6DCB, $ABCDEF, False, False, $000000,
        $1F23B8, THashLibStringArray.Create('CRC-24/FLEXRAY-B'));

    TCRCStandard.CRC24_INTERLAKEN:
      result := TCRC.Create(24, $328B63, $FFFFFF, False, False, $FFFFFF,
        $B4F3E6, THashLibStringArray.Create('CRC-24/INTERLAKEN'));

    TCRCStandard.CRC24_LTEA:
      result := TCRC.Create(24, $864CFB, $000000, False, False, $000000,
        $CDE703, THashLibStringArray.Create('CRC-24/LTE-A'));

    TCRCStandard.CRC24_LTEB:
      result := TCRC.Create(24, $800063, $000000, False, False, $000000,
        $23EF52, THashLibStringArray.Create('CRC-24/LTE-B'));

    TCRCStandard.CRC24_OS9:
      result := TCRC.Create(24, $800063, $FFFFFF, False, False, $FFFFFF,
        $200FA5, THashLibStringArray.Create('CRC-24/OS-9'));

    TCRCStandard.CRC30_CDMA:
      result := TCRC.Create(30, $2030B9C7, $3FFFFFFF, False, False, $3FFFFFFF,
        $04C34ABF, THashLibStringArray.Create('CRC-30/CDMA'));

    TCRCStandard.CRC31_PHILIPS:
      result := TCRC.Create(31, $04C11DB7, $7FFFFFFF, False, False, $7FFFFFFF,
        $0CE9E46C, THashLibStringArray.Create('CRC-31/PHILLIPS'));

    TCRCStandard.CRC32:
      result := TCRC.Create(32, $04C11DB7, $FFFFFFFF, True, True, $FFFFFFFF,
        $CBF43926, THashLibStringArray.Create('CRC-32', 'CRC-32/ADCCP',
        'CRC-32/V-42', 'CRC-32/XZ', 'PKZIP', 'CRC-32/ISO-HDLC'));

    TCRCStandard.CRC32_AUTOSAR:
      result := TCRC.Create(32, $F4ACFB13, $FFFFFFFF, True, True, $FFFFFFFF,
        $1697D06A, THashLibStringArray.Create('CRC-32/AUTOSAR'));

    TCRCStandard.CRC32_BZIP2:
      result := TCRC.Create(32, $04C11DB7, $FFFFFFFF, False, False, $FFFFFFFF,
        $FC891918, THashLibStringArray.Create('CRC-32/BZIP2', 'CRC-32/AAL5',
        'CRC-32/DECT-B', 'B-CRC-32'));

    TCRCStandard.CRC32C:
      result := TCRC.Create(32, $1EDC6F41, $FFFFFFFF, True, True, $FFFFFFFF,
        $E3069283, THashLibStringArray.Create('CRC-32C', 'CRC-32/BASE91-C',
        'CRC-32/CASTAGNOLI', 'CRC-32/INTERLAKEN', 'CRC-32/ISCSI'));

    TCRCStandard.CRC32D:
      result := TCRC.Create(32, $A833982B, $FFFFFFFF, True, True, $FFFFFFFF,
        $87315576, THashLibStringArray.Create('CRC-32D', 'CRC-32/BASE91-D'));

    TCRCStandard.CRC32_MPEG2:
      result := TCRC.Create(32, $04C11DB7, $FFFFFFFF, False, False, $00000000,
        $0376E6E7, THashLibStringArray.Create('CRC-32/MPEG-2'));

    TCRCStandard.CRC32_POSIX:
      result := TCRC.Create(32, $04C11DB7, $FFFFFFFF, False, False, $00000000,
        $0376E6E7, THashLibStringArray.Create('CRC-32/POSIX', 'CKSUM'));

    TCRCStandard.CRC32Q:
      result := TCRC.Create(32, $814141AB, $00000000, False, False, $00000000,
        $3010BF7F, THashLibStringArray.Create('CRC-32Q', 'CRC-32/AIXM'));

    TCRCStandard.JAMCRC:
      result := TCRC.Create(32, $04C11DB7, $FFFFFFFF, True, True, $00000000,
        $340BC6D9, THashLibStringArray.Create('JAMCRC', 'CRC-32/JAMCRC'));

    TCRCStandard.XFER:
      result := TCRC.Create(32, $000000AF, $00000000, False, False, $00000000,
        $BD0BE338, THashLibStringArray.Create('XFER', 'CRC-32/XFER'));

    TCRCStandard.CRC32_CDROMEDC:
      result := TCRC.Create(32, $8001801B, $00000000, True, True, $00000000,
        $6EC2EDC4, THashLibStringArray.Create('CRC-32/CD-ROM-EDC'));

    TCRCStandard.CRC40_GSM:
      result := TCRC.Create(40, $0004820009, $0000000000, False, False,
        $FFFFFFFFFF, $D4164FC646, THashLibStringArray.Create('CRC-40/GSM'));

    TCRCStandard.CRC64:
      result := TCRC.Create(64, $42F0E1EBA9EA3693, $0000000000000000, False,
        False, $0000000000000000, $6C40DF5F0B497347,
        THashLibStringArray.Create('CRC-64', 'CRC-64/ECMA-182'));

    TCRCStandard.CRC64_GOISO:
      result := TCRC.Create(64, $000000000000001B, UInt64($FFFFFFFFFFFFFFFF),
        True, True, UInt64($FFFFFFFFFFFFFFFF), UInt64($B90956C775A41001),
        THashLibStringArray.Create('CRC-64/GO-ISO'));

    TCRCStandard.CRC64_WE:
      result := TCRC.Create(64, $42F0E1EBA9EA3693, UInt64($FFFFFFFFFFFFFFFF),
        False, False, UInt64($FFFFFFFFFFFFFFFF), $62EC59E3F1A4F00A,
        THashLibStringArray.Create('CRC-64/WE'));

    TCRCStandard.CRC64_XZ:
      result := TCRC.Create(64, $42F0E1EBA9EA3693, UInt64($FFFFFFFFFFFFFFFF),
        True, True, UInt64($FFFFFFFFFFFFFFFF), UInt64($995DC9BBDF1939FA),
        THashLibStringArray.Create('CRC-64/XZ', 'CRC-64/GO-ECMA'));

    TCRCStandard.CRC64_1B:
      result := TCRC.Create(64, $000000000000001B, UInt64($0000000000000000),
        True, True, UInt64($0000000000000000), $46A5A9388A5BEFFE,
        THashLibStringArray.Create('CRC-64/1B'));

    TCRCStandard.CRC64_Jones:
      result := TCRC.Create(64, UInt64($AD93D23594C935A9),
        UInt64($FFFFFFFFFFFFFFFF), True, True, UInt64($0000000000000000),
        UInt64($CAA717168609F281), THashLibStringArray.Create('CRC-64/Jones'))

  else
    raise EArgumentInvalidHashLibException.CreateResFmt(@SUnSupportedCRCType,
      [GetEnumName(TypeInfo(TCRCStandard), Ord(AValue))]);

  end;
end;

{$ENDREGION}

procedure TCRC.GenerateTable;
var
  LBit, LCRC: UInt64;
  LIdx, LJdx: Int32;
begin
  System.SetLength(FCRCTable, 256);
  LIdx := 0;
  while LIdx < 256 do
  begin
    LCRC := UInt64(LIdx);
    if (IsInputReflected) then
    begin
      LCRC := Reflect(LCRC, 8);
    end;
    LCRC := LCRC shl (Width - 8);
    LJdx := 0;
    while LJdx < 8 do
    begin

      LBit := LCRC and FCRCHighBitMask;
      LCRC := LCRC shl 1;
      if (LBit <> 0) then
        LCRC := (LCRC xor Polynomial);
      System.Inc(LJdx);
    end;

    if (IsInputReflected) then
    begin
      LCRC := Reflect(LCRC, Width);
    end;
    LCRC := LCRC and FCRCMask;
    FCRCTable[LIdx] := LCRC;
    System.Inc(LIdx);
  end;

  FIsTableGenerated := True;
end;

procedure TCRC.Initialize;
begin
  // initialize some bitmasks
  FCRCHighBitMask := UInt64(1) shl (Width - 1);
  FCRCMask := ((FCRCHighBitMask - 1) shl 1) or 1;
  FHash := InitialValue;

  if (Width > Delta) then // then use table
  begin

    if not FIsTableGenerated then
    begin
      GenerateTable();
    end;

    if (IsInputReflected) then
      FHash := Reflect(FHash, Width);

  end;

end;

class function TCRC.Reflect(AValue: UInt64; AWidth: Int32): UInt64;
var
  LIdx, LJdx: UInt64;
begin
  LJdx := 1;
  result := 0;
  LIdx := UInt64(1) shl (AWidth - 1);
  while LIdx <> 0 do
  begin
    if ((AValue and LIdx) <> 0) then
    begin
      result := result or LJdx;
    end;
    LJdx := LJdx shl 1;
    LIdx := LIdx shr 1;
  end;
end;

procedure TCRC.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LIdx: Int32;
  PtrAData: PByte;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}

  // table driven CRC reportedly only works for 8, 16, 24, 32 LBits
  // HOWEVER, it seems to work for everything > 7 LBits, so use it
  // accordingly

  LIdx := AIndex;

  PtrAData := PByte(AData);

  if (Width > Delta) then
  begin
    CalculateCRCbyTable(PtrAData, ALength, LIdx);
  end
  else
  begin
    CalculateCRCdirect(PtrAData, ALength, LIdx);
  end;

end;

function TCRC.TransformFinal: IHashResult;
var
  LUInt64: UInt64;
  LUInt32: UInt32;
  LUInt16: UInt16;
  LUInt8: UInt8;

begin

  if Width > Delta then
  begin
    if (IsInputReflected xor IsOutputReflected) then
    begin
      FHash := Reflect(FHash, Width);
    end;
  end
  else
  begin
    if (IsOutputReflected) then
    begin
      FHash := Reflect(FHash, Width);
    end;
  end;

  FHash := FHash xor OutputXor;
  FHash := FHash and FCRCMask;

  if Width = 21 then // special case
  begin
    LUInt32 := UInt32(FHash);

    result := THashResult.Create(LUInt32);

    Initialize();

    Exit;
  end;

  case Width shr 3 of
    0:
      begin
        LUInt8 := UInt8(FHash);
        result := THashResult.Create(LUInt8);

      end;

    1 .. 2:
      begin
        LUInt16 := UInt16(FHash);

        result := THashResult.Create(LUInt16);
      end;

    3 .. 4:
      begin
        LUInt32 := UInt32(FHash);

        result := THashResult.Create(LUInt32);
      end
  else
    begin
      LUInt64 := (FHash);

      result := THashResult.Create(LUInt64);
    end;
  end;

  Initialize();

end;

end.
