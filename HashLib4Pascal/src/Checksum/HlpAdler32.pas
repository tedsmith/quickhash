unit HlpAdler32;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpIHashInfo,
  HlpHash,
  HlpIHash,
  HlpHashResult,
  HlpIHashResult,
  HlpConverters;

type
  TAdler32 = class sealed(THash, IChecksum, IHash32, ITransformBlock)

  strict private
  var
    FA, FB: UInt32;

  const
    MOD_ADLER = UInt32(65521);

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TAdler32 }

function TAdler32.Clone(): IHash;
var
  LHashInstance: TAdler32;
begin
  LHashInstance := TAdler32.Create();
  LHashInstance.FA := FA;
  LHashInstance.FB := FB;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TAdler32.Create;
begin
  Inherited Create(4, 1);
end;

procedure TAdler32.Initialize;
begin
  FA := 1;
  FB := 0;
end;

procedure TAdler32.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
var
  LN: Int32;
  LPtrData: PByte;
  LA, LB: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF DEBUG}
  LPtrData := PByte(AData) + AIndex;

  {
    LA := FA;
    LB := FB;
    while ALength > 0 do
    begin
    LA := (LA + LPtrData^) mod MOD_ADLER;
    LB := (LB + LA) mod MOD_ADLER;
    System.Inc(LPtrData);
    System.Dec(ALength);
    end;
    FA := LA;
    FB := LB;
  }

  // lifted from PngEncoder Adler32.cs

  while ALength > 0 do
  begin
    // We can defer the modulo operation:
    // FA maximally grows from 65521 to 65521 + 255 * 3800
    // FB maximally grows by 3800 * median(FA) = 2090079800 < 2^31
    LN := 3800;
    if (LN > ALength) then
    begin
      LN := ALength;
    end;
    ALength := ALength - LN;

    LA := FA;
    LB := FB;
    while (LN - 1) >= 0 do
    begin
      LA := (LA + LPtrData^);
      LB := (LB + LA);
      System.Inc(LPtrData);
      System.Dec(LN);
    end;
    LA := LA mod MOD_ADLER;
    LB := LB mod MOD_ADLER;

    FA := LA;
    FB := LB;
  end;
end;

function TAdler32.TransformFinal: IHashResult;
var
  LBufferBytes: THashLibByteArray;
begin
  System.SetLength(LBufferBytes, HashSize);
  TConverters.ReadUInt32AsBytesBE(UInt32((FB shl 16) or FA), LBufferBytes, 0);

  result := THashResult.Create(LBufferBytes);
  Initialize();
end;

end.
