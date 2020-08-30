unit HlpHMACNotBuildInAdapter;

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpIHashResult,
  HlpArrayUtils;

type

  THMACNotBuildInAdapter = class sealed(THash, IHMAC, IHMACNotBuildIn, ICrypto,
    ICryptoNotBuildIn)

  strict private
  var
    FHash: IHash;
    FOpad, FIpad, FKey: THashLibByteArray;

    constructor Create(const AUnderlyingHash: IHash;
      const AHMACKey: THashLibByteArray);

  strict protected

    function GetName: String; override;

    function GetKey(): THashLibByteArray;
    procedure SetKey(const AValue: THashLibByteArray);
    procedure UpdatePads();

  public

    destructor Destroy; override;

    procedure Clear();

    procedure Initialize(); override;
    function TransformFinal(): IHashResult; override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function Clone(): IHash; override;
    property Key: THashLibByteArray read GetKey write SetKey;
    property Name: String read GetName;

    class function CreateHMAC(const AHash: IHash;
      const AHMACKey: THashLibByteArray): IHMAC; static;

  end;

implementation

{ THMACNotBuildInAdapter }

procedure THMACNotBuildInAdapter.Clear();
begin
  TArrayUtils.ZeroFill(FKey);
end;

function THMACNotBuildInAdapter.Clone(): IHash;
var
  HmacInstance: THMACNotBuildInAdapter;
begin
  HmacInstance := THMACNotBuildInAdapter.Create(FHash.Clone(), FKey);
  HmacInstance.FOpad := System.Copy(FOpad);
  HmacInstance.FIpad := System.Copy(FIpad);
  result := HmacInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor THMACNotBuildInAdapter.Create(const AUnderlyingHash: IHash;
  const AHMACKey: THashLibByteArray);
begin
  Inherited Create(AUnderlyingHash.HashSize, AUnderlyingHash.BlockSize);
  FHash := AUnderlyingHash;
  SetKey(AHMACKey);
  System.SetLength(FIpad, FHash.BlockSize);
  System.SetLength(FOpad, FHash.BlockSize);
end;

destructor THMACNotBuildInAdapter.Destroy;
begin
  Clear();
  inherited Destroy;
end;

function THMACNotBuildInAdapter.GetKey: THashLibByteArray;
begin
  result := System.Copy(FKey);
end;

procedure THMACNotBuildInAdapter.SetKey(const AValue: THashLibByteArray);
begin
  if (AValue = Nil) then
  begin
    FKey := Nil;
  end
  else
  begin
    FKey := System.Copy(AValue);
  end;
end;

procedure THMACNotBuildInAdapter.UpdatePads;
var
  LKey: THashLibByteArray;
  LIdx, LBlockSize: Int32;
begin
  LBlockSize := FHash.BlockSize;
  if (System.Length(Key) > LBlockSize) then
  begin
    LKey := FHash.ComputeBytes(Key).GetBytes();
  end
  else
  begin
    LKey := Key;
  end;

  TArrayUtils.Fill(FIpad, 0, LBlockSize, Byte($36));
  TArrayUtils.Fill(FOpad, 0, LBlockSize, Byte($5C));

  LIdx := 0;
  while (LIdx < System.Length(LKey)) and (LIdx < LBlockSize) do
  begin
    FIpad[LIdx] := FIpad[LIdx] xor LKey[LIdx];
    FOpad[LIdx] := FOpad[LIdx] xor LKey[LIdx];
    System.Inc(LIdx);
  end;

end;

procedure THMACNotBuildInAdapter.Initialize;
begin
  FHash.Initialize();
  UpdatePads();
  FHash.TransformBytes(FIpad);
end;

function THMACNotBuildInAdapter.TransformFinal: IHashResult;
begin
  result := FHash.TransformFinal();
  FHash.TransformBytes(FOpad);
  FHash.TransformBytes(result.GetBytes());
  result := FHash.TransformFinal();
  Initialize();
end;

procedure THMACNotBuildInAdapter.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
{$IFDEF DEBUG}
  System.Assert(AIndex >= 0);
  System.Assert(ALength >= 0);
  System.Assert(AIndex + ALength <= System.Length(AData));
{$ENDIF}
  FHash.TransformBytes(AData, AIndex, ALength);
end;

function THMACNotBuildInAdapter.GetName: String;
begin
  result := Format('%s(%s)', ['THMAC', FHash.Name]);
end;

class function THMACNotBuildInAdapter.CreateHMAC(const AHash: IHash;
  const AHMACKey: THashLibByteArray): IHMAC;
begin
  if Supports(AHash, IHMAC) then
  begin
    result := AHash as IHMAC;
  end
  else
  begin
    result := THMACNotBuildInAdapter.Create(AHash, AHMACKey);
  end;
end;

end.
