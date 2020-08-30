unit HlpNullDigest;

{$I ..\Include\HashLib.inc}

interface

uses
  Classes,
  SysUtils,
  HlpHashLibTypes,
  HlpHash,
  HlpIHash,
  HlpIHashInfo,
  HlpHashResult,
  HlpIHashResult;

resourcestring
  SHashSizeNotImplemented = 'HashSize Not Implemented For "%s"';
  SBlockSizeNotImplemented = 'BlockSize Not Implemented For "%s"';

type
  TNullDigest = class sealed(THash, ITransformBlock)
  strict private
  var
    FOut: TMemoryStream;

  strict protected
    function GetBlockSize: Int32; override;
    function GetHashSize: Int32; override;

  public
    constructor Create();
    destructor Destroy(); override;
    procedure Initialize(); override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ALength: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TNullDigest }

function TNullDigest.GetBlockSize: Int32;
begin
  raise ENotImplementedHashLibException.CreateResFmt
    (@SBlockSizeNotImplemented, [Name]);
end;

function TNullDigest.GetHashSize: Int32;
begin
  raise ENotImplementedHashLibException.CreateResFmt
    (@SHashSizeNotImplemented, [Name]);
end;

function TNullDigest.Clone(): IHash;
var
  LHashInstance: TNullDigest;
begin
  LHashInstance := TNullDigest.Create();
  FOut.Position := 0;
  LHashInstance.FOut.CopyFrom(FOut, FOut.Size);
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TNullDigest.Create;
begin
  Inherited Create(-1, -1); // Dummy State
  FOut := TMemoryStream.Create();
end;

destructor TNullDigest.Destroy;
begin
  FOut.Free;
  inherited Destroy;
end;

procedure TNullDigest.Initialize;
begin
  FOut.Clear;
end;

procedure TNullDigest.TransformBytes(const AData: THashLibByteArray;
  AIndex, ALength: Int32);
begin
  if AData <> Nil then
  begin
    FOut.Write(AData[AIndex], ALength);
  end;
end;

function TNullDigest.TransformFinal: IHashResult;
var
  LResult: THashLibByteArray;
begin
  try
    if FOut.Size > 0 then
    begin
      FOut.Position := 0;
      System.SetLength(LResult, FOut.Size);
      FOut.Read(LResult[0], FOut.Size);
    end;
    result := THashResult.Create(LResult);
  finally
    Initialize();
  end;
end;

end.
