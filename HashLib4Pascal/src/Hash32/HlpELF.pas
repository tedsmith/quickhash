unit HlpELF;

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
  TELF = class sealed(THash, IHash32, ITransformBlock)
  strict private

    Fm_hash: UInt32;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TELF }

function TELF.Clone(): IHash;
var
  HashInstance: TELF;
begin
  HashInstance := TELF.Create();
  HashInstance.Fm_hash := Fm_hash;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TELF.Create;
begin
  Inherited Create(4, 1);
end;

procedure TELF.Initialize;
begin
  Fm_hash := 0;
end;

procedure TELF.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  i: Int32;
  g: UInt32;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  i := a_index;
  while a_length > 0 do
  begin
    Fm_hash := (Fm_hash shl 4) + a_data[i];
    g := Fm_hash and $F0000000;

    if (g <> 0) then
      Fm_hash := Fm_hash xor (g shr 24);

    Fm_hash := Fm_hash and (not g);
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TELF.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
