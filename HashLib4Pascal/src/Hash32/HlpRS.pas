unit HlpRS;

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
  TRS = class sealed(THash, IHash32, ITransformBlock)
  strict private

    Fm_hash, Fm_a: UInt32;

  const
    B = UInt32(378551);

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TRS }

function TRS.Clone(): IHash;
var
  HashInstance: TRS;
begin
  HashInstance := TRS.Create();
  HashInstance.Fm_hash := Fm_hash;
  HashInstance.Fm_a := Fm_a;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TRS.Create;
begin
  Inherited Create(4, 1);
end;

procedure TRS.Initialize;
begin
  Fm_hash := 0;
  Fm_a := 63689;
end;

procedure TRS.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  i: Int32;
begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  i := a_index;
  while a_length > 0 do
  begin
    Fm_hash := (Fm_hash * Fm_a) + a_data[i];
    Fm_a := Fm_a * B;
    System.Inc(i);
    System.Dec(a_length);
  end;

end;

function TRS.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
