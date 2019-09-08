unit HlpAP;

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
  TAP = class sealed(THash, IHash32, ITransformBlock)
  strict private

    Fm_hash: UInt32;
    Fm_index: Int32;

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal(): IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TAP }

function TAP.Clone(): IHash;
var
  HashInstance: TAP;
begin
  HashInstance := TAP.Create();
  HashInstance.Fm_hash := Fm_hash;
  HashInstance.Fm_index := Fm_index;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TAP.Create;
begin
  Inherited Create(4, 1);
end;

procedure TAP.Initialize;
begin
  Fm_hash := $AAAAAAAA;
  Fm_index := 0;
end;

procedure TAP.TransformBytes(const a_data: THashLibByteArray;
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

    if (Fm_index and 1) = 0 then
      Fm_hash := Fm_hash xor ((Fm_hash shl 7) xor a_data[i] * (Fm_hash shr 3))

    else

      Fm_hash := Fm_hash xor
        (not((Fm_hash shl 11) xor a_data[i] xor (Fm_hash shr 5)));

    System.Inc(Fm_index);
    System.Inc(i);
    System.Dec(a_length);
  end;
end;

function TAP.TransformFinal: IHashResult;
begin
  result := THashResult.Create(Fm_hash);
  Initialize();
end;

end.
