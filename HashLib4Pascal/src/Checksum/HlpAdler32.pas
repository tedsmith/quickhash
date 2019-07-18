unit HlpAdler32;

{$I ..\Include\HashLib.inc}

interface

uses
  HlpHashLibTypes,
  HlpIHashInfo,
  HlpHash,
  HlpIHash,
  HlpHashResult,
  HlpIHashResult;

type
  TAdler32 = class sealed(THash, IChecksum, IHash32,
    ITransformBlock)

  strict private

    Fm_a, Fm_b: UInt32;

  const
    MOD_ADLER = UInt32(65521);

  public
    constructor Create();
    procedure Initialize(); override;
    procedure TransformBytes(const a_data: THashLibByteArray;
      a_index, a_length: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;

  end;

implementation

{ TAdler32 }

function TAdler32.Clone(): IHash;
var
  HashInstance: TAdler32;
begin
  HashInstance := TAdler32.Create();
  HashInstance.Fm_a := Fm_a;
  HashInstance.Fm_b := Fm_b;
  result := HashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TAdler32.Create;
begin
  Inherited Create(4, 1);

end;

procedure TAdler32.Initialize;
begin
  Fm_a := 1;
  Fm_b := 0;
end;

procedure TAdler32.TransformBytes(const a_data: THashLibByteArray;
  a_index, a_length: Int32);
var
  i, n: Int32;

begin
{$IFDEF DEBUG}
  System.Assert(a_index >= 0);
  System.Assert(a_length >= 0);
  System.Assert(a_index + a_length <= System.Length(a_data));
{$ENDIF DEBUG}
  i := a_index;

  { while a_length > 0 do
    begin
    Fm_a := (Fm_a + a_data[i]) mod MOD_ADLER;
    Fm_b := (Fm_b + Fm_a) mod MOD_ADLER;
    System.Inc(i);
    System.Dec(a_length);
    end; }

  // lifted from PngEncoder Adler32.cs

  while a_length > 0 do
  begin
    // We can defer the modulo operation:
    // Fm_a maximally grows from 65521 to 65521 + 255 * 3800
    // Fm_b maximally grows by3800 * median(Fm_a) = 2090079800 < 2^31
    n := 3800;
    if (n > a_length) then
    begin
      n := a_length;
    end;
    a_length := a_length - n;

    while (n - 1) >= 0 do
    begin
      Fm_a := (Fm_a + a_data[i]);
      Fm_b := (Fm_b + Fm_a);
      System.Inc(i);
      System.Dec(n);
    end;
    Fm_a := Fm_a mod MOD_ADLER;
    Fm_b := Fm_b mod MOD_ADLER;

  end;

end;

function TAdler32.TransformFinal: IHashResult;
begin
  result := THashResult.Create(UInt32((Fm_b shl 16) or Fm_a));
  Initialize();
end;

end.
