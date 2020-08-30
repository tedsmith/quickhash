unit HlpGrindahl512;

{$I ..\Include\HashLib.inc}

interface

uses
{$IFDEF DELPHI2010}
  SysUtils, // to get rid of compiler hint "not inlined" on Delphi 2010.
{$ENDIF DELPHI2010}
  HlpHashLibTypes,
{$IFDEF DELPHI}
  HlpHash,
  HlpHashBuffer,
{$ENDIF DELPHI}
  HlpBits,
  HlpConverters,
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashCryptoNotBuildIn;

type
  TGrindahl512 = class sealed(TBlockHash, ICryptoNotBuildIn, ITransformBlock)

  strict private
  var
    FState, FTemp: THashLibUInt64Array;

    class var

      FSTable0, FSTable1, FSTable2, FSTable3, FSTable4, FSTable5, FSTable6,
      FSTable7: THashLibUInt64Array;

{$REGION 'Consts'}

  const
    SMasterTable: array [0 .. 255] of UInt64 = (UInt64($C6636397633551A2),
      UInt64($F87C7CEB7CCD1326), UInt64($EE7777C777952952),
      UInt64($F67B7BF77BF50102), UInt64($FFF2F2E5F2D11A34),
      UInt64($D66B6BB76B7561C2), UInt64($DE6F6FA76F5579F2),
      UInt64($91C5C539C572A84B), UInt64($603030C0309BA05B),
      UInt64($020101040108060C), UInt64($CE67678767154992),
      UInt64($562B2BAC2B43FAEF), UInt64($E7FEFED5FEB13264),
      UInt64($B5D7D771D7E2C493), UInt64($4DABAB9AAB2FD7B5),
      UInt64($EC7676C3769D2F5E), UInt64($8FCACA05CA0A8A0F),
      UInt64($1F82823E827C2142), UInt64($89C9C909C912801B),
      UInt64($FA7D7DEF7DC5152A), UInt64($EFFAFAC5FA912A54),
      UInt64($B259597F59FECD81), UInt64($8E474707470E8909),
      UInt64($FBF0F0EDF0C1162C), UInt64($41ADAD82AD1FC39D),
      UInt64($B3D4D47DD4FACE87), UInt64($5FA2A2BEA267E1D9),
      UInt64($45AFAF8AAF0FCF85), UInt64($239C9C469C8C65CA),
      UInt64($53A4A4A6A457F5F1), UInt64($E47272D372BD376E),
      UInt64($9BC0C02DC05AB677), UInt64($75B7B7EAB7CF9F25),
      UInt64($E1FDFDD9FDA93870), UInt64($3D93937A93F4478E),
      UInt64($4C262698262BD4B3), UInt64($6C3636D836ABB473),
      UInt64($7E3F3FFC3FE3821F), UInt64($F5F7F7F1F7F90408),
      UInt64($83CCCC1DCC3A9E27), UInt64($683434D034BBB86B),
      UInt64($51A5A5A2A55FF3FD), UInt64($D1E5E5B9E56968D0),
      UInt64($F9F1F1E9F1C91020), UInt64($E27171DF71A53D7A),
      UInt64($ABD8D84DD89AE6D7), UInt64($623131C43193A657),
      UInt64($2A15155415A87EFC), UInt64($0804041004201830),
      UInt64($95C7C731C762A453), UInt64($4623238C2303CA8F),
      UInt64($9DC3C321C342BC63), UInt64($3018186018C050A0),
      UInt64($3796966E96DC59B2), UInt64($0A05051405281E3C),
      UInt64($2F9A9A5E9ABC71E2), UInt64($0E07071C07381224),
      UInt64($2412124812906CD8), UInt64($1B808036806C2D5A),
      UInt64($DFE2E2A5E2517AF4), UInt64($CDEBEB81EB194C98),
      UInt64($4E27279C2723D2BF), UInt64($7FB2B2FEB2E78119),
      UInt64($EA7575CF7585254A), UInt64($120909240948366C),
      UInt64($1D83833A8374274E), UInt64($582C2CB02C7BE8CB),
      UInt64($341A1A681AD05CB8), UInt64($361B1B6C1BD85AB4),
      UInt64($DC6E6EA36E5D7FFE), UInt64($B45A5A735AE6C795),
      UInt64($5BA0A0B6A077EDC1), UInt64($A452525352A6F7F5),
      UInt64($763B3BEC3BC39A2F), UInt64($B7D6D675D6EAC29F),
      UInt64($7DB3B3FAB3EF8715), UInt64($522929A42953F6F7),
      UInt64($DDE3E3A1E3597CF8), UInt64($5E2F2FBC2F63E2DF),
      UInt64($13848426844C356A), UInt64($A653535753AEF1F9),
      UInt64($B9D1D169D1D2D0BB), UInt64($0000000000000000),
      UInt64($C1EDED99ED2958B0), UInt64($40202080201BC09B),
      UInt64($E3FCFCDDFCA13E7C), UInt64($79B1B1F2B1FF8B0D),
      UInt64($B65B5B775BEEC199), UInt64($D46A6AB36A7D67CE),
      UInt64($8DCBCB01CB028C03), UInt64($67BEBECEBE87A949),
      UInt64($723939E439D39637), UInt64($944A4A334A66A755),
      UInt64($984C4C2B4C56B37D), UInt64($B058587B58F6CB8D),
      UInt64($85CFCF11CF229433), UInt64($BBD0D06DD0DAD6B7),
      UInt64($C5EFEF91EF3954A8), UInt64($4FAAAA9EAA27D1B9),
      UInt64($EDFBFBC1FB992C58), UInt64($86434317432E9139),
      UInt64($9A4D4D2F4D5EB571), UInt64($663333CC3383AA4F),
      UInt64($1185852285443366), UInt64($8A45450F451E8511),
      UInt64($E9F9F9C9F9892040), UInt64($0402020802100C18),
      UInt64($FE7F7FE77FD51932), UInt64($A050505B50B6FBED),
      UInt64($783C3CF03CFB880B), UInt64($259F9F4A9F946FDE),
      UInt64($4BA8A896A837DDA1), UInt64($A251515F51BEFDE1),
      UInt64($5DA3A3BAA36FE7D5), UInt64($8040401B40369B2D),
      UInt64($058F8F0A8F140F1E), UInt64($3F92927E92FC4182),
      UInt64($219D9D429D8463C6), UInt64($703838E038DB903B),
      UInt64($F1F5F5F9F5E90810), UInt64($63BCBCC6BC97A551),
      UInt64($77B6B6EEB6C79929), UInt64($AFDADA45DA8AEACF),
      UInt64($422121842113C697), UInt64($20101040108060C0),
      UInt64($E5FFFFD1FFB93468), UInt64($FDF3F3E1F3D91C38),
      UInt64($BFD2D265D2CADAAF), UInt64($81CDCD19CD32982B),
      UInt64($180C0C300C602850), UInt64($2613134C13986AD4),
      UInt64($C3ECEC9DEC215EBC), UInt64($BE5F5F675FCED9A9),
      UInt64($3597976A97D45FBE), UInt64($8844440B4416831D),
      UInt64($2E17175C17B872E4), UInt64($93C4C43DC47AAE47),
      UInt64($55A7A7AAA74FFFE5), UInt64($FC7E7EE37EDD1F3E),
      UInt64($7A3D3DF43DF38E07), UInt64($C864648B640D4386),
      UInt64($BA5D5D6F5DDED5B1), UInt64($3219196419C856AC),
      UInt64($E67373D773B53162), UInt64($C060609B602D5BB6),
      UInt64($1981813281642B56), UInt64($9E4F4F274F4EB969),
      UInt64($A3DCDC5DDCBAFEE7), UInt64($44222288220BCC83),
      UInt64($542A2AA82A4BFCE3), UInt64($3B90907690EC4D9A),
      UInt64($0B888816882C1D3A), UInt64($8C46460346068F05),
      UInt64($C7EEEE95EE3152A4), UInt64($6BB8B8D6B8B7BD61),
      UInt64($2814145014A078F0), UInt64($A7DEDE55DEAAF2FF),
      UInt64($BC5E5E635EC6DFA5), UInt64($160B0B2C0B583A74),
      UInt64($ADDBDB41DB82ECC3), UInt64($DBE0E0ADE04176EC),
      UInt64($643232C8328BAC43), UInt64($743A3AE83ACB9C23),
      UInt64($140A0A280A503C78), UInt64($9249493F497EAD41),
      UInt64($0C06061806301428), UInt64($48242490243BD8AB),
      UInt64($B85C5C6B5CD6D3BD), UInt64($9FC2C225C24ABA6F),
      UInt64($BDD3D361D3C2DCA3), UInt64($43ACAC86AC17C591),
      UInt64($C4626293623D57AE), UInt64($3991917291E44B96),
      UInt64($3195956295C453A6), UInt64($D3E4E4BDE4616EDC),
      UInt64($F27979FF79E50D1A), UInt64($D5E7E7B1E77964C8),
      UInt64($8BC8C80DC81A8617), UInt64($6E3737DC37A3B27F),
      UInt64($DA6D6DAF6D4575EA), UInt64($018D8D028D040306),
      UInt64($B1D5D579D5F2C88B), UInt64($9C4E4E234E46BF65),
      UInt64($49A9A992A93FDBAD), UInt64($D86C6CAB6C4D73E6),
      UInt64($AC5656435686EFC5), UInt64($F3F4F4FDF4E10E1C),
      UInt64($CFEAEA85EA114A94), UInt64($CA65658F6505458A),
      UInt64($F47A7AF37AFD070E), UInt64($47AEAE8EAE07C989),
      UInt64($1008082008403060), UInt64($6FBABADEBAA7B179),
      UInt64($F07878FB78ED0B16), UInt64($4A2525942533DEA7),
      UInt64($5C2E2EB82E6BE4D3), UInt64($381C1C701CE04890),
      UInt64($57A6A6AEA647F9E9), UInt64($73B4B4E6B4D79531),
      UInt64($97C6C635C66AA25F), UInt64($CBE8E88DE801468C),
      UInt64($A1DDDD59DDB2F8EB), UInt64($E87474CB748D2346),
      UInt64($3E1F1F7C1FF84284), UInt64($964B4B374B6EA159),
      UInt64($61BDBDC2BD9FA35D), UInt64($0D8B8B1A8B34172E),
      UInt64($0F8A8A1E8A3C1122), UInt64($E07070DB70AD3B76),
      UInt64($7C3E3EF83EEB8413), UInt64($71B5B5E2B5DF933D),
      UInt64($CC666683661D4F9E), UInt64($9048483B4876AB4D),
      UInt64($0603030C03180A14), UInt64($F7F6F6F5F6F10204),
      UInt64($1C0E0E380E702448), UInt64($C261619F61255DBA),
      UInt64($6A3535D435B3BE67), UInt64($AE575747578EE9C9),
      UInt64($69B9B9D2B9BFBB6D), UInt64($1786862E865C3972),
      UInt64($99C1C129C152B07B), UInt64($3A1D1D741DE84E9C),
      UInt64($279E9E4E9E9C69D2), UInt64($D9E1E1A9E14970E0),
      UInt64($EBF8F8CDF881264C), UInt64($2B98985698AC7DFA),
      UInt64($22111144118866CC), UInt64($D26969BF69656DDA),
      UInt64($A9D9D949D992E0DB), UInt64($078E8E0E8E1C0912),
      UInt64($3394946694CC55AA), UInt64($2D9B9B5A9BB477EE),
      UInt64($3C1E1E781EF04488), UInt64($1587872A87543F7E),
      UInt64($C9E9E989E9094080), UInt64($87CECE15CE2A923F),
      UInt64($AA55554F559EE5D1), UInt64($502828A0285BF0FB),
      UInt64($A5DFDF51DFA2F4F3), UInt64($038C8C068C0C050A),
      UInt64($59A1A1B2A17FEBCD), UInt64($0989891289241B36),
      UInt64($1A0D0D340D682E5C), UInt64($65BFBFCABF8FAF45),
      UInt64($D7E6E6B5E67162C4), UInt64($8442421342269735),
      UInt64($D06868BB686D6BD6), UInt64($8241411F413E9D21),
      UInt64($2999995299A47BF6), UInt64($5A2D2DB42D73EEC7),
      UInt64($1E0F0F3C0F782244), UInt64($7BB0B0F6B0F78D01),
      UInt64($A854544B5496E3DD), UInt64($6DBBBBDABBAFB775),
      UInt64($2C16165816B074E8));

{$ENDREGION}
    class function CalcTable(AI: Int32): THashLibUInt64Array;

    procedure InjectMsg(AFullProcess: Boolean);

    class constructor Grindahl512();

  strict protected
    procedure Finish(); override;
    function GetResult(): THashLibByteArray; override;
    procedure TransformBlock(AData: PByte; ADataLength: Int32;
      AIndex: Int32); override;

  public
    constructor Create();
    procedure Initialize(); override;
    function Clone(): IHash; override;

  end;

implementation

{ TGrindahl512 }

class function TGrindahl512.CalcTable(AI: Int32): THashLibUInt64Array;
var
  Jdx: Int32;
begin
  System.SetLength(result, 256);
  Jdx := 0;
  while Jdx < 256 do
  begin
    result[Jdx] := TBits.RotateRight64(SMasterTable[Jdx], AI * 8);
    System.Inc(Jdx);
  end;
end;

function TGrindahl512.Clone(): IHash;
var
  LHashInstance: TGrindahl512;
begin
  LHashInstance := TGrindahl512.Create();
  LHashInstance.FState := System.Copy(FState);
  LHashInstance.FTemp := System.Copy(FTemp);
  LHashInstance.FBuffer := FBuffer.Clone();
  LHashInstance.FProcessedBytesCount := FProcessedBytesCount;
  result := LHashInstance as IHash;
  result.BufferSize := BufferSize;
end;

constructor TGrindahl512.Create;
begin
  Inherited Create(64, 8);
  System.SetLength(FState, 13);
  System.SetLength(FTemp, 13);
end;

procedure TGrindahl512.Finish;
var
  LPaddingSize, LIdx: Int32;
  LMessageLength: UInt64;
  LPad: THashLibByteArray;
begin
  LPaddingSize := 16 - Int32(FProcessedBytesCount and UInt32(7));
  LMessageLength := (FProcessedBytesCount shr UInt64(3)) + 1;

  System.SetLength(LPad, LPaddingSize);

  LPad[0] := $80;

  LMessageLength := TConverters.be2me_64(LMessageLength);

  TConverters.ReadUInt64AsBytesLE(LMessageLength, LPad, LPaddingSize - 8);

  TransformBytes(LPad, 0, LPaddingSize - 8);

  FState[0] := TConverters.ReadBytesAsUInt64LE(PByte(LPad), LPaddingSize - 8);

  FState[0] := TConverters.be2me_64(FState[0]);

  InjectMsg(true);

  LIdx := 0;

  while LIdx < 8 do
  begin
    InjectMsg(true);
    System.Inc(LIdx);
  end;
end;

function TGrindahl512.GetResult: THashLibByteArray;
begin
  System.SetLength(result, 8 * System.SizeOf(UInt64));
  TConverters.be64_copy(PUInt64(FState), 5 * System.SizeOf(UInt64),
    PByte(result), 0, System.Length(result));
end;

class constructor TGrindahl512.Grindahl512;
var
  LLowIndex1, LLowIndex2: Int32;
begin
  System.SetLength(FSTable0, System.Length(SMasterTable));

  LLowIndex1 := System.Low(SMasterTable);
  LLowIndex2 := System.Low(FSTable0);

  System.Move(SMasterTable[LLowIndex1], FSTable0[LLowIndex2],
    System.SizeOf(SMasterTable));

  FSTable1 := CalcTable(1);
  FSTable2 := CalcTable(2);
  FSTable3 := CalcTable(3);
  FSTable4 := CalcTable(4);
  FSTable5 := CalcTable(5);
  FSTable6 := CalcTable(6);
  FSTable7 := CalcTable(7);
end;

procedure TGrindahl512.Initialize;
begin
  TArrayUtils.ZeroFill(FState);
  TArrayUtils.ZeroFill(FTemp);
  Inherited Initialize();
end;

procedure TGrindahl512.InjectMsg(AFullProcess: Boolean);
var
  LU: THashLibUInt64Array;
begin
  FState[12] := FState[12] xor $01;

  if (AFullProcess) then
  begin
    FTemp[0] := FSTable0[Byte(FState[12] shr 56)] xor FSTable1
      [Byte(FState[11] shr 48)] xor FSTable2[Byte(FState[10] shr 40)
      ] xor FSTable3[Byte(FState[9] shr 32)] xor FSTable4[Byte(FState[8] shr 24)
      ] xor FSTable5[Byte(FState[7] shr 16)] xor FSTable6[Byte(FState[6] shr 8)
      ] xor FSTable7[Byte(FState[5])];
  end;

  FTemp[1] := FSTable0[Byte(FState[0] shr 56)] xor FSTable1
    [Byte(FState[12] shr 48)] xor FSTable2[Byte(FState[11] shr 40)] xor FSTable3
    [Byte(FState[10] shr 32)] xor FSTable4[Byte(FState[9] shr 24)] xor FSTable5
    [Byte(FState[8] shr 16)] xor FSTable6[Byte(FState[7] shr 8)] xor FSTable7
    [Byte(FState[6])];

  FTemp[2] := FSTable0[Byte(FState[1] shr 56)] xor FSTable1
    [Byte(FState[0] shr 48)] xor FSTable2[Byte(FState[12] shr 40)] xor FSTable3
    [Byte(FState[11] shr 32)] xor FSTable4[Byte(FState[10] shr 24)] xor FSTable5
    [Byte(FState[9] shr 16)] xor FSTable6[Byte(FState[8] shr 8)] xor FSTable7
    [Byte(FState[7])];

  FTemp[3] := FSTable0[Byte(FState[2] shr 56)] xor FSTable1
    [Byte(FState[1] shr 48)] xor FSTable2[Byte(FState[0] shr 40)] xor FSTable3
    [Byte(FState[12] shr 32)] xor FSTable4[Byte(FState[11] shr 24)] xor FSTable5
    [Byte(FState[10] shr 16)] xor FSTable6[Byte(FState[9] shr 8)] xor FSTable7
    [Byte(FState[8])];

  FTemp[4] := FSTable0[Byte(FState[3] shr 56)] xor FSTable1
    [Byte(FState[2] shr 48)] xor FSTable2[Byte(FState[1] shr 40)] xor FSTable3
    [Byte(FState[0] shr 32)] xor FSTable4[Byte(FState[12] shr 24)] xor FSTable5
    [Byte(FState[11] shr 16)] xor FSTable6[Byte(FState[10] shr 8)] xor FSTable7
    [Byte(FState[9])];

  FTemp[5] := FSTable0[Byte(FState[4] shr 56)] xor FSTable1
    [Byte(FState[3] shr 48)] xor FSTable2[Byte(FState[2] shr 40)] xor FSTable3
    [Byte(FState[1] shr 32)] xor FSTable4[Byte(FState[0] shr 24)] xor FSTable5
    [Byte(FState[12] shr 16)] xor FSTable6[Byte(FState[11] shr 8)] xor FSTable7
    [Byte(FState[10])];

  FTemp[6] := FSTable0[Byte(FState[5] shr 56)] xor FSTable1
    [Byte(FState[4] shr 48)] xor FSTable2[Byte(FState[3] shr 40)] xor FSTable3
    [Byte(FState[2] shr 32)] xor FSTable4[Byte(FState[1] shr 24)] xor FSTable5
    [Byte(FState[0] shr 16)] xor FSTable6[Byte(FState[12] shr 8)] xor FSTable7
    [Byte(FState[11])];

  FTemp[7] := FSTable0[Byte(FState[6] shr 56)] xor FSTable1
    [Byte(FState[5] shr 48)] xor FSTable2[Byte(FState[4] shr 40)] xor FSTable3
    [Byte(FState[3] shr 32)] xor FSTable4[Byte(FState[2] shr 24)] xor FSTable5
    [Byte(FState[1] shr 16)] xor FSTable6[Byte(FState[0] shr 8)] xor FSTable7
    [Byte(FState[12])];

  FTemp[8] := FSTable0[Byte(FState[7] shr 56)] xor FSTable1
    [Byte(FState[6] shr 48)] xor FSTable2[Byte(FState[5] shr 40)] xor FSTable3
    [Byte(FState[4] shr 32)] xor FSTable4[Byte(FState[3] shr 24)] xor FSTable5
    [Byte(FState[2] shr 16)] xor FSTable6[Byte(FState[1] shr 8)] xor FSTable7
    [Byte(FState[0])];

  FTemp[9] := FSTable0[Byte(FState[8] shr 56)] xor FSTable1
    [Byte(FState[7] shr 48)] xor FSTable2[Byte(FState[6] shr 40)] xor FSTable3
    [Byte(FState[5] shr 32)] xor FSTable4[Byte(FState[4] shr 24)] xor FSTable5
    [Byte(FState[3] shr 16)] xor FSTable6[Byte(FState[2] shr 8)] xor FSTable7
    [Byte(FState[1])];

  FTemp[10] := FSTable0[Byte(FState[9] shr 56)] xor FSTable1
    [Byte(FState[8] shr 48)] xor FSTable2[Byte(FState[7] shr 40)] xor FSTable3
    [Byte(FState[6] shr 32)] xor FSTable4[Byte(FState[5] shr 24)] xor FSTable5
    [Byte(FState[4] shr 16)] xor FSTable6[Byte(FState[3] shr 8)] xor FSTable7
    [Byte(FState[2])];

  FTemp[11] := FSTable0[Byte(FState[10] shr 56)] xor FSTable1
    [Byte(FState[9] shr 48)] xor FSTable2[Byte(FState[8] shr 40)] xor FSTable3
    [Byte(FState[7] shr 32)] xor FSTable4[Byte(FState[6] shr 24)] xor FSTable5
    [Byte(FState[5] shr 16)] xor FSTable6[Byte(FState[4] shr 8)] xor FSTable7
    [Byte(FState[3])];

  FTemp[12] := FSTable0[Byte(FState[11] shr 56)] xor FSTable1
    [Byte(FState[10] shr 48)] xor FSTable2[Byte(FState[9] shr 40)] xor FSTable3
    [Byte(FState[8] shr 32)] xor FSTable4[Byte(FState[7] shr 24)] xor FSTable5
    [Byte(FState[6] shr 16)] xor FSTable6[Byte(FState[5] shr 8)] xor FSTable7
    [Byte(FState[4])];

  LU := FTemp;
  FTemp := FState;
  FState := LU;
end;

procedure TGrindahl512.TransformBlock(AData: PByte; ADataLength: Int32;
  AIndex: Int32);
begin
  FState[0] := TConverters.ReadBytesAsUInt64LE(AData, AIndex);
  FState[0] := TConverters.be2me_64(FState[0]);
  InjectMsg(false);
end;

end.
