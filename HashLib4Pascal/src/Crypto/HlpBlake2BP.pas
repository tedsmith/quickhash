unit HlpBlake2BP;

{$I ..\Include\HashLib.inc}

interface

uses
  SysUtils,
{$IFDEF USE_DELPHI_PPL}
  System.Classes,
  System.Threading,
{$ENDIF USE_DELPHI_PPL}
{$IFDEF USE_PASMP}
  PasMP,
{$ENDIF USE_PASMP}
{$IFDEF USE_MTPROCS}
  MTProcs,
{$ENDIF USE_MTPROCS}
  HlpHash,
  HlpIHashResult,
  HlpBlake2B,
  HlpIBlake2BParams,
  HlpBlake2BParams,
  HlpIHash,
  HlpIHashInfo,
  HlpArrayUtils,
  HlpHashLibTypes;

type
  TBlake2BP = class sealed(THash, ICryptoNotBuildIn, ITransformBlock)
  strict private

  type
    PDataContainer = ^TDataContainer;

    TDataContainer = record
      PtrData: PByte;
      Counter: UInt64;
    end;

  const
    BlockSizeInBytes = Int32(128);
    OutSizeInBytes = Int32(64);
    ParallelismDegree = Int32(4);

  var
    // had to use the classes directly for performance purposes
    FRootHash: TBlake2B;
    FLeafHashes: THashLibGenericArray<TBlake2B>;
    FBuffer, FKey: THashLibByteArray;
    FBufferLength: UInt64;

    /// <summary>
    /// <br />Blake2B defaults to setting the expected output length <br />
    /// from the <c>HashSize</c> in the <c>TBlake2BConfig</c> class. <br />In
    /// some cases, however, we do not want this, as the output length <br />
    /// of these instances is given by <c>TBlake2BTreeConfig.InnerSize</c>
    /// instead. <br />
    /// </summary>
    function Blake2BPCreateLeafParam(const ABlake2BConfig: IBlake2BConfig;
      const ABlake2BTreeConfig: IBlake2BTreeConfig): TBlake2B;
    function Blake2BPCreateLeaf(AOffset: UInt64): TBlake2B;
    function Blake2BPCreateRoot(): TBlake2B;
    procedure ParallelComputation(AIdx: Int32; ADataContainer: PDataContainer);

    procedure DoParallelComputation(ADataContainer: PDataContainer);

    function DeepCloneBlake2BInstances(const ALeafHashes
      : THashLibGenericArray<TBlake2B>): THashLibGenericArray<TBlake2B>;

    procedure Clear();

    constructor CreateInternal(AHashSize: Int32);

{$IFDEF USE_PASMP}
    procedure PasMPParallelComputationWrapper(const AJob: PPasMPJob;
      const AThreadIndex: LongInt; const ADataContainer: Pointer;
      const AFromIndex, AToIndex: TPasMPNativeInt); inline;
{$ENDIF USE_PASMP}
{$IFDEF USE_MTPROCS}
    procedure MTProcsParallelComputationWrapper(AIdx: PtrInt;
      ADataContainer: Pointer; AItem: TMultiThreadProcItem); inline;
{$ENDIF USE_MTPROCS}
  strict protected
    function GetName: String; override;

  public
    constructor Create(AHashSize: Int32; const AKey: THashLibByteArray);
    destructor Destroy; override;
    procedure Initialize; override;
    procedure TransformBytes(const AData: THashLibByteArray;
      AIndex, ADataLength: Int32); override;
    function TransformFinal: IHashResult; override;
    function Clone(): IHash; override;
  end;

implementation

{ TBlake2BP }

function TBlake2BP.Blake2BPCreateLeafParam(const ABlake2BConfig: IBlake2BConfig;
  const ABlake2BTreeConfig: IBlake2BTreeConfig): TBlake2B;
begin
  Result := TBlake2B.Create(ABlake2BConfig, ABlake2BTreeConfig);
end;

function TBlake2BP.Blake2BPCreateLeaf(AOffset: UInt64): TBlake2B;
var
  LBlake2BConfig: IBlake2BConfig;
  LBlake2BTreeConfig: IBlake2BTreeConfig;
begin
  LBlake2BConfig := TBlake2BConfig.Create(HashSize);
  LBlake2BConfig.Key := FKey;
  LBlake2BTreeConfig := TBlake2BTreeConfig.Create();
  LBlake2BTreeConfig.FanOut := ParallelismDegree;
  LBlake2BTreeConfig.MaxDepth := 2;
  LBlake2BTreeConfig.NodeDepth := 0;
  LBlake2BTreeConfig.LeafSize := 0;
  LBlake2BTreeConfig.NodeOffset := AOffset;
  LBlake2BTreeConfig.InnerHashSize := OutSizeInBytes;
  if AOffset = (ParallelismDegree - 1) then
  begin
    LBlake2BTreeConfig.IsLastNode := True;
  end;
  Result := Blake2BPCreateLeafParam(LBlake2BConfig, LBlake2BTreeConfig);
end;

function TBlake2BP.Blake2BPCreateRoot(): TBlake2B;
var
  LBlake2BConfig: IBlake2BConfig;
  LBlake2BTreeConfig: IBlake2BTreeConfig;
begin
  LBlake2BConfig := TBlake2BConfig.Create(HashSize);
  LBlake2BConfig.Key := FKey;
  LBlake2BTreeConfig := TBlake2BTreeConfig.Create();
  LBlake2BTreeConfig.FanOut := ParallelismDegree;
  LBlake2BTreeConfig.MaxDepth := 2;
  LBlake2BTreeConfig.NodeDepth := 1;
  LBlake2BTreeConfig.LeafSize := 0;
  LBlake2BTreeConfig.NodeOffset := 0;
  LBlake2BTreeConfig.InnerHashSize := OutSizeInBytes;
  LBlake2BTreeConfig.IsLastNode := True;
  Result := TBlake2B.Create(LBlake2BConfig, LBlake2BTreeConfig, False);
end;

procedure TBlake2BP.Clear;
begin
  TArrayUtils.ZeroFill(FKey);
end;

function TBlake2BP.DeepCloneBlake2BInstances(const ALeafHashes
  : THashLibGenericArray<TBlake2B>): THashLibGenericArray<TBlake2B>;
var
  LIdx: Int32;
begin
  System.SetLength(Result, System.Length(ALeafHashes));
  for LIdx := System.Low(ALeafHashes) to System.High(ALeafHashes) do
  begin
    Result[LIdx] := ALeafHashes[LIdx].CloneInternal();
  end;
end;

function TBlake2BP.Clone(): IHash;
var
  LHashInstance: TBlake2BP;
begin
  LHashInstance := TBlake2BP.CreateInternal(HashSize);
  LHashInstance.FKey := System.Copy(FKey);
  if FRootHash <> Nil then
  begin
    LHashInstance.FRootHash := FRootHash.CloneInternal();
  end;
  LHashInstance.FLeafHashes := DeepCloneBlake2BInstances(FLeafHashes);
  LHashInstance.FBuffer := System.Copy(FBuffer);
  LHashInstance.FBufferLength := FBufferLength;
  Result := LHashInstance as IHash;
  Result.BufferSize := BufferSize;
end;

constructor TBlake2BP.CreateInternal(AHashSize: Int32);
begin
  Inherited Create(AHashSize, BlockSizeInBytes);
end;

constructor TBlake2BP.Create(AHashSize: Int32; const AKey: THashLibByteArray);
var
  LIdx: Int32;
begin
  Inherited Create(AHashSize, BlockSizeInBytes);
  System.SetLength(FBuffer, ParallelismDegree * BlockSizeInBytes);
  System.SetLength(FLeafHashes, ParallelismDegree);
  FKey := System.Copy(AKey);
  FRootHash := Blake2BPCreateRoot;
  for LIdx := 0 to System.Pred(ParallelismDegree) do
  begin
    FLeafHashes[LIdx] := Blake2BPCreateLeaf(LIdx);
  end;
end;

destructor TBlake2BP.Destroy;
var
  LIdx: Int32;
begin
  Clear();
  FRootHash.Free;
  FRootHash := Nil;
  for LIdx := System.Low(FLeafHashes) to System.High(FLeafHashes) do
  begin
    FLeafHashes[LIdx].Free;
    FLeafHashes[LIdx] := Nil;
  end;
  FLeafHashes := Nil;
  inherited Destroy;
end;

function TBlake2BP.GetName: String;
begin
  Result := Format('%s_%u', [Self.ClassName, Self.HashSize * 8]);
end;

procedure TBlake2BP.Initialize;
var
  LIdx: Int32;
begin
  FRootHash.Initialize;
  for LIdx := 0 to System.Pred(ParallelismDegree) do
  begin
    FLeafHashes[LIdx].Initialize;
    FLeafHashes[LIdx].HashSize := OutSizeInBytes;
  end;
  TArrayUtils.ZeroFill(FBuffer);
  FBufferLength := 0;
end;

procedure TBlake2BP.ParallelComputation(AIdx: Int32;
  ADataContainer: PDataContainer);
var
  LLeafHashes: THashLibGenericArray<TBlake2B>;
  LTemp: THashLibByteArray;
  LCounter: UInt64;
  LPtrData: PByte;
begin
  System.SetLength(LTemp, BlockSizeInBytes);
  LPtrData := ADataContainer^.PtrData;
  LCounter := ADataContainer^.Counter;
  System.Inc(LPtrData, AIdx * BlockSizeInBytes);
  LLeafHashes := FLeafHashes;
  while (LCounter >= (ParallelismDegree * BlockSizeInBytes)) do
  begin
    System.Move(LPtrData^, LTemp[0], BlockSizeInBytes);
    LLeafHashes[AIdx].TransformBytes(LTemp, 0, BlockSizeInBytes);
    System.Inc(LPtrData, UInt64(ParallelismDegree * BlockSizeInBytes));
    LCounter := LCounter - UInt64(ParallelismDegree * BlockSizeInBytes);
  end;
end;

{$IFDEF USE_PASMP}

procedure TBlake2BP.PasMPParallelComputationWrapper(const AJob: PPasMPJob;
  const AThreadIndex: LongInt; const ADataContainer: Pointer;
  const AFromIndex, AToIndex: TPasMPNativeInt);
begin
  ParallelComputation(AFromIndex, ADataContainer);
end;
{$ENDIF}
{$IFDEF USE_MTPROCS}

procedure TBlake2BP.MTProcsParallelComputationWrapper(AIdx: PtrInt;
  ADataContainer: Pointer; AItem: TMultiThreadProcItem);
begin
  ParallelComputation(AIdx, ADataContainer);
end;
{$ENDIF}
{$IF DEFINED(USE_DELPHI_PPL)}

procedure TBlake2BP.DoParallelComputation(ADataContainer: PDataContainer);

  function CreateTask(AIdx: Int32; ADataContainer: PDataContainer): ITask;
  begin
    Result := TTask.Create(
      procedure()
      begin
        ParallelComputation(AIdx, ADataContainer);
      end);
  end;

var
  LArrayTasks: array of ITask;
  LIdx: Int32;
begin
  System.SetLength(LArrayTasks, ParallelismDegree);
  for LIdx := 0 to System.Pred(ParallelismDegree) do
  begin
    LArrayTasks[LIdx] := CreateTask(LIdx, ADataContainer);
    LArrayTasks[LIdx].Start;
  end;
  TTask.WaitForAll(LArrayTasks);
end;

{$ELSEIF DEFINED(USE_PASMP) OR DEFINED(USE_MTPROCS)}

procedure TBlake2BP.DoParallelComputation(ADataContainer: PDataContainer);
begin
{$IF DEFINED(USE_PASMP)}
  TPasMP.CreateGlobalInstance;
  GlobalPasMP.Invoke(GlobalPasMP.ParallelFor(ADataContainer, 0,
    ParallelismDegree - 1, PasMPParallelComputationWrapper));
{$ELSEIF DEFINED(USE_MTPROCS)}
  ProcThreadPool.DoParallel(MTProcsParallelComputationWrapper, 0,
    ParallelismDegree - 1, ADataContainer);
{$ELSE}
{$MESSAGE ERROR 'Unsupported Threading Library.'}
{$IFEND USE_PASMP}
end;

{$ELSE}

procedure TBlake2BP.DoParallelComputation(ADataContainer: PDataContainer);
var
  LIdx: Int32;
begin
  for LIdx := 0 to System.Pred(ParallelismDegree) do
  begin
    ParallelComputation(LIdx, ADataContainer);
  end;
end;
{$IFEND USE_DELPHI_PPL}

procedure TBlake2BP.TransformBytes(const AData: THashLibByteArray;
AIndex, ADataLength: Int32);
var
  LLeft, LFill, LDataLength: UInt64;
  LPtrData: PByte;
  LIdx: Int32;
  LLeafHashes: THashLibGenericArray<TBlake2B>;
  LPtrDataContainer: PDataContainer;
begin
  LLeafHashes := FLeafHashes;
  LDataLength := UInt64(ADataLength);
  LPtrData := PByte(AData) + AIndex;
  LLeft := FBufferLength;
  LFill := UInt64(System.Length(FBuffer)) - LLeft;

  if (LLeft > 0) and (LDataLength >= LFill) then
  begin
    System.Move(LPtrData^, FBuffer[LLeft], LFill);

    for LIdx := 0 to System.Pred(ParallelismDegree) do
    begin
      LLeafHashes[LIdx].TransformBytes(FBuffer, LIdx * BlockSizeInBytes,
        BlockSizeInBytes);
    end;

    System.Inc(LPtrData, LFill);
    LDataLength := LDataLength - LFill;
    LLeft := 0;
  end;

  LPtrDataContainer := New(PDataContainer);
  try
    LPtrDataContainer^.PtrData := LPtrData;
    LPtrDataContainer^.Counter := LDataLength;
    DoParallelComputation(LPtrDataContainer);
  finally
    Dispose(LPtrDataContainer);
  end;

  System.Inc(LPtrData, LDataLength - (LDataLength mod UInt64(ParallelismDegree *
    BlockSizeInBytes)));
  LDataLength := LDataLength mod UInt64(ParallelismDegree * BlockSizeInBytes);

  if (LDataLength > 0) then
  begin
    System.Move(LPtrData^, FBuffer[LLeft], LDataLength);
  end;

  FBufferLength := LLeft + LDataLength;
end;

function TBlake2BP.TransformFinal: IHashResult;
var
  LHash: THashLibMatrixByteArray;
  LIdx: Int32;
  LLeft: UInt64;
  LLeafHashes: THashLibGenericArray<TBlake2B>;
  LRootHash: TBlake2B;
begin
  LLeafHashes := FLeafHashes;
  LRootHash := FRootHash;
  System.SetLength(LHash, ParallelismDegree);
  for LIdx := System.Low(LHash) to System.High(LHash) do
  begin
    System.SetLength(LHash[LIdx], OutSizeInBytes);
  end;

  for LIdx := 0 to System.Pred(ParallelismDegree) do
  begin
    if (FBufferLength > (LIdx * BlockSizeInBytes)) then
    begin
      LLeft := FBufferLength - UInt64(LIdx * BlockSizeInBytes);
      if (LLeft > BlockSizeInBytes) then
      begin
        LLeft := BlockSizeInBytes;
      end;
      LLeafHashes[LIdx].TransformBytes(FBuffer, LIdx * BlockSizeInBytes,
        Int32(LLeft));
    end;

    LHash[LIdx] := LLeafHashes[LIdx].TransformFinal().GetBytes();
  end;

  for LIdx := 0 to System.Pred(ParallelismDegree) do
  begin
    LRootHash.TransformBytes(LHash[LIdx], 0, OutSizeInBytes);
  end;
  Result := LRootHash.TransformFinal();
  Initialize();
end;

end.
