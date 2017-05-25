unit Unit1;
// Lets see how hard it may be to make QuickHash a multi-threaded application. 
// This is a simple self-contained thread safe example for MD5 that incorporates
// the same FileIterator concepts as used by QuickHash. 
// As of May 2017, there seems to be an issue with controlling the thread counts
// to the point that the system runs out of threads it can allocate and throws an error
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  HlpHashFactory,
  HlpIHash,
  HlpIHashResult, md5;

type
  PInfoData = ^TInfoData;
   TInfoData = record
     Message  :string;
     ThreadID :TThreadID;
     When     :TDateTime;
   end;

  TFileHashingWorkerThread = class(TThread)
  private
    FFileName : string;
    FEvent    : TDataEvent;
  public
    constructor Create(aFilename : string; const FinishEvent:TDataEvent);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    Button2: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    FileToBeHashed : string;
    procedure InvokeHashThread(FileIterator: TFileIterator);
    procedure HashInfo(Data: PtrInt);
    { public declarations }
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

// *** TFileHashingWorkerThread objects follow *** //

constructor TFileHashingWorkerThread.Create(aFilename : string;const FinishEvent:TDataEvent);
begin
    inherited Create(False);  // WHY DOES THIS LINE OCCASIONALLY THROW "Unable to create thread: Insufficient storage to complete this request"?
    FFileName := aFilename;
    FEvent    := FinishEvent;
    FreeOnTerminate := True; // Avoids memory Leaks
end;

// Not sure if this is needed. Taazz didn't include it, but most of the online samples for TThread include Destroy.
destructor TFileHashingWorkerThread.Destroy;
begin
  inherited Destroy;
end;


// Main worker thread. Essentially, it is the hashing routine from the main program
// of QuickHash but stripped down for simplicity, for now, and made thread safe.
procedure TFileHashingWorkerThread.Execute;
const
   BufSize = 64 * 1024;  // 64kb buffer
var
  GeneratedHash         : string;
  fsFileToBeHashed      : TFileStream;
  HashInstanceMD5       : IHash;
  HashInstanceResultMD5 : IHashResult;
  i                     : integer;

  Buffer: array [0 .. BufSize - 1] of Byte;
  TotalBytesRead, LoopCounter : QWord;

  function NewData(msg:string;when:Tdatetime):PInfoData;
  begin
    Result            := New(PInfoData);
    Result^.Message   := msg;
    Result^.ThreadID  := ThreadID;
    Result^.When      := when;
  end;

begin
  i                := 0;
  TotalBytesRead   := 0;
  fsFileToBeHashed := TFileStream.Create(FFileName, fmOpenRead); // Note that FFilename is from TFileHashingWorkerThread = class(TThread)

  // Output data to memo
  Application.QueueAsyncCall(FEvent, PtrUInt(NewData('Started ' + fsFileToBeHashed.FileName + ', using ThreadID ' + IntToStr(ThreadID) + ' ' + FormatDateTime('dd/mm/yy HH:MM:SS', Now), Now)));
  // Initiate hash library and read sourcefile in buffers
  HashInstanceMD5 := THashFactory.TCrypto.CreateMD5();
  HashInstanceMD5.Initialize();
  repeat
  i := fsFileToBeHashed.Read(Buffer, BufSize);
  if i <= 0 then
    break
  else
    begin
    HashInstanceMD5.TransformUntyped(Buffer, i);
    // Provide the user with feedback of bigger files
    inc(TotalBytesRead, i);
    inc(LoopCounter, 1);
    if LoopCounter = 80 then
      begin
        // Use thread safe way to output progress for these bigger files instead of : Form1.Memo2.Lines.Add.... IntToStr(TotalBytesRead) 'read of...'
        LoopCounter := 0;
      end;
    end;
  until false;

  HashInstanceResultMD5 := HashInstanceMD5.TransformFinal();
  generatedhash := HashInstanceResultMD5.ToString();
  // Output finished file data to memo
  Application.QueueAsyncCall(FEvent,PtrUInt(NewData('Finished ' + fsFileToBeHashed.filename + ' at ' + FormatDateTime('dd/mm/yy HH:MM:SS', Now) + ' ' + GeneratedHash, Now)));
  fsFileToBeHashed.Free;
end;

// *** TForm1 objects follow *** //

procedure TForm1.FormCreate(Sender: TObject);
begin
  Showmessage('Processor count : ' + IntToStr(TThread.ProcessorCount));
end;

// Main thread call. Every time a file is found, this is called, to invoke a new thread
// and output the result of the file via @HashInfo
procedure TForm1.InvokeHashThread(FileIterator: TFileIterator);
begin
  FileToBeHashed := FileIterator.FileName;
  if TThread.IsSingleProcessor then
  begin
    Form1.Memo1.Lines.Add(FileToBeHashed + ' ' + MD5Print(MD5File(FileToBeHashed, 2097152)));
  end else
  begin
    TFileHashingWorkerThread.Create(FileToBeHashed, @HashInfo);
  end;
end;

// HashInfo receives all the output from the thread execution and displays it to the user
// via Memo1.
procedure TForm1.HashInfo(Data :PtrInt);
var
  vStr:string;
begin
  writestr(vStr, PInfoData(Data)^.ThreadID, ' : ', PInfoData(Data)^.When, ' : ', PInfoData(Data)^.Message);
  Memo1.Lines.Add(vstr);
  Dispose(PInfoData(Data));
end;

// Choose a folder and have all the files inside hashed via @InvokeHashThread.
procedure TForm1.Button2Click(Sender: TObject);
var
  FileList : TFileSearcher;
  SearchMask : string;
begin
  SearchMask := '*.*';
  if SelectDirectoryDialog1.Execute then
  try
    FileList := TFileSearcher.Create;
    FileList.FileAttribute := faAnyFile;
    FileList.OnFileFound := @InvokeHashThread;
    FileList.Search(SelectDirectoryDialog1.FileName, SearchMask, true, false);
  finally
    FileList.Free;
  end;
end;

end.

