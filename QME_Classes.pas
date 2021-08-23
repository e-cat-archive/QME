unit QME_Classes;

interface

uses
  Windows, SysUtils, StrUtils, Classes, ItemList, Utils;

type
  EQAReferenceError = class(Exception);

  TFileNameEvent = procedure(Sender: TObject; const FileName: string) of object;

  TQAImage = class(TMemoryStream)
  private
    FFmtName: string[31];
    FOnFileName: TFileNameEvent;
    function GetExt: string;
  protected
    property Ext: string read GetExt;
  public
    constructor CreateFromFile(const FileName: string);
    function Info: string;
    procedure RequireFileName;
    property OnFileName: TFileNameEvent read FOnFileName write FOnFileName;
  end;

  TAnswerWay = (awSelect, awCreate, awPlace, awRelate);
  TAnswerWays = set of TAnswerWay;
  
  TQAProp = (qapID, qapText, qapImage, qapWay, qapPoints, qapSelection,
    qapImmediate, qapAnswer, qapComplete);
  TQAProps = set of TQAProp;

  TQAChangeEvent = procedure(Sender: TObject; Props: TQAProps) of object;

  TQA = class(TItem)
  private
    FID: Integer;
    FText: string;
    FImage: TQAImage;
    FWay: TAnswerWays;
    FPoints: IDStrArray;
    FImmediate: StrArray;
    FAnswer: IntArray;
    FComplete: Boolean;
    FVisObj: TObject;
    FSolved: Real;
    FSavedAnswer: StrArray;
    FAutoSelection: IntArray;
    FOnChange: TQAChangeEvent;
    FOnDestroy: TNotifyEvent;
    procedure CompleteAnswer;
    function GetAnswer: StrArray;
    function GetIndex: Integer;
    function GetPoints: StrArray;
    function GetQuestionPoints: StrArray;
    function GetSelection: BoolArray;
    function GetSolved: Real;
    function GetSummary: StrArray;
    function IDText(ID: Integer): string;
    procedure RestoreAnswer;
    procedure SaveAnswer;
    procedure SetAnswer(const Value: StrArray);
    procedure SetComplete(const Value: Boolean);
    procedure SetID(const Value: Integer);
    procedure SetImage(const Value: TQAImage);
    procedure SetImmediate(const Value: StrArray);
    procedure SetPoints(const Value: StrArray);
    procedure SetSelection(const Value: BoolArray);
    procedure SetText(const Value: string);
    procedure SetWay(const Value: TAnswerWays);
    function TextID(const Text: string): Integer;
    procedure UpdateIsRelate;
  protected
    FChecked: Boolean;
    procedure Changed(Props: TQAProps); virtual;
    function GetReference: TQA; virtual;
    procedure MixUpIDs;
    procedure ResetAnswer;
  public
    destructor Destroy; override;
    procedure EndSetImmediate;
    procedure MixUpPoints;
    procedure PrepareForEdit;
    property Answer: StrArray read GetAnswer write SetAnswer;
    property Complete: Boolean read FComplete write SetComplete;
    property ID: Integer read FID write SetID;
    property Image: TQAImage read FImage write SetImage;
    property Index: Integer read GetIndex;
    property Immediate: StrArray read FImmediate write SetImmediate;
    property Points: StrArray read GetPoints write SetPoints;
    property QuestionPoints: StrArray read GetQuestionPoints;
    property Reference: TQA read GetReference;
    property Selection: BoolArray read GetSelection write SetSelection;
    property Solved: Real read GetSolved;
    property Summary: StrArray read GetSummary;
    property Text: string read FText write SetText;
    property VisObj: TObject read FVisObj write FVisObj;
    property Way: TAnswerWays read FWay write SetWay;
    property OnChange: TQAChangeEvent read FOnChange write FOnChange;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
  end;

  TQAList = class(TItemList)
  private
    FID: Integer;
    FTitle: string;
    FAuthor: string;
    FRefFile: string;
    FReference: TQAList;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TQA;
    function GetQA(ID: Integer): TQA;
    function GetReference: TQAList;
    function GetSolved: Real;
    procedure SetSource(const Value: string);
    procedure SetTitle(const Value: string);
  protected
    procedure Changed; virtual;
    procedure Notification(Item: TItem; Action: TItemAction); override;
  public
    function AllComplete: Boolean;
    destructor Destroy; override;
    constructor CreateFromFile(const FileName: string);
    constructor CreateFromStream(Stream: TStream);
    constructor CreateFromTextStream(Stream: TStream);
    function IncompleteCount: Integer;
    procedure SaveToFile(const FileName: string);
    procedure SaveToStream(Stream: TStream);
    procedure SaveToTextStream(Stream: TStream);
    procedure SetQuestions(ACount: Integer);
    procedure Sort(SortProp: TQAProp; Descending: Boolean = False);
    function UniqueID: Integer;
    property Author: string read FAuthor write SetSource;
    property Items[Index: Integer]: TQA read GetItem; default;
    property QA[ID: Integer]: TQA read GetQA;
    property Reference: TQAList read GetReference;
    property RefFile: string read FRefFile write FRefFile;
    property Solved: Real read GetSolved;
    property Title: string read FTitle write SetTitle;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

const
  SelectedSign = '+';
  UnselectedSign = '-';
  CreateSign = '*';
  PointSign = '.';
  ImageLinkSign = '~';
  EOL = #13#10;
  SQAClass = '.qme';
  STextClass = '.txt';
  SAnyClass = '.*';

  AllQAProps = [qapID, qapText, qapImage, qapWay, qapPoints, qapSelection,
    qapImmediate, qapAnswer, qapComplete];

function SupportPoints(Way: TAnswerWays): Boolean;
function AnswerWayToStr(Way: TAnswerWays): string;

var
  StreamImages: Boolean = True;
  RefsPath: string;


implementation

uses
  RTLConsts, Masks_2, ExtUtils, Math;
  
const
  QASignature: array[0..3] of Char = 'QME0';
{}OldSignature: array[0..3] of Char = 'PTT0';

resourcestring
  SImageTmpFile = '%sQME%.8x%s';
  SImageInfo = '%s, %d КБ';
  SNoReference = 'Отсуствует референсный вопрос-ответ для # %d';
  SInvalidReference = 'Вопрос-ответ не является референсным для # %d';

function SupportPoints(Way: TAnswerWays): Boolean;
begin
  Result := Way * [awSelect, awPlace] <> [];
end;

function UnconditionalPoints(Way: TAnswerWays): Boolean; 
begin
  Result := Way * [awSelect, awPlace] = [awPlace];
end;

function AnswerWayToStr(Way: TAnswerWays): string;
const
  WaySigns: array[TAnswerWay] of Char = (SelectedSign, CreateSign, '#', '=');
var
  I: Integer;
  W: TAnswerWay;
  Buf: array[0..15] of Char;
begin
  if awRelate in Way then
    Exclude(Way, awPlace);
  I := 0;
  for W := Low(TAnswerWay) to High(TAnswerWay) do
    if W in Way then
    begin
      Buf[I] := WaySigns[W];
      Inc(I);
    end;
  SetString(Result, Buf, I);
end;

{ TQAImage }

constructor TQAImage.CreateFromFile(const FileName: string);
begin
  FFmtName := UpperCase(Copy(ExtractFileExt(FileName), 2, MaxInt));
  LoadFromFile(FileName);
end;

function TQAImage.GetExt: string;
begin
  Result := '.' + LowerCase(FFmtName);
end;

function TQAImage.Info: string;
begin
  if Self <> nil then
    Result := Format(SImageInfo, [FFmtName, Ceil(Size / $400)]);
end;

procedure TQAImage.RequireFileName;
var
  Buf: array[0..MAX_PATH - 1] of Char;
  FileName: string;
begin
  if Assigned(FOnFileName) then
  begin
    GetTempPath(SizeOf(Buf), Buf);
    repeat
      FileName := Format(SImageTmpFile, [Buf, Random(MaxInt), Ext]);
    until not FileExists(FileName);
    SaveToFile(FileName);
    FOnFileName(Self, FileName);
    DeleteFile(FileName);
  end;
end;

{ TQA }

procedure TQA.Changed(Props: TQAProps);
begin
  FChecked := False;
  if Assigned(FOnChange) then
    FOnChange(Self, Props);
  NotifyChange;
end;

procedure TQA.CompleteAnswer;
var
  PC, AC, I: Integer;
begin
  PC := Length(FPoints);
  AC := Length(FAnswer);
  SetLength(FAnswer, PC + Length(FImmediate));
  if UnconditionalPoints(FWay) then
    for I := 0 to PC - 1 do
      AddInt(FAnswer, FPoints[I].ID, AC);
  if awCreate in FWay then
    for I := PC to PC + High(FImmediate) do
      AddInt(FAnswer, I, AC);
  SetLength(FAnswer, AC);
end;

destructor TQA.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FImage.Free;
  inherited;
end;

procedure TQA.EndSetImmediate;
begin
  FAutoSelection := nil;
end;

function TQA.GetAnswer: StrArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FAnswer));
  for I := 0 to High(FAnswer) do
    Result[I] := IDText(FAnswer[I]);
end;

function TQA.GetIndex: Integer;
begin
  Result := -1;
  if Owner <> nil then
    Result := Owner.IndexOf(Self);
end;

function TQA.GetPoints: StrArray;
var
  I: Integer;
begin
  SetLength(Result, Length(FPoints));
  for I := 0 to High(FPoints) do
    Result[I] := FPoints[I].S;
end;

function TQA.GetQuestionPoints: StrArray;
const
  MaxCount = 100;
var
  I, J, P, Len: Integer;
  S: string;
begin
  Len := Length(FText);
  SetLength(Result, MaxCount);
  P := 1;
  I := 0;
  while (P < Len) and (I < MaxCount) do
  begin
    S := Format('%d) ', [I + 1]);
    P := PosEx(S, FText, P);
    if P = 0 then
      Break;
    J := P;
    Inc(P, Length(S));
    if (J = 1) or (FText[J - 1] = ' ') then
    begin
      J := P;
      while (J <= Len) and not (FText[J] in [';', '.', '?']) do
        Inc(J);
      Result[I] := Copy(FText, P, J - P);
      P := J + 1;
      Inc(I);
    end;
  end;
  SetLength(Result, I);
end;

function TQA.GetReference: TQA;
begin
  Result := nil;
  if (Owner is TQAList) then
    Result := TQAList(Owner).Reference.GetQA(FID);
  if Result = nil then
    raise EQAReferenceError.CreateResFmt(@SNoReference, [FID]);
end;

function TQA.GetSelection: BoolArray;
var
  I: Integer;
begin
  if awSelect in FWay then
  begin
    Setlength(Result, Length(FPoints));
    for I := 0 to High(FPoints) do
      Result[I] := IntIndex(FPoints[I].ID, FAnswer) <> -1;
  end;
end;

function TQA.GetSolved: Real;

  function ConvertIndexes(const Indexes: IntArray; IndexBase: Integer;
    const SourceStrs, TargetStrs: StrArray): IntArray;
  var
    I, J, X: Integer;
    Avail: BoolArray;
    Found: Boolean;
  begin
    if (SourceStrs <> nil) and (TargetStrs <> nil) then
    begin
      Result := Copy(Indexes, 0, MaxInt);
      SetLength(Avail, Length(TargetStrs));
      FillChar(Avail[0], Length(Avail), True);
      X := IndexBase + Length(TargetStrs);
      for I := 0 to High(Indexes) do
      begin
        J := Indexes[I] - IndexBase;
        if J >= 0 then
        begin
          Found := False;
          with TMask.Create(SourceStrs[J]) do
            try
              for J := 0 to High(TargetStrs) do
                if Avail[J] and Matches(TargetStrs[J]) then
                begin
                  Result[I] := IndexBase + J;
                  Avail[J] := False;
                  Found := True;
                  Break;
                end;
            finally
              Free;
            end;
          if not Found then
          begin
            Result[I] := X;
            Inc(X);
          end;
        end;
      end;
    end else
      Result := Indexes; 
  end;

  function SelectionSimilarity(const A, B: IntArray): Real;
  var
    I, Sum, Intersect: Integer;
  begin
    Sum := Length(A) + Length(B);
    if Sum > 0 then
    begin
      Intersect := 0;
      for I := 0 to High(A) do
        if IntIndex(A[I], B) <> -1 then
          Inc(Intersect);
      Result := Intersect / (Sum - Intersect);
    end else
      Result := 1.0;
  end;
  
  function OrderSimilarity(const A, B: IntArray): Real;

    function SumDistances(n: Integer): Integer;
    begin
      Result := (n*n*n - n) div 6;
    end;

    function SumAbsDistanceDif(const A, B: IntArray): Integer;

      function IndexLUT(const A: IntArray; Len: Integer): IntArray;
      var
        I: Integer;
      begin
        SetLength(Result, Len);
        for I := 0 to Len - 1 do
          Result[I] := -1;
        for I := 0 to High(A) do
          Result[A[I]] := I;
      end;

    var
      I, J, X, Y, D: Integer;
      AIndexes, BIndexes: IntArray;
    begin
      J := 0;
      for I := 0 to High(A) do
        if J < A[I] then
          J := A[I];
      for I := 0 to High(B) do
        if J < B[I] then
          J := B[I];
      Inc(J);
      AIndexes := IndexLUT(A, J);
      BIndexes := IndexLUT(B, J);
      Result := 0;
      for I := 0 to High(A) - 1 do
        for J := I + 1 to High(A) do
        begin
          X := BIndexes[A[I]];
          Y := BIndexes[A[J]];
          if (X <> -1) and (Y <> -1) then
            D := Y - X
          else
            D := 0;
          Inc(Result, Abs(D - (J - I)));
        end;
      for I := 0 to High(B) - 1 do
        for J := I + 1 to High(B) do
          if (AIndexes[B[I]] = -1) or (AIndexes[B[J]] = -1) then
            Inc(Result, J - I);
    end;

  var
    ALen, BLen: Integer;
  begin
    ALen := Length(A);
    BLen := Length(B);
    if (ALen > 1) and (BLen > 1) then
      Result := 1.0 - SumAbsDistanceDif(A, B) / (SumDistances(ALen)
        + SumDistances(BLen))
    else
      if (ALen = BLen) and ((ALen = 0) or (A[0] = B[0])) then
        Result := 1.0
      else
        Result := 0.0;
  end;

  function SequenceSimilarity(const A, B: IntArray): Real;
  var
    Len, Equal, I: Integer;
  begin
    Len := Max(Length(A), Length(B));
    if Len > 0 then
    begin
      Equal := 0;
      for I := 0 to Min(High(A), High(B)) do
        if A[I] = B[I] then
          Inc(Equal);
      Result := Equal / Len;
    end else
      Result := 1.0;
  end;

  function AnswerSimilarity(AReference: TQA): Real;
  var
    A, B: IntArray;
  begin
    if (FWay <> AReference.FWay) or
      (Length(FPoints) <> Length(AReference.FPoints)) then
      raise EQAReferenceError.CreateResFmt(@SInvalidReference, [FID]);
    A := FAnswer;
    if awCreate in FWay then
      B := ConvertIndexes(AReference.FAnswer, Length(FPoints),
        AReference.FImmediate, FImmediate)
    else
      B := AReference.FAnswer;
    if awPlace in FWay then
      if awRelate in FWay then
        Result := SequenceSimilarity(A, B) else
        Result := OrderSimilarity(A, B)
    else
      Result := SelectionSimilarity(A, B);
  end;

begin
  if not FChecked then
  begin
    FSolved := AnswerSimilarity(GetReference);
    FChecked := True;
  end;
  Result := FSolved;
end;

function TQA.GetSummary: StrArray;
var
  I: Integer;
  
  procedure AddFmt(const Fmt: string; const Args: array of const);
  begin
    Result[I] := Format(Fmt, Args);
    Inc(I);
  end;

  function PointsToStr(APoints: IDStrArray): string;
  var
    I: Integer;
  begin
    for I := 0 to High(APoints) do
      with APoints[I] do
        Result := Format('%s%d="%s"; ', [Result, ID, S]);
  end;

begin
  I := 0;
  SetLength(Result, 16);
  AddFmt('ID = %d', [FID]);
  AddFmt('Text = %s', [FText]);
  if FImage <> nil then
    AddFmt('Image: %d bytes, %s', [FImage.Size, FImage.FFmtName]);
  AddFmt('Way = %s', [AnswerWayToStr(FWay)]);
  AddFmt('Points (%d): %s', [Length(FPoints), PointsToStr(FPoints)]);
  AddFmt('Immediate (%d): %s', [Length(FImmediate), ArrayToStr(FImmediate,
    ', ', '"', '"')]);
  AddFmt('Answer (%d): %s', [Length(FAnswer), IntsToStr(FAnswer)]);
  AddFmt('Complete = %s', [BoolToStr(FComplete)]);
  AddFmt('VisObj = %.8x', [Cardinal(FVisObj)]);
  if FChecked then
    AddFmt('Solved = %g', [FSolved]);
  AddFmt('SavedAnswer = %s', [ArrayToStr(FSavedAnswer, ', ', '"', '"')]);
  AddFmt('AutoSelection: %s', [IntsToStr(FAutoSelection)]);
  AddFmt('Index = %d', [GetIndex]);
  SetLength(Result, I);
end;

function TQA.IDText(ID: Integer): string;
var
  I: Integer;
begin
  Result := '<N/A>';
  if ID < Length(FPoints) then
  begin
    if SupportPoints(FWay) then
    begin
      I := IDIndex(ID, FPoints);
      if I <> -1 then
        Result := FPoints[I].S;
    end;
  end else
    if awCreate in FWay then
    begin
      Dec(ID, Length(FPoints));
      if ID < Length(FImmediate) then
        Result := FImmediate[ID];
    end;
end;

procedure TQA.MixUpIDs;
var
  Len, I, ID: Integer;
  MixTable: IntArray;
begin
  Len := Length(FPoints);
  MixTable := RandomOrder(Len);
  for I := 0 to Len - 1 do
    with FPoints[I] do
      ID := MixTable[ID];
  for I := 0 to High(FAnswer) do
  begin
    ID := FAnswer[I];
    if ID < Len then
      FAnswer[I] := MixTable[ID];
  end;
end;

procedure TQA.MixUpPoints;
var
  Len, I: Integer;
  MixTable: IntArray;
  A: IDStrArray;
begin
  Len := Length(FPoints);
  MixTable := RandomOrder(Len);
  SetLength(A, Len);
  for I := 0 to Len - 1 do
    A[I] := FPoints[MixTable[I]];
  FPoints := A;
  Changed([qapPoints, qapSelection]);
end;

procedure TQA.PrepareForEdit;
begin
  SaveAnswer;
end;

procedure TQA.ResetAnswer;
var
  Len, I: Integer;
  MixTable: IntArray;
begin
  FImmediate := nil;
  if UnconditionalPoints(FWay) then
  begin
    Len := Length(FPoints);
    SetLength(FAnswer, Len);
    MixTable := RandomOrder(Len);
    for I := 0 to Len - 1 do
      FAnswer[I] := FPoints[MixTable[I]].ID;
  end else
    FAnswer := nil;
  FComplete := False;
  Changed([qapImmediate, qapAnswer, qapComplete]);
  MixTable := nil;
end;

procedure TQA.RestoreAnswer;
var
  Len, PC, IC, AC, I, J: Integer;
  S: string;
begin
  Len := Length(FSavedAnswer);
  SetLength(FAnswer, Len);
  SetLength(FImmediate, Len);
  PC := Length(FPoints);
  IC := 0;
  AC := 0;
  for I := 0 to Len - 1 do
  begin
    S := FSavedAnswer[I];
    J := StrIndex(S, FPoints);
    if J <> -1 then
    begin
      FAnswer[AC] := FPoints[J].ID;
      Inc(AC);
    end else
      if awCreate in FWay then
      begin
        FImmediate[IC] := S;
        FAnswer[AC] := PC + IC;
        Inc(IC);
        Inc(AC);
      end;
  end;
  SetLength(FAnswer, AC);
  SetLength(FImmediate, IC);
  CompleteAnswer;
end;

procedure TQA.SaveAnswer;
begin
  FSavedAnswer := GetAnswer;
end;

procedure TQA.SetAnswer(const Value: StrArray);
var
  AC, I, ID: Integer;
begin
  AC := 0;
  for I := 0 to High(Value) do
  begin
    ID := TextID(Value[I]);
    if ID <> -1 then
      AddInt(FAnswer, ID, AC);
  end;
  SetLength(FAnswer, AC);
  CompleteAnswer;
  SaveAnswer;
  Changed([qapAnswer]);
end;

procedure TQA.SetComplete(const Value: Boolean);
begin
  FComplete := Value;
  Changed([qapComplete]);
end;

procedure TQA.SetID(const Value: Integer);
begin
  FID := Value;
  Changed([qapID]);
end;

procedure TQA.SetImage(const Value: TQAImage);
begin
  if FImage <> nil then
    FImage.Free;
  FImage := Value;
  Changed([qapImage]);
end;

procedure TQA.SetImmediate(const Value: StrArray);
var
  Temp: StrArray;
  SC, AC, IC, I, J, ID, PC, AC1: Integer;
  Change: TQAProps;
  S: string;
begin
  if awCreate in FWay then
  begin
    Temp := Copy(FImmediate, 0, MaxInt);
    if (awSelect in FWay) and (FAutoSelection <> nil) then
      RemoveInts(FAnswer, FAutoSelection);
    SC := 0;
    IC := 0;
    AC := Length(FAnswer);
    Change := [qapImmediate, qapAnswer];
    for I := 0 to High(Value) do
    begin
      S := FilterStr(Value[I]);
      if S <> '' then
      begin
        J := StrIndex(S, FPoints);
        if J <> -1 then
        begin
          if awSelect in FWay then
          begin
            ID := FPoints[J].ID;
            if AddInt(FAnswer, ID, AC) then
            begin
              AddInt(FAutoSelection, ID, SC);
              Include(Change, qapSelection);
            end;
          end;
        end else
          AddStr(FImmediate, S, IC);
      end;
    end;
    SetLength(FAutoSelection, SC);
    SetLength(FImmediate, IC);
    PC := Length(FPoints);
    AC1 := 0;
    for I := 0 to AC - 1 do
    begin
      ID := FAnswer[I];
      if ID >= PC then
      begin
        ID := StrIndex(Temp[ID - PC], FImmediate);
        if ID <> -1 then
          Inc(ID, PC);
      end;
      if ID <> -1 then
      begin
        FAnswer[AC1] := ID;
        Inc(AC1);
      end;
    end;
    SetLength(FAnswer, AC1);
    CompleteAnswer;
    SaveAnswer;
    Changed(Change);
  end;
  Temp := nil;
end;

procedure TQA.SetPoints(const Value: StrArray);
var
  PC, I: Integer;
  IDs: IntArray;
begin
  if SupportPoints(FWay) then
  begin
    PC := 0;
    for I := 0 to High(Value) do
      AddStr(FPoints, FilterStr(Value[I]), PC);
    SetLength(FPoints, PC);
    IDs := RandomOrder(PC);
    for I := 0 to PC - 1 do
      FPoints[I].ID := IDs[I];
    RestoreAnswer;
    Changed([qapPoints, qapImmediate, qapAnswer, qapSelection]);
  end;
  IDs := nil;
end;

procedure TQA.SetSelection(const Value: BoolArray);
var
  AC, DC, Len, I, ID: Integer;
  Del: IntArray;
begin
  if awSelect in FWay then
  begin
    AC := Length(FAnswer);
    DC := 0;
    Len := Length(Value);
    for I := 0 to High(FPoints) do
    begin
      ID := FPoints[I].ID;
      if (I < Len) and Value[I] then
        AddInt(FAnswer, ID, AC)
      else
        AddInt(Del, ID, DC);
    end;
    SetLength(FAnswer, AC);
    SetLength(Del, DC);
    RemoveInts(FAnswer, Del);
    SaveAnswer;
    Changed([qapSelection, qapAnswer]);
  end;
end;

procedure TQA.SetText(const Value: string);
begin
  FText := FilterStr(Value);
  UpdateIsRelate;
  Changed([qapText]);
end;

procedure TQA.SetWay(const Value: TAnswerWays);
var
  Change: TQAProps;
begin
  if Value <> FWay then
  begin
    Change := [qapWay];
    if FPoints <> nil then
    begin
      if not SupportPoints(Value) then
      begin
        Change := Change + [qapPoints, qapAnswer];
        if awCreate in Value then
          Include(Change, qapImmediate);
        FPoints := nil;
      end else
        if (awSelect in FWay) <> (awSelect in Value) then
        begin
          Include(Change, qapAnswer);
          if (awSelect in Value) then
            Include(Change, qapSelection);
        end;
    end;
    if (FImmediate <> nil) <> (awCreate in Value) then
    begin
      Change := Change + [qapImmediate, qapAnswer];
      FImmediate := nil;
    end;
    FWay := Value;
    UpdateIsRelate;
    RestoreAnswer;
    Changed(Change);
  end;
end;

function TQA.TextID(const Text: string): Integer;
begin
  Result := StrIndex(Text, FPoints);
  if Result <> -1 then
    Result := FPoints[Result].ID
  else
    if awCreate in FWay then
    begin
      Result := StrIndex(Text, FImmediate);
      if Result <> -1 then
        Inc(Result, Length(FPoints));
    end;
end;

procedure TQA.UpdateIsRelate;
var
  I, J: Integer;
begin
  if awPlace in FWay then
  begin
    Exclude(FWay, awRelate);
    I := Pos('1) ', FText);
    J := Pos(' 2) ', FText);
    if (I > 0) and (J > I) then
      Include(FWay, awRelate);
  end;
end;

{ TQAList }

function TQAList.AllComplete: Boolean;
begin
  Result := IncompleteCount = 0;
end;

procedure TQAList.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TQAList.CreateFromFile(const FileName: string);
var
  Stream: TStream;
  CurrentDir: string;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if SameText(ExtractFileExt(FileName), SQAClass) then
      CreateFromStream(Stream)
    else
    if SameText(ExtractFileExt(FileName), STextClass) then
    begin
      CurrentDir := GetCurrentDir;
      try
        SetCurrentDir(ExtractFilePath(ExpandFileName(FileName)));
        CreateFromTextStream(Stream);
      finally
        SetCurrentDir(CurrentDir);
      end;
    end;
  finally
    Stream.Free;
  end;
end;

constructor TQAList.CreateFromStream(Stream: TStream);
var
  Signature: LongWord;
  I, J: Integer;
  ImageHdr: string;
{}OldFormat: Boolean;
begin
  with TReader.Create(Stream, $1000) do
    try
      Read(Signature, SizeOf(Signature));
{}OldFormat := Signature = LongWord(OldSignature);
{}if not OldFormat then
      if Signature <> LongWord(QASignature) then
        raise EReadError.CreateRes(@SInvalidImage);
      FID := ReadInteger;
      FTitle := ReadString;
      Capacity := ReadInteger;
      for I := 0 to Capacity - 1 do
        with TQA.Create(Self) do
        begin
          FID := ReadInteger;
          FText := ReadString;
{}if OldFormat then
{}begin
{}  if ReadBoolean then
{}  begin
{}    FImage := TQAImage.Create;
{}    FImage.FFmtName := ReadString;
{}    DefineBinaryProperty('', FImage.LoadFromStream, nil, True);
{}    ReadString;
{}  end
{}end else
{}begin
          ImageHdr := ReadStr;
          if ImageHdr <> '' then
            if StreamImages then
            begin
              FImage := TQAImage.Create;
              with FImage do
              begin
                DefineBinaryProperty('', LoadFromStream, nil, True);
                FFmtName := ImageHdr;
              end;
            end
            else
              SkipValue;
{}end;
          FWay := TAnswerWays(Byte(ReadInteger));
          SetLength(FPoints, ReadInteger);
          for J := 0 to High(FPoints) do
            with FPoints[J] do
            begin
              ID := ReadInteger;
              S := ReadString;
            end;
          SetLength(FImmediate, ReadInteger);
          for J := 0 to High(FImmediate) do
            FImmediate[J] := ReadString;
          SetLength(FAnswer, ReadInteger);
          for J := 0 to High(FAnswer) do
            FAnswer[J] := ReadInteger;
          FComplete := ReadBoolean;
        end;
      FAuthor := ReadString;
      FRefFile := ReadString;
    finally
      Free;
    end;
end;

constructor TQAList.CreateFromTextStream(Stream: TStream);
var
  TextReader: TTextReader;
  ErrorCount, Len, I, EC, PC, IC, AC: Integer;
  AtBegin, Success, OuterLoop: Boolean;
  S, P, T: PChar;
  C: Char;

  procedure SkipBlanks;
  begin
    while (P < T) and (P^ <= ' ') do
      Inc(P);
  end;

  function TerminateStr: Boolean;
  begin
    T := S + Len - 1;
    while (T >= S) and (T <= ' ') do
      Dec(T);
    Inc(T);
    T^ := #0;          //
    Result := S < T;
    P := S;
    SkipBlanks;
    S := P;
  end;

  function GetStr: string;
  var
    Temp: string;
  begin
    SetString(Temp, P, T - P);
    Result := FilterStr(Temp);
  end;

begin
  TextReader := TTextReader.Create(Stream{}.Read{});
  try
    ErrorCount := 0;
    AtBegin := True;
    while TextReader.Read(S, Len) do
      if TerminateStr then
      begin
        repeat
          Success := False;
          OuterLoop := True;
          case S^ of
            '0'..'9':
              begin
                while P^ in ['0'..'9', 'x'] do
                  Inc(P);
                if P^ <> '.' then
                  Break;
                P^ := #0;
                Val(S, I, EC);
                if EC <> 0 then
                  Break;
                SkipBlanks;
                if (P = T) or (T - P > 1024) then
                  Break;
                Success := True;
                if QA[I] <> nil then
                  I := UniqueID;
                with TQA.Create(Self) do
                begin
                  FID := I;
                  FText := GetStr;
                  PC := 0;
                  IC := 0;
                  AC := 0;
                  while TextReader.Read(S, Len) do
                    if TerminateStr then
                    begin
                      C := P^;
                      Inc(P);
                      SkipBlanks;
                      case C of
                        ImageLinkSign:
                          if (T - P <= MAX_PATH) and (FImage = nil) and
                            StreamImages
                          then
                            try
                              FImage := TQAImage.CreateFromFile(GetStr);
                            except
                            end;
                        SelectedSign, UnselectedSign, CreateSign, '>', PointSign:
                          begin
                            case C of
                              SelectedSign, UnselectedSign: Include(FWay, awSelect);
                              CreateSign, '>': Include(FWay, awCreate);
                              PointSign: Include(FWay, awPlace);
                            end;
                            if {C <> }not (C in [CreateSign, '>']) then
                            begin
                              I := PC;
                              AddStr(FPoints, GetStr, PC);
                            end else
                            begin
                              AddStr(FImmediate, GetStr, IC);
                              I := -IC;
                            end;
                            if C <> UnselectedSign then
                              AddInt(FAnswer, I, AC);
                          end;
                      else
                        P := S;
                        OuterLoop := False;
                        Break;
                      end;
                    end
                    else
                      if FWay <> [] then Break;
                  UpdateIsRelate;
                  SetLength(FPoints, PC);
                  SetLength(FImmediate, IC);
                  SetLength(FAnswer, AC);
                  for I := 0 to High(FAnswer) do
                    if FAnswer[I] < 0 then
                    begin
                      FAnswer[I] := PC;
                      Inc(PC);
                    end;
                  RandSeed := FID * FID;
                  MixUpIDs;
                end;
                AtBegin := False;
              end;
            '#':
              if AtBegin and (FID = 0) then
              begin
                Inc(P);
                SkipBlanks;
                Val(P, I, EC);
                if (EC = 0) and (I > 0) then
                begin
                  FID := I;
                  Success := True;
                end;
              end;
            '@':
              if FAuthor = '' then
              begin
                Inc(P);
                SkipBlanks;
                if (P < T) and (T - P <= $80) then
                begin
                  FAuthor := GetStr;
                  Success := True;
                end;
              end;
            '[', '+', '>': if T - P <=10 then Success := True;
          else
            if AtBegin and (FTitle = '') and (T - P <= $80) then
            begin
              FTitle := GetStr;
              Success := True;
            end;
          end;
        until OuterLoop;
        if not Success then
        begin
          Inc(ErrorCount);
          if ErrorCount > Count + 10 then
            raise Exception.Create('');
        end;
      end;
  finally
    TextReader.Free;
  end;
end;

destructor TQAList.Destroy;
begin
  FReference.Free;
  inherited;
end;

function TQAList.GetItem(Index: Integer): TQA;
begin
  Result := inherited Items[Index];
end;

function TQAList.GetQA(ID: Integer): TQA;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Get(I);
    if Result.FID = ID then
      Exit;
  end;
  Result := nil;
end;

function TQAList.GetReference: TQAList;
begin
  if FReference = nil then
  begin
    StreamImages := False;
    try
      FReference := TQAList.CreateFromFile(RefsPath + FRefFile);
    finally
      StreamImages := True;
    end;
  end;
  Result := FReference;
end;

function TQAList.GetSolved: Real;
var
  I, J: Integer;
  F: Real;
begin
  F := 0.0;
  J := 0;
  for I := 0 to Count - 1 do
    if Items[I].FComplete then
    begin
      F := F + Items[I].GetSolved;
      Inc(J);
    end;
  if J > 0 then
    Result := F / J
  else
    Result := 1.0;
end;

function TQAList.IncompleteCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    if not Items[I].FComplete then
      Inc(Result);
end;

procedure TQAList.Notification(Item: TItem; Action: TItemAction);
var
  I, J: integer;
begin
  if Action = iaAdded then
    with TQA(Item) do
    if Owner <> self then
    begin
      J := IndexOf(Item);
      for I := 0 to Count - 1 do
        if (I <> J) and (ID = Items[I].ID) then
        begin
          {!}FID := UniqueID; // very slow
          Break;
        end;
    end;            

  inherited;
  Changed;
end;

procedure TQAList.SaveToFile(const FileName: string);
var
  Stream: TStream;
  CurrentDir: string;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    if LowerCase(ExtractFileExt(FileName)) = SQAClass then
      SaveToStream(Stream)
    else
    begin
      CurrentDir := GetCurrentDir;
      try
        SetCurrentDir(ExtractFilePath(ExpandFileName(FileName)));
        SaveToTextStream(Stream);
      finally
        SetCurrentDir(CurrentDir);
      end;
    end;
  finally
    Stream.Free;
  end;
end;

procedure TQAList.SaveToStream(Stream: TStream);
var
  I, J: Integer;
begin
  with TWriter.Create(Stream, $1000) do
    try
      Write(QASignature, SizeOf(QASignature));
      WriteInteger(FID);
      WriteString(FTitle);
      WriteInteger(Count);
      for I := 0 to Count - 1 do
        with Items[I] do
        begin
          WriteInteger(FID);
          WriteString(FText);
          if (FImage <> nil) and StreamImages then
            with FImage do
              DefineBinaryProperty(FFmtName, nil, SaveToStream, True)
          else
            WriteListEnd;
          WriteInteger(Byte(FWay));
          WriteInteger(Length(FPoints));
          for J := 0 to High(FPoints) do
            with FPoints[J] do
            begin
              WriteInteger(ID);
              WriteString(S);
            end;
          WriteInteger(Length(FImmediate));
          for J := 0 to High(FImmediate) do
            WriteString(FImmediate[J]);
          WriteInteger(Length(FAnswer));
          for J := 0 to High(FAnswer) do
            WriteInteger(FAnswer[J]);
          WriteBoolean(FComplete);
        end;
      WriteString(FAuthor);
      WriteString(FRefFile);
    finally
      Free;
    end;
end;

resourcestring
  SID = '  # %d' + EOL;
  STitle = '  %s' + EOL;
  SQuestion = '%d. %s' + EOL;
  SImageFileName  = 'quest_%d%s';
  SImageLink = ImageLinkSign + '%s' + EOL;
  SPoint = ' %s %s' + EOL;
  SFrom = '  @ %s' + EOL;


procedure TQAList.SaveToTextStream(Stream: TStream);
var
  Writer: TWriter;

  procedure WriteFmt(const Format: string; const Args: array of const);
  var
    Buffer: array[0..4095] of Char;
  begin
    Writer.Write(Buffer, FormatBuf(Buffer, SizeOf(Buffer), Pointer(Format)^,
      Length(Format), Args));
  end;

const
  WaySigns: array[awSelect..awPlace] of Char = (SelectedSign, CreateSign, PointSign);
  PointSigns: array[Boolean] of Char = (SelectedSign, PointSign);
var
  I, J, A: Integer;
  C: Char;
  TempStr: string;
  PointIncl: array[Boolean] of Boolean;
  W: TAnswerWay; WriteWay: TAnswerWays;
begin
  Writer := TWriter.Create(Stream, 1 shl 12);
  try
    if FID <> 0 then
      WriteFmt(SID, [FID]);
    if FTitle <> '' then
      WriteFmt(STitle, [FTitle]);
    if (FID <> 0) or (FTitle <> '') then
      WriteFmt(EOL, []);
    for I := 0 to Count - 1 do
      with Items[I] do
      begin
        WriteFmt(SQuestion, [FID, FText]);
        if (FImage <> nil) and StreamImages then
          try
            TempStr := Format(SImageFileName, [FID, FImage.Ext]);
            WriteFmt(SImageLink, [TempStr]);
            FImage.SaveToFile(TempStr);
          except
          end;
        FillChar(PointIncl, SizeOf(PointIncl), False);
        for J := 0 to High(FPoints) do
          PointIncl[IntIndex(FPoints[J].ID, FAnswer) <> -1] := True;
        WriteWay := FWay;
        if (PointIncl[True] and not (awPlace in FWay)) or PointIncl[False] then
          Exclude(WriteWay, awSelect);
        if FImmediate <> nil then
          Exclude(WriteWay, awCreate);
        if PointIncl[True] then
          Exclude(WriteWay, awPlace);
        for W := awSelect to awPlace do
          if W in WriteWay then
            WriteFmt(SPoint, [WaySigns[W], '']);
        for J := 0 to High(FAnswer) do
        begin
          A := FAnswer[J];
          if A < Length(FPoints) then
            C := PointSigns[awPlace in FWay]
          else
            C := CreateSign;
          WriteFmt(SPoint, [C, IDText(A)]);
        end;
        if awSelect in FWay then
          for J := 0 to High(FPoints) do
            with FPoints[J] do
              if IntIndex(ID, FAnswer) = -1 then
                WriteFmt(SPoint, [UnselectedSign, S]);
        WriteFmt(EOL, []);
      end;
    WriteFmt(EOL, []);
    if FAuthor <> '' then
      WriteFmt(SFrom, [FAuthor]);
  finally
    Writer.Free;
  end;
end;

procedure TQAList.SetQuestions(ACount: Integer);
var
  Base: TQAList;
  Indexes: IntArray;
  Temp: TQA;
  I: Integer;
begin
  Base := TQAList.CreateFromFile(RefsPath + FRefFile);
  try
    if ACount > Base.Count then
      ACount := Base.Count;
    Indexes := RandomOrder(Base.Count, ACount);
    Clear;
    Capacity := ACount;
    for I := 0 to ACount - 1 do
    begin
      Temp := Base[Indexes[I]];
      Temp.ResetAnswer;
      Temp.MixUpPoints;
      Add(Temp);
    end;
  finally
    Base.Free;
  end;
end;

procedure TQAList.SetSource(const Value: string);
begin
  FAuthor := Value;
  Changed;
end;

procedure TQAList.SetTitle(const Value: string);
begin
  FTitle := Value;
  Changed;
end;

threadvar
  SortByProp: TQAProp;
  SortDescending: Boolean;

procedure TQAList.Sort(SortProp: TQAProp; Descending: Boolean);

  function Compare(Item1, Item2: TQA): Integer;
  begin
    case SortByProp of
      qapID: Result := Item1.ID - Item2.ID;
      qapText: Result := AnsiCompareText(Item1.Text, Item2.Text);
      qapImage: Result := AnsiCompareText(Item1.Image.Info, Item2.Image.Info);
      qapWay: Result := Byte(Item1.Way) - Byte(Item2.Way);
      qapPoints: Result := Length(Item1.FPoints) - Length(Item2.FPoints);
      qapComplete: Result := Ord(Item1.Complete) - Ord(Item2.Complete);
    else
      Result := 0;
    end;
    if SortDescending then
      Result := -Result;
  end;

begin
  SortByProp := SortProp;
  SortDescending := Descending;
  inherited Sort(@Compare);
end;

function TQAList.UniqueID: Integer;

  function CompareIDs(Item1, Item2: TQA): Integer;
  begin
    Result := Item1.ID - Item2.ID;
  end;

var
  I, J: Integer;
begin
  with TList.Create do
    try
      Assign(Self);
      Sort(@CompareIDs);
      I := 0;
      while (I < Count) and (TQA(Items[I]).ID < 0) do
        Inc(I);
      Result := 0;  
      for I := I to Count - 1 do
      begin
        J := TQA(Items[I]).ID;
        if J > Result then
          Exit;
        Result := J + 1;
      end;
    finally
      Free;
    end;
end;

initialization
  RefsPath := ExtractFilePath(Paramstr(0)) + 'Books\';

end.
