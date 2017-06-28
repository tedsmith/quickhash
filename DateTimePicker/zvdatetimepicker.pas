{
TZVDateTimePicker control for Lazarus
- - - - - - - - - - - - - - - - - - -
Author: Zoran Vučenović, January and February 2010
        Зоран Вученовић, јануар и фебруар 2010.

   This unit is part of ZVDateTimeCtrls package for Lazarus.

   Delphi's Visual Component Library (VCL) has a control named TDateTimePicker,
which I find very useful for editing dates. Lazarus Component Library (LCL),
however, does not have this control, because VCL wraps native Windows control
and it seems that such control does not exist on other platforms. Given that
LCL is designed to be platform independent, it could not use native Win control.
   Instead, for editing dates LCL has a control named TDateEdit, but I prefer
the VCL's TDateTimePicker.
   Therefore, I tried to create a custom control which would resemble VCL's
TDateTimePicker as much as possible, but not to rely on native Windows control.

   This TZVDateTimePicker control does not use native Win control. It has been
tested on Windows with win32/64 and qt widgetsets, as well as on Linux with
qt and gtk2 widgetsets.

-----------------------------------------------------------
LICENCE
- - - -
   Modified LGPL -- see the file COPYING.modifiedLGPL.

-----------------------------------------------------------
NO WARRANTY
- - - - - -
   There is no warranty whatsoever.

-----------------------------------------------------------
BEST REGARDS TO LAZARUS COMMUNITY!
- - - - - - - - - - - - - - - - - -
   I do hope this control will be useful.
}
unit ZVDateTimePicker;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef unix}
  clocale, // needed to initialize default locale settings on Linux.
  {$endif}
  Classes, SysUtils, Controls, LCLType, Graphics, Math, StdCtrls, Buttons,
  ExtCtrls, Forms, ComCtrls, Types, LMessages, LazUTF8, CalendarControlWrapper,
  LCLVersion;

const
{ With future Lazarus 1.4, use DateTimeCtrls package: }
{$if lcl_fullversion >= 01040000}
{$Hint From Lazarus version 1.4, package ZVDateTimeCtrls should be replaced by
 package DateTimeCtrls, which is included in Lazarus distribution.}
{$endif}

  { We will deal with the NullDate value the special way. It will be especially
    useful for dealing with null values from database. }
  NullDate = TDateTime(Math.MaxDouble);

  { The biggest date a user can enter. }
  TheBiggestDate = TDateTime(2958465.0); // 31. dec. 9999.

  { The smallest date a user can enter.
    Note:
      TCalendar does not accept smaller dates then 14. sep. 1752 on Windows OS
      (see the implementation of TCustomCalendar.SetDateTime).
      In Delphi help it is documented that Windows controls act weird with dates
      older than 24. sep. 1752. Actually, TCalendar control has problems to show
      dates before 1. okt. 1752. (try putting one calendar on the form, run the
      application and see what september 1752. looks like).
      Let's behave uniformely as much as
      possible -- we won't allow dates before 1. okt. 1752. on any OS (who cares
      about those).
      So, this will be the down limit:  }
  TheSmallestDate = TDateTime(-53780.0); // 1. okt. 1752.

var
  DefaultCalendarWrapperClass: TCalendarControlWrapperClass = nil;

type
  TYMD = record
    Year, Month, Day: Word;
  end;

  THMSMs = record
    Hour, Minute, Second, MiliSec: Word;
  end;

  { Used by DateDisplayOrder property to determine the order to display date
    parts -- d-m-y, m-d-y or y-m-d.
    When ddoTryDefault is set, the actual order is determined from
    ShortDateFormat global variable -- see coments above
    AdjustEffectiveHideDateTimeParts procedure }
  TDateDisplayOrder = (ddoDMY, ddoMDY, ddoYMD, ddoTryDefault);

  TTimeDisplay = (tdHM,   // hour and minute
                  tdHMS,  // hour, minute and second
                  tdHMSMs // hour, minute, second and milisecond
                  );

  TTimeFormat = (tf12, // 12 hours format, with am/pm string
                 tf24  // 24 hours format
                 );

  { TDateTimeKind determines if we should display date, time or both: }
  TDateTimeKind = (dtkDate, dtkTime, dtkDateTime);

  TTextPart = 1..8;
  TDateTimePart = (dtpDay, dtpMonth, dtpYear, dtpHour, dtpMinute,
                                dtpSecond, dtpMiliSec, dtpAMPM);
  TDateTimeParts = set of dtpDay..dtpMiliSec; // without AMPM,
           // because this set type is used for HideDateTimeParts property,
           // where hiding of AMPM part is tied to hiding of hour (and, of
           // course, it makes a difference only when TimeFormat is set to tf12)

  TArrowShape = (asClassicSmaller, asClassicLarger, asModernSmaller,
                                           asModernLarger, asYetAnotherShape);

  TDTDateMode = (dmComboBox, dmUpDown, dmNone);

  { TCustomZVDateTimePicker }

  TCustomZVDateTimePicker = class(TCustomControl)
  private
    FAutoAdvance: Boolean;
    FAutoButtonSize: Boolean;
    FCalendarWrapperClass: TCalendarControlWrapperClass;
    FCascade: Boolean;
    FCenturyFrom, FEffectiveCenturyFrom: Word;
    FDateDisplayOrder: TDateDisplayOrder;
    FHideDateTimeParts: TDateTimeParts;
    FEffectiveHideDateTimeParts: set of TDateTimePart;
    FKind: TDateTimeKind;
    FLeadingZeros: Boolean;
    FMonthNames: String;
    FMonthNamesArray: TMonthNameArray;
    FNullInputAllowed: Boolean;
    FDateTime: TDateTime;
    FDateSeparator: String;
    FReadOnly: Boolean;
    FMaxDate, FMinDate: TDate;
    FShowMonthNames: Boolean;
    FTextForNullDate: String;
    FTimeSeparator: String;
    FTimeDisplay: TTimeDisplay;
    FTimeFormat: TTimeFormat;
    FTrailingSeparator: Boolean;
    FUseDefaultSeparators: Boolean;
    FUserChangedText: Boolean;
    FYearPos, FDayPos, FMonthPos: 1..3;
    FTextPart: array[1..3] of String;
    FTimeText: array[dtpHour..dtpAMPM] of String;
    FUserChanging: Integer;
    FDigitWidth: Integer;
    FTextHeight: Integer;
    FSeparatorWidth: Integer;
    FSepNoSpaceWidth: Integer;
    FTimeSeparatorWidth: Integer;
    FMonthWidth: Integer;
    FNullMonthText: String;
    FSelectedTextPart: TTextPart;
    FRecalculatingTextSizesNeeded: Boolean;
    FJumpMinMax: Boolean;
    FAMPMWidth: Integer;
    FDateWidth: Integer;
    FTimeWidth: Integer;
    FTextWidth: Integer;
    FArrowShape: TArrowShape;
    FDateMode: TDTDateMode;
    FTextEnabled: Boolean;
    FCheckBox: TCheckBox;
    FUpDown: TCustomUpDown;
    FOnChange: TNotifyEvent;
    FOnCheckBoxChange: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FEffectiveDateDisplayOrder: TDateDisplayOrder;

    FArrowButton: TCustomSpeedButton;
    FCalendarForm: TCustomForm;
    FDoNotArrangeControls: Boolean;
    FConfirmedDateTime: TDateTime;
    FNoEditingDone: Integer;
    FAllowDroppingCalendar: Boolean;
    FChangeInRecursiveCall: Boolean;

    function AreSeparatorsStored: Boolean;
    function GetChecked: Boolean;
    function GetDate: TDate;
    function GetDateTime: TDateTime;
    function GetShowCheckBox: Boolean;
    function GetTime: TTime;
    procedure SetArrowShape(const AValue: TArrowShape);
    procedure SetAutoButtonSize(AValue: Boolean);
    procedure SetCalendarWrapperClass(AValue: TCalendarControlWrapperClass);
    procedure SetCenturyFrom(const AValue: Word);
    procedure SetChecked(const AValue: Boolean);
    procedure CheckTextEnabled;
    procedure SetDateDisplayOrder(const AValue: TDateDisplayOrder);
    procedure SetDateMode(const AValue: TDTDateMode);
    procedure SetHideDateTimeParts(AValue: TDateTimeParts);
    procedure SetKind(const AValue: TDateTimeKind);
    procedure SetLeadingZeros(const AValue: Boolean);
    procedure SetMonthNames(AValue: String);
    procedure SetNullInputAllowed(const AValue: Boolean);
    procedure SetDate(const AValue: TDate);
    procedure SetDateTime(const AValue: TDateTime);
    procedure SetDateSeparator(const AValue: String);
    procedure SetMaxDate(const AValue: TDate);
    procedure SetMinDate(const AValue: TDate);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetShowCheckBox(const AValue: Boolean);
    procedure SetShowMonthNames(AValue: Boolean);
    procedure SetTextForNullDate(const AValue: String);
    procedure SetTime(const AValue: TTime);
    procedure SetTimeSeparator(const AValue: String);
    procedure SetTimeDisplay(const AValue: TTimeDisplay);
    procedure SetTimeFormat(const AValue: TTimeFormat);
    procedure SetTrailingSeparator(const AValue: Boolean);
    procedure SetUseDefaultSeparators(const AValue: Boolean);

    function GetHour: Word;
    function GetMiliSec: Word;
    function GetMinute: Word;
    function GetSecond: Word;
    procedure RecalculateTextSizesIfNeeded;
    function GetDay: Word;
    function GetMonth: Word;
    function GetYear: Word;
    function GetHMSMs(const NowIfNull: Boolean = False): THMSMs;
    function GetYYYYMMDD(const TodayIfNull: Boolean = False): TYMD;
    procedure SetHour(const AValue: Word);
    procedure SetMiliSec(const AValue: Word);
    procedure SetMinute(const AValue: Word);
    procedure SetSecond(const AValue: Word);
    procedure SetSeparators(const DateSep, TimeSep: String);
    procedure SetDay(const AValue: Word);
    procedure SetMonth(const AValue: Word);
    procedure SetYear(const AValue: Word);
    procedure SetYYYYMMDD(const AValue: TYMD);
    procedure SetHMSMs(const AValue: THMSMs);
    procedure UpdateIfUserChangedText;
    function GetSelectedText: String;
    procedure AdjustEffectiveCenturyFrom;
    procedure AdjustEffectiveDateDisplayOrder;
    procedure AdjustEffectiveHideDateTimeParts;
    procedure SelectDateTimePart(const DateTimePart: TDateTimePart);
    procedure MoveSelectionLR(const ToLeft: Boolean);
    procedure DestroyCalendarForm;
    procedure DropDownCalendarForm;
    procedure UpdateShowArrowButton;
    procedure DestroyUpDown;
    procedure DestroyArrowBtn;
    procedure ArrowMouseDown(Sender: TObject; Button: TMouseButton;
                                            Shift: TShiftState; X, Y: Integer);
    procedure UpDownClick(Sender: TObject; Button: TUDBtnType);
    procedure CheckBoxChange(Sender: TObject);
    procedure SetFocusIfPossible;
    procedure AutoResizeButton;

  protected
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
    procedure WMSize(var Message: TLMSize); message LM_SIZE;

    class function GetControlClassDefaultSize: TSize; override;

    procedure ConfirmChanges; virtual;
    procedure UndoChanges; virtual;

    function GetDateTimePartFromTextPart(TextPart: TTextPart): TDateTimePart;
    function GetSelectedDateTimePart: TDateTimePart;
    procedure FontChanged(Sender: TObject); override;
    function GetTextOrigin(IgnoreRightToLeft: Boolean = False): TPoint;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: char); override;
    procedure SelectTextPartUnderMouse(XMouse: Integer);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    procedure UpdateDate; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Click; override;
    procedure DblClick; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure SetBiDiMode(AValue: TBiDiMode); override;

    procedure IncreaseCurrentTextPart;
    procedure DecreaseCurrentTextPart;
    procedure IncreaseMonth;
    procedure IncreaseYear;
    procedure IncreaseDay;
    procedure DecreaseMonth;
    procedure DecreaseYear;
    procedure DecreaseDay;
    procedure IncreaseHour;
    procedure IncreaseMinute;
    procedure IncreaseSecond;
    procedure IncreaseMiliSec;
    procedure DecreaseHour;
    procedure DecreaseMinute;
    procedure DecreaseSecond;
    procedure DecreaseMiliSec;
    procedure ChangeAMPM;

    procedure SelectDay;
    procedure SelectMonth;
    procedure SelectYear;
    procedure SelectHour;
    procedure SelectMinute;
    procedure SelectSecond;
    procedure SelectMiliSec;
    procedure SelectAMPM;

    procedure SetEnabled(Value: Boolean); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure CreateWnd; override;
    procedure SetDateTimeJumpMinMax(const AValue: TDateTime);
    procedure ArrangeCtrls; virtual;
    procedure Change; virtual;
    procedure DoDropDown; virtual;
    procedure DoCloseUp; virtual;
    procedure DrawArrowButtonGlyph; virtual;

    property BorderStyle default bsSingle;
    property AutoSize default True;
    property TabStop default True;
    property ParentColor default False;
    property CenturyFrom: Word
             read FCenturyFrom write SetCenturyFrom;
    property DateDisplayOrder: TDateDisplayOrder
             read FDateDisplayOrder write SetDateDisplayOrder default ddoTryDefault;
    property MaxDate: TDate
             read FMaxDate write SetMaxDate;
    property MinDate: TDate
             read FMinDate write SetMinDate;
    property DateTime: TDateTime read GetDateTime write SetDateTime;
    property TrailingSeparator: Boolean
             read FTrailingSeparator write SetTrailingSeparator;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property LeadingZeros: Boolean read FLeadingZeros write SetLeadingZeros;
    property TextForNullDate: String
             read FTextForNullDate write SetTextForNullDate;
    property NullInputAllowed: Boolean
             read FNullInputAllowed write SetNullInputAllowed default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCheckBoxChange: TNotifyEvent
             read FOnCheckBoxChange write FOnCheckBoxChange;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property ShowCheckBox: Boolean
             read GetShowCheckBox write SetShowCheckBox default False;
    property Checked: Boolean read GetChecked write SetChecked default True;
    property ArrowShape: TArrowShape
             read FArrowShape write SetArrowShape default asModernSmaller;
    property Kind: TDateTimeKind
             read FKind write SetKind;
    property DateSeparator: String
             read FDateSeparator write SetDateSeparator stored AreSeparatorsStored;
    property TimeSeparator: String
             read FTimeSeparator write SetTimeSeparator stored AreSeparatorsStored;
    property UseDefaultSeparators: Boolean
             read FUseDefaultSeparators write SetUseDefaultSeparators;
    property TimeFormat: TTimeFormat read FTimeFormat write SetTimeFormat;
    property TimeDisplay: TTimeDisplay read FTimeDisplay write SetTimeDisplay;
    property Time: TTime read GetTime write SetTime;
    property Date: TDate read GetDate write SetDate;
    property DateMode: TDTDateMode read FDateMode write SetDateMode;
    property Cascade: Boolean read FCascade write FCascade default False;
    property AutoButtonSize: Boolean
             read FAutoButtonSize write SetAutoButtonSize default False;
    property AutoAdvance: Boolean
             read FAutoAdvance write FAutoAdvance default True;
    property HideDateTimeParts: TDateTimeParts
             read FHideDateTimeParts write SetHideDateTimeParts;
    property CalendarWrapperClass: TCalendarControlWrapperClass
             read FCalendarWrapperClass write SetCalendarWrapperClass;
    property MonthNames: String read FMonthNames write SetMonthNames;
    property ShowMonthNames: Boolean
             read FShowMonthNames write SetShowMonthNames default False;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function DateIsNull: Boolean;
    procedure SelectDate;
    procedure SelectTime;

    procedure Paint; override;
    procedure EditingDone; override;

  published
    //
  end;

  {TZVDateTimePicker}

  TZVDateTimePicker = class(TCustomZVDateTimePicker)
  public
    property DateTime;
    property CalendarWrapperClass;
  published
    property ArrowShape;
    property ShowCheckBox;
    property Checked;
    property CenturyFrom;
    property DateDisplayOrder;
    property MaxDate;
    property MinDate;
    property ReadOnly;
    property AutoSize;
    property Font;
    property ParentFont;
    property TabOrder;
    property TabStop;
    property BorderStyle;
    property BorderSpacing;
    property Enabled;
    property Color;
    property ParentColor;
    property DateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property LeadingZeros;
    property ShowHint;
    property ParentShowHint;
    property Align;
    property Anchors;
    property Constraints;
    property Cursor;
    property PopupMenu;
    property Visible;
    property NullInputAllowed;
    property Kind;
    property TimeSeparator;
    property TimeFormat;
    property TimeDisplay;
    property DateMode;
    property Date;
    property Time;
    property UseDefaultSeparators;
    property Cascade;
    property AutoButtonSize;
    property AutoAdvance;
    property HideDateTimeParts;
    property BiDiMode;
    property ParentBiDiMode;
    property MonthNames;
    property ShowMonthNames;
// events:
    property OnChange;
    property OnCheckBoxChange;
    property OnDropDown;
    property OnCloseUp;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseLeave;
    property OnResize;
    property OnUTF8KeyPress;
  end;

function EqualDateTime(const A, B: TDateTime): Boolean;
function IsNullDate(DT: TDateTime): Boolean;

implementation

uses
  DateUtils, LCLCalendarWrapper;

function NumberOfDaysInMonth(const Month, Year: Word): Word;
begin
  Result := 0;
  if Month in [1..12] then
    Result := MonthDays[IsLeapYear(Year), Month];
end;

{ EqualDateTime
  --------------
  Returns True when two dates are equal or both are null }
function EqualDateTime(const A, B: TDateTime): Boolean;
begin
  if IsNullDate(A) then
    Result := IsNullDate(B)
  else
    Result := (not IsNullDate(B)) and (A = B);
end;

function IsNullDate(DT: TDateTime): Boolean;
begin
  Result := IsNan(DT) or IsInfinite(DT) or
            (DT > SysUtils.MaxDateTime) or (DT < SysUtils.MinDateTime);
end;

type

  { TDTCalendarForm }

  TDTCalendarForm = class(TForm)
  private
    DTPicker: TCustomZVDateTimePicker;
    Cal: TCalendarControlWrapper;
    Shape: TShape;
    RememberedCalendarFormOrigin: TPoint;
    FClosing: Boolean;
    DTPickersParentForm: TCustomForm;

    procedure SetClosingCalendarForm;
    procedure AdjustCalendarFormSize;
    procedure AdjustCalendarFormScreenPosition;
    procedure CloseCalendarForm(const AndSetTheDate: Boolean = False);

    procedure CalendarKeyDown(Sender: TObject; var Key: Word;
                                      Shift: TShiftState);
    procedure CalendarResize(Sender: TObject);
    procedure CalendarMouseUp(Sender: TObject; Button: TMouseButton;
                                      Shift: TShiftState; X, Y: Integer);
    procedure VisibleOfParentChanged(Sender: TObject);

  protected
    procedure Deactivate; override;
    procedure DoShow; override;
    procedure DoClose(var CloseAction: TCloseAction); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    constructor CreateNewDTCalendarForm(AOwner: TComponent;
                  ADTPicker: TCustomZVDateTimePicker);
    destructor Destroy; override;
  published
  end;

procedure TDTCalendarForm.SetClosingCalendarForm;
begin
  if not FClosing then begin
    FClosing := True;

    if Assigned(DTPicker) and (DTPicker.FCalendarForm = Self) then
      DTPicker.FCalendarForm := nil;

  end;
end;

procedure TDTCalendarForm.AdjustCalendarFormSize;
begin
  if not FClosing then begin
    ClientWidth := Cal.GetCalendarControl.Width + 2;
    ClientHeight := Cal.GetCalendarControl.Height + 2;

    Shape.SetBounds(0, 0, ClientWidth, ClientHeight);

    AdjustCalendarFormScreenPosition;

  end;
end;

procedure TDTCalendarForm.AdjustCalendarFormScreenPosition;
var
  M: TMonitor;
  R: TRect;
  P: TPoint;
  H, W: Integer;
begin
  H := Height;
  W := Width;

  if IsRightToLeft then
    P := DTPicker.ControlToScreen(Point(DTPicker.Width - W, DTPicker.Height))
  else
    P := DTPicker.ControlToScreen(Point(0, DTPicker.Height));

  M := Screen.MonitorFromWindow(DTPicker.Handle);

  R := M.WorkareaRect;
  // WorkareaRect sometimes is not implemented (gtk2?). Depends on widgetset
  // and window manager or something like that. Then it returns (0,0,0,0) and
  // the best we can do is use BoundsRect instead:
  if (R.Right <= R.Left) or (R.Bottom <= R.Top) then
    R := M.BoundsRect;

  if P.y > R.Bottom - H then
    P.y := P.y - H - DTPicker.Height;

  if P.y < R.Top then
    P.y := R.Top;

  if P.x > R.Right - W then
    P.x := R.Right - W;

  if P.x < R.Left then
    P.x := R.Left;

  if (P.x <> RememberedCalendarFormOrigin.x)
            or (P.y <> RememberedCalendarFormOrigin.y) then begin
    SetBounds(P.x, P.y, W, H);
    RememberedCalendarFormOrigin := P;
  end;

end;

procedure TDTCalendarForm.CloseCalendarForm(const AndSetTheDate: Boolean);
begin
  if not FClosing then
    try
      SetClosingCalendarForm;
      Visible := False;

      if Assigned(DTPicker) and DTPicker.IsVisible then begin

        if AndSetTheDate then begin
          Inc(DTPicker.FUserChanging);
          try
            if DTPicker.DateIsNull then begin
              // we'll set the time to 0.0 (midnight):
              DTPicker.SetDateTime(Int(Cal.GetDate));
            end else if not EqualDateTime(Int(DTPicker.DateTime),
                                          Int(Cal.GetDate)) then begin
              // we'll change the date, but keep the time:
              DTPicker.SetDateTime(ComposeDateTime(Cal.GetDate, DTPicker.DateTime));
            end;
          finally
            Dec(DTPicker.FUserChanging);
          end;
        end;

        if Screen.ActiveCustomForm = Self then
          DTPicker.SetFocusIfPossible;

        DTPicker.DoCloseUp;
      end;

    finally
      Release;
    end;

end;

procedure TDTCalendarForm.CalendarKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      CloseCalendarForm;

    VK_RETURN, VK_SPACE:
      CloseCalendarForm(True);

  end;
end;

procedure TDTCalendarForm.CalendarResize(Sender: TObject);
begin
  AdjustCalendarFormSize;
end;

procedure TDTCalendarForm.CalendarMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Cal.AreCoordinatesOnDate(X, Y) then
    CloseCalendarForm(True);

end;

{ This procedure is added to list of "visible change handlers" of DTPicker's
  parent form, so that hiding of DTPicker's parent form does not leave the
  calendar form visible. }
procedure TDTCalendarForm.VisibleOfParentChanged(Sender: TObject);
begin
  SetClosingCalendarForm;
  Release;
end;

procedure TDTCalendarForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (AComponent = DTPickersParentForm) and (Operation = opRemove) then
    DTPickersParentForm := nil;

end;

procedure TDTCalendarForm.Deactivate;
begin
  inherited Deactivate;

  CloseCalendarForm;
end;

procedure TDTCalendarForm.DoShow;
begin
  if not FClosing then begin
    inherited DoShow;

    AdjustCalendarFormSize;
    DTPicker.DoDropDown; // calls OnDropDown event handler
  end;
end;

procedure TDTCalendarForm.DoClose(var CloseAction: TCloseAction);
begin
  SetClosingCalendarForm;
  CloseAction := caFree;

  inherited DoClose(CloseAction);
end;

type
  { To be able to access TControl's protected members,
    we derive our class TDTControl from TControl: }
  TDTControl = class(TControl);

constructor TDTCalendarForm.CreateNewDTCalendarForm(AOwner: TComponent;
  ADTPicker: TCustomZVDateTimePicker);
var
  P: TPoint;
  CalClass: TCalendarControlWrapperClass;
begin
  inherited CreateNew(AOwner);

  ADTPicker.FAllowDroppingCalendar := False;
  FClosing := False;

  DTPicker := ADTPicker;
  BiDiMode := DTPicker.BiDiMode;
  DTPickersParentForm := GetParentForm(DTPicker);
  if Assigned(DTPickersParentForm) then begin
    DTPickersParentForm.AddHandlerOnVisibleChanged(@VisibleOfParentChanged);
    DTPickersParentForm.FreeNotification(Self);
  end;
  PopupMode := pmAuto;

  P := Point(0, 0);

  if ADTPicker.FCalendarWrapperClass = nil then begin
    if DefaultCalendarWrapperClass = nil then
      CalClass := TLCLCalendarWrapper
    else
      CalClass := DefaultCalendarWrapperClass;
  end else
    CalClass := ADTPicker.FCalendarWrapperClass;

  Cal := CalClass.Create;

  Cal.GetCalendarControl.ParentBiDiMode := True;
  Cal.GetCalendarControl.AutoSize := True;
  Cal.GetCalendarControl.GetPreferredSize(P.x, P.y);
  Cal.GetCalendarControl.Align := alNone;
  Cal.GetCalendarControl.SetBounds(1, 1, P.x, P.y);

  SetBounds(-8000, -8000, P.x + 2, P.y + 2);
  RememberedCalendarFormOrigin := Point(-8000, -8000);

  ShowInTaskBar := stNever;
  BorderStyle := bsNone;

  Shape := TShape.Create(nil);
  Shape.Brush.Style := bsClear;

  if DTPicker.DateIsNull then
    Cal.SetDate(Max(DTPicker.MinDate, Min(SysUtils.Date, DTPicker.MaxDate)))

  else if DTPicker.DateTime < DTPicker.MinDate then // These "out of bounds" values
    Cal.SetDate(DTPicker.MinDate)      // can happen when DateTime was set with
  else if DTPicker.DateTime > DTPicker.MaxDate then // "SetDateTimeJumpMinMax" protected
    Cal.SetDate(DTPicker.MaxDate)      // procedure (used in TDBZVDateTimePicker control).

  else
    Cal.SetDate(DTPicker.Date);

  Cal.GetCalendarControl.OnResize := @CalendarResize;
  TDTControl(Cal.GetCalendarControl).OnMouseUp := @CalendarMouseUp;
  if Cal.GetCalendarControl is TWinControl then begin
    TWinControl(Cal.GetCalendarControl).OnKeyDown := @CalendarKeyDown;
    TWinControl(Cal.GetCalendarControl).TabStop := True;
    TWinControl(Cal.GetCalendarControl).SetFocus;
  end;

  Shape.Parent := Self;
  Cal.GetCalendarControl.Parent := Self;
  Cal.GetCalendarControl.BringToFront;
end;

destructor TDTCalendarForm.Destroy;
begin
  SetClosingCalendarForm;
  if Assigned(DTPickersParentForm) then
    DTPickersParentForm.RemoveAllHandlersOfObject(Self);

  if Assigned(Cal) then begin
    Cal.GetCalendarControl.OnResize := nil;
    TDTControl(Cal.GetCalendarControl).OnMouseUp := nil;
    if Cal.GetCalendarControl is TWinControl then
      TWinControl(Cal.GetCalendarControl).OnKeyDown := nil;
    Cal.Free;
    Cal := nil;
  end;
  FreeAndNil(Shape);

  if Assigned(DTPicker) then begin
    if (Screen.ActiveControl = DTPicker) then
      DTPicker.Invalidate;

    if DTPicker.FCalendarForm = nil then
      DTPicker.FAllowDroppingCalendar := True;

  end;

  inherited Destroy;
end;

{ TCustomZVDateTimePicker }

procedure TCustomZVDateTimePicker.SetChecked(const AValue: Boolean);
begin
  if Assigned(FCheckBox) then
    FCheckBox.Checked := AValue;

  CheckTextEnabled;
  Invalidate;
end;

procedure TCustomZVDateTimePicker.CheckTextEnabled;
begin
  FTextEnabled := Self.Enabled and GetChecked;

  if Assigned(FArrowButton) then
    FArrowButton.Enabled := FTextEnabled;

  if Assigned(FUpDown) then
    FUpDown.Enabled := FTextEnabled;

  if Assigned(FCheckBox) then
    FCheckBox.Enabled := Self.Enabled;
end;

procedure TCustomZVDateTimePicker.SetDateDisplayOrder(
  const AValue: TDateDisplayOrder);
var
  PreviousEffectiveDDO: TDateDisplayOrder;
begin
  if FDateDisplayOrder <> AValue then begin
    PreviousEffectiveDDO := FEffectiveDateDisplayOrder;
    FDateDisplayOrder := AValue;
    AdjustEffectiveDateDisplayOrder;
    if FEffectiveDateDisplayOrder <> PreviousEffectiveDDO then
      UpdateDate;
  end;
end;

procedure TCustomZVDateTimePicker.SetDateMode(const AValue: TDTDateMode);
begin
  FDateMode := AValue;
  UpdateShowArrowButton;
end;

procedure TCustomZVDateTimePicker.SetHideDateTimeParts(AValue: TDateTimeParts);
begin
  if FHideDateTimeParts <> AValue then begin
    FHideDateTimeParts := AValue;
    AdjustEffectiveHideDateTimeParts;
  end;
end;

procedure TCustomZVDateTimePicker.SetKind(const AValue: TDateTimeKind);
begin
  if FKind <> AValue then begin
    FKind := AValue;
    AdjustEffectiveHideDateTimeParts;
  end;
end;

procedure TCustomZVDateTimePicker.SetLeadingZeros(const AValue: Boolean);
begin
  if FLeadingZeros = AValue then Exit;

  FLeadingZeros := AValue;
  UpdateDate;
end;

procedure TCustomZVDateTimePicker.SetMonthNames(AValue: String);
var
  I, N, LenMNSep: Integer;
  MonthNamesSeparator: String;

begin
  if FMonthNames <> AValue then begin
    AValue := TrimRight(AValue);
    FMonthNames := AValue;

    if UpperCase(AValue) = 'SHORT' then
      for I := Low(TMonthNameArray) to High(TMonthNameArray) do
        FMonthNamesArray[I] := AnsiToUtf8(DefaultFormatSettings.ShortMonthNames[I])
    else begin
      N := 0;
      if Length(AValue) >= 24 then begin
        MonthNamesSeparator := UTF8Copy(AValue, 1, 1);
        LenMNSep := Length(MonthNamesSeparator);
        if LenMNSep > 0 then begin
          Delete(AValue, 1, LenMNSep);

          while N < 12 do begin
            I := Pos(MonthNamesSeparator, AValue);
            if I <= 1 then begin
              if (I = 0) and (N = 11) and (Length(AValue) > 0) then begin
                Inc(N);
                FMonthNamesArray[N] := AValue;
              end;

              Break;
            end;

            Inc(N);
            Dec(I);
            FMonthNamesArray[N] := Copy(AValue, 1, I);
            Delete(AValue, 1, I + LenMNSep);
          end;

        end;
      end;

      if N < 12 then
        for I := Low(TMonthNameArray) to High(TMonthNameArray) do
          FMonthNamesArray[I] := AnsiToUtf8(DefaultFormatSettings.LongMonthNames[I]);
    end;

    if FShowMonthNames and
              not (dtpMonth in FEffectiveHideDateTimeParts) then begin
      FRecalculatingTextSizesNeeded := True;
      UpdateDate;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.SetNullInputAllowed(const AValue: Boolean);
begin
  FNullInputAllowed := AValue;
end;

procedure TCustomZVDateTimePicker.SetDate(const AValue: TDate);
begin
  if IsNullDate(AValue) then
    DateTime := NullDate
  else if DateIsNull then
    DateTime := Int(AValue)
  else
    DateTime := ComposeDateTime(AValue, FDateTime);
end;

procedure TCustomZVDateTimePicker.SetDateTime(const AValue: TDateTime);
begin
  if not EqualDateTime(AValue, FDateTime) then begin
    if IsNullDate(AValue) then
      FDateTime := NullDate
    else
      FDateTime := AValue;
  end;
  UpdateDate;
end;

procedure TCustomZVDateTimePicker.SetDateSeparator(const AValue: String);
begin
  SetSeparators(AValue, FTimeSeparator);
end;

procedure TCustomZVDateTimePicker.SetMaxDate(const AValue: TDate);
begin
  if not IsNullDate(AValue) then begin

    if AValue > TheBiggestDate then
      FMaxDate := TheBiggestDate
    else if AValue <= FMinDate then
      FMaxDate := FMinDate
    else
      FMaxDate := Int(AValue);

    if not DateIsNull then
      if FMaxDate < GetDate then
        SetDate(FMaxDate);

    AdjustEffectiveCenturyFrom;
  end;
end;

procedure TCustomZVDateTimePicker.SetMinDate(const AValue: TDate);
begin
  if not IsNullDate(AValue) then begin

    if AValue < TheSmallestDate then
      FMinDate := TheSmallestDate
    else if AValue >= FMaxDate then
      FMinDate := FMaxDate
    else
      FMinDate := Int(AValue);

    if not DateIsNull then
      if FMinDate > GetDate then
        SetDate(FMinDate);

    AdjustEffectiveCenturyFrom;
  end;
end;

procedure TCustomZVDateTimePicker.SetReadOnly(const AValue: Boolean);
begin
  if FReadOnly <> AValue then begin
    if AValue then
      ConfirmChanges;

    FReadOnly := AValue;
  end;
end;

type
  TDTCheckBox = class(TCheckBox)
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;
  end;

procedure TDTCheckBox.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  PreferredHeight := 1;
end;

procedure TCustomZVDateTimePicker.SetShowCheckBox(const AValue: Boolean);
begin
  if GetShowCheckBox <> AValue then begin
    DisableAlign;
    try
      if AValue then begin
        FCheckBox := TDTCheckBox.Create(Self);

        {$IFNDEF WINDOWS}
        { On Windows, the following line seems to not have any effect, but I
         enclosed it in IFNDEF anyway. }
        FCheckBox.Color := clBtnFace; { This line is here because of CheckBox's
         strange behavior in Linux -- when parent's colour is white, which is
         the default in our case (actually, our default is clWindow, but it's
         usually white) and when the check box is on a form shown modally, if
         we close the form and then show it again, the check box refuses to
         paint it's "checker" shape.

         I spent a lot of time trying to solve this and this is the best I
         came up with -- setting the check box's colour to clBtnFace seems to
         be a workaround.

         Nice thing is that it seems not to really effect neither the checker's
         colour on the screen, nor the colour of check box's "box", so we didn't
         actually spoil the check box's default appearence on the screen.   }
        {$ENDIF}

        FCheckBox.ControlStyle := FCheckBox.ControlStyle +
                                    [csNoFocus, csNoDesignSelectable];
        FCheckBox.AllowGrayed := False;
        FCheckBox.TabStop := False;

        FCheckBox.Checked := True;
        FCheckBox.Enabled := Self.Enabled;

        FCheckBox.Parent := Self;

        FCheckBox.OnChange := @CheckBoxChange;
      end else begin
        FCheckBox.OnChange := nil;
        FreeAndNil(FCheckBox);

      end;
      ArrangeCtrls;

    finally
      EnableAlign;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.SetShowMonthNames(AValue: Boolean);
begin
  if FShowMonthNames <> AValue then begin
    FShowMonthNames := AValue;
    if not (dtpMonth in FEffectiveHideDateTimeParts) then begin
      FRecalculatingTextSizesNeeded := True;
      UpdateDate;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.SetTextForNullDate(const AValue: String);
begin
  if FTextForNullDate = AValue then
    Exit;

  FTextForNullDate := AValue;
  if DateIsNull then
    Invalidate;
end;

procedure TCustomZVDateTimePicker.SetTime(const AValue: TTime);
begin
  if IsNullDate(AValue) then
    DateTime := NullDate
  else if DateIsNull then
    DateTime := ComposeDateTime(Max(Min(SysUtils.Date, MaxDate), MinDate), AValue)
  else
    DateTime := ComposeDateTime(FDateTime, AValue);
end;

procedure TCustomZVDateTimePicker.SetTimeSeparator(const AValue: String);
begin
  SetSeparators(FDateSeparator, AValue);
end;

procedure TCustomZVDateTimePicker.SetTimeDisplay(const AValue: TTimeDisplay);
begin
  if FTimeDisplay <> AValue then begin
    FTimeDisplay:=AValue;
    AdjustEffectiveHideDateTimeParts;
  end;
end;

procedure TCustomZVDateTimePicker.SetTimeFormat(const AValue: TTimeFormat);
begin
  if FTimeFormat <> AValue then begin
    FTimeFormat := AValue;
    AdjustEffectiveHideDateTimeParts;
  end;
end;

procedure TCustomZVDateTimePicker.SetTrailingSeparator(const AValue: Boolean);
begin
  if FTrailingSeparator <> AValue then begin
    FTrailingSeparator := AValue;
    FRecalculatingTextSizesNeeded := True;
    UpdateIfUserChangedText;
    Invalidate;
  end;
end;

procedure TCustomZVDateTimePicker.SetUseDefaultSeparators(const AValue: Boolean);
begin
  if FUseDefaultSeparators <> AValue then begin
    if AValue then begin
      SetSeparators(DefaultFormatSettings.DateSeparator,
                      DefaultFormatSettings.TimeSeparator);
        // Note that here, in SetSeparators procedure,
        // the field FUseDefaultSeparators is set to False.
    end;
    // Therefore, the following line must NOT be moved above.
    FUseDefaultSeparators := AValue;
  end;
end;

function TCustomZVDateTimePicker.GetHour: Word;
begin
  Result := GetHMSMs.Hour;
end;

function TCustomZVDateTimePicker.GetMiliSec: Word;
begin
  Result := GetHMSMs.MiliSec;
end;

function TCustomZVDateTimePicker.GetMinute: Word;
begin
  Result := GetHMSMs.Minute;
end;

function TCustomZVDateTimePicker.GetSecond: Word;
begin
  Result := GetHMSMs.Second;
end;

{ RecalculateTextSizesIfNeeded
 --------------------------------
  In this procedure we measure text and store the values in the following
  fields: FDateWidth, FTimeWidth, FTextWidth, FTextHeigth, FDigitWidth,
  FSeparatorWidth, FTimeSeparatorWidth, FSepNoSpaceWidth. These fields are used
  in calculating our preffered size and when painting.
  The procedure is called internally when needed (when properties which
  influence the appearence change). }
procedure TCustomZVDateTimePicker.RecalculateTextSizesIfNeeded;
const
  NullMonthChar = 'x';
var
  C: Char;
  N, J: Integer;
  S: String;
  I: TDateTimePart;
  DateParts, TimeParts: Integer;
begin
  if HandleAllocated and FRecalculatingTextSizesNeeded then begin
    FRecalculatingTextSizesNeeded := False;

    FDigitWidth := 0;
    for C := '0' to '9' do begin
      N := Canvas.GetTextWidth(C);
      if N > FDigitWidth then
        FDigitWidth := N;
    end;

    DateParts := 0;
    FSepNoSpaceWidth := 0;
    FSeparatorWidth := 0;
    FMonthWidth := 0;
    FDateWidth := 0;
    FNullMonthText := '';
    S := '';
    if FKind in [dtkDate, dtkDateTime] then begin

      for I := dtpDay to dtpYear do
        if not (I in FEffectiveHideDateTimeParts) then begin
          Inc(DateParts);
          if I = dtpYear then begin
            FDateWidth := FDateWidth + 4 * FDigitWidth;
          end else if (I = dtpMonth) and FShowMonthNames then begin
            FMonthWidth := FDigitWidth; // Minimal MonthWidth is DigitWidth.
            for J := Low(TMonthNameArray) to High(TMonthNameArray) do begin
              N := Canvas.GetTextWidth(FMonthNamesArray[J]);
              if N > FMonthWidth then
                FMonthWidth := N;
            end;

            N := Canvas.GetTextWidth(NullMonthChar);
            if N > 0 then begin
              N := FMonthWidth div N - 1;
              if N > 1 then
                FNullMonthText := StringOfChar(NullMonthChar, N);
            end;
            if N <= 1 then
              FNullMonthText := NullMonthChar;

            FDateWidth := FDateWidth + FMonthWidth;
          end else
            FDateWidth := FDateWidth + 2 * FDigitWidth;

        end;

      if DateParts > 0 then begin
        if FTrailingSeparator then begin
          FSepNoSpaceWidth := Canvas.GetTextWidth(TrimRight(FDateSeparator));
          Inc(FDateWidth, FSepNoSpaceWidth);
        end;

        if DateParts > 1 then begin
          FSeparatorWidth := Canvas.GetTextWidth(FDateSeparator);
          S := FDateSeparator;

          FDateWidth := FDateWidth + (DateParts - 1) * FSeparatorWidth;
        end;
      end;

    end;

    TimeParts := 0;
    FTimeWidth := 0;
    FAMPMWidth := 0;
    FTimeSeparatorWidth := 0;
    if FKind in [dtkTime, dtkDateTime] then begin

      for I := dtpHour to dtpMiliSec do
        if not (I in FEffectiveHideDateTimeParts) then begin
          Inc(TimeParts);
          if TimeParts > 1 then
            Inc(FTimeWidth, FTimeSeparatorWidth);

          if I = dtpMiliSec then
            FTimeWidth := FTimeWidth + 3 * FDigitWidth
          else
            FTimeWidth := FTimeWidth + 2 * FDigitWidth;

        end;

      if TimeParts > 1 then begin
        FTimeSeparatorWidth := Canvas.GetTextWidth(FTimeSeparator);
        S := S + FTimeSeparator;
        FTimeWidth := FTimeWidth + (TimeParts - 1) * FTimeSeparatorWidth;
      end;

      if not (dtpAMPM in FEffectiveHideDateTimeParts) then begin
        S := S + 'APM';
        FAMPMWidth := Max(Canvas.TextWidth('AM'), Canvas.TextWidth('PM'));
        FTimeWidth := FTimeWidth + FDigitWidth + FAMPMWidth;
      end;

    end;

    FTextWidth := FDateWidth + FTimeWidth;
    if (DateParts > 0) and (TimeParts > 0) then
      FTextWidth := FTextWidth + 2 * FDigitWidth;

    FTextHeight := Canvas.GetTextHeight('0123456789' + S);

  end;
end;

function TCustomZVDateTimePicker.GetDay: Word;
begin
  Result := GetYYYYMMDD.Day;
end;

function TCustomZVDateTimePicker.GetMonth: Word;
begin
  Result := GetYYYYMMDD.Month;
end;

function TCustomZVDateTimePicker.GetYear: Word;
begin
  Result := GetYYYYMMDD.Year;
end;

function TCustomZVDateTimePicker.GetHMSMs(const NowIfNull: Boolean): THMSMs;
begin
  if DateIsNull then begin
    if NowIfNull then
      DecodeTime(SysUtils.Time, Result.Hour, Result.Minute, Result.Second, Result.MiliSec)
    else
      with Result do begin
        Hour := 0;
        Minute := 0;
        Second := 0;
        MiliSec := 0;
      end;
  end else
    DecodeTime(FDateTime, Result.Hour, Result.Minute, Result.Second, Result.MiliSec);
end;

function TCustomZVDateTimePicker.GetYYYYMMDD(const TodayIfNull: Boolean): TYMD;
begin
  if DateIsNull then begin
    if TodayIfNull then
      DecodeDate(SysUtils.Date, Result.Year, Result.Month, Result.Day)
    else
      with Result do begin
        Day := 0;
        Month := 0;
        Year := 0;
      end;
  end else
    DecodeDate(FDateTime, Result.Year, Result.Month, Result.Day);
end;

procedure TCustomZVDateTimePicker.SetHour(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectHour;

  HMSMs := GetHMSMs(True);
  HMSMs.Hour := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomZVDateTimePicker.SetMiliSec(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectMiliSec;

  HMSMs := GetHMSMs(True);
  HMSMs.MiliSec := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomZVDateTimePicker.SetMinute(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectMinute;

  HMSMs := GetHMSMs(True);
  HMSMs.Minute := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomZVDateTimePicker.SetSecond(const AValue: Word);
var
  HMSMs: THMSMs;
begin
  SelectSecond;

  HMSMs := GetHMSMs(True);
  HMSMs.Second := AValue;

  SetHMSMs(HMSMs);
end;

procedure TCustomZVDateTimePicker.SetSeparators(const DateSep, TimeSep: String);
var
  SeparatorsChanged: Boolean;
begin
  FUseDefaultSeparators := False;
  SeparatorsChanged := False;

  if FDateSeparator <> DateSep then begin
    FDateSeparator := DateSep;
    SeparatorsChanged := True;
  end;

  if FTimeSeparator <> TimeSep then begin
    FTimeSeparator := TimeSep;
    SeparatorsChanged := True;
  end;

  if SeparatorsChanged then begin
    FRecalculatingTextSizesNeeded := True;
    Invalidate;
  end;

end;

procedure TCustomZVDateTimePicker.SetDay(const AValue: Word);
var
  YMD: TYMD;
begin
  SelectDay;
  YMD := GetYYYYMMDD(True);

  YMD.Day := AValue;
  SetYYYYMMDD(YMD);
end;

procedure TCustomZVDateTimePicker.SetMonth(const AValue: Word);
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  YMD := GetYYYYMMDD(True);

  YMD.Month := AValue;

  N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
  if YMD.Day > N then
    YMD.Day := N;

  SetYYYYMMDD(YMD);
end;

procedure TCustomZVDateTimePicker.SetYear(const AValue: Word);
var
  YMD: TYMD;
begin
  SelectYear;

  YMD := GetYYYYMMDD(True);
  YMD.Year := AValue;
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;

  SetYYYYMMDD(YMD);
end;

procedure TCustomZVDateTimePicker.SetYYYYMMDD(const AValue: TYMD);
var
  D: TDateTime;
begin
  if TryEncodeDate(AValue.Year, AValue.Month, AValue.Day, D) then
    SetDate(D)
  else
    UpdateDate;
end;

procedure TCustomZVDateTimePicker.SetHMSMs(const AValue: THMSMs);
var
  T: TDateTime;
begin
  if TryEncodeTime(AValue.Hour, AValue.Minute,
                                  AValue.Second, AValue.MiliSec, T) then
    SetTime(T)
  else
    UpdateDate;
end;

procedure TCustomZVDateTimePicker.UpdateIfUserChangedText;
var
  W: Word;
  S: String;
begin
  if FUserChangedText then begin
    Inc(FUserChanging);
    try
      FUserChangedText := False;
      S := Trim(GetSelectedText);

      if FSelectedTextPart = 8 then begin
        W := GetHour;
        if upCase(S[1]) = 'A' then begin
          if W >= 12 then
            Dec(W, 12);
        end else begin
          if W < 12 then
            Inc(W, 12);
        end;
        SetHour(W);
        FSelectedTextPart := 8;
      end else begin

        W := StrToInt(S);
        case GetSelectedDateTimePart of
          dtpYear:
            begin
              if Length(S) <= 2 then begin
                // If user entered the year in two digit format (or even only
                // one digit), we will set the year according to the CenturyFrom
                // property (We actually use FEffectiveCenturyFrom field, which
                // is adjusted to take care of MinDate and MaxDate properties,
                // besides CenturyFrom).
                if W >= (FEffectiveCenturyFrom mod 100) then
                  W := W + 100 * (FEffectiveCenturyFrom div 100)
                else
                  W := W + 100 * (FEffectiveCenturyFrom div 100 + 1);

              end;
              SetYear(W);
            end;

          dtpDay:
            SetDay(W);

          dtpMonth:
            SetMonth(W);

          dtpHour:
            begin
              if (FTimeFormat = tf12) then begin
                if GetHour < 12 then begin
                  if W = 12 then
                    SetHour(0)
                  else
                    SetHour(W);
                end else begin
                  if W = 12 then
                    SetHour(W)
                  else
                    SetHour(W + 12);
                end;
              end else
                SetHour(W);
            end;
          dtpMinute:
            SetMinute(W);
          dtpSecond:
            SetSecond(W);
          dtpMiliSec:
            SetMiliSec(W);

        end;

      end;
    finally
      Dec(FUserChanging);
    end;
  end;
end;

function TCustomZVDateTimePicker.GetSelectedText: String;
begin
  if FSelectedTextPart <= 3 then
    Result := FTextPart[FSelectedTextPart]
  else
    Result := FTimeText[TDateTimePart(FSelectedTextPart - 1)];
end;

procedure TCustomZVDateTimePicker.AdjustEffectiveCenturyFrom;
var
  Y1, Y2, M, D: Word;
begin
  DecodeDate(FMinDate, Y1, M, D);

  if Y1 > FCenturyFrom then
    FEffectiveCenturyFrom := Y1 // If we use CenturyFrom which is set to value
         // below MinDate's year, then when user enters two digit year, the
         // DateTime would automatically be set to MinDate value, even though
         // we perhaps allow same two-digit year in following centuries. It
         // would be less user friendly.
         // This is therefore better.

  else begin
    DecodeDate(FMaxDate, Y2, M, D);

    if Y2 < 100 then
      Y2 := 0
    else
      Dec(Y2, 99); // -- We should not use CenturyFrom if it is set to value
       // greater then MaxDate's year minus 100 years.
       // For example:
       // if CenturyFrom = 1941 and MaxDate = 31.12.2025, then if user enters
       // Year 33, we could not set the year to 2033 anyway, because of MaxDate
       // limit. Note that if we just leave CenturyFrom to effectively remain as
       // is, then in case of our example the DateTime would be automatically
       // reduced to MaxDate value. Setting the year to 1933 is rather expected
       // behaviour, so our internal field FEffectiveCenturyFrom should be 1926.

    // Therefore:
    if Y2 < FCenturyFrom then
      FEffectiveCenturyFrom := Max(Y1, Y2)
    else
      FEffectiveCenturyFrom := FCenturyFrom; // -- FCenturyFrom has passed all
                   // our tests, so we'll really use it without any correction.
  end;
end;

{ AdjustEffectiveDateDisplayOrder procedure
 -------------------------------------------
  If date display order ddoTryDefault is set, then we will decide which
  display order to use according to ShortDateFormat global variable. This
  procedure tries to achieve that by searching through short date format string,
  to see which letter comes first -- d, m or y. When it finds any of these
  characters, it assumes that date order should be d-m-y, m-d-y, or y-m-d
  respectively. If the search through ShortDateFormat is unsuccessful by any
  chance, we try the same with LongDateFormat global variable. If we don't
  succeed again, we'll assume y-m-d order.  }
procedure TCustomZVDateTimePicker.AdjustEffectiveDateDisplayOrder;
var
  S: String;
  I, J, Le: Integer;
  InQuoteChar: Char;
begin
  if FDateDisplayOrder = ddoTryDefault then begin
    S := DefaultFormatSettings.ShortDateFormat;
    FEffectiveDateDisplayOrder := ddoTryDefault;

    repeat
      InQuoteChar := Chr(0);
      Le := Length(S);

      I := 0;
      while I < Le do begin
        Inc(I);
        if InQuoteChar = Chr(0) then begin
          case S[I] of
            '''', '"':
              InQuoteChar := S[I];
            'D', 'd':
              begin
                { If 3 or more "d"-s are standing together, then it's day
                  of week, but here we are interested in day of month.
                  So, we have to see what is following:  }
                J := I + 1;
                while (J <= Le) and (upCase(S[J]) = 'D') do
                  Inc(J);

                if J <= I + 2 then begin
                  FEffectiveDateDisplayOrder := ddoDMY;
                  Break;
                end;

                I := J - 1;
              end;
            'M', 'm':
              begin
                FEffectiveDateDisplayOrder := ddoMDY;
                Break;
              end;
            'Y', 'y':
              begin
                FEffectiveDateDisplayOrder := ddoYMD;
                Break;
              end;
          end;
        end else
          if S[I] = InQuoteChar then
            InQuoteChar := Chr(0);

      end;

      if FEffectiveDateDisplayOrder = ddoTryDefault then begin
        { We couldn't decide with ShortDateFormat, let's try with
          LongDateFormat now. }
        S := DefaultFormatSettings.LongDateFormat;
        { But now we must set something to be default. This ensures that the
          repeat loop breaks next time. If we don't find anything in
          LongDateFormat, we'll leave with y-m-d order. }
        FEffectiveDateDisplayOrder := ddoYMD;

      end else
        Break;

    until False;

  end else
    FEffectiveDateDisplayOrder := FDateDisplayOrder;

  case FEffectiveDateDisplayOrder of
    ddoDMY:
      begin
        FDayPos := 1;
        FMonthPos := 2;
        FYearPos := 3;
      end;
    ddoMDY:
      begin
        FDayPos := 2;
        FMonthPos := 1;
        FYearPos := 3;
      end;
    ddoYMD:
      begin
        FDayPos := 3;
        FMonthPos := 2;
        FYearPos := 1;
      end;
  end;

end;

procedure TCustomZVDateTimePicker.AdjustEffectiveHideDateTimeParts;
var
  I: TDateTimePart;
  PreviousEffectiveHideDateTimeParts: set of TDateTimePart;
begin
  PreviousEffectiveHideDateTimeParts := FEffectiveHideDateTimeParts;
  FEffectiveHideDateTimeParts := [];

  for I := Low(TDateTimeParts) to High(TDateTimeParts) do
    if I in FHideDateTimeParts then
      Include(FEffectiveHideDateTimeParts, I);

  if FKind = dtkDate then
    FEffectiveHideDateTimeParts := FEffectiveHideDateTimeParts +
                       [dtpHour, dtpMinute, dtpSecond, dtpMiliSec, dtpAMPM]
  else begin
    if FKind = dtkTime then
      FEffectiveHideDateTimeParts := FEffectiveHideDateTimeParts +
                            [dtpDay, dtpMonth, dtpYear];

    case FTimeDisplay of
      tdHM:
        FEffectiveHideDateTimeParts := FEffectiveHideDateTimeParts +
                            [dtpSecond, dtpMiliSec];
      tdHMS:
        FEffectiveHideDateTimeParts := FEffectiveHideDateTimeParts +
                                        [dtpMiliSec];
    end;

    if (FTimeFormat = tf24) or (dtpHour in FEffectiveHideDateTimeParts) then
      Include(FEffectiveHideDateTimeParts, dtpAMPM);
  end;

  if FEffectiveHideDateTimeParts
                          <> PreviousEffectiveHideDateTimeParts then begin
    if GetDateTimePartFromTextPart(FSelectedTextPart) in
                     FEffectiveHideDateTimeParts then
      MoveSelectionLR(False);

    FRecalculatingTextSizesNeeded := True;
    UpdateShowArrowButton;
    UpdateDate;
  end;
end;

procedure TCustomZVDateTimePicker.SelectDateTimePart(
  const DateTimePart: TDateTimePart);
begin
  if not (DateTimePart in FEffectiveHideDateTimeParts) then begin
    case DateTimePart of
      dtpDay:
        FSelectedTextPart := FDayPos;
      dtpMonth:
        FSelectedTextPart := FMonthPos;
      dtpYear:
        FSelectedTextPart := FYearPos;
    else
      FSelectedTextPart := 1 + Ord(DateTimePart);
    end;

    Invalidate;
  end;
end;

procedure TCustomZVDateTimePicker.DestroyCalendarForm;
begin
  if Assigned(FCalendarForm) then begin
    TDTCalendarForm(FCalendarForm).FClosing := True;
    FCalendarForm.Release;
    FCalendarForm := nil;
  end;
end;

class function TCustomZVDateTimePicker.GetControlClassDefaultSize: TSize;
begin
  Result.cx := 102;
  Result.cy := 23;
end;

procedure TCustomZVDateTimePicker.ConfirmChanges;
begin
  UpdateIfUserChangedText;
  FConfirmedDateTime := FDateTime;
end;

procedure TCustomZVDateTimePicker.UndoChanges;
begin
  FDateTime := FConfirmedDateTime;
  UpdateDate;
end;

{ GetDateTimePartFromTextPart function
 -----------------------------------------------
  Returns part of date/time from the position (1-8). }
function TCustomZVDateTimePicker.GetDateTimePartFromTextPart(
  TextPart: TTextPart): TDateTimePart;
begin
  Result := TDateTimePart(TextPart - 1);

  case FEffectiveDateDisplayOrder of
    ddoMDY:
      if Result = dtpDay then
        Result := dtpMonth
      else if Result = dtpMonth then
        Result := dtpDay;
    ddoYMD:
      if Result = dtpDay then
        Result := dtpYear
      else if Result = dtpYear then
        Result := dtpDay;
  end;
end;

{ GetSelectedDateTimePart function
 ---------------------------------
  Returns part of date/time which is currently selected. }
function TCustomZVDateTimePicker.GetSelectedDateTimePart: TDateTimePart;
begin
  Result := GetDateTimePartFromTextPart(FSelectedTextPart);
end;

procedure TCustomZVDateTimePicker.FontChanged(Sender: TObject);
begin
  FRecalculatingTextSizesNeeded := True;
  inherited FontChanged(Sender);
end;

{ GetTextOrigin
 ---------------
  Returns upper left corner of the rectangle where the text is written.
  Also used in calculating our preffered size. }
function TCustomZVDateTimePicker.GetTextOrigin(IgnoreRightToLeft: Boolean
  ): TPoint;
begin
  Result.y := BorderSpacing.InnerBorder + BorderWidth;
  if Assigned(FCheckBox) then
    Result.x := Result.y + FCheckBox.BorderSpacing.Left
             + FCheckBox.BorderSpacing.Right + FCheckBox.Width
  else
    Result.x := Result.y;

  if (not IgnoreRightToLeft) and IsRightToLeft then
    Result.x := ClientWidth - Result.x - FTextWidth;
end;

{ MoveSelectionLR
 -----------------
  Moves selection to left or to right. If parameter ToLeft is true, then the
  selection moves to left, otherwise to right. }
procedure TCustomZVDateTimePicker.MoveSelectionLR(const ToLeft: Boolean);
var
  I, SafetyTextPart: TTextPart;
begin
  UpdateIfUserChangedText;

  SafetyTextPart := Low(TTextPart);
  I := FSelectedTextPart;
  repeat
    if ToLeft then begin
      if I <= Low(TTextPart) then
        I := High(TTextPart)
      else
        Dec(I);
    end else begin
      if I >= High(TTextPart) then
        I := Low(TTextPart)
      else
        Inc(I);
    end;

    if not (GetDateTimePartFromTextPart(I)
                in FEffectiveHideDateTimeParts) then
      FSelectedTextPart := I;

    { Is it possible that all parts are hidden? Yes it is!
      So we need to ensure that this doesn't loop forever.
      When this insurance text part gets to high value, break }
    Inc(SafetyTextPart);
  until (I = FSelectedTextPart) or (SafetyTextPart >= High(TTextPart));
end;

procedure TCustomZVDateTimePicker.KeyDown(var Key: Word; Shift: TShiftState);
var
  K: Word;
begin
  Inc(FUserChanging);
  try
    if FTextEnabled then
      inherited KeyDown(Key, Shift); // calls OnKeyDown event

    if (Key = VK_SPACE) then begin
      if GetShowCheckBox then
        FCheckBox.Checked := not FCheckBox.Checked;

    end else if FTextEnabled then begin

      case Key of
        VK_LEFT, VK_RIGHT, VK_OEM_COMMA, VK_OEM_PERIOD, VK_DIVIDE,
            VK_OEM_MINUS, VK_SEPARATOR, VK_DECIMAL, VK_SUBTRACT:
          begin
            K := Key;
            Key := 0;
            MoveSelectionLR(K = VK_LEFT);
            Invalidate;
          end;
        VK_UP:
          begin
            Key := 0;
            UpdateIfUserChangedText;
            if not FReadOnly then
              IncreaseCurrentTextPart;
          end;
        VK_DOWN:
          begin
            Key := 0;
            UpdateIfUserChangedText;
            if not FReadOnly then
              DecreaseCurrentTextPart;
          end;
        VK_RETURN:
          if not FReadOnly then
            EditingDone;

        VK_ESCAPE:
          if not FReadOnly then begin
            UndoChanges;
            EditingDone;
          end;
        VK_N:
          if (not FReadOnly) and FNullInputAllowed then
            SetDateTime(NullDate);
      end;

    end;
  finally
    Dec(FUserChanging);
  end;

end;

procedure TCustomZVDateTimePicker.KeyPress(var Key: char);
var
  S: String;
  DTP: TDateTimePart;
  N, L: Integer;
  YMD: TYMD;
  HMSMs: THMSMs;
  D, T: TDateTime;
  Finished, ForceChange: Boolean;

begin
  if FTextEnabled then begin
    Inc(FUserChanging);
    try
      inherited KeyPress(Key);

      if (not FReadOnly) then begin
        Finished := False;
        ForceChange := False;

        if FSelectedTextPart = 8 then begin
          case upCase(Key) of
            'A': S := 'AM';
            'P': S := 'PM';
          else
            Finished := True;
          end;
          ForceChange := True;

        end else if Key in ['0'..'9'] then begin

          DTP := GetSelectedDateTimePart;

          if DTP = dtpYear then
            N := 4
          else if DTP = dtpMiliSec then
            N := 3
          else
            N := 2;

          S := Trim(GetSelectedText);
          if FUserChangedText and (Length(S) < N) then
            S := S + Key
          else
            S := Key;

          if (Length(S) >= N) then begin

            L := StrToInt(S);
            if DTP < dtpHour then begin
              YMD := GetYYYYMMDD(True);
              case DTP of
                dtpDay: YMD.Day := L;
                dtpMonth: YMD.Month := L;
                dtpYear: YMD.Year := L;
              end;

              if TryEncodeDate(YMD.Year, YMD.Month, YMD.Day, D)
                        and (D >= MinDate) and (D <= MaxDate) then
                ForceChange := True
              else if N = 4 then begin
                UpdateDate;
                Finished := True;
              end else
                S := Key;

            end else begin
              if (DTP = dtpHour) and (FTimeFormat = tf12) then begin
                if not (L in [1..12]) then
                  S := Key
                else
                  ForceChange := True;

              end else begin

                HMSMs := GetHMSMs(True);
                case DTP of
                  dtpHour: HMSMs.Hour := L;
                  dtpMinute: HMSMs.Minute := L;
                  dtpSecond: HMSMs.Second := L;
                  dtpMiliSec: HMSMs.MiliSec := L;
                end;
                if not TryEncodeTime(HMSMs.Hour, HMSMs.Minute, HMSMs.Second,
                                             HMSMs.MiliSec, T) then
                  S := Key
                else
                  ForceChange := True;

              end;
            end;

          end;
        end else
          Finished := True;

        if (not Finished) and (GetSelectedText <> S) then begin
          if (not FUserChangedText) and DateIsNull then
            if FSelectedTextPart <= 3 then
              DateTime := SysUtils.Date
            else
              DateTime := SysUtils.Now;

          if (not FLeadingZeros) and (FSelectedTextPart <= 4) then
            while (Length(S) > 1) and (S[1] = '0') do
              Delete(S, 1, 1);

          if FSelectedTextPart <= 3 then
            FTextPart[FSelectedTextPart] := S
          else
            FTimeText[TDateTimePart(FSelectedTextPart - 1)] := S;

          FUserChangedText := True;

          if ForceChange then begin
            if FAutoAdvance then begin
              MoveSelectionLR(False);
              Invalidate;
            end else
              UpdateIfUserChangedText;
          end else
            Invalidate;

        end;

      end;

    finally
      Dec(FUserChanging);
    end;

  end;
end;

{ SelectTextPartUnderMouse
 --------------------------
  This procedure determines which text part (date or time part -- day, month,
  year, hour, minute...) should be selected in response to mouse message.
  Used in MouseDown and DoMouseWheel methods. }
procedure TCustomZVDateTimePicker.SelectTextPartUnderMouse(XMouse: Integer);
var
  I, M, NX: Integer;
  InTime: Boolean;
  DTP: TDateTimePart;
begin
  UpdateIfUserChangedText;
  SetFocusIfPossible;

  if Focused then begin
// Calculating mouse position inside text
//       in order to select date part under mouse cursor.
    NX := XMouse - GetTextOrigin.x;

    InTime := False;
    if FTimeWidth > 0 then begin
      if FDateWidth > 0 then begin
        if NX >= FDateWidth + FDigitWidth then begin
          InTime := True;
          NX := NX - FDateWidth - 2 * FDigitWidth;
        end;
      end else
        InTime := True;
    end;

    if InTime then begin
      FSelectedTextPart := 8;

      if (dtpAMPM in FEffectiveHideDateTimeParts) or
            (NX < FTimeWidth - FAMPMWidth - FDigitWidth div 2) then begin
        FSelectedTextPart := 7;
        I := 4;
        M := FTimeSeparatorWidth div 2;
        while I <= 6 do begin
          DTP := GetDateTimePartFromTextPart(I);
          if not (DTP in FEffectiveHideDateTimeParts) then begin
            Inc(M, 2 * FDigitWidth);
            if M > NX then begin
              FSelectedTextPart := I;
              Break;
            end;

            Inc(M, FTimeSeparatorWidth);
          end;
          Inc(I);
        end;
      end;

    end else if FDateWidth > 0 then begin

      FSelectedTextPart := 3;
      I := 1;
      M := FSeparatorWidth div 2;
      while I <= 2 do begin
        if not(GetDateTimePartFromTextPart(I)
                      in FEffectiveHideDateTimeParts) then begin
          if I = FYearPos then
            Inc(M, 4 * FDigitWidth)
          else if (I = FMonthPos) and FShowMonthNames then
            Inc(M, FMonthWidth)
          else
            Inc(M, 2 * FDigitWidth);

          if M > NX then begin
            FSelectedTextPart := I;
            Break;
          end;

          Inc(M, FSeparatorWidth);
        end;

        Inc(I);
      end;

    end;

    if GetDateTimePartFromTextPart(FSelectedTextPart)
                    in FEffectiveHideDateTimeParts then
      MoveSelectionLR(True);

    Invalidate;
//-------------------------------------------------------
  end;
end;

procedure TCustomZVDateTimePicker.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FTextEnabled then begin
    SelectTextPartUnderMouse(X);
    inherited MouseDown(Button, Shift, X, Y);
  end else
    SetFocusIfPossible;

end;

function TCustomZVDateTimePicker.DoMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := False;
  if FTextEnabled then begin
    SelectTextPartUnderMouse(MousePos.x);
    if not FReadOnly then begin
      Inc(FUserChanging);
      try
        if WheelDelta < 0 then
          DecreaseCurrentTextPart
        else
          IncreaseCurrentTextPart;

        Result := True;
      finally
        Dec(FUserChanging);
      end;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
var
  TextOrigin: TPoint;
  M: Integer;

begin
  RecalculateTextSizesIfNeeded;
  TextOrigin := GetTextOrigin(True);

  // We must use TextOrigin's x + y (x is, of course, left margin, but not right
  // margin if check box is shown. However, y, which is top margin, always
  // equals right margin).
  PreferredWidth := TextOrigin.x + TextOrigin.y;

  if Assigned(FUpDown) then
    Inc(PreferredWidth, FUpDown.Width)
  else if Assigned(FArrowButton) then
    Inc(PreferredWidth, FArrowButton.Width);

  PreferredWidth := PreferredWidth + FTextWidth;

  M := Width - ClientWidth;
  PreferredWidth := PreferredWidth + M;

  PreferredHeight := 2 * TextOrigin.y + FTextHeight + M;
end;

procedure TCustomZVDateTimePicker.SetBiDiMode(AValue: TBiDiMode);
begin
  inherited SetBiDiMode(AValue);
  ArrangeCtrls;
end;

procedure TCustomZVDateTimePicker.IncreaseCurrentTextPart;
begin
  if DateIsNull then begin
    if FSelectedTextPart <= 3 then
      SetDateTime(SysUtils.Date)
    else
      SetDateTime(SysUtils.Now);

  end else begin
    case GetSelectedDateTimePart of
      dtpDay: IncreaseDay;
      dtpMonth: IncreaseMonth;
      dtpYear: IncreaseYear;
      dtpHour: IncreaseHour;
      dtpMinute: IncreaseMinute;
      dtpSecond: IncreaseSecond;
      dtpMiliSec: IncreaseMiliSec;
      dtpAMPM: ChangeAMPM;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseCurrentTextPart;
begin
  if DateIsNull then begin
    if FSelectedTextPart <= 3 then
      SetDateTime(SysUtils.Date)
    else
      SetDateTime(SysUtils.Now);

  end else begin
    case GetSelectedDateTimePart of
      dtpDay: DecreaseDay;
      dtpMonth: DecreaseMonth;
      dtpYear: DecreaseYear;
      dtpHour: DecreaseHour;
      dtpMinute: DecreaseMinute;
      dtpSecond: DecreaseSecond;
      dtpMiliSec: DecreaseMiliSec;
      dtpAMPM: ChangeAMPM;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.IncreaseMonth;
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  if Cascade then
    SetDateTime(IncMonth(DateTime))
  else begin
    YMD := GetYYYYMMDD(True);

    if YMD.Month >= 12 then
      YMD.Month := 1
    else
      Inc(YMD.Month);

    N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
    if YMD.Day > N then
      YMD.Day := N;

    SetYYYYMMDD(YMD);
  end;
end;

procedure TCustomZVDateTimePicker.IncreaseYear;
var
  YMD: TYMD;
begin
  SelectYear;
  YMD := GetYYYYMMDD(True);

  Inc(YMD.Year);
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;

  SetYYYYMMDD(YMD);
end;

procedure TCustomZVDateTimePicker.IncreaseDay;
var
  YMD: TYMD;
begin
  SelectDay;
  if Cascade then
    SetDateTime(IncDay(DateTime))
  else begin
    YMD := GetYYYYMMDD(True);

    if YMD.Day >= NumberOfDaysInMonth(YMD.Month, YMD.Year) then
      YMD.Day := 1
    else
      Inc(YMD.Day);

    SetYYYYMMDD(YMD);
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseMonth;
var
  YMD: TYMD;
  N: Word;
begin
  SelectMonth;
  if Cascade then
    SetDateTime(IncMonth(DateTime, -1))
  else begin
    YMD := GetYYYYMMDD(True);

    if YMD.Month <= 1 then
      YMD.Month := 12
    else
      Dec(YMD.Month);

    N := NumberOfDaysInMonth(YMD.Month, YMD.Year);
    if YMD.Day > N then
      YMD.Day := N;

    SetYYYYMMDD(YMD);
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseYear;
var
  YMD: TYMD;
begin
  SelectYear;
  YMD := GetYYYYMMDD(True);
  Dec(YMD.Year);
  if (YMD.Month = 2) and (YMD.Day > 28) and (not IsLeapYear(YMD.Year)) then
    YMD.Day := 28;
  SetYYYYMMDD(YMD);
end;

procedure TCustomZVDateTimePicker.DecreaseDay;
var
  YMD: TYMD;
begin
  SelectDay;
  if Cascade then
    SetDateTime(IncDay(DateTime, -1))
  else begin
    YMD := GetYYYYMMDD(True);

    if YMD.Day <= 1 then
      YMD.Day := NumberOfDaysInMonth(YMD.Month, YMD.Year)
    else
      Dec(YMD.Day);

    SetYYYYMMDD(YMD);
  end;
end;

procedure TCustomZVDateTimePicker.IncreaseHour;
var
  HMSMs: THMSMs;
begin
  SelectHour;
  if Cascade then
    SetDateTime(IncHour(DateTime))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.Hour >= 23 then
      HMSMs.Hour := 0
    else
      Inc(HMSMs.Hour);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.IncreaseMinute;
var
  HMSMs: THMSMs;
begin
  SelectMinute;
  if Cascade then
    SetDateTime(IncMinute(DateTime))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.Minute >= 59 then
      HMSMs.Minute := 0
    else
      Inc(HMSMs.Minute);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.IncreaseSecond;
var
  HMSMs: THMSMs;
begin
  SelectSecond;
  if Cascade then
    SetDateTime(IncSecond(DateTime))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.Second >= 59 then
      HMSMs.Second := 0
    else
      Inc(HMSMs.Second);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.IncreaseMiliSec;
var
  HMSMs: THMSMs;
begin
  SelectMiliSec;
  if Cascade then
    SetDateTime(IncMilliSecond(DateTime))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.MiliSec >= 999 then
      HMSMs.MiliSec := 0
    else
      Inc(HMSMs.MiliSec);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseHour;
var
  HMSMs: THMSMs;
begin
  SelectHour;
  if Cascade then
    SetDateTime(IncHour(DateTime, -1))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.Hour <= 0 then
      HMSMS.Hour := 23
    else
      Dec(HMSMs.Hour);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseMinute;
var
  HMSMs: THMSMs;
begin
  SelectMinute;
  if Cascade then
    SetDateTime(IncMinute(DateTime, -1))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.Minute <= 0 then
      HMSMs.Minute := 59
    else
      Dec(HMSMs.Minute);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseSecond;
var
  HMSMs: THMSMs;
begin
  SelectSecond;
  if Cascade then
    SetDateTime(IncSecond(DateTime, -1))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.Second <= 0 then
      HMSMs.Second := 59
    else
      Dec(HMSMs.Second);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.DecreaseMiliSec;
var
  HMSMs: THMSMs;
begin
  SelectMiliSec;
  if Cascade then
    SetDateTime(IncMilliSecond(DateTime, -1))
  else begin
    HMSMs := GetHMSMs(True);

    if HMSMs.MiliSec <= 0 then
      HMSMs.MiliSec := 999
    else
      Dec(HMSMs.MiliSec);

    SetHMSMs(HMSMs);
  end;
end;

procedure TCustomZVDateTimePicker.ChangeAMPM;
var
  HMSMs: THMSMs;
begin
  SelectAMPM;
  HMSMs := GetHMSMs(True);

  if HMSMs.Hour >= 12 then
    Dec(HMSMS.Hour, 12)
  else
    Inc(HMSMS.Hour, 12);

  SetHMSMs(HMSMs);
end;

procedure TCustomZVDateTimePicker.UpdateDate;
var
  W: Array[1..3] of Word;
  WT: Array[dtpHour..dtpAMPM] of Word;
  DTP: TDateTimePart;
begin
  if HandleAllocated then begin
    FUserChangedText := False;

    if not (DateIsNull or FJumpMinMax) then begin
      if Int(FDateTime) > FMaxDate then
        FDateTime := ComposeDateTime(FMaxDate, FDateTime);

      if FDateTime < FMinDate then
        FDateTime := ComposeDateTime(FMinDate, FDateTime);
    end;

    if not FChangeInRecursiveCall then begin // we'll skip the next part in
           // recursive calls which could be made through Change or UndoChanges
      FChangeInRecursiveCall := True;
      try
        if FUserChanging > 0 then begin // this means that the change is caused by user interaction
          try
            Change;
          except
            UndoChanges;
            raise;
          end
        end else
          FConfirmedDateTime := FDateTime;
      finally
        FChangeInRecursiveCall := False;
      end;
    end;

    if DateIsNull then begin
      if dtpYear in FEffectiveHideDateTimeParts then
        FTextPart[FYearPos] := ''
      else
        FTextPart[FYearPos] := '0000';

      if dtpMonth in FEffectiveHideDateTimeParts then
        FTextPart[FMonthPos] := ''
      else
        FTextPart[FMonthPos] := '00';

      if dtpDay in FEffectiveHideDateTimeParts then
        FTextPart[FDayPos] := ''
      else
        FTextPart[FDayPos] := '00';

      for DTP := dtpHour to dtpAMPM do begin
        if DTP in FEffectiveHideDateTimeParts then
          FTimeText[DTP] := ''
        else if DTP = dtpAMPM then
          FTimeText[DTP] := 'XX'
        else if DTP = dtpMiliSec then
          FTimeText[DTP] := '999'
        else
          FTimeText[DTP] := '99';
      end;

    end else begin
      DecodeDate(FDateTime, W[3], W[2], W[1]);

      if dtpYear in FEffectiveHideDateTimeParts then
        FTextPart[FYearPos] := ''
      else if FLeadingZeros then
        FTextPart[FYearPos] := RightStr('000' + IntToStr(W[3]), 4)
      else
        FTextPart[FYearPos] := IntToStr(W[3]);

      if dtpMonth in FEffectiveHideDateTimeParts then
        FTextPart[FMonthPos] := ''
      else if FShowMonthNames then
        FTextPart[FMonthPos] := FMonthNamesArray[W[2]]
      else if FLeadingZeros then
        FTextPart[FMonthPos] := RightStr('0' + IntToStr(W[2]), 2)
      else
        FTextPart[FMonthPos] := IntToStr(W[2]);

      if dtpDay in FEffectiveHideDateTimeParts then
        FTextPart[FDayPos] := ''
      else if FLeadingZeros then
        FTextPart[FDayPos] := RightStr('0' + IntToStr(W[1]), 2)
      else
        FTextPart[FDayPos] := IntToStr(W[1]);

      DecodeTime(FDateTime, WT[dtpHour], WT[dtpMinute], WT[dtpSecond], WT[dtpMiliSec]);

      if dtpAMPM in FEffectiveHideDateTimeParts then
        FTimeText[dtpAMPM] := ''
      else begin
        if WT[dtpHour] < 12 then begin
          FTimeText[dtpAMPM] := 'AM';
          if WT[dtpHour] = 0 then
            WT[dtpHour] := 12;
        end else begin
          FTimeText[dtpAMPM] := 'PM';
          if WT[dtpHour] > 12 then
            Dec(WT[dtpHour], 12);
        end;
      end;

      for DTP := dtpHour to dtpMiliSec do begin
        if DTP in FEffectiveHideDateTimeParts then
          FTimeText[DTP] := ''
        else if (DTP = dtpHour) and (not FLeadingZeros) then
          FTimeText[DTP] := IntToStr(WT[dtpHour])
        else if DTP = dtpMiliSec then
          FTimeText[DTP] := RightStr('00' + IntToStr(WT[DTP]), 3)
        else
          FTimeText[DTP] := RightStr('0' + IntToStr(WT[DTP]), 2);

      end;

    end;

    Invalidate;
  end;
end;

procedure TCustomZVDateTimePicker.DoEnter;
begin
  inherited DoEnter;
  Invalidate;
end;

procedure TCustomZVDateTimePicker.DoExit;
begin
  inherited DoExit;
  Invalidate;
end;

procedure TCustomZVDateTimePicker.Click;
begin
  if FTextEnabled then
    inherited Click;
end;

procedure TCustomZVDateTimePicker.DblClick;
begin
  if FTextEnabled then
    inherited DblClick;
end;

procedure TCustomZVDateTimePicker.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if FTextEnabled then
    inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomZVDateTimePicker.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FTextEnabled then
    inherited KeyUp(Key, Shift);
end;

procedure TCustomZVDateTimePicker.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  if FTextEnabled then
    inherited UTF8KeyPress(UTF8Key);
end;

procedure TCustomZVDateTimePicker.SelectDay;
begin
  SelectDateTimePart(dtpDay);
end;

procedure TCustomZVDateTimePicker.SelectMonth;
begin
  SelectDateTimePart(dtpMonth);
end;

procedure TCustomZVDateTimePicker.SelectYear;
begin
  SelectDateTimePart(dtpYear);
end;

procedure TCustomZVDateTimePicker.SelectHour;
begin
  SelectDateTimePart(dtpHour);
end;

procedure TCustomZVDateTimePicker.SelectMinute;
begin
  SelectDateTimePart(dtpMinute);
end;

procedure TCustomZVDateTimePicker.SelectSecond;
begin
  SelectDateTimePart(dtpSecond);
end;

procedure TCustomZVDateTimePicker.SelectMiliSec;
begin
  SelectDateTimePart(dtpMiliSec);
end;

procedure TCustomZVDateTimePicker.SelectAMPM;
begin
  SelectDateTimePart(dtpAMPM);
end;

procedure TCustomZVDateTimePicker.SetEnabled(Value: Boolean);
begin
  if GetEnabled <> Value then begin
    inherited SetEnabled(Value);
    CheckTextEnabled;
    Invalidate;
  end;
end;

procedure TCustomZVDateTimePicker.SetAutoSize(Value: Boolean);
begin
  if AutoSize <> Value then begin
    if Value then
      InvalidatePreferredSize;

    inherited SetAutoSize(Value);
  end;
end;

// I had to override CreateWnd, because in design time on Linux Lazarus crashes
// if we try to do anchoring of child controls in constructor.
// Therefore, I needed to ensure that controls anchoring does not take place
// before CreateWnd has done. So, I moved all anchoring code to a procedure
// ArrangeCtrls and introduced a boolean field FDoNotArrangeControls which
// prevents that code from executing before CreateWnd.
//!!! Later, I simplified the arranging procedure, so maybe it can be done now
//    before window creation is done. It's better to leave this delay system,
//    anyway -- we might change anchoring code again for some reason.
procedure TCustomZVDateTimePicker.CreateWnd;
begin
  inherited CreateWnd;

  if FDoNotArrangeControls then begin { This field is set to True in constructor.
    Its purpose is to prevent control anchoring until this point. That's because
    on Linux Lazarus crashes when control is dropped on form in designer if
    particular anchoring code executes before CreateWnd has done its job. }
    UpdateDate;
    FDoNotArrangeControls := False;
    ArrangeCtrls;
  end;
end;

procedure TCustomZVDateTimePicker.SetDateTimeJumpMinMax(const AValue: TDateTime);
begin
  FJumpMinMax := True;
  try
    SetDateTime(AValue);
  finally
    FJumpMinMax := False;
  end;
end;

procedure TCustomZVDateTimePicker.ArrangeCtrls;
var
  C: TControl;
begin
  if not FDoNotArrangeControls then begin //Read the note above CreateWnd procedure.
    DisableAlign;
    try
      if GetShowCheckBox then begin
        if IsRightToLeft then begin
          FCheckBox.Align := alRight;
          FCheckBox.BorderSpacing.Left := 0;
          FCheckBox.BorderSpacing.Right := 2;
        end else begin
          FCheckBox.Align := alLeft;
          FCheckBox.BorderSpacing.Left := 2;
          FCheckBox.BorderSpacing.Right := 0;
        end;
        FCheckBox.BringToFront;
      end;

      if Assigned(FUpDown) then
        C := FUpDown
      else if Assigned(FArrowButton) then
        C := FArrowButton
      else
        C := nil;

      if Assigned(C) then begin
        if IsRightToLeft then
          C.Align := alLeft
        else
          C.Align := alRight;

        C.BringToFront;
      end;

      CheckTextEnabled;
      InvalidatePreferredSize;
      AdjustSize;

      Invalidate;
    finally
      EnableAlign;
    end;
  end;
end;

procedure TCustomZVDateTimePicker.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomZVDateTimePicker.SelectDate;
begin
  if (FSelectedTextPart > 3)
          or (GetDateTimePartFromTextPart(FSelectedTextPart)
                  in FEffectiveHideDateTimeParts) then
    FSelectedTextPart := 1;

  if GetDateTimePartFromTextPart(FSelectedTextPart)
                  in FEffectiveHideDateTimeParts then
    MoveSelectionLR(False);

  Invalidate;
end;

procedure TCustomZVDateTimePicker.SelectTime;
begin
  if (FSelectedTextPart < 4)
          or (GetDateTimePartFromTextPart(FSelectedTextPart)
                  in FEffectiveHideDateTimeParts) then
    FSelectedTextPart := 4;

  if GetDateTimePartFromTextPart(FSelectedTextPart)
                  in FEffectiveHideDateTimeParts then
    MoveSelectionLR(False);

  Invalidate;
end;

procedure TCustomZVDateTimePicker.Paint;
var
  I, M, N, K, L: Integer;
  DD: Array[1..8] of Integer;
  R: TRect;
  SelectStep: 0..8;
  TextStyle: TTextStyle;
  DTP: TDateTimePart;
  S: String;

begin
  if ClientRectNeedsInterfaceUpdate then // In Qt widgetset, this solves the
    DoAdjustClientRectChange;           // problem of dispositioned client rect.

  if FRecalculatingTextSizesNeeded then begin
    if AutoSize then begin
      InvalidatePreferredSize;
      AdjustSize;
    end;

    RecalculateTextSizesIfNeeded;
  end;

  TextStyle := Canvas.TextStyle;

  Canvas.Brush.Style := bsSolid;
  Canvas.Brush.Color := Color;
  Canvas.FillRect(ClientRect);

  R.TopLeft := GetTextOrigin;

  M := 2 * R.Top + FTextHeight;
  M := (ClientHeight - M) div 2;

  Inc(R.Top, M);

  R.Bottom := R.Top + FTextHeight;

  TextStyle.Layout := tlCenter;
  TextStyle.Wordbreak := False;
  TextStyle.Opaque := False;
  TextStyle.RightToLeft := IsRightToLeft;

  if DateIsNull and (FTextForNullDate > '')
                       and (not (FTextEnabled and Focused)) then begin

    if IsRightToLeft then begin
      TextStyle.Alignment := taRightJustify;
      R.Right := R.Left + FTextWidth;
      R.Left := 0;
    end else begin
      TextStyle.Alignment := taLeftJustify;
      R.Right := Width;
    end;

    if FTextEnabled then
      Canvas.Font.Color := Font.Color
    else
      Canvas.Font.Color := clGrayText;

    Canvas.TextRect(R, R.Left, R.Top, FTextForNullDate, TextStyle);

  end else begin
    TextStyle.Alignment := taRightJustify;

    SelectStep := 0;
    if FTextEnabled then begin
      Canvas.Font.Color := Font.Color;
      if Focused then
        SelectStep := FSelectedTextPart;
    end else begin
      Canvas.Font.Color := clGrayText;
    end;

    if dtpYear in FEffectiveHideDateTimeParts then begin
      DD[FYearPos] := 0;
      M := 4;
      L := 0;
    end else begin
      DD[FYearPos] := 4 * FDigitWidth;
      M := FYearPos;
      L := FYearPos;
    end;

    if dtpMonth in FEffectiveHideDateTimeParts then
      DD[FMonthPos] := 0
    else begin
      if FShowMonthNames then
        DD[FMonthPos] := FMonthWidth
      else
        DD[FMonthPos] := 2 * FDigitWidth;

      if FMonthPos < M then
        M := FMonthPos;

      if FMonthPos > L then
        L := FMonthPos;

    end;

    if dtpDay in FEffectiveHideDateTimeParts then
      DD[FDayPos] := 0
    else begin
      DD[FDayPos] := 2 * FDigitWidth;
      if FDayPos < M then
        M := FDayPos;
      if FDayPos > L then
        L := FDayPos;
    end;

    N := 3;
    K := 3;
    for DTP := dtpHour to dtpAMPM do begin
      I := Ord(DTP) + 1;
      if DTP in FEffectiveHideDateTimeParts then
        DD[I] := 0
      else if DTP = dtpAMPM then begin
        DD[I] := FAMPMWidth;
        N := I;
      end else begin
        if DTP = dtpMiliSec then
          DD[I] := 3 * FDigitWidth
        else
          DD[I] := 2 * FDigitWidth;

        if K < I then
          K := I;
      end;

      if N < K then
        N := K;

    end;

    for I := M to N do begin
      if DD[I] <> 0 then begin

        R.Right := R.Left + DD[I];
        if I <= 3 then begin
          if (I = FMonthPos) and FShowMonthNames then begin
            TextStyle.Alignment := taCenter;
            if DateIsNull then
              S := FNullMonthText
            else
              S := FTextPart[I];
          end else
            S := FTextPart[I];

        end else
          S := FTimeText[TDateTimePart(I - 1)];

        if I = SelectStep then begin
          TextStyle.Opaque := True;
          Canvas.Brush.Color := clHighlight;
          Canvas.Font.Color := clHighlightText;

          Canvas.TextRect(R, R.Left, R.Top, S, TextStyle);

          TextStyle.Opaque := False;
          Canvas.Brush.Color := Color;
          Canvas.Font.Color := Self.Font.Color;
        end else
          Canvas.TextRect(R, R.Left, R.Top, S, TextStyle);

        TextStyle.Alignment := taRightJustify;
        R.Left := R.Right;

        if I < L then begin
          R.Right := R.Left + FSeparatorWidth;
          if not ((I = FMonthPos) and FShowMonthNames) then
            Canvas.TextRect(R, R.Left, R.Top, FDateSeparator, TextStyle);
        end else if I > L then begin
          if I = K then begin
            R.Right := R.Left + FDigitWidth;
          end else if I < K then begin
            R.Right := R.Left + FTimeSeparatorWidth;
            Canvas.TextRect(R, R.Left, R.Top, FTimeSeparator, TextStyle);
          end;
        end else begin
          if FTrailingSeparator then begin
            R.Right := R.Left + FSepNoSpaceWidth;
            Canvas.TextRect(R, R.Left, R.Top,
                                      TrimRight(FDateSeparator), TextStyle);
          end;
          if FTimeWidth > 0 then
            R.Right := R.Right + 2 * FDigitWidth;

        end;
        R.Left := R.Right;
      end;
    end;

  end;

  inherited Paint;
end;

procedure TCustomZVDateTimePicker.EditingDone;
begin
  if FNoEditingDone <= 0 then begin
    ConfirmChanges;

    inherited EditingDone;
  end;
end;

procedure TCustomZVDateTimePicker.ArrowMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  DropDownCalendarForm;
end;

procedure TCustomZVDateTimePicker.UpDownClick(Sender: TObject;
  Button: TUDBtnType);
begin
  SetFocusIfPossible;

  if not FReadOnly then begin
    Inc(FUserChanging);
    try
      if Button = btNext then
        IncreaseCurrentTextPart
      else
        DecreaseCurrentTextPart;
    finally
      Dec(FUserChanging);
    end;
  end;
end;

procedure TCustomZVDateTimePicker.DoDropDown;
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TCustomZVDateTimePicker.DoCloseUp;
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

function TCustomZVDateTimePicker.GetChecked: Boolean;
begin
  Result := (not Assigned(FCheckBox)) or (FCheckBox.State = cbChecked);
end;

{ DrawArrowButtonGlyph
 ----------------------
 Draws the arrow shape on button (when DateMode dmComboBox is set). }
procedure TCustomZVDateTimePicker.DrawArrowButtonGlyph;
const
  ArrowColor = TColor($8D665A);
begin
// First I ment to put arrow images in a lrs file. In my opinion, however, that
// wouldn't be an elegant option for so simple shapes.

  if Assigned(FArrowButton) then begin
    FArrowButton.Glyph.SetSize(9, 6);
    FArrowButton.Glyph.Canvas.Brush.Style := bsSolid;
    FArrowButton.Glyph.Canvas.Brush.Color := clSkyBlue;
    FArrowButton.Glyph.Canvas.FillRect(0, 0, 9, 6);
    FArrowButton.Glyph.Canvas.Pen.Color := ArrowColor;
    FArrowButton.Glyph.Canvas.Brush.Color := FArrowButton.Glyph.Canvas.Pen.Color;

{ Let's draw shape of the arrow on the button: }
    case FArrowShape of
      asClassicLarger:
        { triangle: }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(8, 1),
                                                        Point(4, 5)]);
      asClassicSmaller:
        { triangle -- smaller variant:  }
        FArrowButton.Glyph.Canvas.Polygon([Point(1, 2), Point(7, 2),
                                                        Point(4, 5)]);
      asModernLarger:
        { modern: }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0),
                          Point(4, 3), Point(7, 0), Point(8, 1), Point(4, 5)]);
      asModernSmaller:
        { modern -- smaller variant:    }
        FArrowButton.Glyph.Canvas.Polygon([Point(1, 2), Point(2, 1),
                          Point(4, 3), Point(6, 1), Point(7, 2), Point(4, 5)]);
      asYetAnotherShape:
        { something in between, not very pretty:  }
        FArrowButton.Glyph.Canvas.Polygon([Point(0, 1), Point(1, 0),
              Point(2, 1), Point(6, 1),Point(7, 0), Point(8, 1), Point(4, 5)]);
    end;

    FArrowButton.Glyph.Mask(clSkyBlue);
  end;
end;

function TCustomZVDateTimePicker.AreSeparatorsStored: Boolean;
begin
  Result := not FUseDefaultSeparators;
end;

function TCustomZVDateTimePicker.GetDate: TDate;
begin
  if DateIsNull then
    Result := NullDate
  else
    Result := Int(FDateTime);
end;

function TCustomZVDateTimePicker.GetDateTime: TDateTime;
begin
  if DateIsNull then
    Result := NullDate
  else
    Result := FDateTime;
end;

function TCustomZVDateTimePicker.GetShowCheckBox: Boolean;
begin
  Result := Assigned(FCheckBox);
end;

function TCustomZVDateTimePicker.GetTime: TTime;
begin
  if DateIsNull then
    Result := NullDate
  else
    Result := Abs(Frac(FDateTime));
end;

procedure TCustomZVDateTimePicker.SetArrowShape(const AValue: TArrowShape);
begin
  if FArrowShape = AValue then Exit;

  FArrowShape := AValue;
  DrawArrowButtonGlyph;
end;

const
  DefaultUpDownWidth = 15;
  DefaultArrowButtonWidth = DefaultUpDownWidth + 2;

procedure TCustomZVDateTimePicker.SetAutoButtonSize(AValue: Boolean);
begin
  if FAutoButtonSize <> AValue then begin
    FAutoButtonSize := AValue;

    if AValue then
      AutoResizeButton
    else begin
      if Assigned(FUpDown) then
        FUpDown.Width := DefaultUpDownWidth
      else if Assigned(FArrowButton) then
        FArrowButton.Width := DefaultArrowButtonWidth;
    end;

  end;
end;

procedure TCustomZVDateTimePicker.SetCalendarWrapperClass(
  AValue: TCalendarControlWrapperClass);
begin
  if FCalendarWrapperClass = AValue then Exit;
  FCalendarWrapperClass := AValue;
end;

procedure TCustomZVDateTimePicker.SetCenturyFrom(const AValue: Word);
begin
  if FCenturyFrom = AValue then Exit;

  FCenturyFrom := AValue;
  AdjustEffectiveCenturyFrom;
end;

procedure TCustomZVDateTimePicker.CheckBoxChange(Sender: TObject);
begin
  CheckTextEnabled;
  SetFocusIfPossible;

  if Assigned(FOnCheckBoxChange) then
    FOnCheckBoxChange(Sender);

  Invalidate;
end;

procedure TCustomZVDateTimePicker.SetFocusIfPossible;
var
  F: TCustomForm;

begin
  Inc(FNoEditingDone);
  try
    F := GetParentForm(Self);

    if Assigned(F) and F.CanFocus and CanTab then
      SetFocus;

  finally
    Dec(FNoEditingDone);
  end;
end;

procedure TCustomZVDateTimePicker.AutoResizeButton;
begin
  if Assigned(FArrowButton) then
    FArrowButton.Width := MulDiv(ClientHeight, 9, 10)
  else if Assigned(FUpDown) then
    FUpDown.Width := MulDiv(ClientHeight, 79, 100);

end;

procedure TCustomZVDateTimePicker.WMKillFocus(var Message: TLMKillFocus);
begin
  // On Qt it seems that WMKillFocus happens even when focus jumps to some other
  // form. This behaviour differs from win and gtk 2 (where it happens only when
  // focus jumps to other control on the same form) and we should prevent it at
  // least for our calendar, because it triggers EditingDone.
  if Screen.ActiveCustomForm <> FCalendarForm then
    inherited WMKillFocus(Message);
end;

procedure TCustomZVDateTimePicker.WMSize(var Message: TLMSize);
begin
  inherited WMSize(Message);

  if FAutoButtonSize then
    AutoResizeButton;
end;

procedure TCustomZVDateTimePicker.DropDownCalendarForm;
begin
  SetFocusIfPossible;

  if FAllowDroppingCalendar then begin
    if not (FReadOnly or Assigned(FCalendarForm)
                          or (csDesigning in ComponentState)) then begin
      FCalendarForm := TDTCalendarForm.CreateNewDTCalendarForm(nil, Self);
      FCalendarForm.Show;
    end;

  end else begin
    DestroyCalendarForm;
    FAllowDroppingCalendar := True;
  end;

end;

type

  { TDTUpDown }

{ The two buttons contained by UpDown control are never disabled in original
  UpDown class. This class is defined here to override this behaviour. }
  TDTUpDown = class(TCustomUpDown)
  private
    DTPicker: TCustomZVDateTimePicker;
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure WndProc(var Message: TLMessage); override;
  end;

  { TDTSpeedButton }

  TDTSpeedButton = class(TCustomSpeedButton)
  private
    DTPicker: TCustomZVDateTimePicker;
  protected
    procedure CalculatePreferredSize(var PreferredWidth,
                  PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
      override;
  end;

{ TDTUpDown }

{ When our UpDown control gets enabled/disabled, the two its buttons' Enabled
  property is set accordingly. }
procedure TDTUpDown.SetEnabled(Value: Boolean);

  procedure SetEnabledForAllChildren(AWinControl: TWinControl);
  var
    I: Integer;
    C: TControl;
  begin
    for I := 0 to AWinControl.ControlCount - 1 do begin
      C := AWinControl.Controls[I];
      C.Enabled := Value;

      if C is TWinControl then
        SetEnabledForAllChildren(TWinControl(C));

    end;
  end;

begin
  inherited SetEnabled(Value);

  SetEnabledForAllChildren(Self);
end;

{ Our UpDown control is always alligned, but setting its PreferredHeight
  uncoditionally to 1 prevents the UpDown to mess with our PreferredHeight.
  The problem is that if we didn't do this, when our Height is greater than
  really preffered, UpDown prevents it to be set correctly when we set AutoSize
  to True. }
procedure TDTUpDown.CalculatePreferredSize(var PreferredWidth, PreferredHeight:
  integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  PreferredHeight := 1;
end;

{ We don't want to let EditingDone event to fire each time up-down buttons get
  clicked. That is why WndProc is overriden. }
procedure TDTUpDown.WndProc(var Message: TLMessage);
begin
  if ((Message.msg >= LM_MOUSEFIRST) and (Message.msg <= LM_MOUSELAST))
      or ((Message.msg >= LM_MOUSEFIRST2) and (Message.msg <= LM_MOUSELAST2)) then begin

    Inc(DTPicker.FNoEditingDone);
    try
      inherited WndProc(Message);
    finally
      Dec(DTPicker.FNoEditingDone);
    end

  end else
    inherited WndProc(Message);

end;

{ TDTSpeedButton }

{ See the coment above TDTUpDown.CalculatePreferredSize }
procedure TDTSpeedButton.CalculatePreferredSize(var PreferredWidth,
  PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight,
    WithThemeSpace);

  PreferredHeight := 1;
end;

{ Prevent EditingDone to fire whenever the SpeedButton gets clicked }
procedure TDTSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  Inc(DTPicker.FNoEditingDone);
  try
    inherited MouseDown(Button, Shift, X, Y);
  finally
    Dec(DTPicker.FNoEditingDone);
  end;
end;

procedure TCustomZVDateTimePicker.UpdateShowArrowButton;

  procedure CreateArrowBtn;
  begin
    if not Assigned(FArrowButton) then begin
      DestroyCalendarForm;
      DestroyUpDown;

      FArrowButton := TDTSpeedButton.Create(Self);
      FArrowButton.ControlStyle := FArrowButton.ControlStyle +
                                            [csNoFocus, csNoDesignSelectable];
      TDTSpeedButton(FArrowButton).DTPicker := Self;
      FArrowButton.SetBounds(0, 0, DefaultArrowButtonWidth, 1);

      DrawArrowButtonGlyph;

      FArrowButton.Parent := Self;
      FAllowDroppingCalendar := True;

      TDTSpeedButton(FArrowButton).OnMouseDown := @ArrowMouseDown;

    end;
  end;

  procedure CreateUpDown;
  begin
    if not Assigned(FUpDown) then begin
      DestroyArrowBtn;

      FUpDown := TDTUpDown.Create(Self);
      FUpDown.ControlStyle := FUpDown.ControlStyle +
                                     [csNoFocus, csNoDesignSelectable];

      TDTUpDown(FUpDown).DTPicker := Self;

      FUpDown.SetBounds(0, 0, DefaultUpDownWidth, 1);

      FUpDown.Parent := Self;

      TDTUpDown(FUPDown).OnClick := @UpDownClick;

    end;
  end;

var
  ReallyShowArrowButton: Boolean;

begin
  if FDateMode = dmNone then begin
    DestroyArrowBtn;
    DestroyUpDown;

  end else begin
    ReallyShowArrowButton := (FDateMode = dmComboBox) and
                          not (dtpDay in FEffectiveHideDateTimeParts);

    if (ReallyShowArrowButton <> Assigned(FArrowButton)) or
                       (Assigned(FArrowButton) = Assigned(FUpDown)) then begin
      DisableAlign;
      try
        if ReallyShowArrowButton then
          CreateArrowBtn
        else
          CreateUpDown;

        ArrangeCtrls;

      finally
        EnableAlign;
      end;
    end;

  end;
end;

procedure TCustomZVDateTimePicker.DestroyUpDown;
begin
  if Assigned(FUpDown) then begin
    TDTUpDown(FUPDown).OnClick := nil;
    FreeAndNil(FUpDown);
  end;
end;

procedure TCustomZVDateTimePicker.DestroyArrowBtn;
begin
  if Assigned(FArrowButton) then begin
    TDTSpeedButton(FArrowButton).OnMouseDown := nil;
    DestroyCalendarForm;
    FreeAndNil(FArrowButton);
  end;
end;

constructor TCustomZVDateTimePicker.Create(AOwner: TComponent);
var
  I: Integer;
  DTP: TDateTimePart;
begin
  inherited Create(AOwner);

  with GetControlClassDefaultSize do
    SetInitialBounds(0, 0, cx, cy);

  FChangeInRecursiveCall := False;
  FNoEditingDone := 0;
  FArrowShape := asModernSmaller;
  FAllowDroppingCalendar := True;

  FOnDropDown := nil;
  FOnCloseUp := nil;

  ParentColor := False;
  FCheckBox := nil;
  FArrowButton := nil;
  FUpDown := nil;

  FKind := dtkDate;
  FNullInputAllowed := True;

  { Thanks to Luiz Américo for this:
    Lazarus ignores empty string when saving to lrs. Therefore, the problem
    is, when TextForNullDate is set to an empty string and after the project
    is saved and opened again, then, this property gets default value NULL
    instead of empty string. The following condition seems to be a workaround
    for this. }
  if (AOwner = nil) or not (csReading in Owner.ComponentState) then
    FTextForNullDate := 'NULL';

  FCenturyFrom := 1941;
  FRecalculatingTextSizesNeeded := True;
  FOnChange := nil;
  FOnCheckBoxChange := nil;
  FSeparatorWidth := 0;
  FSepNoSpaceWidth := 0;
  FDigitWidth := 0;
  FTimeSeparatorWidth := 0;
  FAMPMWidth := 0;
  FDateWidth := 0;
  FTimeWidth := 0;
  FTextWidth := 0;
  FTextHeight := 0;
  FMonthWidth := 0;
  FHideDateTimeParts := [];
  FShowMonthNames := False;
  FNullMonthText := '';

  for I := Low(FTextPart) to High(FTextPart) do
    FTextPart[I] := '';

  for DTP := dtpHour to dtpAMPM do
    FTimeText[DTP] := '';

  FTimeDisplay := tdHMS;
  FTimeFormat := tf24;

  FLeadingZeros := True;
  FUserChanging := 0;
  FReadOnly := False;
  FDateTime := SysUtils.Now;
  FConfirmedDateTime := FDateTime;
  FMinDate := TheSmallestDate;
  FMaxDate := TheBiggestDate;
  FTrailingSeparator := False;
  FDateDisplayOrder := ddoTryDefault;
  FSelectedTextPart := 1;
  FUseDefaultSeparators := True;
  FDateSeparator := DefaultFormatSettings.DateSeparator;
  FTimeSeparator := DefaultFormatSettings.TimeSeparator;
  FEffectiveCenturyFrom := FCenturyFrom;
  FJumpMinMax := False;

  ParentColor := False;
  TabStop := True;
  BorderWidth := 2;
  BorderStyle := bsSingle;
  ParentFont := True;
  AutoSize := True;

  FTextEnabled := True;
  FCalendarForm := nil;
  FDoNotArrangeControls := True;
  FCascade := False;
  FAutoButtonSize := False;
  FAutoAdvance := True;
  FCalendarWrapperClass := nil;
  FEffectiveHideDateTimeParts := [];

  AdjustEffectiveDateDisplayOrder;
  AdjustEffectiveHideDateTimeParts;

  SetMonthNames('Long');
  SetDateMode(dmComboBox);
end;

destructor TCustomZVDateTimePicker.Destroy;
begin
  FDoNotArrangeControls := True;
  DestroyArrowBtn;
  DestroyUpDown;
  SetShowCheckBox(False);

  inherited Destroy;
end;

function TCustomZVDateTimePicker.DateIsNull: Boolean;
begin
  Result := IsNullDate(FDateTime);
end;

end.
