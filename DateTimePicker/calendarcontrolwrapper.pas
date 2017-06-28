{
CalendarControlWrapper
- - - - - - - - - - - - - - - - -
Author: Zoran Vučenović
        Зоран Вученовић

   This unit is part of ZVDateTimeCtrls package for Lazarus.

   By default, TZVDateTimePicker uses LCL's TCalendar to represent the
drop-down calendar, but you can use some other calendar control instead.

   In order to use another calendar control, you should "wrap" that control with
a CalendarControlWrapper.

   To be used by ZVDateTimePicker, the calendar control must at least provide
a way to determine whether the coordinates are on the date (when this control
gets clicked, we must decide if the date has just been chosen - then we should
respond by closing the drop-down form and setting the date from calendar to
ZVDateTimePicker - for example in LCL's TCalendar we will respond when the
calendar is clicked on date, but not when the user clicks in title area changing
months or years, then we let the user keep browsing the calendar).

   When creating new wrapper, there are four abstract methods which need to be
overriden. Please see the coments in code below.

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
   I do hope the ZVDateTimeCtrls package will be useful.
}
unit CalendarControlWrapper;

{$mode objfpc}{$H+}

interface

uses
  Controls;

type

  { TCalendarControlWrapper }

  TCalendarControlWrapper = class
  private
    FCalendarControl: TControl;
  public
  { There are four methods that derived classes should override: }

  { Should be overriden to just return the class of the calendar control. }
    class function GetCalendarControlClass: TControlClass; virtual abstract;

  { Should be overriden to set the date in the calendar control. }
    procedure SetDate(Date: TDate); virtual abstract;

  { Should be overriden to get the date from the calendar control. }
    function GetDate: TDate; virtual abstract;

  { This function should return True if coordinates (X, Y) are on the date in
    the calendar control (ZVDateTimePicker calls this function when the calendar
    is clicked, to determine whether the drop-down calendar should return the
    date or not).  }
    function AreCoordinatesOnDate(X, Y: Integer): Boolean; virtual abstract;

    function GetCalendarControl: TControl;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TCalendarControlWrapperClass = class of TCalendarControlWrapper;

implementation

{ TCalendarControlWrapper }

function TCalendarControlWrapper.GetCalendarControl: TControl;
begin
  Result := FCalendarControl;
end;

constructor TCalendarControlWrapper.Create;
begin
  FCalendarControl := GetCalendarControlClass.Create(nil);
end;

destructor TCalendarControlWrapper.Destroy;
begin
  FCalendarControl.Free;
  inherited Destroy;
end;

end.

