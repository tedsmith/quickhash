{
LCLCalendarWrapper
- - - - - - - - - - - - - - - - -
Author: Zoran Vučenović
        Зоран Вученовић

   This unit is part of ZVDateTimeCtrls package for Lazarus.

   TLCLCalendarWrapper is the default implementation of TCalendarControlWrapper
abstract class, used by ZVDateTimePicker. Wraps LCL's TCalendar.

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
unit LCLCalendarWrapper;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, Calendar, CalendarControlWrapper;

type

  { TLCLCalendarWrapper }

  TLCLCalendarWrapper = class(TCalendarControlWrapper)
  public
    class function GetCalendarControlClass: TControlClass; override;
    procedure SetDate(Date: TDate); override;
    function GetDate: TDate; override;
    function AreCoordinatesOnDate(X, Y: Integer): Boolean; override;
  end;

implementation

{ TLCLCalendarWrapper }

class function TLCLCalendarWrapper.GetCalendarControlClass: TControlClass;
begin
  Result := TCalendar;
end;

procedure TLCLCalendarWrapper.SetDate(Date: TDate);
begin
  TCalendar(GetCalendarControl).DateTime := Date;
end;

function TLCLCalendarWrapper.GetDate: TDate;
begin
  Result := TCalendar(GetCalendarControl).DateTime;
end;

function TLCLCalendarWrapper.AreCoordinatesOnDate(X, Y: Integer): Boolean;
begin
  Result :=
    TCalendar(GetCalendarControl).HitTest(Point(X, Y)) in [cpDate, cpNoWhere];
end;

end.

