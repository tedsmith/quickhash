ZVDateTimeControls Package for Lazarus
--------------------------------------
http://wiki.lazarus.freepascal.org/ZVDateTimeControls_Package

About:
	The package contains two controls — TZVDateTimePicker and TDBZVDateTimePicker.

	Delphi's VCL has a control named TDateTimePicker, which I find very useful for editing dates. LCL, however, does not have this control. Instead, for editing dates LCL has a control named TDateEdit, but I prefer the VCL's TDateTimePicker. Therefore, I tried to create a cross-platform Lazarus control which would resemble VCL's TDateTimePicker as much as possible. The TZVDateTimePicker control does not use native Win control, but it descends from LCL's TCustomControl to be cross-platform.

	TDBZVDateTimePicker is a data-aware version of TZVDateTimePicker, with nice way of handling null database values.

Author: Zoran Vučenović

License: Modified LGPL, same as Lazarus component library.