(* === UExcept ===
 * Ver: 1.0.0
 *  By: Igor Nunes
 * Exception handling routines. *)

{$mode objfpc}
unit UExcept;

interface
uses crt, sysutils, strutils, classes,
     UNavigation;

procedure WriteError(ex : Exception);
procedure WriteError(msg : string);
procedure WriteError(const X, Y : word; msg : string); overload;



implementation

procedure WriteError(ex : Exception);
begin
   WriteError('(' + ex.classname + ') ' + ex.message);
end;


procedure WriteError(msg : string);
begin
   TextColor(12);
   write('[ERR] ');
   TextColor(14);
   write(msg);
   TextColor(7);
end;


procedure WriteError(const X, Y : word; msg : string); overload;
begin
   GotoXY(X, Y);
   WriteError(X, Y, msg);
   GotoXY(X, Y);
   write(DupeString(' ', Length('[ERR] ' + msg)));
end;


procedure FatalInfo(ex : Exception);
var i : word;

begin
   TextBackground(8);
   clrscr;
   
   TextBackground(4);
   for i := 5 to 20 do begin
      GotoXY(10, i);
      write(DupeString(' ', 60));
   end;
   
   TextColor(12);
   GotoXY(10, 5);
   write(PadCenter('Mini Calc 4', 60));
   
   TextColor(15);
   GotoXY(11, 7);
   write('A fatal error occurred and the application must shutdown.');
   
   GotoXY(11, 9);
   write('  Class: '); textcolor(14); write(ex.classname);
   TextColor(15);
   GotoXY(11,10);
   write('Message: '); textcolor(14); write(ex.message);
   
   TextColor(15);
   GotoXY(11,12);
   write('Report this error to the author - it might be a bug.');
   
   GotoXY(11,15);
   Pause('Press ENTER to exit...');
   
   TextBackground(8);
end;


end.
