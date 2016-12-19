(* === UNavigation ===
 * Ver: 1.0.1
 *  By: Igor Nunes
 * Some useful routines for I/O and navigation. *)

{$mode objfpc}
unit UNavigation;

interface
uses crt, sysutils{ , strutils };

const DEFAULT_TEXTBACKGROUND = 0;
      DEFAULT_TEXTCOLOR = 7;
      CRLF = #13+#10;

TYPE TKeys = set of char;

procedure Pause(const PauseText : string; const KeysToProceed : TKeys; out KeyReceiver : char);
procedure Pause(const PauseText : string; const KeysToProceed : TKeys); overload;
procedure Pause(const KeysToProceed : TKeys; out KeyReceiver : char);
procedure Pause(const PauseText : string); overload;
procedure Pause(const KeysToProceed : TKeys); overload;
procedure Pause; overload;

procedure WriteXY(x, y : word; s : string; tcolor : word = DEFAULT_TEXTCOLOR; bcolor : word = DEFAULT_TEXTBACKGROUND);


implementation

procedure Pause(const PauseText : string; const KeysToProceed : TKeys; out KeyReceiver : char);
begin
   write(PauseText);
   repeat
      KeyReceiver := UpCase(ReadKey);
   until KeyReceiver in KeysToProceed;
end;


procedure Pause(const PauseText : string; const KeysToProceed : TKeys); overload;
var Key : char;
begin
   Pause(PauseText, KeysToProceed, Key);
end;


procedure Pause(const KeysToProceed : TKeys; out KeyReceiver : char);
const StrEmpty : string = '';
begin
   Pause(StrEmpty, KeysToProceed, KeyReceiver);
end;


procedure Pause(const PauseText : string); overload;
begin
   Pause(PauseText, [#13]);
end;


procedure Pause(const KeysToProceed : TKeys); overload;
const StrEmpty : string = '';
begin
   Pause(StrEmpty, KeysToProceed);
end;


procedure Pause; overload;
const StrEmpty : string = '';
begin
   Pause(StrEmpty);
end;


procedure WriteXY(x, y : word; s : string; tcolor : word = DEFAULT_TEXTCOLOR; bcolor : word = DEFAULT_TEXTBACKGROUND);
begin
   GotoXY(x, y);
   TextColor(tcolor);
   TextBackground(bcolor);
   write(s);
   TextBackground(DEFAULT_TEXTBACKGROUND);
   TextColor(DEFAULT_TEXTCOLOR);
end;

end.