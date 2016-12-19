{$mode objfpc}
unit mcLog;

interface
uses dateutils, classes;

type
   TLogRecorder = class(TStringList)
      published
         procedure Rec(local, msg : string);
   end;



implementation

procedure TLogRecorder.Rec(local, msg : string);
var
   recording : string = '';
begin
   DateTimeToString(recording, '', Now);
   
end;



end.