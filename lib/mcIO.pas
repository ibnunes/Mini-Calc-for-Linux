(* === mcIO ===
 * Ver: 1.0.0
 *  By: Igor Nunes
 * Specific I/O methods for situations where "readln" cannot handle properly. *)

{$mode objfpc}
unit mcIO;

interface
uses crt, { strutils,} sysutils, math, classes;

type TArrayStr = array of string;
     TIOResult = (IOnull, IOcommand, IOvariable, IOconstant, IOreal, IOinteger, IOerror);
     
     TSecureRead = class(TObject)
     private
        var vErrorPos : word;

     public
        function TLongint(const PROMPT : string; out i : longint) : boolean;
        function TReal(const PROMPT : string; out r : real) : boolean;
        function TFloat(const PROMPT : string; out f : float) : boolean;
        property ErrorPos : word read vErrorPos;
     end;

function IntelliRead(out s : string; out r : real; out e : word; const prompt : string = '') : TIOResult;
function ReadStrList(var l : TArrayStr) : Word;

function IsCommand(s : string) : boolean;
function IsVariable(s : string) : boolean;
function IsConstant(s : string) : boolean;
function IsInteger(const r : Real) : boolean;

var SecureRead : TSecureRead;


implementation

function IsCommand(s : string) : boolean;
begin
   s := LowerCase(s);
   IsCommand := (s = 'exit')   or
                (s = 'cancel') or
                (s = 'help');
end;

function IsVariable(s : string) : boolean;
begin
   s := LowerCase(s);
   IsVariable := (s = 'ans');
end;

function IsConstant(s : string) : boolean;
begin
   s := LowerCase(s);
   IsConstant := (s = '$pi') or
                 (s = '$e');
end;


function IsInteger(const r : Real) : boolean;
begin
   IsInteger := Frac(r) = 0.0;
end;


function IntelliRead(out s : string; out r : real; out e : word; const prompt : string = '') : TIOResult;
(* Processes the input and gets its type. *)
begin
   s := '';
   r := 0.0;
   e := 0;
   
   IntelliRead := IOreal;
   if prompt <> '' then
      write(prompt);
   readln(s);
   if (s = '') then
      IntelliRead := IOnull
   else begin
      Val(s, r, e);
      if (e > 0) then
         if IsCommand(s) then begin
            IntelliRead := IOcommand;
         end else
         if IsVariable(s) then begin
            IntelliRead := IOvariable;
         end else
         if IsConstant(s) then begin
            IntelliRead := IOconstant;
         end else
            IntelliRead := IOerror
      else
         if IsInteger(r) then
            IntelliRead := IOinteger;
   end;
end;


function ReadStrList(var l : TArrayStr) : Word;
const SEPARATOR = ',';
      SPACERS   = [' '];
var s : string;
    i : word;
    temp : string = '';
begin
   readln(s);
   SetLength(l, 0);
   
   for i := 1 to Length(s) do begin
      if s[i] in SPACERS then
         continue
         
      else if (s[i] = SEPARATOR) or (i = Length(s)) then begin
         if (i = Length(s)) and (s[i] <> SEPARATOR) then 
            temp := temp + s[i];
         SetLength(l, Length(l)+1);
         l[High(l)] := temp;
         temp := '';
         
      end else begin
         temp := temp + s[i];
      end;
   end;
   
   ReadStrList := Length(l);
end;


function TSecureRead.TLongint(const PROMPT : string; out i : longint) : boolean;
var s : string;
    e : word;
begin
    Write(PROMPT);
    ReadLn(s);
    Val(s, i, e);
    TLongint := (e = 0);
    self.vErrorPos := e;
end;


function TSecureRead.TReal(const PROMPT : string; out r : real) : boolean;
var s : string;
    e : word;
begin
    Write(PROMPT);
    ReadLn(s);
    Val(s, r, e);
    TReal := (e = 0);
    self.vErrorPos := e;
end;


function TSecureRead.TFloat(const PROMPT : string; out f : float) : boolean;
var s : string;
    e : word;
begin
    Write(PROMPT);
    ReadLn(s);
    Val(s, f, e);
    TFloat := (e = 0);
    self.vErrorPos := e;
end;


initialization
    SecureRead := TSecureRead.Create;

finalization
    SecureRead.Free;

end.