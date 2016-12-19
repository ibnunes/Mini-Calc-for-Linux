(* === mcVLM ===
 * Ver: 1.1.0
 *  By: Igor Nunes
 * Visual List Manager *)

{$mode objfpc}
unit mcVLM;

interface
uses crt, sysutils, strutils, math, classes, fpexprpars;

const EListMgrInvalid = -1;
      MAXLIST = 1000;
      // Keys
      KEY_UP      = #72;
      KEY_DOWN    = #80;
      KEY_LEFT    = #75;
      KEY_RIGHT   = #77;
      KEY_ESC     = #27;
      KEY_ENTER   = #13;
      KEY_SELECT  = 'S';

type TArrayDouble = array [1..MAXLIST] of double;
     TList = record
                data   : TArrayDouble;
                id     : string;
                count  : word;
                active : boolean;
             end;
     TArrayList = array of TList;
     TFnListName  = function : string;
     TFnListValue = function : double;
     TProcException = procedure (ex : Exception);
     TProcOutput    = procedure (s : string);
     TExprEvaluater = function (s : string; out answer : float) : boolean;
     TAutoSaveDef = record
                       Status   : boolean;
                       FileName : string;
                    end;
     TListsNames = array of string;
     
     TListMgr = class(TObject)
        private
           var vLists : TArrayList;
               vCount : word;
               justcreated : boolean;
               vLNames : TListsNames;
           
           const WIDTH  = 15;
                 HEIGHT = 15;
                 LIST_IDENTIFIER = '{%s}';  // mandatory structure - if you change it, you must change the analyser (in 'ProcessFormula')
                 KEY_DELETE   = '-';
                 KEY_DELLIST  = 'D';
                 KEY_EDITLIST = 'N';
                 KEY_CLEARLIST= 'C';
                 KEY_FORMULA  = 'F';
                 KEY_GET      = '+';
                 KEY_LIST     = '*';
                 NAVKEYS      = [KEY_UP, KEY_DOWN, KEY_GET];
                 EXITKEYS     = [KEY_LEFT, KEY_RIGHT, KEY_ESC];
           
           function GetElem(const s : string; const i : LongInt) : Double;
           procedure SetElem(const s : string; const i : LongInt; val : Double);
           function GetListStr(const s : string) : TList;
           function GetList(const i : word) : TList;
           
           procedure DoAutoSave;
           
           procedure RefreshNameList;
           
        protected
           function GetIndexFromID(id : string) : LongInt;
           procedure Show(ind : word; const X, Y : word);
           // procedure ProcessFormula(id : string; const form : string; par : TExprEvaluater; fnexc : TProcException);
           
        public
           var AutoSave : TAutoSaveDef;
           
           constructor Create;
           
           function IDExists(const ID : string) : boolean;
           
           function CreateNewList(id : string) : boolean;
           procedure DeleteList(id : string);
           procedure DeleteList(i : word); overload;
           procedure ClearAll;
           
           procedure Flush;
           
           property ListsStr[s : string] : TList read GetListStr; default;
           property Lists[i : word] : TList read GetList;
           property Elements[s : string; i : LongInt] : Double read GetElem write SetElem;
           property ListCount : word read vCount write vCount;
           
           function AppendToList(id : string; value : Double) : boolean;
           function AddToList(id : string; value : Double; ind : word) : boolean;
           function DeleteFromList(id : string; ind : word) : boolean;
           function AppendToList(i : LongInt; value : Double) : boolean; overload;
           function AddToList(i : LongInt; value : Double; ind : word) : boolean; overload;
           function DeleteFromList(i : LongInt; ind : word) : boolean; overload;
           
           function NavigateThroughList(id : string; const X, Y : word; out value : Double; fn : TFnListValue; const ISPANEL : boolean = false) : char; overload;
           function NavigateThroughList(ind : word; const X, Y : word; out value : Double; fn : TFnListValue; const ISPANEL : boolean = false) : char;
           
           function Panel(const X, Y : word; out value : Double; fn : TFnListName; fnv : TFnListValue; fnform : TFnListName; fnexc : TProcException; par : TExprEvaluater; const NUMCOLS : byte = 4) : char;
           
           procedure SaveToFile(const fname : string);
           procedure LoadFromFile(const fname : string; const RESETLIST : boolean = false);
           
           procedure ProcessFormula(id : string; const form : string; par : TExprEvaluater; fnexc : TProcException);
     end;


var
   listmgr : TListMgr;


implementation

constructor TListMgr.Create;
begin
   vCount := 0;
   SetLength(self.vLists, 0);
   SetLength(self.vLNames, 0);
   self.justcreated := true;
   inherited Create;
end;


procedure TListMgr.RefreshNameList;
var elem : TList;
begin
   SetLength(self.vLNames, 0);
   for elem in self.vLists do begin
      SetLength(self.vLNames, Succ(Length(self.vLNames)));
      self.vLNames[High(self.vLNames)] := elem.id;
   end;
end;


procedure TListMgr.DoAutoSave;
begin
   if self.AutoSave.Status and (self.AutoSave.FileName <> '') then
      self.SaveToFile(self.AutoSave.FileName);
end;


function TListMgr.IDExists(const ID : string) : boolean;
begin
   IDExists := (self.GetIndexFromID(ID) >= 0);
end;


function TListMgr.GetIndexFromID(id : string) : LongInt;
var i : word;
begin
   id := UpCase(id);
   GetIndexFromID := EListMgrInvalid;
   if self.vCount > 0 then
      for i := Low(self.vLists) to High(self.vLists) do
         if (self.vLists[i].id = ID) and self.vLists[i].active then begin
            GetIndexFromID := i;
            break;
         end;
end;


function TListMgr.GetElem(const s : string; const i : LongInt) : Double;
var ind : LongInt;
begin
   ind := self.GetIndexFromID(s);
   if ind >= 0 then
      GetElem := self.vLists[ind].data[i];
end;


function TListMgr.GetListStr(const s : string) : TList;
var ind : LongInt;
begin
   ind := self.GetIndexFromID(s);
   if ind >= 0 then
      GetListStr := self.vLists[ind];
end;


function TListMgr.GetList(const i : word) : TList;
begin
   GetList := self.vLists[i];
end;


procedure TListMgr.SetElem(const s : string; const i : LongInt; val : Double);
var ind : LongInt;
begin
   ind := self.GetIndexFromID(s);
   if ind >= 0 then
      self.vLists[ind].data[i] := val;
   
   self.DoAutoSave;
end;


function TListMgr.CreateNewList(id : string) : boolean;
begin
   id := UpCase(id);
   
   if self.justcreated then begin
      self.justcreated := false;
      CreateNewList := true;
   end else
      CreateNewList := not self.IDExists(id);
   
   if CreateNewList then begin
      SetLength(self.vLists, Length(self.vLists) + 1);
      self.vLists[High(self.vLists)].id := id;
      self.vLists[High(self.vLists)].active := true;
   end;
   
   Inc(self.vCount);
   self.RefreshNameList;
   self.DoAutoSave;
end;


procedure TListMgr.DeleteList(id : string);
begin
   id := UpCase(id);
   if self.IDExists(id) then begin
      with self.vLists[self.GetIndexFromID(id)] do begin
         active := false;
         count := 0;
      end;
   end;
   self.Flush;
end;


procedure TListMgr.DeleteList(i : word); overload;
begin
   self.vLists[i].active := false;
   self.vLists[i].count := 0;
   self.Flush;
end;


procedure TListMgr.ClearAll;
var i : word;
begin
   for i := 0 to self.vCount-1 do
      self.vLists[i].active := false;
   
   self.Flush;
end;


procedure TListMgr.Flush;
var i : word;
    diff : word = 0;
begin
   if self.vCount > 0 then begin
      for i := Low(self.vLists) to High(self.vLists) do begin
         if not self.vLists[i].active then
            Inc(diff);
         if (diff > 0) and (i + diff <= High(self.vLists)) and self.vLists[i+diff].active then
            self.vLists[i] := self.vLists[i+diff];
      end;
      
      if (diff > 0) then begin
         SetLength(self.vLists, Length(self.vLists) - diff);
         Dec(self.vCount, diff);
      end;
      
      if self.vCount = 0 then
         self.justcreated := true;
      
      self.RefreshNameList;
      self.DoAutoSave;
   end;
end;


function TListMgr.AppendToList(i : LongInt; value : Double) : boolean; overload;
begin
   AppendToList := (i <> EListMgrInvalid) and (self.vLists[i].count < MAXLIST);
   
   if AppendToList then begin
      Inc(self.vLists[i].count);
      self.vLists[i].data[self.vLists[i].count] := value;
   end;
   
   self.DoAutoSave;
end;


function TListMgr.AddToList(i : LongInt; value : Double; ind : word) : boolean; overload;
var n : word;
begin
   AddToList := (i <> EListMgrInvalid) and (self.vLists[i].count < MAXLIST);
   
   if AddToList then begin
      Inc(self.vLists[i].count);
      
      for n := self.vLists[i].count - 1 downto ind do begin
         self.vLists[i].data[n+1] := self.vLists[i].data[n];
      end;
      
      self.vLists[i].data[ind] := value;
   end;
   
   self.DoAutoSave;
end;


function TListMgr.DeleteFromList(i : LongInt; ind : word) : boolean; overload;
var n : word;
begin
   DeleteFromList := (i <> EListMgrInvalid) and (self.vLists[i].count > 0);
   
   if DeleteFromList then begin
      for n := ind to self.vLists[i].count - 1 do begin
         self.vLists[i].data[n] := self.vLists[i].data[n+1];
      end;
      
      Dec(self.vLists[i].count);
   end;
   
   self.DoAutoSave;
end;


function TListMgr.AddToList(id : string; value : Double; ind : word) : boolean;
begin
   id := UpCase(id);
   AddToList := self.AddToList(self.GetIndexFromID(id), value, ind);
end;


function TListMgr.AppendToList(id : string; value : Double) : boolean;
begin
   id := UpCase(id);
   AppendToList := self.AppendToList(self.GetIndexFromID(id), value);
end;


function TListMgr.DeleteFromList(id : string; ind : word) : boolean;
begin
   id := UpCase(id);
   DeleteFromList := self.DeleteFromList(self.GetIndexFromID(id), ind);
end;


procedure TListMgr.Show(ind : word; const X, Y : word);
var i : word;
begin
   TextBackground(8);
   
   GotoXY(X, Y);
   TextColor(15);
   write(self.vLists[ind].id);
   
   TextColor(7);
   for i := 1 to Min(HEIGHT-2, self.vLists[ind].count+1) do begin
      GotoXY(X, Y+i+1);
      if i <= self.vLists[ind].count then
         write(LeftStr(PadRight(FloatToStrF(self.vLists[ind].data[i], ffGeneral, 15, 1), WIDTH), WIDTH-1))
      else
         write(PadRight('---', WIDTH));
   end;
end;


procedure TListMgr.SaveToFile(const fname : string);
var f : file of TList;
    elem : TList;
begin
   Assign(f, fname);
   Rewrite(f);
   
   for elem in self.vLists do
      write(f, elem);
   
   Close(f);
end;


procedure TListMgr.LoadFromFile(const fname : string; const RESETLIST : boolean = false);
var f : file of TList;
    elem : TList;
    ind : LongInt;
begin
   if FileExists(fname) then begin
      Assign(f, fname);
      Reset(f);
      
      if RESETLIST then
         SetLength(self.vLists, 0);
      
      while not eof(f) do begin
         read(f, elem);
         self.CreateNewList(elem.id);
         ind := self.GetIndexFromID(elem.id);
         self.vLists[ind] := elem;
      end;
      
      if self.vCount > 0 then
         self.Flush;
      
      Close(f);
   end;
end;


function TListMgr.NavigateThroughList(id : string; const X, Y : word; out value : Double; fn : TFnListValue; const ISPANEL : boolean = false) : char; overload;
begin
   NavigateThroughList := self.NavigateThroughList(self.GetIndexFromID(id), X, Y, value, fn, ISPANEL);
end;


function TListMgr.NavigateThroughList(ind : word; const X, Y : word; out value : Double; fn : TFnListValue; const ISPANEL : boolean = false) : char;
var i : word;
    n : LongInt = 1;
    init : word = 1;
    opt : char = #0;
    count : word;
    v : double;
    specialkeys : set of char = [];

begin
   GotoXY(X, Y);
   TextColor(15);
   write(self.vLists[ind].id);
   
   if ISPANEL then
      specialkeys := [KEY_LIST, KEY_DELLIST, KEY_SELECT, KEY_EDITLIST, KEY_FORMULA, KEY_CLEARLIST];
   
   repeat
      for i := 1 to Min(HEIGHT-2, self.vLists[ind].count+1) do begin
         GotoXY(X, Y+i+1);
         if init + i - 1 = n then begin
            textcolor(16);
            textbackground(7);
         end else begin
            textcolor(7);
            textbackground(8);
         end;
         count := init+i-1;
         if count <= self.vLists[ind].count then
            write(LeftStr(PadRight(FloatToStrF(self.vLists[ind].data[count], ffGeneral, 15, 1), WIDTH), WIDTH-1))
         else
            write(PadRight('---', WIDTH-1));
      end;
      
      repeat
         opt := UpCase(ReadKey);
      until opt in NAVKEYS + EXITKEYS + [KEY_DELETE, KEY_ENTER] + specialkeys;
      
      if opt in [KEY_ENTER, KEY_GET] then begin
         if (opt = KEY_GET) then begin
            v := fn();
            self.AddToList(ind, v, n);
            opt := KEY_DOWN;
         end else if (n > self.vLists[ind].count) and (opt = KEY_ENTER) then begin
            v := fn();
            self.AppendToList(ind, v);
            opt := KEY_DOWN;
         end else begin
            v := fn();
            self.vLists[ind].data[n] := v;
            opt := KEY_DOWN;
         end;
         self.DoAutoSave;
      end;
      
      case opt of
         KEY_DELETE :
            begin
               self.DeleteFromList(ind, n);
               self.Show(ind, X, Y);
               if self.vLists[ind].count < HEIGHT-2 then begin
                  GotoXY(X, Y+self.vLists[ind].count+3);
                  write(DupeString(' ', WIDTH));
               end;
               self.DoAutoSave;
            end;
         
         KEY_UP :
            begin
               Dec(n);
               if n <= 0 then begin
                  n := self.vLists[ind].count+1;
                  if Min(HEIGHT-2, self.vLists[ind].count+1) = HEIGHT-2 then
                     init := n - HEIGHT + 3
                  else
                     init := 1;
               end else begin
                  if n < init then
                     Dec(init);
               end;
            end;
            
         KEY_DOWN :
            begin
               Inc(n);
               if n > self.vLists[ind].count+1 then begin
                  n := 1;
                  init := 1;
               end else begin
                  if n > init + HEIGHT - 3 then
                     Inc(init);
               end;
            end;
      end;
   until opt in EXITKEYS + specialkeys;
   
   self.Show(ind, X, Y);
   
   NavigateThroughList := opt;
   if (opt = KEY_SELECT) or (count <= self.vLists[ind].count) then
      value := self.vLists[ind].data[n]
   else
      value := 0.0;
end;


function TListMgr.Panel(const X, Y : word; out value : Double; fn : TFnListName; fnv : TFnListValue; fnform : TFnListName; fnexc : TProcException; par : TExprEvaluater; const NUMCOLS : byte = 4) : char;

const NEWLISTMSG = '<New>';

var xx : LongInt = 0;
    yy : word;
    i  : word;
    opt : char = #0;
    init : word = 0;
    listname : string = '';
    form : string = '';

begin
   repeat
      for yy := Y to Y + HEIGHT do begin
         GotoXY(X, yy);
         write(DupeString(' ', WIDTH*NUMCOLS));
      end;
      
      for i := init to init + Min(NUMCOLS - 1, self.vCount { - 1 }) do begin
         if i < self.vCount then
            self.Show(i, X+(i-init)*WIDTH, Y)
         else begin
            TextColor(15);
            GotoXY(X+(i-init)*WIDTH, Y);
            write(NEWLISTMSG);
            TextColor(7);
         end;
      end;
      
      repeat
         if xx < self.vCount then
            opt := self.NavigateThroughList(xx, X+(xx-init)*WIDTH, Y, value, fnv, true)
         else begin
            TextBackground(7);
            TextColor(16);
            GotoXY(X+(xx-init)*WIDTH, Y);
            write(NEWLISTMSG);
            TextColor(7);
            TextBackground(0);
            opt := UpCase(ReadKey);
            if opt = KEY_SELECT then
               opt := #0;
         end;
      until opt in NAVKEYS + EXITKEYS + [KEY_LIST, KEY_DELETE, KEY_DELLIST, KEY_ENTER, KEY_SELECT, KEY_EDITLIST, KEY_FORMULA, KEY_CLEARLIST];
      
      if (opt = KEY_EDITLIST) and (xx < self.vCount) then begin
         listname := fn();
         self.vLists[xx].id := UpCase(listname);
         opt := #0;
         self.DoAutoSave;
         
      end else if (opt = KEY_LIST) or ((opt = KEY_ENTER) and (xx >= self.vCount)) then begin
         listname := fn();
         self.CreateNewList(listname);
         xx := self.vCount - 1 + 1;
         if Min(NUMCOLS, self.vCount+1) = NUMCOLS then
            init := xx - NUMCOLS + 1
         else
            init := 0;
         opt := #0;
         self.DoAutoSave;
         
      end else if (opt = KEY_DELLIST) and (xx < self.vCount) then begin
         self.DeleteList(self.vLists[xx].id);
         if xx > self.vCount-1+1 then
            opt := KEY_LEFT;
         self.DoAutoSave;
       end;
      
      case opt of
         KEY_CLEARLIST :
            begin
               listname := self.vLists[xx].id;
               self.DeleteList(xx);
               self.CreateNewList(listname);
            end;
         
         KEY_FORMULA :
            begin
               if xx < self.vCount then begin
                  form := fnform();
                  
                  (* Analyse formula:
                        1. Get names of lists in formula;
                        2. Compare their dimensions;
                        3. For each index, replace in expression for its value;
                           a. Compute;
                           b. Append to list. *)
                  self.ProcessFormula(self.vLists[xx].id, form, par, fnexc);
               end;
            end;
         
         KEY_LEFT :
            begin
               Dec(xx);
               if xx < 0 then begin
                  xx := self.vCount - 1 + 1;
                  if Min(NUMCOLS, self.vCount+1) = NUMCOLS then
                     init := xx - NUMCOLS + 1 { + 1 }
                  else
                     init := 0;
               end else begin
                  if xx < init then
                     Dec(init);
               end;
            end;
         
         KEY_RIGHT :
            begin
               Inc(xx);
               if xx > self.vCount - 1 + 1 then begin
                  xx := 0;
                  init := 0;
               end else begin
                  if xx > init + NUMCOLS - 1 { + 1 } then
                     Inc(init);
               end;
            end;
      end;
      
   until opt in [KEY_ENTER, KEY_ESC, KEY_SELECT];
   
   Panel := opt;
   if opt <> KEY_SELECT then
      value := 0.0;
end;


procedure TListMgr.ProcessFormula(id : string; const form : string; par : TExprEvaluater; fnexc : TProcException);
var
   i : word;
   _extract : boolean = false;
   temp : string = '';
   givennames : TListsNames = nil;
   dim : LongInt = -1;
   _valid : boolean;
   expression : string;
   res : float;

begin
   try
      if LowerCase(form) = 'cancel' then
         exit;
      
      // Extract names
      for i := 1 to Length(form) do begin
         if form[i] = LIST_IDENTIFIER[1] then begin
            _extract := true;
            continue;
         end;
         
         if _extract then begin
            if form[i] = LIST_IDENTIFIER[4] then begin
               _extract := false;
               SetLength(givennames, Succ(Length(givennames)));
               givennames[High(givennames)] := temp;
               temp := '';
            end else
               temp := temp + form[i];
            
         end;
         
         if (i = Length(form)) and _extract and (form[i] <> LIST_IDENTIFIER[4]) then
            raise Exception.Create('Unexpected end of formula');
      end;
      
      if Length(givennames) = 0 then
         raise Exception.Create('No lists were defined');
      
      // Verify names and compare dimensions
      for temp in givennames do begin
         if not self.IDExists(temp) then
            raise Exception.Create('List ' + temp + ' does not exist')
         else begin
            if dim = -1 then
               dim := self[temp].count
            else if self[temp].count <> dim then
               raise Exception.Create('Dimension mismatch (with list ' + temp + ')');
         end;
      end;
      
      // Everything's ok, lets do some math...
      self.DeleteList(id);
      self.CreateNewList(id);
      try
         for i := 1 to dim do begin
            // Replace identifier with value
            expression := form;
            for temp in givennames do
               expression := AnsiReplaceText(expression, Format(LIST_IDENTIFIER, [temp]), FloatToStrF(self.Elements[temp, i], ffGeneral, 15, 1));
            
            _valid := par(expression, res);
            if _valid then
               self.AppendToList(id, Double(res))
            else
               raise Exception.Create('Expression "' + expression + '" is invalid');
         end;
      except
         on ex : Exception do begin
            self.DeleteList(id);
            self.CreateNewList(id);
            raise ex;
         end;
      end;
      
   except
      on ex : Exception do
         fnexc(ex);
   end;
end;



initialization
   listmgr := TListMgr.Create;
   // load from memory

finalization
   listmgr.Free;

end.
