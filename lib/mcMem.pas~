(* === mcMem ===
 * Ver: 1.1.0
 *  By: Igor Nunes
 * Memory Manager for Mini Calc 4 - not for VLM nor Plotter! *)

{$mode objfpc}
unit mcMem;

interface
uses crt, math, sysutils, strutils, classes, fpexprpars,
     mcEq, mcVLM, mcGraph;

type
   TKeySet = set of char;
   TVarDef = record
      id   : ShortString;
      case rt : TResultType of
         rtInteger : (datai : Int64);
         rtFloat   : (dataf : Double);
         rtString  : (datas : string);
   end;
   TFloatMem = array of TVarDef;
   TSetTypes = set of TResultType;
   
   TMemMgr = class(TObject)
      public
         class procedure SaveToFile(const FNAME : string; par : TFPExpressionParser); static;
         class procedure LoadFromFile(const FNAME : string; par : TFPExpressionParser); static;
         
         class procedure SaveToFile(const FNAME : string; fn : TFnMgr); overload; static;
         class procedure LoadFromFile(const FNAME : string; fn : TFnMgr); overload; static;
         
         class function ParserCountVars(par : TFPExpressionParser; const rtTYPES : TSetTypes; out maxlen : word; out init : LongInt) : LongInt; static;
         
         (* VLM takes care of itself *)
         (* Plotter takes care of itself *)
         
         class procedure ShowVars(const X, Y, HEIGHT : word; par : TFPExpressionParser; const rtTYPES : TSetTypes; const CONSTANT_PREFIX : char = '$'); static;
         class procedure ResetVars(par : TFPExpressionParser; const rtTYPES : TSetTypes; const CONSTANT_PREFIX : char = '$'); static;
         
         class function ShowListsInfo(const X, Y, HEIGHT : word; lmgr : TListMgr; var count_i : LongInt; EXTRAKEYS : TKeySet = []; const COMPLETE : boolean = true; const ALLOWDEL : boolean = true) : char; static;
   end;
   
   memory = TMemMgr;  // for compatibility with preliminary code of Mini Calc



implementation

procedure SetColors(i, j : byte);
begin
   if i = j then begin
      TextColor(15);
      TextBackground(4);
   end else begin
      TextColor(7);
      TextBackground(8);
   end;
end;


class procedure TMemMgr.SaveToFile(const FNAME : string; par : TFPExpressionParser);
var
   f    : file of TVarDef;
   elem : TFPExprIdentifierDef;
   fvar : TVarDef;
   i    : LongInt;

begin
   Assign(f, FNAME);
   Rewrite(f);
   
   i := 0;
   while true do begin
      try
         elem := par.Identifiers[i];
         
         if (elem.IdentifierType = itVariable) and (elem.ResultType in [rtFloat, rtInteger, rtString]) then begin
            fvar.id := elem.Name;
            fvar.rt := elem.ResultType;
            case elem.ResultType of
               rtFloat   : fvar.dataf := elem.AsFloat;
               rtInteger : fvar.datai := elem.AsInteger;
               rtString  : fvar.datas := elem.AsString;
            end;
            write(f, fvar);
         end;
      except
         break;
      end;
      Inc(i);
   end;
   
   Close(f);
end;


class procedure TMemMgr.LoadFromFile(const FNAME : string; par : TFPExpressionParser);
var
   f    : file of TVarDef;
   elem : TFPExprIdentifierDef;
   fvar : TVarDef;

begin
   if FileExists(FNAME) then begin
      Assign(f, FNAME);
      Reset(f);
      
      while not eof(f) do begin
         read(f, fvar);
         try
            elem := par.Identifiers.IdentifierByName(fvar.id);
            case TResultType(fvar.rt) of
               rtFloat   : elem.AsFloat := fvar.dataf;
               rtInteger : elem.AsInteger := fvar.datai;
               rtString  : elem.AsString := fvar.datas;
            end;
         except
            case TResultType(fvar.rt) of
               rtFloat   : par.Identifiers.AddFloatVariable(fvar.id, fvar.dataf);
               rtInteger : par.Identifiers.AddFloatVariable(fvar.id, fvar.datai);
               rtString  : par.Identifiers.AddStringVariable(fvar.id, fvar.datas);
            end;
         end;
      end;
      
      Close(f);
   end;
end;


class procedure TMemMgr.SaveToFile(const FNAME : string; fn : TFnMgr); overload;
begin
   fn.SaveToFile(FNAME);
end;


class procedure TMemMgr.LoadFromFile(const FNAME : string; fn : TFnMgr); overload;
begin
   if FileExists(FNAME) then
      fn.LoadFromFile(FNAME);
end;


class function TMemMgr.ParserCountVars(par : TFPExpressionParser; const rtTYPES : TSetTypes; out maxlen : word; out init : LongInt) : LongInt;
var
   elem : TFPExprIdentifierDef;
   i    : LongInt;

begin
   i := 0;
   maxlen := 0;
   ParserCountVars := 0;
   init := -1;
   
   while true do begin
      try
         elem := par.Identifiers[i];
         if (elem.IdentifierType = itVariable) and (elem.ResultType in rtTYPES) then begin
            if init = -1 then
               init := i;
            Inc(ParserCountVars);
            if Length(elem.Name) > maxlen then begin
               maxlen := Length(elem.Name);
            end;
         end;
      except
         break;
      end;
      Inc(i);
   end;
end;


class procedure TMemMgr.ResetVars(par : TFPExpressionParser; const rtTYPES : TSetTypes; const CONSTANT_PREFIX : char = '$');
var
   elem : TFPExprIdentifierDef;
   i    : LongInt;

begin
   i := 0;
   
   while true do begin
      try
         elem := par.Identifiers[i];
         if (elem.IdentifierType = itVariable) and (elem.ResultType in rtTYPES) and (elem.Name[Length(elem.Name)] <> CONSTANT_PREFIX) then begin
            case elem.ResultType of
               rtFloat   : elem.AsFloat := 0.0;
               rtInteger : elem.AsInteger := 0;
               rtString  : elem.AsString := '';
            end;
         end;
      except
         break;
      end;
      Inc(i);
   end;
end;


class procedure TMemMgr.ShowVars(const X, Y, HEIGHT : word; par : TFPExpressionParser; const rtTYPES : TSetTypes; const CONSTANT_PREFIX : char = '$');

const
   KEY_EXIT = #27;
   KEY_UP   = #72;
   KEY_DOWN = #80;
   KEY_DEL  = #8;
   NAVKEYS = [KEY_DEL, KEY_EXIT, KEY_UP, KEY_DOWN];

var
   i        : word;         // line of table (index i)
   count_i  : LongInt = 0;  // highlighted index i
   init     : word = 0;
   INITINDEX : LongInt = 0;
   opt      : char = #0;
   elem     : TFPExprIdentifierDef;
   VARCOUNT : LongInt;
   MAXLEN   : word = 0;

begin
   VARCOUNT := TMemMgr.ParserCountVars(par, rtTYPES, MAXLEN, INITINDEX);
   
   if MAXLEN < 5 then
      MAXLEN := 5;
   
   TextBackground(7);
   TextColor(0);
   GotoXY(X, Y);
   write(PadLeft(' VAR', MAXLEN+1), PadRight('   VALUE ', 23));
   SetColors(1, 2);
   write(' (count = ', VARCOUNT, ')');
   
   repeat
      for i := 0 to Min(VARCOUNT-1, HEIGHT) do begin
         elem := par.Identifiers[i+INITINDEX+init];
         SetColors(init + i, count_i);
         
         GotoXY(X, Y+i+1);
         if (elem.IdentifierType = itVariable) and (elem.ResultType in rtTYPES) then begin
            write(PadLeft(elem.Name, MAXLEN+1), ' = ');
            
            case elem.ResultType of
               rtFloat   : write(LeftStr(PadRight(FloatToStrF(elem.AsFloat, ffGeneral, 15, 1), 35), 35));
               rtInteger : write(LeftStr(PadRight(IntToStr(elem.AsInteger), 35), 35));
               rtString  : write(LeftStr(PadRight(elem.AsString, 35), 35));
            end;
         end;
      end;
      SetColors(1, 2);
      
      repeat
         opt := UpCase(ReadKey);
      until opt in NAVKEYS;
      
      case opt of
         KEY_UP :
            begin
               Dec(count_i);
               if count_i < 0 then begin
                  count_i := VARCOUNT-1;
                  if Min(VARCOUNT, HEIGHT) = HEIGHT then
                     init := count_i - HEIGHT
                  else
                     init := 0;
               end else begin
                  if count_i < init then
                     Dec(init);
               end;
            end;
         
         KEY_DOWN :
            begin
               Inc(count_i);
               if count_i > VARCOUNT-1 then begin
                  count_i := 0;
                  init := 0;
               end else begin
                  if count_i > init + HEIGHT then
                     Inc(init);
               end;
            end;
         
         KEY_DEL :
            begin
               elem := par.Identifiers[count_i];
               if elem.Name[Length(elem.name)] <> CONSTANT_PREFIX then begin
                  case elem.ResultType of
                     rtFloat   : elem.AsFloat := 0.0;
                     rtInteger : elem.AsInteger := 0;
                     rtString  : elem.AsString := '';
                  end;
               end;
            end;
      end;
      
   until opt = KEY_EXIT;
   
   SetColors(1, 2);
   for i := 0 to HEIGHT+1 do begin
      GotoXY(X, Y+i);
      clreol;
   end;
end;


class function TMemMgr.ShowListsInfo(const X, Y, HEIGHT : word; lmgr : TListMgr; var count_i : LongInt; EXTRAKEYS : TKeySet = []; const COMPLETE : boolean = true; const ALLOWDEL : boolean = true) : char;

var
   MAXLEN : word = 0;
   DIFF : word = 0;

   function GetMaxLen : word;
   var i : word;
   begin
      GetMaxLen := 0;
      for i := 0 to lmgr.ListCount-1 do
         if Length(lmgr.Lists[i].id) > GetMaxLen then
            GetMaxLen := Length(lmgr.Lists[i].id);
   end;

   procedure ClearArea;
   var i : word;
   begin
      SetColors(1, 2);
      for i := DIFF to HEIGHT do begin
         GotoXY(X, Y+i);
         clreol;
      end;
   end;
   
   procedure WriteHeader;
   begin
      TextBackground(7);
      TextColor(0);
      GotoXY(X, Y);
      write(PadLeft(' LIST', MAXLEN+1), PadRight('   ELEMENTS ', 23));
   end;
   
   function NoLists : boolean;
   begin
      if lmgr.ListCount = 0 then begin
         WriteHeader;
         TextBackground(0);
         TextColor(7);
         NoLists := true;
         GotoXY(X, Y+2);
         write('No lists.');
         repeat until ReadKey = #13;
         DIFF := 0;
         ClearArea;
      end else
         NoLists := false;
   end;

const
   KEY_EXIT = #27;
   KEY_UP   = #72;
   KEY_DOWN = #80;
   KEY_DEL  = #8;
   NAVKEYS = [KEY_DEL, KEY_EXIT, KEY_UP, KEY_DOWN];

var
   i    : word;         // line of table (index i)
   init : word = 0;
   opt  : char = #0;

begin
   if NoLists then Exit;
   
   MAXLEN := GetMaxLen;
   if COMPLETE then begin
      if MAXLEN < 4 then
         MAXLEN := 4;
      (* TextBackground(7);
      TextColor(0);
      GotoXY(X, Y);
      write(PadLeft(' LIST', MAXLEN+1), PadRight('   ELEMENTS ', 23)); *)
      WriteHeader;
      DIFF := 2;
   end;
   
   repeat
      if COMPLETE then begin
         SetColors(1, 2);
         GotoXY(X+MAXLEN+1+23, Y);
         write(' (count = ', lmgr.ListCount, ')');
      end;
      
      if NoLists then break;
      
      for i := 0 to Min(lmgr.ListCount-1, HEIGHT) do begin
         SetColors(init + i, count_i);
         GotoXY(X, Y+i+DIFF);
         write(PadLeft(lmgr.Lists[i].id, MAXLEN+1) + ' ' + IfThen(COMPLETE, '  ' + PadRight(IntToStr(lmgr.Lists[i].count) + ' of 1000', 15), ''));
      end;
      SetColors(1, 2);
      
      repeat
         opt := UpCase(ReadKey);
      until opt in NAVKEYS + EXTRAKEYS;
      
      case opt of
         KEY_UP :
            begin
               Dec(count_i);
               if count_i < 0 then begin
                  count_i := lmgr.ListCount-1;
                  if Min(lmgr.ListCount, HEIGHT) = HEIGHT then
                     init := count_i - HEIGHT
                  else
                     init := 0;
               end else begin
                  if count_i < init then
                     Dec(init);
               end;
            end;
         
         KEY_DOWN :
            begin
               Inc(count_i);
               if count_i > lmgr.ListCount-1 then begin
                  count_i := 0;
                  init := 0;
               end else begin
                  if count_i > init + HEIGHT then
                     Inc(init);
               end;
            end;
         
         KEY_DEL :
            begin
               if ALLOWDEL then begin
                  lmgr.DeleteList(count_i);
                  ClearArea;
               end;
               if count_i > lmgr.ListCount-1 then
                  count_i := lmgr.ListCount-1;
            end;
      end;
      
   until opt in [KEY_EXIT] + EXTRAKEYS;
   
   DIFF := 0;
   ClearArea;
   ShowListsInfo := opt;
end;

end.