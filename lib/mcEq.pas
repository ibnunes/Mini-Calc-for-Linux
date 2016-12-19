(* === mcEq ===
 * Ver: 1.0.2
 *  By: Igor Nunes
 * Management of functions; routines for equations. *)

{$mode objfpc}
unit mcEq;

interface
uses crt, sysutils, strutils, math, classes, fpexprpars,
     UMath, mcIO;

const
   KEY_FN_SELECT  = 'T';  // table
   KEY_FN_GRAPH   = 'G';
   KEY_FN_VISIBLE = 'V';  // for plotter
   KEY_FN_DELETE  = #8 ;
   KEY_FN_EDIT    = #13;
   KEY_FN_EXIT    = #27;
   KEY_FN_UP      = #72;
   KEY_FN_DOWN    = #80;

type
   TKeySet = set of char;
   TFnGetName = procedure (out name : string);
   TFnMgr = class (classes.TStringList) // Class Interceptor
      protected
         type
            TTableDef = record
               init : real;
               step : real;
            end;
         
         const
            DEFAULT_TABLE_HEIGHT = 13;  // table height
            DEFAULT_TABLE_WIDTH  = 33;  // total table width
            DEFAULT_TABLE_COLUMN = 15;  // column width
            DEFAULT_TABLEDEF : TTableDef =
               (
                  init : 0.0;
                  step : 1.0;
               );
         
         var
            tabledef : TTableDef;
         
         function Compute(idx : word; x : float; out excpt : boolean; par : TFPExpressionParser = nil) : float;
         
      public
         constructor CreateNew;
         
         function NavigateThroughFunctions(const X, Y : word; var idx : LongInt; fn : TFnGetName; const EXTRAKEYS : TKeySet; par : TFPExpressionParser) : char;
         function Table(const X, Y : word; const idx : LongInt; par : TFPExpressionParser) : char;
         procedure ClearTable(const X, Y : word);
   end;

procedure EvaluateQuadratic(const a, b, c : Double; out d : Double; out x1, x2 : TComplex; out vx, vy : Double);

var mcFunctions : TFnMgr;


implementation

procedure EvaluateQuadratic(const a, b, c : Double; out d : Double; out x1, x2 : TComplex; out vx, vy : Double);
begin
   d := Sqr(b) - 4*a*c;
   
   vx := -b/(2*a);
   vy := -d/(4*a);
   
   if d >= 0 then begin
      x1 := (-b + Sqrt(d)) / (2*a);
      x2 := (-b - Sqrt(d)) / (2*a);
   end else begin
      x1.Re := vx;
      x2.Re := vx;
      x1.Im := Sqrt(Abs(d))/(2*a);
      x2.Im := -Sqrt(Abs(d))/(2*a);
   end;
end;


constructor TFnMgr.CreateNew;
begin
   self.tabledef := DEFAULT_TABLEDEF;
   inherited Create;
end;


function TFnMgr.Compute(idx : word; x : float; out excpt : boolean; par : TFPExpressionParser = nil) : float;
var identifier: TFPExprIdentifierDef;
    eng_defined : boolean;

begin
   eng_defined := (par <> nil);
   
   if not eng_defined then begin
      par := TFPExpressionParser.Create(nil);
      par.Builtins := [bcMath];
   end;
   
   try
      identifier := par.Identifiers.IdentifierByName('x');
      identifier.AsFloat := x;
   except
      identifier := par.Identifiers.AddFloatVariable('x', x);
   end;
   
   try
      par.Expression := self[idx];
      Compute := par.Evaluate.ResFloat;
      excpt := false;
   except
      Compute := 0.0;
      excpt := true;
   end;
   
   if not eng_defined then
      par.Destroy;
end;


function TFnMgr.NavigateThroughFunctions(const X, Y : word; var idx : LongInt; fn : TFnGetName; const EXTRAKEYS : TKeySet; par : TFPExpressionParser) : char;
(* NOTE: in the future, if we need more functions, it will have do work as the VLM. *)

const
   WIDTH  = 38;
   { HEIGHT = 10; }  // Read initial NOTE.
   NAVKEYS = [KEY_FN_SELECT, KEY_FN_DELETE, KEY_FN_EDIT, KEY_FN_EXIT, KEY_FN_UP, KEY_FN_DOWN];

var
   opt : char = #0;
   { init : word = 0; }  // Read initial NOTE.
   i : word;
   equation : string;

begin
   idx := 0;
   
   repeat      
      for i := 0 to pred(self.count) do begin
         if i = idx then begin
            TextBackground(4);
            TextColor(15);
         end else begin
            TextBackground(8);
            TextColor(15);
         end;
         
         GotoXY(X, Y+i);
         write(AnsiLeftStr(PadRight('Y' + IntToStr(i) + '= ' + self[i], WIDTH), WIDTH));
      end;
      
      repeat
         opt := UpCase(ReadKey);
      until opt in NAVKEYS + EXTRAKEYS;
      
      case opt of
         KEY_FN_DOWN :
            begin
               Inc(idx);
               if idx > pred(self.count) then
                  idx := 0;
            end;
         
         KEY_FN_UP :
            begin
               Dec(idx);
               if idx < 0 then
                  idx := pred(self.count);
            end;
         
         KEY_FN_DELETE :
            begin
               self[idx] := '';
            end;
         
         KEY_FN_EDIT :
            begin
               fn(equation);
               
               try
                  par.Expression := equation;
                  par.Evaluate;
               except
                  on ex : EExprScanner do
                     equation := '';
                  on ex : EExprParser do
                     equation := '';
                  on ex : Exception do    // Without this, unhandled exceptions crash the entire program
                     equation := '';
               end;
               
               if equation <> '' then begin
                  self[idx] := equation;
               end;
            end;
      end;
      
   until opt in [KEY_FN_EXIT, KEY_FN_SELECT] + EXTRAKEYS;
   
   NavigateThroughFunctions := opt;
end;




function TFnMgr.Table(const X, Y : word; const idx : LongInt; par : TFPExpressionParser) : char;

   procedure SetColors(i, j : byte);
   begin
      if i = j then begin
         TextColor(15);
         TextBackground(4);
      end else begin
         TextColor(7);
         TextBackground(0);
      end;
   end;

   procedure EditTableDefs(var c : word);
   var
      key   : char= #0;
      count : byte;
   
   begin
      count := 1;
      GotoXY(X, Y + DEFAULT_TABLE_HEIGHT + 2);
      TextBackground(0);
      TextColor(15);
      write('TABLE SETTINGS');
      GotoXY(X, Y + DEFAULT_TABLE_HEIGHT + 3);
      TextColor(8);
      write(PadLeft('Enter = edit | Esc = apply', DEFAULT_TABLE_WIDTH));
      
      repeat
         GotoXY(X, Y + DEFAULT_TABLE_HEIGHT + 4);
         SetColors(1, count);
         write(PadRight('Start = ' + FloatToStrF(self.tabledef.init, ffGeneral, DEFAULT_TABLE_WIDTH-8, 1), DEFAULT_TABLE_WIDTH));
         
         GotoXY(X, Y + DEFAULT_TABLE_HEIGHT + 5);
         SetColors(2, count);
         write(PadRight(' Step = ' + FloatToStrF(self.tabledef.step, ffGeneral, DEFAULT_TABLE_WIDTH-8, 1), DEFAULT_TABLE_WIDTH));
         
         repeat
            key := UpCase(ReadKey);
         until key in [KEY_FN_DOWN, KEY_FN_UP, KEY_FN_EDIT, KEY_FN_EXIT];
         
         case key of
            KEY_FN_DOWN, KEY_FN_UP :
               if count = 1 then
                  count := 2
               else
                  count := 1;
            
            KEY_FN_EDIT :
               case count of
                  1: begin
                        repeat
                           GotoXY(X+8, Y + DEFAULT_TABLE_HEIGHT + 4);
                           SetColors(1, count);
                           write(DupeString(' ', DEFAULT_TABLE_WIDTH-8));
                           
                           GotoXY(X+8, Y + DEFAULT_TABLE_HEIGHT + 4);
                        until SecureRead.TReal('', self.tabledef.init);
                        c := 0;
                     end;
                  
                  2: begin
                        repeat
                           GotoXY(X+8, Y + DEFAULT_TABLE_HEIGHT + 5);
                           SetColors(2, count);
                           write(DupeString(' ', DEFAULT_TABLE_WIDTH-8));
                           
                           GotoXY(X+8, Y + DEFAULT_TABLE_HEIGHT + 5);
                        until SecureRead.TReal('', self.tabledef.step);
                     end;
               end;
         end;
         
      until (key = KEY_FN_EXIT);
      
      SetColors(1, 2);  // forces general case
      for count := 2 to 5 do begin
         GotoXY(X, Y + DEFAULT_TABLE_HEIGHT + count);
         write(DupeString(' ', DEFAULT_TABLE_WIDTH));
      end;
   end;

const
   NAVKEYS = [KEY_FN_EDIT, KEY_FN_EXIT, KEY_FN_UP, KEY_FN_DOWN];

var
   i       : word;     // line of table (index i)
   count_i : word;     // highlighted index i
   opt     : char;
   err     : boolean;
   res     : Float;

begin
   count_i := 0;
   
   GotoXY(X, Y);
   // TextBackGround();
   TextColor(14);
   write(PadRight(' x', DEFAULT_TABLE_COLUMN), ' | ', PadRight(' Y' + IntToStr(idx), DEFAULT_TABLE_COLUMN));
   
   repeat
      for i := 0 to DEFAULT_TABLE_HEIGHT-1 do begin
         SetColors(i, count_i);
         
         res := self.Compute(idx, self.tabledef.init + self.tabledef.step*i, err, par);
         
         GotoXY(X, Y+i+1);
         write(LeftStr(PadRight(FloatToStrF(self.tabledef.init + self.tabledef.step*i, ffGeneral, DEFAULT_TABLE_COLUMN, 1), DEFAULT_TABLE_COLUMN), DEFAULT_TABLE_COLUMN) + ' | ' + LeftStr(PadRight(IfThen(err, '[ERR]', FloatToStrF(res, ffGeneral, DEFAULT_TABLE_COLUMN, 1)), DEFAULT_TABLE_COLUMN), DEFAULT_TABLE_COLUMN));
      end;
      
      repeat
         opt := UpCase(ReadKey);
      until opt in NAVKEYS;
      
      case opt of
         KEY_FN_DOWN :
            begin
               if count_i < DEFAULT_TABLE_HEIGHT-1 then
                  Inc(count_i)
               else
                  self.tabledef.init := self.tabledef.init + self.tabledef.step;
            end;
         
         KEY_FN_UP :
            begin
               if count_i > 0 then 
                  Dec(count_i)
               else
                  self.tabledef.init := self.tabledef.init - self.tabledef.step;
            end;
         
         KEY_FN_EDIT :
            begin
               EditTableDefs(count_i);
            end;
      end;
      
   until opt in [KEY_FN_EXIT];
   
   self.tabledef.init := self.tabledef.init + self.tabledef.step*count_i;
   Table := opt;
end;


procedure TFnMgr.ClearTable(const X, Y : word);
var i : word;
begin
   for i := 0 to DEFAULT_TABLE_HEIGHT do begin
      GotoXY(X, Y+i);
      write(DupeString(' ', DEFAULT_TABLE_WIDTH));
   end;
end;



var i : byte;  // for initialization and finalization

initialization
   mcFunctions := TFnMgr.CreateNew;
   mcFunctions.Sorted := false;
   // mcFunctions.OwnsObjects := true;
   for i := 0 to 9 do
      mcFunctions.Append('');


finalization
   mcFunctions.Destroy;

end.
