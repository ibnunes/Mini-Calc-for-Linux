(* === mcGraph ===
 * Ver: 1.1.0
 *  By: Igor Nunes
 * Plotter. *)

(* IMPORTANT: I still cannot use the constants and variables to define the lines color and style. It must be hard-coded.
The base of the necessary code to use constants and variables is implemented but commented.
For now, each function has a predefined color given by a nested function. However, the goal is it to be defined by the user/developer. *)

{$unitpath /wingraph}
{$mode objfpc}
unit mcGraph;

interface
uses sysutils, math, types, classes, wingraph, fpexprpars,
     UWinBMP;

type
   TLineStyle = record
      visible : boolean;
      { color : word;
      style : word;
      thick : word; }
   end;
   
   (* TMarkStyle = record
      visible : boolean;
      // color : word;
      { Add:
         - form (square, triangle, inverted triangle, rhombus/diamond, ...)
         - fill style (empty, full)
         - line style
         - size }
   end; *)
   
   { TPlotStyle = record
      line : TLineStyle;
      mark : TMarkStyle;
   end; }
   
   TZoom = record
      xMin, xMax, xScl : real;
      yMin, yMax, yScl : real;
   end;
   
   TFnPoint = record
      x, y  : word;
      valid : boolean;
   end;
   
   TEquationDef = record
      expression : string;
      points     : array of TFnPoint;
      line       : TLineStyle;
   end;
   
   TEquationList = array of TEquationDef;
   
   { TPlotData = array of double;
   TPlotDef = record
      data  : TPlotData;
      point : array of TFnPoint;
      style : TPlotStyle;
   end;
   
   TPlotList = array of TPlotDef; }
   
   TAutoSaveDef = record
      Status   : boolean;
      FileName : record
         Definitions : string;
         Equations : string;
      end;
   end;
   
   TPlotter = class(TObject)
      private
         type
            TDefSave = record
               grid : TLineStyle;
               axes : TLineStyle;
               zoom : TZoom;
               size : TSize;
            end;
            TEqSave = record
               expr : string;
               line : TLineStyle;
            end;
         
         const
            WINSCALE = 1.2;
            DEFAULT_LINE : TLineStyle =
               (
                  visible : true;
                  { color : 172;
                  style : DoubleWidth;
                  thick : SolidLn; }
               );
            DEFAULT_AXES : TLineStyle =
               (
                  visible : true;
                  { color : 250;
                  style : NormWidth;
                  thick : SolidLn; }
               );
            DEFAULT_GRID : TLineStyle =
               (
                  visible : true;
                  { color : 103;
                  style : NormWidth;
                  thick : DashedLn; }
               );
         
         var
            vZoom : TZoom;
            vSize : TSize;
            vWindow : TSize;
            vAxes : TLineStyle;
            vGrid : TLineStyle;
            vPar  : TFPExpressionParser;
            vWinVisible : boolean;
         
      
      protected
         procedure ProcessData;
         
         procedure SetZoom(z : TZoom);
         procedure SetSize(s : TSize);
         
         procedure SetNumFn(n : word);
         function GetNumFn : word;
         
         procedure DrawFunctions;
         procedure DrawPlotter;
         
      
      public
         var
            Equation : TEquationList;
            AutoSave : TAutoSaveDef;
         
         constructor Create(sx, sy : word);
         
         procedure SetFunction(idx : word; expr : string { ; ln : TLineStyle } );
         { procedure SetFunction(idx : word; expr : string); overload; }
         
         procedure ShowWindow;
         procedure RefreshWindow;
         procedure HideWindow;
         function SaveWindow : boolean;
         
         procedure SaveToFile(const FNAME_DEF, FNAME_EQ : string);
         procedure LoadFromFile(const FNAME_DEF, FNAME_EQ : string; const FORCEINITSIZE : boolean = true; const INITSIZE : word = 10);
         procedure DoAutoSave;
         
         property NumberOfFunction : word read GetNumFn write SetNumFn;
         property Parser : TFPExpressionParser write vPar;
         property Zoom : TZoom read vZoom write SetZoom;
         property Size : TSize read vSize write SetSize;
         property Axes : boolean read vAxes.visible write vAxes.visible;
         property Grid : boolean read vGrid.visible write vGrid.visible;
         property WindowVisible : boolean read vWinVisible;
   end;

var
   plotter : TPlotter;

const
   DEFAULT_PLOTTER_SIZE : TSize =
      (cx : 600;
       cy : 500);
   DEFAULT_PLOTTER_ZOOM : TZoom =
       (xMin : -10.0;
        xMax :  10.0;
        xScl :   1.0;
        yMin : -10.0;
        yMax :  10.0;
        yScl :   1.0);


implementation

constructor TPlotter.Create(sx, sy : word);
begin
   self.vSize.cx := sx;
   self.vSize.cy := sy;
   self.vWindow.cx := trunc(WINSCALE * self.vSize.cx);
   self.vWindow.cy := trunc(WINSCALE * self.vSize.cy);
   
   { self.vGrid := DEFAULT_GRID;
   self.vAxes := DEFAULT_AXES; }
   self.vGrid.visible := true;
   self.vAxes.visible := true;
   
   SetLength(self.Equation, 0);
   
   vWinVisible := false;
   
   inherited Create;
end;


procedure TPlotter.SaveToFile(const FNAME_DEF, FNAME_EQ : string);
var f_def : file of TDefSave;
    f_eq  : file of TEqSave;
    e_def : TDefSave;
    e_eq  : TEqSave;
    eq    : TEquationDef;
begin
   // definitions
   Assign(f_def, FNAME_DEF);
   Rewrite(f_def);
   
   with e_def do begin
      zoom := self.vZoom;
      grid := self.vGrid;
      axes := self.vAxes;
      size := self.vSize;
   end;
   
   write(f_def, e_def);
   Close(f_def);
   
   // functions
   Assign(f_eq, FNAME_EQ);
   Rewrite(f_eq);
   
   for eq in self.Equation do begin
      with e_eq do begin
         expr := eq.expression;
         line := eq.line;
      end;
      write(f_eq, e_eq);
   end;
   
   Close(f_eq);
end;


procedure TPlotter.LoadFromFile(const FNAME_DEF, FNAME_EQ : string; const FORCEINITSIZE : boolean = true; const INITSIZE : word = 10);
var f_def : file of TDefSave;
    f_eq  : file of TEqSave;
    e_def : TDefSave;
    e_eq  : TEqSave;
begin
   // Definitions
   if FileExists(FNAME_DEF) then begin
      Assign(f_def, FNAME_DEF);
      Reset(f_def);
      
      read(f_def, e_def);
      with e_def do begin
         self.vZoom := zoom;
         self.vGrid := grid;
         self.vAxes := axes;
         self.vSize := size;
      end;
      
      Close(f_def);
   end else begin
      self.vZoom := DEFAULT_PLOTTER_ZOOM;
      self.vSize := DEFAULT_PLOTTER_SIZE;
      self.vAxes := DEFAULT_AXES;
      self.vGrid := DEFAULT_GRID;
   end;
   
   // write('OK?'); readln;
   // Functions
   if FileExists(FNAME_EQ) then begin
      Assign(f_eq, FNAME_EQ);
      Reset(f_eq);
      
      while not eof(f_eq) do begin
         read(f_eq, e_eq);
         // write('OK?'); readln;
         self.SetNumFn(Succ(self.GetNumFn));
         // SetLength(self.Equation, Succ(Length(self.Equation)));
         with self.Equation[High(self.Equation)] do begin
            expression := e_eq.expr;
            line := e_eq.line;
         end;
      end;
      
      Close(f_eq);
      
      if FORCEINITSIZE then
         self.NumberOfFunction := INITSIZE;
   end else
      self.NumberOfFunction := INITSIZE;
end;


function TPlotter.SaveWindow : boolean;
var FNAME : AnsiString;
begin
   DateTimeToString(FNAME, 'ddmmyyhhnnsszzz', Now);
   FNAME := 'mcGraph_' + FNAME;
   SaveWindow := SaveBMP(FNAME);
end;


procedure TPlotter.DoAutoSave;
begin
   if self.AutoSave.Status and (self.AutoSave.FileName.Equations <> '') and (self.AutoSave.FileName.Definitions <> '') then
      self.SaveToFile(self.AutoSave.FileName.Definitions,
                      self.AutoSave.FileName.Equations);
end;


procedure TPlotter.ProcessData;

   procedure SetX(v : real);
   var p : TFPExprIdentifierDef;
   begin
      p := self.vPar.Identifiers.IdentifierByName('x');
      p.AsFloat := v;
   end;

var eq, i : word;
    p : record
      x, y : real;
    end;

begin
   for eq := Low(self.Equation) to High(self.Equation) do begin
      
      if (self.Equation[eq].expression <> '') then begin
         SetLength(self.Equation[eq].points, Succ(self.vSize.cx));
         
         self.vPar.expression := self.Equation[eq].expression;
         
         for i := Low(self.Equation[eq].points) to High(self.Equation[eq].points) do begin
            
            p.x := self.vZoom.xMin + i * (self.vZoom.xMax - self.vZoom.xMin) / self.vSize.cx;
            SetX(p.x);
            self.Equation[eq].points[i].x := i;
            
            try
               p.y := ArgToFloat(self.vPar.Evaluate);
               self.Equation[eq].points[i].valid := true;
               self.Equation[eq].points[i].y := trunc(((p.y - self.vZoom.yMax) * self.vSize.cy) / (self.vZoom.yMin - self.vZoom.yMax));
            except
               p.y := 0.0;
               self.Equation[eq].points[i].y := 0;
               self.Equation[eq].points[i].valid := false;
            end;
            
         end;
         
      end;
   end;
   
   self.DoAutoSave;
end;


procedure TPlotter.SetZoom(z : TZoom);
begin
   self.vZoom := z;
   if self.GetNumFn > 0 then
      self.ProcessData;
end;


procedure TPlotter.SetSize(s : TSize);
begin
   self.vSize := s;
   self.vWindow.cx := trunc(WINSCALE * self.vSize.cx);
   self.vWindow.cy := trunc(WINSCALE * self.vSize.cy);
   if self.GetNumFn > 0 then
      self.ProcessData;
end;


procedure TPlotter.SetNumFn(n : word);
begin
   SetLength(self.Equation, n);
end;


function TPlotter.GetNumFn : word;
begin
   GetNumFn := Length(self.Equation);
end;


procedure TPlotter.SetFunction(idx : word; expr : string { ; ln : TLineStyle } );
begin
   // if expr <> '' then begin
      if idx > Pred(self.GetNumFn) then
         self.SetNumFn(Succ(idx));
      
      with self.Equation[idx] do begin
         expression := expr;
         { line := ln; }
         line.visible := true;
      end;
   // end;
   
   self.DoAutoSave;
end;


{ procedure TPlotter.SetFunction(idx : word; expr : string); overload;
begin
   self.SetFunction(idx, expr, DEFAULT_LINE);
end; }


procedure TPlotter.DrawFunctions;

   function DefColor(var n : byte) : longword;
   const
      MIN = 1;
      MAX = 5;
   begin
      case n of
         1 : DefColor := OrangeRed;
         2 : DefColor := Cobalt;
         3 : DefColor := Green;
         4 : DefColor := Sangria;
         5 : DefColor := Yellow;
      end;
      
      Inc(n);
      if n > MAX then
         n := MIN;
   end;

var eq : TEquationDef;
    i  : word;
    diff : TPoint;
    c : byte = 1;
begin
   if self.vWinVisible then begin
      
      diff.x := (- self.vSize.cx + self.vWindow.cx) div 2;
      diff.y := (- self.vSize.cy + self.vWindow.cy) div 2;
      
      for eq in self.Equation do begin
         if eq.line.visible then begin
            { SetColor(eq.line.color);
            SetLineStyle(eq.line.style, 0, eq.line.thick); }
            
            SetColor(DefColor(c));
            SetLineStyle(SolidLn, 0, DoubleWidth);
            
            for i := Low(eq.points) to Pred(High(eq.points)) do begin
               if eq.points[i].valid and eq.points[Succ(i)].valid and (eq.points[i].y >= 0) and (eq.points[i].y <= self.vSize.cy) and (eq.points[Succ(i)].y >= 0) and (eq.points[Succ(i)].y <= self.vSize.cy) then
                  Line(diff.x + eq.points[i].x,
                       diff.y + eq.points[i].y,
                       diff.x + eq.points[Succ(i)].x,
                       diff.y + eq.points[Succ(i)].y);
            end;
         end;
      end;
      
   end;
end;


procedure TPlotter.DrawPlotter;
var
   p : record
      x, y : real;
   end;
   diff : TPoint;
   pnt : TPoint;

begin
   if self.vWinVisible then begin
      
      diff.x := (- self.vSize.cx + self.vWindow.cx) div 2;
      diff.y := (- self.vSize.cy + self.vWindow.cy) div 2;
      
      if self.vGrid.visible then begin
         { SetColor(self.vGrid.color);
         SetLineStyle(self.vGrid.style, 0, self.vGrid.thick); }
         
         SetColor(GrayAsparagus);
         SetLineStyle(DottedLn, 0, NormWidth);
         
         // horizontal
         p.y := Min(0, self.vZoom.yMax);
         while p.y >= self.vZoom.yMin do begin
            pnt.y := trunc(((p.y - self.vZoom.yMax) * self.vSize.cy) / (self.vZoom.yMin - self.vZoom.yMax));
            
            Line(diff.x,
                 diff.y + pnt.y,
                 self.vWindow.cx - diff.x,
                 diff.y + pnt.y);
            
            p.y := p.y - self.vZoom.yScl;
         end;
         
         p.y := Max(0, self.vZoom.yMin);
         while p.y <= self.vZoom.yMax do begin
            pnt.y := trunc(((p.y - self.vZoom.yMax) * self.vSize.cy) / (self.vZoom.yMin - self.vZoom.yMax));
            
            Line(diff.x,
                 diff.y + pnt.y,
                 self.vWindow.cx - diff.x,
                 diff.y + pnt.y);
            
            p.y := p.y + self.vZoom.yScl;
         end;
         
         // vertical
         p.x := Max(0, self.vZoom.xMin);
         while p.x <= self.vZoom.xMax do begin
            pnt.x := trunc(((p.x - self.vZoom.xMin) * self.vSize.cx) / (self.vZoom.xMax - self.vZoom.xMin));
            
            Line(diff.x + pnt.x,
                 diff.y,
                 diff.x + pnt.x,
                 self.vWindow.cy - diff.y);
            
            p.x := p.x + self.vZoom.xScl;
         end;
         
         p.x := Min(0, self.vZoom.xMax);
         while p.x >= self.vZoom.xMin do begin
            pnt.x := trunc(((p.x - self.vZoom.xMin) * self.vSize.cx) / (self.vZoom.xMax - self.vZoom.xMin));
            
            Line(diff.x + pnt.x,
                 diff.y,
                 diff.x + pnt.x,
                 self.vWindow.cy - diff.y);
            
            p.x := p.x - self.vZoom.xScl;
         end;
         
      end;
      
      if self.vAxes.visible then begin
         { SetColor(self.vAxes.color);
         SetLineStyle(self.vAxes.style, 0, self.vAxes.thick); }
         
         SetColor(White);
         SetLineStyle(SolidLn, 0, NormWidth);
         
         pnt.x := trunc(((0.0 - self.vZoom.xMin ) * self.vSize.cx) / (self.vZoom.xMax - self.vZoom.xMin));
         pnt.y := trunc(((0.0 - self.vZoom.yMax) * self.vSize.cy) / (self.vZoom.yMin - self.vZoom.yMax));
         
         // horizontal
         if (self.vZoom.yMax >= 0) and (self.vZoom.yMin <= 0) then
            Line(diff.x,
                 diff.y + pnt.y,
                 self.vWindow.cx - diff.x,
                 diff.y + pnt.y);
         
         // vertical
         if (self.vZoom.xMax >= 0) and (self.vZoom.xMin <= 0) then
            Line(diff.x + pnt.x,
                 diff.y,
                 diff.x + pnt.x,
                 self.vWindow.cy - diff.y);
      end;
      
      // Draw border
      SetColor(GrayAsparagus);
      SetLineStyle(SolidLn, 0, NormWidth);
      Rectangle(diff.x,
                diff.y,
                diff.x + self.vSize.cx,
                diff.y + self.vSize.cy);
   end;
end;


procedure TPlotter.ShowWindow;
var driver, mode : smallint;
begin
   if not self.vWinVisible then begin
      self.vWinVisible := true;
      self.ProcessData;
      DetectGraph(driver, mode);
      mode := mCustom;
      SetWindowSize(self.vWindow.cx, self.vWindow.cy);
      InitGraph(driver, mode, 'Mini Calc - Graphical Calculator');
      
      // self.DrawPlotter;
   end;
end;


procedure TPlotter.RefreshWindow;
begin
   if self.vWinVisible then begin
      ClearViewPort;
      self.ProcessData;
      self.DrawPlotter;
      self.DrawFunctions;
   end;
end;


procedure TPlotter.HideWindow;
begin
   if self.vWinVisible then begin
      self.vWinVisible := false;
      CloseGraph;
   end;
end;



initialization
   plotter := TPlotter.Create(DEFAULT_PLOTTER_SIZE.cx, DEFAULT_PLOTTER_SIZE.cy);
   // plotter.Zoom := DEFAULT_PLOTTER_ZOOM;


finalization
   plotter.Free;



end.