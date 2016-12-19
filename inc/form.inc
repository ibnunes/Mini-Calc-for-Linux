(* Auxiliar functions for quadratic formulas. *)

   function FormatQuad(a, b, c : Double; const SHOWZERO : boolean = true) : string;
   var res : string = '';
   begin
      // Analysis parameter-by-parameter
      if a <> 0.0 then begin
         if a = -1.0 then
            res := '-'
         else if a <> 1.0 then
            res := Real2Str(a) + ' * ';
         res := res + 'x^2';
      end;
      
      if b <> 0.0 then begin
         res := res + IfThen((a=0.0), '', ' ');
         if b < 0.0 then
            res := res + '-' + IfThen((a=0.0) and (b=1.0), '', ' ') + IfThen((b<>1.0), Real2Str(Abs(b)) + ' * ', '')
         else if b <> 1.0 then
            res := res + IfThen((a=0.0), '', '+ ') + Real2Str(b) + ' * '
         else
            res := IfThen((a=0.0), '', '+');
         res := res + 'x';
      end;
      
      if c <> 0.0 then begin
         res := res + IfThen((a=0.0) and (b=0.0), '', ' ');
         if c > 0.0 then
            res := res + IfThen((a=0.0) and (b=0.0), '', '+')
         else
            res := res + '-';
         res := res + IfThen((a=0.0) and (b=0.0), '', ' ');
         res := res + Real2Str(Abs(c));
      end;
      
      if SHOWZERO then
         res := res + ' = 0';
      FormatQuad := res;
   end;

(* ------------------------------------------ *)

procedure form_quad;

   procedure GetReal(out v : Double; const prompt : string; const X, Y : word; out canceled : boolean);
   var
      s : string;
      e : word;
   begin
      repeat
         WriteXY(X, Y, prompt, 15);
      until IntelliRead(s, v, e) in [IOreal, IOinteger, IOcommand];
      canceled := (LowerCase(s) = 'cancel');
      _EXITING := (LowerCase(s) = 'exit');
   end;


var
   a, b, c, d : Double;
   x1, x2 : TComplex;
   vx, vy : Double;
   canceled : boolean;
   opt : char = #0;

begin
   repeat
      clrscr;
      WriteXY(1, 7, PadCenter('ax^2 + bx + c = 0', SCREENWIDTH), 15);
      
      WriteXY(2, 2, 'Introduce parameters:');
      
      GetReal(a, 'a? ', 4, 3, canceled);
         if canceled or _EXITING then Exit;
      
      GetReal(b, 'b? ', 4, 4, canceled);
         if canceled or _EXITING then Exit;
      
      GetReal(c, 'c? ', 4, 5, canceled);
         if canceled or _EXITING then Exit;
      
      WriteXY(1, 7, PadCenter(FormatQuad(a, b, c), SCREENWIDTH), 11);
      
      // Save into variables for future use.
      SetVar('a', a);
      SetVar('b', b);
      SetVar('c', c);
      
      if a <> 0.0 then begin
         EvaluateQuadratic(a, b, c, d, x1, x2, vx, vy);
         
         SetVar('d', d);
         
         WriteXY(4, 9 , 'Discriminant, d = ', 15); write(Real2Str(d));
         WriteXY(6, 10, 'It has ' + IfThen(d>0, '2 real roots', IfThen(d=0, '1 double root', '2 imaginary roots')) + '.');
         WriteXY(4, 11, 'Roots:', 15);
         WriteXY(6, 12, 'x1 = ', 15); write(Complex2Str(x1));
         WriteXY(6, 13, 'x2 = ', 15); write(Complex2Str(x2));
         WriteXY(4, 14, 'Vertex:', 15);
         WriteXY(7, 15, 'vx = ', 15); write(Real2Str(vx));
         WriteXY(7, 16, 'vy = ', 15); write(Real2Str(vy));
         
         SetVar('x1', x1.Re);
         SetVar('x2', x2.Re);
         SetVar('vx', vx);
         SetVar('vy', vy);
         
      end else if b <> 0.0 then begin
         // x = -c/b
         WriteXY(4, 9, 'a=0, so it is a LINEAR function!');
         WriteXY(6, 11, 'x1 = '); write(Real2Str(-c/b));
         SetVar('x1', -c/b);
      
      end else if c = 0.0 then
         WriteXY(4, 9, 'c=0, no action to take.')
      else
         WriteXY(4, 9, Real2Str(c) + '=0 makes no sense!', 12);
      
      SetVarStr('quadeq', Real2Str(a) + '*pow(x,2) + ' + Real2Str(b) + '*x + ' + Real2Str(c));
      
      yesnomenu.Title := 'Repeat?';
      opt := yesnomenu.GetChoice(2, 19);
      
   until opt = 'N';
end;