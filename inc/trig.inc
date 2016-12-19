procedure trig_analyse;
(* Analyses an angle - it includes nested procedures and functions for this purpose. *)
var angle : TAngle = nil;
    s : string;
    r : real;
    res : float;
    e : word;

   function InputQuery : TIOResult;
   (* Gets the input until it is valid. *)
   var valid : boolean = true;
   begin
      clrscr;
      
      textcolor(15);
      WriteXY(2,  5, 'After the angle, write the unit as follows (case insensitive):');
      WriteXY(4,  6, 'Degrees    - D');
      WriteXY(4,  7, 'Radians    - R');
      WriteXY(4,  8, 'Radians*pi - PI');
      WriteXY(4,  9, 'Grads      - G');
      WriteXY(2, 10, 'E.g.: "45 d" means 45 degrees;');
      WriteXY(2+6, 11, '"2 pi" is 2*pi radians (360 degrees).');
      WriteXY(2, 12, 'You must put a SPACE between the value and the unit!');
      WriteXY(2, 13, 'If no unit is given, it will be assumed it is degrees.');
      
      repeat
         TextColor(15);
         WriteXY(2, 2, 'Angle?', 15); clreol;
         GotoXY(9, 2);
         valid := true;
         InputQuery := IntelliRead(s, r, e);
         
         case InputQuery of
            IOreal, IOinteger : angle.Deg := r;  // It is just a number.
            IOerror :
               begin    // It has letters - needs further analysis...
                  if AnsiEndsText(' d', s) then begin
                     e := Length(s)-2;
                     if ComputeF(AnsiLeftStr(s, e), res) then begin
                        InputQuery := IOreal;
                        angle.Deg := res;
                     end;
                  end else
                  
                  if AnsiEndsText(' r', s) then begin
                     e := Length(s)-2;
                     if ComputeF(AnsiLeftStr(s, e), res) then begin
                        InputQuery := IOreal;
                        angle.Rad := res;
                     end;
                  end else
                  
                  if AnsiEndsText(' pi', s) then begin
                     e := Length(s)-3;
                     if ComputeF('(' + AnsiLeftStr(s, e) + ')*$pi', res) then begin
                        InputQuery := IOreal;
                        angle.Rad := res;
                     end;
                  end else
                  
                  if AnsiEndsText(' g', s) then begin
                     e := Length(s)-2;
                     if ComputeF(AnsiLeftStr(s, e), res) then begin
                        InputQuery := IOreal;
                        angle.Grad := res;
                     end;
                  end else
                  
                  begin
                     if ComputeF(s, res) then begin
                        angle.Deg := res;
                        InputQuery := IOreal;
                     end else begin
                        WriteXY(2, 3, 'Invalid input.   ', 12);
                        valid := false;
                     end;
                  end;
               end;
            // IOcommand : valid := true;  // It is an internal command - it isn't a matter for this procedure.
         end;
         
         if not (InputQuery in [IOreal, IOinteger, IOcommand]) and valid then begin
            WriteXY(2, 3, 'Unexpected input.', 12);
            valid := false;
         end;
      until valid;
   end;
   
   procedure OutputInfo;
   (* Pure GUI - shows the analysis information. *)
   begin
      clrscr;
      WriteXY( 2, 2, 'Angle in different units:', 15);
      WriteXY( 2, 3, ' Deg = ' + Real2Str(angle.Deg));
      WriteXY( 2, 4, ' Rad = ' + Real2Str(angle.Rad));
      WriteXY( 2+7, 5, Real2Str(angle.Rad / system.pi) + ' * pi');
      WriteXY( 2, 6, 'Grad = ' + Real2Str(angle.Grad));
      
      WriteXY(40, 2, 'Main argument:', 15);
      WriteXY(40, 3, ' Deg = ' + Real2Str(angle.MainArgument.Deg));
      WriteXY(40, 4, ' Rad = ' + Real2Str(angle.MainArgument.Rad));
      WriteXY(40+7, 5, Real2Str(angle.MainArgument.Rad / system.pi) + ' * pi');
      WriteXY(40, 6, 'Grad = ' + Real2Str(angle.MainArgument.Grad));
      
      WriteXY(2, 8, 'Trigonometric functions:', 15);
      WriteXY(2, 9, 'Sin = ' + Real2Str(Sin(angle.Rad)));
      WriteXY(2, 10, 'Cos = ' + Real2Str(Cos(angle.Rad)));
      WriteXY(2, 11, 'Tan = ' + TryFn2Str(angle.Rad, @Tan));
      WriteXY(40, 9, 'Sec = ' + TryFn2Str(angle.Rad, @Sec));
      WriteXY(40, 10, 'Csc = ' + TryFn2Str(angle.Rad, @Csc));
      WriteXY(40, 11, 'Cot = ' + TryFn2Str(angle.Rad, @Cot));
      
      WriteXY(2, 13, 'Location in the unit circle:', 15);
      WriteXY(4, 14, angle.QuadrantShort + ' - ' + angle.Quadrant);
      
   end;

var opt : char = #0;
    item : integer = 0;
begin
   angle := TAngle.Create;
   
   repeat
      case InputQuery of  // After getting the input, it is time to process...
         IOreal, IOinteger :   // Analysis of the angle.
            begin
               OutputInfo;
               opt := trig_redirmenu.GetChoice(2, 16, item);
            end;
         
         IOcommand :    // Processes the command.
            begin
               case LowerCase(s) of
                  'exit'   : _EXITING := true;
                  'cancel' : opt := #8;
                  'help'   : ;
               end;
            end;
      end;
      
      textcolor(7);
   until (opt = #8) or _EXITING;
   
   angle.Free;
end;


procedure trig_seecircle;
(* Shows the "main angles" in the unit circle - 0?, 30?, 45?, 60?, 90? *)

   procedure ShowTable(const X, Y : word);
   begin
      TextColor(14);
      GotoXY(X, Y);   write('  DEG  |      0      |     30      |     45      |     60      |     90      |');
      TextColor(15);
      GotoXY(X, Y+1); write('  sin  |');
      GotoXY(X, Y+2); write('  cos  |');
      GotoXY(X, Y+3); write('  tan  |');
      GotoXY(X, Y+4); write('  cot  |');
      GotoXY(X, Y+5); write('  sec  |');
      GotoXY(X, Y+6); write(' cosec |');
      
      TextColor(7);
      GotoXY(X+8, Y+1); write('      0      |    1 / 2    | 1 / sqrt(2) | sqrt(3) / 2 |      1      |');
      GotoXY(X+8, Y+2); write('      1      | sqrt(3) / 2 | 1 / sqrt(2) |    1 / 2    |      0      |');
      GotoXY(X+8, Y+3); write('      0      | 1 / sqrt(3) |      1      |   sqrt(3)   |     N/D     |');
      GotoXY(X+8, Y+4); write('     N/D     |   sqrt(3)   |      1      | 1 / sqrt(3) |      0      |');
      GotoXY(X+8, Y+5); write('      1      | 2 / sqrt(3) |   sqrt(2)   |      2      |     N/D     |');
      GotoXY(X+8, Y+6); write('     N/D     |      2      |   sqrt(2)   | 2 / sqrt(3) |      1      |');
   end;

   procedure InitUI;
   begin
      clrscr;
      ShowTable(2, 2);
   end;

var opt : char = #0;
    item : integer = 0;
begin
   InitUI;
   
   repeat
      opt := trig_identitymenu.GetChoice(2, 13, item);
      
      case opt of
         '1' : InitUI;
      end;
   until opt = #8;
end;
