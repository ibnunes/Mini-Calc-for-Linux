procedure random_sim;
const MAXRANGE = 15;

var rgen : TRandGen;
    l : TArrayStr = nil;
    s : string;
    r : real;
    e : word;
    valid : boolean;
    cancel : boolean;
    opt : char = #0;


   function OutputInfo : char;
   var i : word;
       maxlen : word;
   
      function GetMaxLen(const L : TRandRange) : word;
      var s : string;
      begin
         GetMaxLen := 0;
         for s in L do
            if Length(s) > GetMaxLen then
               GetMaxLen := Length(s);
      end;

   begin
      maxlen := GetMaxLen(rgen.Range);
      
      repeat
         clrscr;
         
         for i := Low(rgen.Range) to High(rgen.Range) do
            WriteXY(2, 2+i, PadLeft(rgen.Range[i], maxlen) + ': ' + IntToStr(rgen.Result[i]) + '(' + Real2Str(100 * rgen.Result[i] / rgen.Tries) + '%)');
         
         OutputInfo := random_redirmenu.GetChoice(2, MAXRANGE + 3);
      until OutputInfo in ['1', #8];
   end;

var i : byte;
// Needs modulation
begin
   repeat
      clrscr;
      try
         rgen := TRandGen.Create;
         
         try
            cancel := false;
            // Get list of elements to rand from
            WriteXY(2, 7, 'Separate elements with a comma (","). Spaces are ignored.');
            WriteXY(4, 8, 'E.g.: "face, crown".');
            WriteXY(2, 9, 'Maximum of ' + IntToStr(MAXRANGE) + ' elements allowed.');
            
            repeat
               WriteXY(2, 2, 'List to rand? ', 15); clreol;
               for i := 3 to 6 do begin
                  GotoXY(1, i);
                  clreol;
               end;
               GotoXY(16, 2);
            until (ReadStrList(l) in [0..MAXRANGE]);
            
            rgen.Range := TRandRange(l);
            
            if (Length(l) = 1) then
               case LowerCase(l[0]) of
                  'exit'   : _EXITING := true;
                  'cancel' : cancel := true;
                  'help'   : ;
               end
            else if (Length(l) = 0) then
               cancel := true;
            
            // Get number of tries
            valid := false;
            while not (valid or cancel or _EXITING) do begin
               WriteXY(2, 11, 'Number of tries? ', 15); clreol;
               GotoXY(19, 11);
               
               case IntelliRead(s, r, e) of
                  IOinteger :
                     begin
                        valid := true;
                        rgen.Tries := Trunc(r);
                     end;
                  
                  IOcommand :
                     begin
                        valid := true;
                        case LowerCase(s) of
                           'exit'   : _EXITING := true;
                           'cancel' : cancel := true;
                           'help'   : ;
                        end;
                     end;
                     
               else begin
                  WriteXY(2, 12, 'Unexpected input.', 12);
                  valid := false;
               end;
               end;
            end;
            
            GotoXY(1, 8); clreol;
            
            // Lets make some mess :D
            if not cancel and not _EXITING then begin
               WriteXY(2, 14, 'Running...');
               rgen.Execute;

               if rgen.Done then
                  write(' [done]')
               else  // this should not happen
                  write(' [unknown]');
               
               GotoXY(2, 16);
               Pause('Press Enter to continue...');
               
               opt := OutputInfo;
            end;
            
         except
            on ex : Exception do
               writeln('ERROR ', ex.classname, ', ', ex.message);
         end;
         
      finally
         rgen.Free;
      end;
   until (opt = #8) or cancel or _EXITING;
end;
