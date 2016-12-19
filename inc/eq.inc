(* Function for visual manager. *)

   procedure GetEquation(out name : string);
   begin
      TextBackground(8);
      GotoXY(2, LIST_MSGPOS);
      clreol;
      GotoXY(2, LIST_MSGPOS);
      TextColor(15);
      write('New eq? ');
      TextColor(7);
      readln(name);
      case LowerCase(name) of
         'regeq'  : name := GetVarStr('regeq');
         'quadeq' : name := GetVarStr('quadeq');
      end;
      GotoXY(2, LIST_MSGPOS);
      clreol;
   end;

(* ----------------------------- *)

procedure eq_editer;
begin
   eq_environment;
end;


{ procedure eq_graph;
begin
   eq_environment(false);
   // Turn true when interactive mode implemented.
end; }


procedure eq_environment;

   procedure ShowInstructions(const X, Y : word);
   begin
      WriteXY(X, Y  , 'Up/Down = navigation', 8);
      WriteXY(X, Y+1, 'Enter = edit', 8);
      WriteXY(X, Y+1, '   <- = clear', 8);
      WriteXY(X, Y+3, '    T = table', 8);
      WriteXY(X, Y+5, 'Esc = back to main menu', 8);
   end;


var opt  : char = #0;
    opt2 : char = #0;
    fn_idx : LongInt = 0;

begin
   clrscr;
   DrawSeparator(41, 1, SCREENHEIGHT-3);
   ShowInstructions(2, 14);
      // Space available for instructions is bigger, LIST_KEYTXT not used.
   
   repeat
      opt := mcFunctions.NavigateThroughFunctions(2, 2, fn_idx, TFnGetName(@GetEquation), [], mcParser);
      
      case opt of
         KEY_FN_SELECT :
            if (mcFunctions[fn_idx] <> '') then begin
               opt2 := mcFunctions.Table(43, 2, fn_idx, mcParser);
               textbackground(8);
               textcolor(7);
               if opt2 = KEY_FN_EXIT then
                  mcFunctions.ClearTable(43, 2);
            end;
      end;
      
   until opt = KEY_FN_EXIT;
end;
