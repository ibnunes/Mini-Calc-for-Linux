procedure help_about(const INTERACT : boolean = true);
begin
   clrscr;
   BuildMainScreen(2);
   GotoXY(2, 10);
   TextColor(15);
   write(APP_NAME + ' ');
   TextColor(12);
   write(APP_SERIES + 'st series');
   
   WriteXY(2, 11, 'Version: ' + APP_VER);
   WriteXY(2, 12, '  Build: ' { + APP_BUILD + ', at ' } + APP_BUILDTIME);
   WriteXY(2, 14, '     By: ' + APP_AUTHOR);
   WriteXY(2, 15, '         All rights reserved.');
   WriteXY(2, 18, 'This program is FREEWARE. You should NOT pay for it!', 15);
   WriteXY(2, 20, 'Please, report any bug with its proper description.');
   
   if INTERACT then begin
      GotoXY(2, 24);
      TextColor(8);
      Pause('Press Enter to go back...');
   end else
      writeln;
   TextColor(7);
end;


procedure help_contents;
type
   THelpProc = procedure; stdcall;

{ var
   helprec : record
      exists : boolean;
      proc   : THelpProc;
      dll    : TLibHandle;
   end;

   function HelpInitialization : boolean;
   const
      DIR = 'help.dll';
   begin
      HelpInitialization := true;
      
      try
         helprec.dll := LoadLibrary(PChar(DIR));
         
         if helprec.dll <> NilHandle then begin
            helprec.proc := THelpProc(GetProcedureAddress(helprec.dll, 'Execute'));
            if not Assigned(helprec.proc) then
               HelpInitialization := false;
         end else
            HelpInitialization := false;
      except
         HelpInitialization := false;
      end;
   end;

   procedure HelpFinalization;
   begin
      if helprec.exists and (helprec.dll <> NilHandle) then begin
         if UnloadLibrary(helprec.dll) then
            helprec.dll := NilHandle;
      end;
   end; }


begin
   // helprec.exists := HelpInitialization;
   
   // if helprec.exists then begin
   //    helprec.proc;
   // end else begin
      // HELPPOSCOLLAB in 'gui.inc'
      // GotoXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1+2); clreol;
      // GotoXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1+3); clreol;
      WriteXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1+2, 'Help not available in the app.', 8);
      // WriteXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1+3, '  - refer to "help.chm".', 8);
      Pause;
   // end;
   
   // HelpFinalization;
end;


procedure help_shell(const WITHPAUSE : boolean);

   procedure HelpPause;
   const
      PAUSE_MSG = 'Press enter to continue...';
   begin
      Pause(PAUSE_MSG);
      GotoXY(1, WhereY); clreol;
      GotoXY(1, WhereY);
   end;

begin
   TextBackground(7);
   TextColor(8);
   write(APP_NAME, ' ', APP_VER, ', by ', APP_AUTHOR);
   TextBackground(0);
   writeln;
   
   TextColor(10);
   writeln('Usage: mcalc < [-e] "exp" | -f "fn" <-x num | -t min max [step] [+l id]> |');
   writeln('               +f<x> "fn" | -l <add | append | del | new> id [values] >');
   
   TextColor(7);
   writeln(CRLF,
           'Parameter            Description');
   writeln(DupeString('-', 31));
   writeln('-e "exp"             Optional, the same as no parameter with "exp" alone;');
   writeln('                     Computes "exp" and returns the result.');;
   writeln(CRLF,
           '-f "fn"              Computes a function "fn" ("fn" can be Yx (0 <= x <= 9):');
   writeln('   -x num            Value at point ''num'';');
   writeln('   -t min max [step]');
   writeln('                     Outputs a table with values at points from min to max with step.');
   writeln('           [+l id]   Saves the results in the list ''id'' - current values are erased.');
   writeln(CRLF,
           '+f<x> "fn"           Adds a function "fn" to Y<x>.');
   
   if WITHPAUSE then
      HelpPause;
   
   writeln(CRLF,
           '-l op id [values]    Operates with the list ''id''. ''op'' can be:');
   writeln('   add value index   Adds a value at an index. No more than 5 parameters;');
   writeln('   append (values)   Appends a set of values to the list;');
   writeln('   del (indexes)     Deletes the given indexes of list.');
   writeln('                     If no indexes are given, deletes the entire list.');
   writeln('   new (values)      Creates a new list with the given (optional) values.');
   writeln('   show <id|all>     Shows the list ''id'' or all of them.');
   
   writeln;
   // TextColor(7);
end;
