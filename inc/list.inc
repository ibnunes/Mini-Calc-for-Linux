// Using VLM System

(* Function for visual manager. *)

   function GetListName : string;
   begin
      TextBackground(8);
      GotoXY(2, LIST_MSGPOS);
      TextColor(15);
      write('New list name: ');
      TextColor(7);
      repeat
         readln(GetListName);
      until GetListName <> '';
      GotoXY(2, LIST_MSGPOS);
      clreol;
   end;


   function GetListFormula : string;
   begin
      TextBackground(8);
      GotoXY(2, LIST_MSGPOS);
      TextColor(15);
      write('Formula? ');
      TextColor(7);
      readln(GetListFormula);
      GotoXY(2, LIST_MSGPOS);
      clreol;
   end;


   function GetListValue : Double;

      function IsNumber(s : string) : boolean;
      var r : real;
          e : word;
      begin
         Val(s, r, e);
         IsNumber := (e = 0);
      end;

   var s : string;
       e : word;
       r : Float;
   begin
      repeat
         TextBackground(8);
         GotoXY(2, LIST_MSGPOS);
         clreol;
         GotoXY(2, LIST_MSGPOS);
         TextColor(15);
         write('New value: ');
         TextColor(7);
         readln(s);
      until ComputeF(s, r);
      GetListValue := Double(r);
      GotoXY(2, LIST_MSGPOS);
      clreol;
   end;

   procedure VLMException(ex : Exception);
   begin
      WriteXY(2, LIST_MSGPOS+1, '[ERR] ', 12);
      WriteXY(8, LIST_MSGPOS+1, ex.message, 14);
      Pause(' (Press Enter)');
      GotoXY(2, LIST_MSGPOS+1);
      clreol;
   end;

(* ---------------------------- *)

procedure list_editer;
const
   INITPOS : record
      x, y : word;
   end = (x:2; y:2);


   procedure ShowInstructions(const X, Y : word);
   begin
      GotoXY(X, Y);
      TextColor(8);
      writeln('Left/Right/Up/Down = navigation        | Esc = back to main menu');
      
      GotoXY(X, Y+1);
      writeln('For values: + = add value | - = delete | Enter = edit  | S = select to save');
      
      GotoXY(X, Y+2);
      writeln(' For lists: * = new list  | D = delete | N = edit name | F = insert formula');
      
      GotoXY(X, Y+3);
      writeln('     Enter: in "---" = append value    | in "<New>" = new list');
      TextColor(7);
   end;


   procedure SelectedValue(v : Double);
   var mem : char = #0;
       p : TFPExprIdentifierDef;
   begin
      TextBackground(8);
      GotoXY(2, LIST_MSGPOS);
      clreol;
      GotoXY(2, LIST_MSGPOS);
      TextColor(15);
      write('''', Real2Str(v), ''' selected: save where? (a-z) ');
      
      
      repeat
         mem := LowerCase(ReadKey);
      until mem in ['a'..'z'];
      
      p := mcParser.Identifiers.IdentifierByName(ShortString(mem));
      p.AsFloat := v;
      
      TextColor(7);
      write('Saved in ''', mem, '''.');
      Pause;
      
      GotoXY(2, LIST_MSGPOS);
      clreol;
   end;


var
   opt : char = #0;
   v   : Double;

begin
   clrscr;
   
   ShowInstructions(2, LIST_KEYTXT);
   
   repeat
      opt := listmgr.Panel(INITPOS.x, INITPOS.y, v, @GetListName, @GetListValue , @GetListFormula, @VLMException, @ComputeF, 5);
      
      case opt of
         mcVLM.KEY_SELECT : SelectedValue(v);
      end;
      
   until opt = #27;
end;