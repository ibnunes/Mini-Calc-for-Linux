(* === mcGUI ===
 * Ver: 1.0.0
 *  By: Igor Nunes
 * Generic user interface methods. *)

unit mcGUI;

interface
uses crt;

procedure DrawSeparator(const X, Y, HEIGHT : word);



implementation

procedure DrawSeparator(const X, Y, HEIGHT : word);
   var i : word;
   begin
      TextBackground(4);
      for i := Y to HEIGHT do begin
         GotoXY(X, i);
         write(' ');
      end;
      TextBackground(0);
   end;


end.