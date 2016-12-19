type
   TFn1Arg = function (r : Extended) : Extended;

const
   NPRECISION = 15;         // Float precision
   NDIGITS    =  1;         // Float digits (round)
   NFORMAT    = ffGeneral;  // Float format
   Bool2Str : array [boolean] of string = ('False', 'True');
   // Bool2Int : array [boolean] of byte = (0, 1);

function Real2Str(n : float) : string;
(* Converts Float to String analysing the numbers with methods from de RTL Math unit. *)
begin
   if IsZero(n) then
      Real2Str := '0'
   else if IsNan(n) then
      Real2Str := 'NaN'
   else if IsInfinite(n) then
      Real2Str := 'Infinity'
   else
      Real2Str := FloatToStrF(n, NFORMAT, NPRECISION, NDIGITS);
end;


function Complex2Str(z : TComplex) : string;
begin
   Complex2Str := '';
   
   if (z.Re = 0.0) and (z.Im = 0.0) then
      Complex2Str := '0'
   else begin
      if (z.Re <> 0.0) or (z.Im = 0.0) then
         Complex2Str := Real2Str(z.Re);
      
      if (z.Im <> 0.0) then begin
         Complex2Str := Complex2Str + IfThen(Complex2Str = '', '', ' ') + IfThen(z.Im > 0, '+', '-') + IfThen(z.Re = 0.0, '', ' ') + Real2Str(Abs(z.Im)) + 'i';
      end;
   end;
end;


function TryFn2Str(r : double; f : TFn1Arg) : string;
begin   
   try
      TryFn2Str := Real2Str(f(r));
   except
      TryFn2Str := 'NaN';
   end;
end;


function List2Str(l : mcVLM.TList) : string;
var w : word;
begin
   List2Str := '[';
   for w := 1 to l.count do
      List2Str := List2Str + Real2Str(l.data[w]) + IfThen(w = l.count, '', ',');
   List2Str := List2Str + ']';
end;
