(* === mcStat ===
 * Ver: 1.1.1
 *  By: Igor Nunes
 * Statistical analysis. *)

{$unitpath /wingraph}
{$mode objfpc}
unit mcStat;

interface
uses crt, sysutils, math, mcVLM, UNavigation, types;

type
   TListElements = array of Extended;
   TKurtosis = record
      m1, m2, m3, m4 : Extended;
      skew : Extended;
      kurtosis : Extended;
   end;
   TLinReg = record
      a, b  : Extended;
      r, r2 : Extended;
   end;

procedure ExtractElements(vlm : TListMgr; const ID : string; out elements : TListElements);
procedure CumSum(const a : TListElements; out r : TListElements);
procedure AddToManager(vlm : TListMgr; const ID : string; elements : TListElements);
procedure SimpleLinearRegression(x, y : TListElements; out reg : TLinReg);

function mean2(xi, fi : TListElements) : Extended;
function stddev2(xi, fi : TListElements) : Extended;

operator * (a : TListElements; b : TListElements) r : TListElements;
operator / (a : TListElements; b : Extended) r : TListElements;


implementation

function mean2(xi, fi : TListElements) : Extended;
var
   i : word;
   n : Extended = 0.0;
begin
   mean2 := 0;
   for i := Low(xi) to High(xi) do begin
      n := n + fi[i];
      mean2 := mean2 + xi[i] * fi[i];
   end;
   if n <> 0 then
      mean2 := mean2 / n;
end;


function stddev2(xi, fi : TListElements) : Extended;
var
   xifi : TListElements;
   mean_xifi : Extended;
   n : Extended = 0.0;
   i : word;
begin
   xifi := xi * fi;
   mean_xifi := mean2(xi, fi);
   stddev2 := 0.0;
   for i := Low(xi) to High(xi) do begin
      stddev2 := stddev2 + (xifi[i] - mean_xifi);
      n := n + fi[i];
   end;
   if n <> 0 then
      stddev2 := sqrt(stddev2 / n);
end;


procedure ExtractElements(vlm : TListMgr; const ID : string; out elements : TListElements);
var
   i : word;

begin
   SetLength(elements, vlm[ID].count);
   for i := 0 to Pred(vlm[ID].count) do
      elements[i] := vlm.Elements[ID, Succ(i)];
end;


procedure CumSum(const a : TListElements; out r : TListElements);
var
   i : word;

begin
   SetLength(r, Pred(Length(a)));
   
   if Length(a) >= 2 then begin
      r[0] := a[0] + a[1];
      for i := 1 to Pred(High(a)) do
         r[i] := r[Pred(i)] + a[Succ(i)];
   end else
      r[0] := a[0];
end;


procedure AddToManager(vlm : TListMgr; const ID : string; elements : TListElements);
var
   elem : Extended;

begin
   if not vlm.IDExists(ID) then
      vlm.CreateNewList(ID)
   else begin
      vlm.DeleteList(ID);
      vlm.CreateNewList(ID);
   end;
   
   for elem in elements do
      vlm.AppendToList(ID, elem);
end;


operator * (a : TListElements; b : TListElements) r : TListElements;
// a and b must have the same length!
var
   i : word;
begin
   SetLength(r, Length(a));
   for i := Low(r) to High(r) do
      r[i] := a[i] * b[i];
end;


operator / (a : TListElements; b : Extended) r : TListElements;
var
   i : word;

begin
   SetLength(r, Length(a));
   for i := Low(a) to High(a) do
      r[i] := a[i] / b;
end;


procedure SimpleLinearRegression(x, y : TListElements; out reg : TLinReg);
var
   sx, sy : Extended;
   mean_x, mean_y : Extended;
   xy : TListElements;
   mean_xy : Extended;
   sqr_x, sqr_y : TListElements;
   mean_sqr_x, mean_sqr_y : Extended;

begin
   // Auxiliar calculations
   mean_x := mean(x);
   mean_y := mean(y);
   sx := stddev(x);
   sy := stddev(y);
   xy := x * y;
   mean_xy := mean(xy);
   sqr_x := x*x;
   sqr_y := y*y;
   mean_sqr_x := mean(sqr_x);
   mean_sqr_y := mean(sqr_y);
   
   // Final calculations
   reg.r := (mean_xy - mean_x * mean_y) / sqrt((mean_sqr_x - sqr(mean_x)) * (mean_sqr_y - sqr(mean_y)));
   reg.r2 := sqr(reg.r);
   
   // f = AX+B
   reg.a := reg.r*(sy/sx);
   reg.b := mean_y - reg.a * mean_x;
end;

end.
