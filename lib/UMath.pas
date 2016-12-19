(* * * * * * * * * * * * * === Unit  UMath === * * * * * * * * * * * * * *
 *      Version: 1.2.1                                                   *
 * Release date: January 13th, 2015                                      *
 *      License: GNU GPL 3.0  (included with the source)                 *
 *       Author: Igor Nunes, aka thoga31 @ www.portugal-a-programar.pt   *
 *                                                                       *
 *  Description: Unit with useful overloads and methods for open arrays. *
 *  ADAPTATION FROM 1.2.0 FOR SPECIFIC USE WITH MINI CALC 4.             *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)


{$mode objfpc}
unit UMath;

interface

type generic TArrayHandler<T> = class(TObject)
        private
            type TFnT    = function (n : T) : T;
                 TFnBool = function (n : T) : boolean;
                 TArr    = array of T;
            
        public
            class function  Map(list : TArr; f : TFnT)       : TArr;  static;
            class function  Filter(list : TArr; f : TFnBool) : TArr;  static;
            class procedure AddToArray(var list : TArr; const n : T); static;
     end;
     
     TIntegerArray = array of integer;
     TRealArray    = array of real;
     TByteSet      = set   of byte;
     
     TComplex = record
        re : Double;
        im : Double;
     end;


(* Open array of integer number *)
operator  *  (n : integer;          list : TIntegerArray) res : TIntegerArray;
operator  *  (list : TIntegerArray; n : integer         ) res : TIntegerArray;
operator  +  (n : integer;          list : TIntegerArray) res : TIntegerArray;
operator  +  (list : TIntegerArray; n : integer         ) res : TIntegerArray;
operator  -  (n : integer;          list : TIntegerArray) res : TIntegerArray;
operator  -  (list : TIntegerArray; n : integer         ) res : TIntegerArray;
operator div (n : integer;          list : TIntegerArray) res : TIntegerArray;
operator div (list : TIntegerArray; n : integer         ) res : TIntegerArray;
operator mod (n : integer;          list : TIntegerArray) res : TIntegerArray;
operator mod (list : TIntegerArray; n : integer         ) res : TIntegerArray;
operator  /  (n : integer;          list : TIntegerArray) res : TRealArray;
operator  /  (list : TIntegerArray; n : integer         ) res : TRealArray;
operator in  (n : integer;          list : TIntegerArray) res : boolean;

operator  -  (list : TIntegerArray) res : TIntegerArray;

operator  >  (list1, list2 : TIntegerArray) res : boolean;
operator  >= (list1, list2 : TIntegerArray) res : boolean;
operator  <  (list1, list2 : TIntegerArray) res : boolean;
operator  <= (list1, list2 : TIntegerArray) res : boolean;

operator  := (list : TByteSet) res : TIntegerArray;


(* Complex numbers - ADD SUPORT FOR THIS RESOURCE *)
operator  := (r : Double) z : TComplex;



implementation

class function TArrayHandler.Map(list : TArr; f : TFnT) : TArr;
var i : word;
begin
    Map := nil;
    SetLength(Map, Length(list));
    for i := Low(list) to High(list) do
        Map[i] := f(list[i]);
end;


class function TArrayHandler.Filter(list : TArr; f : TFnBool) : TArr;
var elem : T;
begin
    Filter := nil;
    for elem in list do
        if f(elem) then begin
            SetLength(Filter, succ(Length(Filter)));
            Filter[High(Filter)] := elem;
        end;
end;


class procedure TArrayHandler.AddToArray(var list : TArr; const n : T);
begin
    SetLength(list, succ(Length(list)));
    list[High(list)] := n;
end;


operator * (n : integer; list : TIntegerArray) res : TIntegerArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := n * list[i];
end;


operator * (list : TIntegerArray; n : integer) res : TIntegerArray;
begin
    res := n * list;
end;


operator + (n : integer; list : TIntegerArray) res : TIntegerArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := n + list[i];
end;


operator + (list : TIntegerArray; n : integer) res : TIntegerArray;
begin
    res := n + list;
end;


operator - (n : integer; list : TIntegerArray) res : TIntegerArray;
begin
    res := n + (-list);
end;


operator - (list : TIntegerArray; n : integer) res : TIntegerArray;
begin
    res := (-n) + list;
end;


operator div (n : integer; list : TIntegerArray) res : TIntegerArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := n div list[i];
end;


operator div (list : TIntegerArray; n : integer) res : TIntegerArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := list[i] div n;
end;


operator mod (n : integer; list : TIntegerArray) res : TIntegerArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := n mod list[i];
end;


operator mod (list : TIntegerArray; n : integer) res : TIntegerArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := list[i] mod n;
end;


operator / (n : integer; list : TIntegerArray) res : TRealArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := n / list[i];
end;


operator / (list : TIntegerArray; n : integer) res : TRealArray;
var i : word;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := list[i] / n;
end;


operator in (n : integer; list : TIntegerArray) res : boolean;
var elem : integer;
begin
    res := false;
    for elem in list do
        if n = elem then begin
            res := true;
            break;
        end;
end;


operator - (list : TIntegerArray) res : TIntegerArray;
var i : integer;
begin
    SetLength(res, Length(list));
    for i := Low(list) to High(list) do
        res[i] := -list[i];
end;


operator > (list1, list2 : TIntegerArray) res : boolean;
var i : integer;
begin
   res := Length(list1) = Length(list2);
   if res then
      for i := Low(list1) to High(list1) do
         if not(list1[i] > list2[i]) then begin
            res := false;
            break;
         end;
end;


operator >= (list1, list2 : TIntegerArray) res : boolean;
var i : integer;
begin
   res := Length(list1) = Length(list2);
   if res then
      for i := Low(list1) to High(list1) do
         if not(list1[i] >= list2[i]) then begin
            res := false;
            break;
         end;
end;


operator < (list1, list2 : TIntegerArray) res : boolean;
begin
   res := list2 > list1;
end;


operator <= (list1, list2 : TIntegerArray) res : boolean;
begin
   res := list2 >= list1;
end;


operator := (list : TByteSet) res : TIntegerArray;
var elem : byte;
begin
   res := nil;
   for elem in list do begin
      SetLength(res, succ(Length(res)));
      res[High(res)] := elem;
   end;
end;


operator  := (r : Double)   z : TComplex;
begin
   z.re := r;
   z.im := 0.0;
end;

end.