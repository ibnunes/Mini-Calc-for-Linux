(* === mcRand ===
 * Ver: 1.0.2
 *  By: Igor Nunes
 * Random Generator for Mini Calc 4. *)

{$unitpath /wingraph}
{$mode objfpc}
unit mcRand;

interface
uses crt, sysutils, math, types, classes,
     UNavigation;

const RAND_MAXTRIES = 400000000;  // 400 million, LongWord range = 0..4294967295

type TRandRange  = array of string;
     TRandResult = array of LongWord;
     TRandTries  = LongWord;
     
     TRandGen = class(TObject)
        private
           var vRange  : TRandRange;
               vResult : TRandResult;
               vTries  : TRandTries;
               vDone   : boolean;

        public
           procedure Execute;

           constructor Create;
           property Range  : TRandRange  read vRange write vRange;
           property Tries  : TRandTries  read vTries write vTries;
           property Result : TRandResult read vResult;
           property Done   : boolean     read vDone;
     end;



implementation

constructor TRandGen.Create;
begin
   self.vRange  := nil;
   self.vResult := nil;
   self.vTries  := 0;
   self.vDone   := false;
   inherited Create;
end;


procedure TRandGen.Execute;
var i : LongWord = 1;
begin
   SetLength(self.vResult, Length(self.vRange));
   
   Randomize;
   while (i <= self.vTries) do begin
      Inc(self.vResult[Random(Length(self.vRange))]);
      Inc(i);
   end;

   self.vDone := true;
end;

end.
