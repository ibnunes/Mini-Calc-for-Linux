(* === mcTrig ===
 * Ver: 1.0.2
 *  By: Igor Nunes
 * Trigonometry routines for Mini Calc 4. *)

{$unitpath /wingraph}
{$mode objfpc}
unit mcTrig;

interface
uses crt, sysutils, math,
     UNavigation;

type TPoint = record
        x, y : word;
     end;
     TAngleUnit = (angleRad, angleDeg, angleGrad);
     TRecAngle = record
        Deg, Rad, Grad : float;
     end;
     TQuadrant = (quad1, quad2, quad3, quad4, between12, between23, between34, between14);
     
     TAngle = class(TObject)
     private
        const cQuad2StrLong : array [TQuadrant] of string =
                  ('first quadrant',
                   'second quadrant',
                   'third quadrant',
                   'fourth quadrant',
                   'between first and second quadrants',
                   'between second and third quadrants',
                   'between third and fourth quadrants',
                   'between first and fourth quadrants');
              cQuad2StrShort : array [TQuadrant] of string = ('Q1','Q2','Q3','Q4','L1/2','L2/3','L3/4','L1/4');
        var vValue   : TRecAngle;
            vMainArg : TRecAngle;
            vQuad    : TQuadrant;
        
        procedure SetNewValue(newvalue : float; angunit : TAngleUnit = angleRad);
        procedure SetDeg(newvalue : float);
        procedure SetRad(newvalue : float);
        procedure SetGrad(newvalue : float);
        procedure CalcMainArg;
        
     public
        constructor Create;
        constructor Create(initvalue : float; angunit : TAngleUnit = angleRad); overload;
        
        property Deg  : float read vValue.deg  write SetDeg;
        property Rad  : float read vValue.rad  write SetRad;
        property Grad : float read vValue.grad write SetGrad;
        property MainArgument : TRecAngle read vMainArg;
        property Quad : TQuadrant read vQuad;
        
        function Quadrant : string;
        function QuadrantShort : string;
     end;


implementation

constructor TAngle.Create;
begin
   self.SetNewValue(0.0, angleDeg);
end;

constructor TAngle.Create(initvalue : float; angunit : TAngleUnit = angleRad); overload;
begin
   self.SetNewValue(initvalue, angunit);
end;


procedure TAngle.CalcMainArg;
begin
   self.vMainArg.Deg := self.vValue.Deg - 360 * RoundTo(self.vValue.Deg / 360, 0);
   self.vMainArg.Rad := DegToRad(self.vMainArg.Deg);
   self.vMainArg.Grad := DegToGrad(self.vMainArg.Deg);
   
   with self, self.vMainArg do begin
      if Deg = 0 then
         vQuad := between14
      else if Deg = 90 then
         vQuad := between12
      else if Abs(Deg) = 180 then
         vQuad := between23
      else if Deg = -90 then
         vQuad := between34
      else if (Deg > 0) and (Deg < 90) then
         vQuad := quad1
      else if (Deg > 90) and (Deg < 180) then
         vQuad := quad2
      else if (Deg > -180) and (Deg < -90) then
         vQuad := quad3
      else if (Deg > -90) and (Deg < 0) then
         vQuad := quad4;
   end;
end;


procedure TAngle.SetDeg(newvalue : float);
begin
   self.SetNewValue(newvalue, angleDeg);
end;

procedure TAngle.SetRad(newvalue : float);
begin
   self.SetNewValue(newvalue, angleRad);
end;

procedure TAngle.SetGrad(newvalue : float);
begin
   self.SetNewValue(newvalue, angleGrad);
end;


procedure TAngle.SetNewValue(newvalue : float; angunit : TAngleUnit = angleRad);
begin
   case angunit of
      angleRad  : begin
                     self.vValue.Rad := newvalue;
                     self.vValue.Deg := RadToDeg(newvalue);
                     self.vValue.Grad := RadToGrad(newvalue);
                  end;
      angleDeg  : begin
                     self.vValue.Deg := newvalue;
                     self.vValue.Rad := DegToRad(newvalue);
                     self.vValue.Grad := DegToGrad(newvalue);
                  end;
      angleGrad : begin
                     self.vValue.Grad := newvalue;
                     self.vValue.Deg := GradToDeg(newvalue);
                     self.vValue.Rad := GradToRad(newvalue);
                  end;
   end;
   self.CalcMainArg;
end;


function TAngle.Quadrant : string;
begin
   Quadrant := self.cQuad2StrLong[vQuad];
end;


function TAngle.QuadrantShort : string;
begin
   QuadrantShort := self.cQuad2StrShort[vQuad];
end;

end.
