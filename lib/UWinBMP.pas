(* === UWinBMP ===
 * Ver: 1.0.0
 *  By: Igor Nunes
 * Tiny tools to manage BMP files with wingraph unit. *)

{$unitpath /wingraph}
{$mode objfpc}
unit UWinBMP;

interface
uses sysutils, wingraph;

function LoadBMP(FILENAME : string; bitmap : pointer) : boolean;
function SaveBMP(FILENAME : string) : boolean;



implementation

function LoadBMP(FILENAME : string; bitmap : pointer) : boolean;
var
   f : file;
   { bitmap : pointer; }
   size : longint; 
begin
   FILENAME := FILENAME + '.bmp';
   LoadBMP := FileExists(FILENAME);
   
   if LoadBMP then begin
      size := FileSize(f);
      GetMem(bitmap, size);
      BlockRead(f, bitmap^, size);
      Close(f);
      // PutImage(0, 0, bitmap^, NormalPut);
      // FreeMem(bitmap);
   end;
end;

function SaveBMP(FILENAME : string) : boolean;
var
   f : file;
   bitmap : pointer;
   size : longint; 
begin
   FILENAME := FILENAME + '.bmp';
   {$I-} Assign(f, FILENAME); Rewrite(f,1); {$I+}
   SaveBMP := (IOResult = 0);
   
   if SaveBMP then begin 
      size := ImageSize(0, 0, GetMaxX, GetMaxY);
      GetMem(bitmap, size);
      GetImage(0, 0, GetMaxX, GetMaxY, bitmap^);
      BlockWrite(f, bitmap^, size);
      Close(f);
      FreeMem(bitmap);
   end;
end;




end.