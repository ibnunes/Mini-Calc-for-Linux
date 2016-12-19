{$mode objfpc}
unit nixShell;

interface
uses
   sysutils, strutils, UExcept;

type
   EInvalidParam = class(Exception);

function ParamValidation(var valid : boolean) : boolean;


implementation

function ParamValidation(var valid : boolean) : boolean;
begin
   ParamValidation := false;
   valid := true;
   
   try
      if (ParamCount = 0) or ((ParamCount = 1) and (ParamStr(1) = '--interact')) then
         Exit
      else
         ParamValidation := true;
      
      case ParamStr(1) of      // raise EInvalidParam.Create('');
         '-?', '--help', '--about' :
            begin
               if (ParamCount > 1) then
                  raise EInvalidParam.Create(ParamStr(1) + ' has more parameters than it should.');
            end;
         
         '-e' :
            begin
               if (ParamCount <> 2) then
                  raise EInvalidParam.Create(ParamStr(1) + ' has ' + IfThen(ParamCount > 2, 'more', 'less') + ' parameters than it should.');
            end;
         
         '-f' :
            begin
               if (ParamCount <= 3) then
                  raise EInvalidParam.Create('Insufficient parameters for function operations.')
               else begin
                  case ParamStr(3) of
                     '-x' :
                        if (ParamCount <> 4) then
                           raise EInvalidParam.Create(ParamStr(1) + ' with ' + ParamStr(3) + ' has ' + IfThen(ParamCount > 4, 'more', 'less') + ' parameters than it should.');
                     
                     '-t' :
                        if not (ParamCount in [5..8]) then
                           raise EInvalidParam.Create(ParamStr(1) + ' with ' + ParamStr(3) + ' has ' + IfThen(ParamCount > 8, 'more', 'less') + ' parameters than it should.')
                        else if (ParamStr(6) = '+l') and (ParamCount > 7) then
                           raise EInvalidParam.Create(ParamStr(1) + ' with ' + ParamStr(6) + ' has more parameters than it should.');
                  else
                     raise EInvalidParam.Create(ParamStr(1) + ' isn''t followed by a valid parameter (-x or -i).');
                  end;
               end;
            end;
         
         '+f1','+f2','+f3','+f4','+f5','+f6','+f7','+f8','+f9','+f0' :
            begin
               if (ParamCount <> 2) then
                  raise EInvalidParam.Create('Wrong number of parameters for function memory management.');
            end;
         
         '-l' :
            begin
               case ParamStr(2) of
                  'add'    :
                     begin
                        if (ParamCount <> 5) then
                           raise EInvalidParam.Create(ParamStr(2) + ' has not the correct number of parameters.');
                     end;
                  
                  'append' : ;
                  'del'    : ;
                  
                  'form' :
                     begin
                        if (ParamCount <> 4) then
                           raise EInvalidParam.Create(ParamStr(2) + ' has not the correct number of parameters.');
                     end;
                  
                  'new'    : ;
                  'show'   :
                     begin
                        if (ParamCount <> 3) then
                           raise EInvalidParam.Create(ParamStr(2) + ' has not the correct number of parameters.');
                     end;
               else
                  raise EInvalidParam.Create(ParamStr(1) + ' isn''t followed by a valid parameter (add, append, del, form, new, show).');
               end;
            end;
         
         { '' :
            begin
               
            end;
         
         '' :
            begin
               
            end; }
      end;
      
      
      
   except
      on ex : Exception do begin
         valid := false;
         WriteError(ex.message);
         writeln;
      end;
   end;
end;



end.
