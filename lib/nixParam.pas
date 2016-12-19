{$mode objfpc}

unit nixParam;


interface
   type
      TArgSyntax = string;
      
      TParam =
         record
            cmd : string;
            case IsAlias : boolean of
               false : (argsyn : TArgSyntax);
               true  : (mainarg : string);
         end;
      
      TParamArray = array of TParam;
      
      TParamMgr = class(TObject)
         private
            var
               validParams : TParamArray;
               caseSens : boolean;
            
            function strcmp(str1, str2 : string; isCase : boolean = true) : boolean;
            
         public
            procedure AddParam(p : string; args : TArgSyntax);
            procedure AliasParam(old, new : string);
            function HasParam(p : string) : boolean;
            
            property CaseSensitive : boolean write caseSens;
      end;


implementation
   
   function TParamMgr.strcmp(str1, str2 : string; isCase : boolean = true) : boolean;
   begin
      if isCase then
         strcmp := (str1 = str2)
      else
         strcmp := (UpCase(str1) = UpCase(str2));
   end;
   

   procedure TParamMgr.AddParam(p : string; args : TArgSyntax);
   begin
      SetLength(self.validParams, Succ(Length(self.validParams)));
      
      with self.validParams[High(self.validparams)] do begin
         cmd := p;
         IsAlias := false;
         argsyn := args;
      end;
   end;
   
   
   procedure TParamMgr.AliasParam(old, new : string);
   begin
      SetLength(self.validParams, Succ(Length(self.validParams)));
      
      with self.validParams[High(self.validparams)] do begin
         cmd := new;
         IsAlias := true;
         mainarg := old;
      end;
   end;
   
   
   function TParamMgr.HasParam(p : string) : boolean;
   var
      elem : TParam;
   begin
      HasParam := false;
      for elem in self.validParams do
         if self.strcmp(p, elem.cmd, self.caseSens) then begin
            HasParam := true;
            break;
         end;
   end;
   
end.
