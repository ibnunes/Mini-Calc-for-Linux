unit mcTypes;

interface

const MAXLIST = 1000;

type TArrayDouble = array [1..MAXLIST] of double;
     TList = record
                data   : TArrayDouble;
                id     : string;
                count  : word;
                active : boolean;
             end;


implementation

end.