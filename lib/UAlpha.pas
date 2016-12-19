(* * * * * * * * * * * * * * * * === UAlpha ===  * * * * * * * * * * * *
 *      Version: 1.0.0                                                 *
 * Release date: June 7th, 2014                                        *
 *      License: GNU GPL 3.0  (included with the source)               *
 *       Author: Igor Nunes, aka thoga31 @ www.portugal-a-programar.pt *
 *                                                                     *
 *  Description: Writes big letters.                                   *
 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *)

{$mode objfpc}
unit UAlpha;

interface
uses crt;

const HEIGHT = 5;
      WIDTH  = 5;

type TLine   = string[WIDTH];
     TLetter = array [1..HEIGHT] of TLine;

const DEFAULT_SEPARATOR = ' ';
      DEFAULT_SPACING = 1;
      NULLCH : TLetter = ('00000','00000','00000','00000','00000');
      
      DIGITS : array ['0'..'9'] of TLetter  = (('01110','10001','10101','10001','01110'),  // 0
                                               ('00110','00001','00001','00001','00001'),  // 1
                                               ('11110','00001','01110','10000','11111'),  // 2
                                               ('11110','00001','00111','00001','11110'),  // 3
                                               ('10001','10001','01111','00001','00001'),  // 4
                                               ('01111','10000','01110','00001','11110'),  // 5
                                               ('01110','10000','11110','10001','01110'),  // 6
                                               ('01110','10001','00001','00001','00001'),  // 7
                                               ('01110','10001','01110','10001','01110'),  // 8
                                               ('01110','10001','01111','00001','01110')); // 9
      
      LETTERS : array ['A'..'Z'] of Tletter = (('01110','10001','11111','10001','10001'),  // A
                                               ('11110','10001','11110','10001','11110'),  // B
                                               ('01111','10000','10000','10000','01111'),  // C
                                               ('11110','10001','10001','10001','11110'),  // D
                                               ('01111','10000','11100','10000','01111'),  // E
                                               ('01111','10000','11100','10000','10000'),  // F
                                               ('01111','10000','10011','10001','01110'),  // G
                                               ('10001','10001','11111','10001','10001'),  // H
                                               ('01110','00100','00100','00100','01110'),  // I
                                               ('11111','00001','00001','10001','01110'),  // J
                                               ('10010','10100','11100','10010','10001'),  // K
                                               ('10000','10000','10000','10000','01110'),  // L
                                               ('01110','10101','10001','10001','10001'),  // M
                                               ('11001','10101','10101','10101','10011'),  // N
                                               ('01110','10001','10001','10001','01110'),  // O
                                               ('11110','10001','11110','10000','10000'),  // P
                                               ('01110','10001','10101','10011','01111'),  // Q
                                               ('11110','10001','11110','10010','10001'),  // R
                                               ('01110','10000','01110','00001','01110'),  // S
                                               ('01110','00100','00100','00100','00100'),  // T
                                               ('10001','10001','10001','10001','01110'),  // U
                                               ('10001','10001','01010','01010','00100'),  // V
                                               ('10001','10001','10101','10101','01110'),  // W
                                               ('10001','01010','00100','01010','10001'),  // X
                                               ('10001','10001','01111','00001','01110'),  // Y
                                               ('11111','00010','00100','01000','11111')); // Z
      SYMBOL_LIST = '!:-''.,;+';
      SYMBOLS : array [1..8] of TLetter = (('00100','00100','00100','00000','00100'),  // !
                                           ('00000','00100','00000','00100','00000'),  // :
                                           ('00000','00000','01110','00000','00000'),  // -
                                           ('00100','00100','00000','00000','00000'),  // '
                                           ('00000','00000','00000','00000','00100'),  // .
                                           ('00000','00000','00000','00100','00100'),  // ,
                                           ('00000','00100','00000','00100','00100'),  // ;
                                           ('00000','00100','01110','00100','00000')   // +
                                          );


procedure WriteBig(statement : string; const X, Y : word; spacing : byte = DEFAULT_SPACING; separator : char = DEFAULT_SEPARATOR);
function CodeToVisible(code : string) : string;



implementation

function NStrings(st : string; n : byte) : string;
var i : byte;
begin
    NStrings := '';
    for i := 1 to n do
        NStrings := NStrings + st;
end;

function CodeToVisible(code : string) : string;
const NOTHING = ' ';
      FULL    = '#';
var ch : char;
begin
    CodeToVisible := '';
    for ch in code do
        case ch of
            '0' : CodeToVisible := CodeToVisible + NOTHING;
            '1' : CodeToVisible := CodeToVisible + FULL;
        end;
end;

procedure WriteBig(statement : string; const X, Y : word; spacing : byte = DEFAULT_SPACING; separator : char = DEFAULT_SEPARATOR);
var ch : char;
    i  : word;
begin
    statement := UpCase(statement);
    for i := 1 to HEIGHT do begin
        GotoXY(X, Y+i-1);
        for ch in statement do
            if ch in ['0'..'9'] then
                write(CodeToVisible(DIGITS[ch][i]), NStrings(separator, spacing))
            else if ch in ['A'..'Z'] then
                write(CodeToVisible(LETTERS[ch][i]), NStrings(separator, spacing))
            else if Pos(ch, SYMBOL_LIST) > 0 then
                write(CodeToVisible(SYMBOLS[pos(ch, SYMBOL_LIST)][i]), NStrings(separator, spacing))
            else
                write(CodeToVisible(NULLCH[i]), NStrings(separator, spacing));
    end;
end;


end.
