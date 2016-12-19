(* === Unit Menu ===                                                                             *
 * By: Igor Nunes, aka thoga31 @ Portugal-a-Programar                                            *
 * Date: February 6th, 2015                                                                      *
 * Version: 1.1.0                                                                                *
 * Description:                                                                                  *
 *    This unit contains a useful class which allows to create menus very easily with rendering. *
 * Licensed under the GNU GPL 3.0                                                                *)

{$mode objfpc}
unit UMenu;

interface
uses crt, sysutils, strutils, math;

(* Some useful constants *)
const sys : record   // cannot be used to define other constants :(
         KEY_ENTER : char;
         KEY_ESC : char;
         NEWLINE : ShortString;
      end = (KEY_ENTER:#13; KEY_ESC:#27; NEWLINE:#13+#10);

type TStrArr = array of string;  // used for renderers
     TActKind = (akProc, akPlugin, akUnknow);
     TProc = procedure;
     TProcPlug = procedure of object;
     
     TMenu = class(TObject)  // Procedures and Functions described on their implementation
        private
            const
               DEFAULT_HEIGHT = 10;
            
            type TOption = record
                    prompt : string;  // Text to write
                    key_prompt : string;
                    key : char;       // Key that must be pressed for this option
                    hassubmenu : boolean;
                    case acttype : TActKind of
                       akProc   : (action : TProc);   // Procedure associated with this option
                       akPlugin : (plugexec : TProcPlug);
                 end;
                 TKeysSet = set of char;
                 TOptList = array of TOption;
                 TPoint = record
                             x, y : byte;
                          end;
            
            var VMenu : TOptList;        // contains all the options of the menu
                VMenuSorted : TOptList;  // a sorted version of the menu, by keys
                VKeys : TKeysSet;        // a set of all keys used in this menu
                VSorted : boolean;       // informs if this menu must be shown sorted
                VMaxLength : word;       // helps to calculate the size of the menu with renderer
                VPos : TPoint;
                VTitle : string;
                HEIGHT : word;           // used as constant
                menu_to_show : TOptList;
            
            procedure Sort;
            procedure MyShow(const X, Y : byte; count : integer; var init : integer);
            
        public
            constructor Create(maxheight : word =  DEFAULT_HEIGHT; mysorted : boolean = false);
            
            procedure Add(myprompt : string; mykeyprompt : string; mykey : char; myproc : TProc; hsm : boolean = false);
            procedure Add(myprompt : string; mykeyprompt : string; mykey : char; myproc : TProcPlug; hsm : boolean = false); overload;
            
            procedure Show(const X, Y : byte; count : integer);
            function GetChoice(const X, Y : byte; var count : integer; performAction : boolean = true) : char;
            function GetChoice(const X, Y : byte; performAction : boolean = true) : char; overload;
            procedure Clear;
            
            property KEYS : TKeysSet read VKeys;                   // Gets the set of keys
            property sorted : boolean read VSorted write VSorted;  // Defines if the menu must be shown sorted by keys
            property title : string read VTitle write VTitle;
            
            var color : record
                   selected : record
                      background, text : word;
                   end;
                   regular : record
                      background, text : word;
                   end;
                   title : record
                      background, text : word;
                   end;
                end;
     end;

function SplitAtChar(const S : string; const CH : char = ' ') : TStrArr;


implementation


function Char2Str(ch : char) : string;
const SPECIAL : array [0..32] of string = ('NUL', 'SOH', 'ETX', 'EOT', 'ENQ', 'ACK', 'BEL', 'BS', 'TAB', 'TAB', 'LF', 'VT', 'FF', 'CR', 'SO', 'SI', 'DLE', 'DC1', 'DC2', 'DC3', 'DC4', 'NAK', 'SYN', 'ETB', 'CAN', 'EM', 'SUB', 'ESC', 'FS', 'GS', 'RS', 'US', 'Spc');
      DEL_ORD = 127;
begin
   if Ord(ch) in [0..32] then
      Char2Str := PadRight(SPECIAL[ord(ch)], 3)
   else if Ord(ch) = DEL_ORD then
      Char2Str := 'DEL'
   else
      Char2Str := PadRight(ch, 3);
end;


function SplitAtChar(const S : string; const CH : char = ' ') : TStrArr;
(* Splits a string by a char, returning the substrings, without the char, in a dynamic array of strings. *)
var i : integer;
    t : string = '';
begin
    SetLength(SplitAtChar, 0);
    for i := 1 to length(S) do begin
        if (S[i] = CH) or (i = length(S)) then begin
            SetLength(SplitAtChar, length(SplitAtChar)+1);
            SplitAtChar[high(SplitAtChar)] := t + IfThen(i = length(S), s[i], '');
            t := '';
        end else begin
            t := t + s[i];
        end;
    end;
end;

constructor TMenu.Create(maxheight : word = DEFAULT_HEIGHT; mysorted : boolean = false);
(* Initialize the variants of the class *)
begin
    inherited Create;
    SetLength(self.VMenu, 0);
    self.VKeys := [];
    self.VSorted := mysorted;
    self.VMaxLength := 0;
    self.VTitle := '';
    self.HEIGHT := maxheight;
    with self.color do begin
       with selected do begin
          text := 15;         // old = 16
          background := 4;    // old = 7
       end;
       with regular do begin
          text := 7;
          background := 8;
       end;
       with title do begin
          text := 15;
          background := 8;
       end;
    end;
end;

procedure TMenu.Sort;
(* Sorts the menu by keys in a second variant, "VMenuSorted". *)
var temp : TOption;
    i, j : integer;
begin
    self.VMenuSorted := self.VMenu;
    for i := 0 to high(self.VMenuSorted)-1 do
        for j := i to high(self.VMenuSorted) do
            if self.VMenuSorted[i].key > self.VMenuSorted[j].key then begin
                temp := self.VMenuSorted[i];
                self.VMenuSorted[i] := self.VMenuSorted[j];
                self.VMenuSorted[j] := temp;
            end;
end;

procedure TMenu.Clear;
var i : word;
    postitle : word = 0;
begin
   TextBackground(8);
   if title <> '' then begin
      postitle := 1;
      GotoXY(self.VPos.x, self.VPos.y);
      write(DupeString(' ', Length(self.vTitle)));
   end;
   for i := 0 to Length(self.VMenu)-1 do begin
      GotoXY(self.VPos.x, self.VPos.y+i+postitle);
      write(DupeString(' ', self.VMaxLength+8));
   end;
end;

procedure TMenu.Add(myprompt : string; mykeyprompt : string; mykey : char; myproc : TProc; hsm : boolean = false);
(* Add a new item to the menu. *)
begin
    SetLength(self.VMenu, length(self.VMenu)+1);
    with self.VMenu[high(self.VMenu)] do begin
        prompt := myprompt;
        if self.VMaxLength < length(myprompt) then
            self.VMaxLength := length(myprompt);
        key := mykey;
        key_prompt := mykeyprompt;
        hassubmenu := hsm;
        Include(self.VKeys, mykey);
        acttype := akProc;
        action := myproc;
    end;
end;


procedure TMenu.Add(myprompt : string; mykeyprompt : string; mykey : char; myproc : TProcPlug; hsm : boolean = false); overload;
begin
    SetLength(self.VMenu, length(self.VMenu)+1);
    with self.VMenu[high(self.VMenu)] do begin
        prompt := myprompt;
        if self.VMaxLength < length(myprompt) then
            self.VMaxLength := length(myprompt);
        key := mykey;
        key_prompt := mykeyprompt;
        hassubmenu := hsm;
        Include(self.VKeys, mykey);
        acttype := akPlugin;
        plugexec := myproc;
    end;
end;


procedure TMenu.MyShow(const X, Y : byte; count : integer; var init : integer);
var i : integer;
    postitle : word = 0;
begin
   self.VPos.x := X;
   self.VPos.y := Y;
   
   if self.VSorted then begin
      self.Sort;
      menu_to_show := self.VMenuSorted;
   end else
      menu_to_show := self.VMenu;
        
   TextColor(self.color.title.text);
   TextBackground(self.color.title.background);
   if title <> '' then begin
      GotoXY(x,y);
      postitle := 1;
      write(title);
   end;
   
   for i := Low(menu_to_show) to Min(self.HEIGHT, High(menu_to_show)) do begin
      if init + i = count then begin
         TextColor(self.color.selected.text);
         TextBackground(self.color.selected.background);
      end else begin
         TextColor(self.color.regular.text);
         TextBackground(self.color.regular.background);
      end;
      gotoxy(x,y+i+postitle);
      write(' ' + PadLeft(menu_to_show[i + init].key_prompt, 3) + '  ' + PadRight(menu_to_show[i + init].prompt, self.VMaxLength+1), IfThen(menu_to_show[i + init].hassubmenu, '>', ' '));
   end;
end;


procedure TMenu.Show(const X, Y : byte; count : integer);
var ghost_init : integer = 0;
begin
   self.MyShow(X, Y, count, ghost_init);
end;


function TMenu.GetChoice(const X, Y : byte; performAction : boolean = true) : char; overload;
var nothing : integer = 0;
begin
   GetChoice := self.GetChoice(X, Y, nothing, performAction);
end;

function TMenu.GetChoice(const X, Y : byte; var count : integer; performAction : boolean = true) : char;
(* Waits for the user's option. *)
const KEY_UP   = #72;
      KEY_DOWN = #80;
      NAVKEYS  = [KEY_UP, KEY_DOWN];

var option : TOption;
    init : integer = 0;

begin
    repeat
        self.MyShow(X, Y, count, init);   // change
        
        repeat
            GetChoice := upcase(ReadKey);
            if GetChoice = #0 then begin
               GetChoice := upcase(ReadKey);
               case GetChoice of
                  KEY_DOWN : begin
                                Inc(count);
                                if count > High(menu_to_show) then begin
                                   count := 0;
                                   init  := 0;
                                end else begin
                                   if count > init + HEIGHT then
                                      Inc(init);
                                end;
                             end;
                  
                  KEY_UP   : begin
                                Dec(count);
                                if count < Low(menu_to_show) then begin
                                   count := High(menu_to_show);
                                   if Min(HEIGHT, High(menu_to_show)) = HEIGHT then
                                      init := count - HEIGHT
                                   else
                                      init := 0;
                                end else begin
                                   if count < init then
                                      Dec(init);
                                end;
                             end;
               end;
            end else if GetChoice in NAVKEYS then
               GetChoice := #0;
        until GetChoice in self.VKeys + NAVKEYS + [sys.KEY_ENTER];
        
        if GetChoice = sys.KEY_ENTER then
           GetChoice := menu_to_show[count].key;
        
    until GetChoice in self.VKeys;
    
    if performAction then
        for option in self.VMenu do
            if GetChoice = option.key then begin
                if option.action <> nil then
                    option.action;
                break;
            end;
    TextColor(7);
    TextBackground(8);
end;

end.
