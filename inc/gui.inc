const MAINMENU_POS_X = 2;
      MAINMENU_POS_Y = 4;
      SUBMENU_POS_X = 25;
      SUBMENU_POS_Y = MAINMENU_POS_Y;

var // 1. Generic menus
    mainmenu   : TMenu;
    trigmenu   : TMenu;
    listmenu   : TMenu;
    systemmenu : TMenu;
    eqmenu     : TMenu;
    pluginmenu : TMenu;  // Loaded in main block
    helpmenu   : TMenu;
    yesnomenu  : TMenu;
    
    // 2. Specific menus
    // 2.1. Trigonometry
    trig_redirmenu    : TMenu;
    trig_identitymenu : TMenu;
    // 2.2. Random
    random_redirmenu : TMenu;
    
    mem_submenu : TMenu;


procedure LoadUI;
(* Loads every GUI elements of the program. *)

   procedure Next(var c : char);
   begin
      c := Succ(c);
   end;

var idx : char = '1';
begin
   // Yes-No generic menu, used anywhere is needed.
   yesnomenu := TMenu.Create;
   // yesnomenu.Title - change whenever needed.
   yesnomenu.Add('Yes', 'Y', 'Y', TProc(nil));
   yesnomenu.Add('No' , 'N', 'N', TProc(nil));
   
   // Main menu
   mainmenu := TMenu.Create;
   mainmenu.Add('Calculator'  , idx  , idx, TProc(@menu_calculator));   Next(idx);
   mainmenu.Add('Trigonometry', idx  , idx, TProc(@menu_trigonometry), true); Next(idx);
   mainmenu.Add('Equations'   , idx  , idx, TProc(@menu_equation), true);     Next(idx);
   // mainmenu.Add('Vector'      , idx  , idx, nil);                       Next(idx);
   // mainmenu.Add('Matrix'      , idx  , idx, nil);                       Next(idx);
   mainmenu.Add('Lists & Stat', idx  , idx, TProc(@menu_list), true);         Next(idx);
   mainmenu.Add('Random'      , idx  , idx, TProc(@menu_random));       Next(idx);
   mainmenu.Add('Plugins'     , idx  , idx, TProc(@menu_tools), true);
   mainmenu.Add('System'      , 'F5' , #63, TProc(@menu_system), true);
   mainmenu.Add('Help'        , 'F6' , #64, TProc(@menu_help), true);
   mainmenu.Add('Exit'        , 'ESC', OPTION_EXIT, TProc(nil));
   mainmenu.color.regular.text := 15;
   
   // Trigonometry menu
   trigmenu := TMenu.Create;    idx := '1'; 
   trigmenu.Add('Analyse angle' , idx , idx, TProc(@trig_analyse));    Next(idx);
   trigmenu.Add('Table', idx , idx, TProc(@trig_seecircle));  // Next(idx);
   // trigmenu.Add('See table'      , idx , idx, nil);
   trigmenu.Add('Back'           , '<-', #8 , TProc(nil));
   
   // Equation menu
   eqmenu := TMenu.Create;                           idx := '1';
   eqmenu.Add('Edit functions...', idx , idx, TProc(@eq_editer));  Next(idx);
   // eqmenu.Add('Graphic'          , idx , idx, (@eq_graph));  Next(idx);
   // eqmenu.Add('Solver'           , idx , idx, nil);  Next(idx);
   eqmenu.Add('Quadratic formula', idx , idx, TProc(@form_quad));
   eqmenu.Add('Back'             , '<-', #8 , TProc(nil));
   
   // Lists menu
   listmenu := TMenu.Create;                    idx := '1';
   listmenu.Add('Edit...'      , idx , idx, TProc(@list_editer)); Next(idx);
   listmenu.Add('1-Var Stat'   , idx , idx, TProc(@stat_1var));   Next(idx);
   listmenu.Add('2-Var Stat'   , idx , idx, TProc(@stat_2var));   Next(idx);
   listmenu.Add('LinReg (ax+b)', idx , idx, TProc(@stat_linreg));
   listmenu.Add('Back'         , '<-', #8 , TProc(nil));
   
   // Random
   { randommenu := TMenu.Create;   idx := '1';
   randommenu.Add('Simulator', idx , idx, (@random_sim)); Next(idx);
   randommenu.Add('Back'     , '<-', #8 , nil); }
   
   // System menu
   systemmenu := TMenu.Create;                        idx := '1';
   // systemmenu.Add('Settings'      , idx , idx, nil);  Next(idx);
   systemmenu.Add('Memory Manager', idx , idx, TProc(@mem_screen));
   systemmenu.Add('Back'          , '<-', #8 , TProc(nil));
   
   // Help
   helpmenu := TMenu.Create;        idx := '1';
   helpmenu.Add('Help contents', idx , idx, TProc(@help_contents));  Next(idx);
   // helpmenu.Add('Documentation', idx , idx, nil);
   helpmenu.Add('About...'     , 'A' , 'A', TProc(@help_about));
   helpmenu.Add('Back'         , '<-', #8, TProc(nil));
   
   // Memory sub-menu
   mem_submenu := TMenu.Create;     idx := '1';
   mem_submenu.Add('Numbers'  , idx  , idx, TProc(nil));   Next(idx);
   mem_submenu.Add('Lists'    , idx  , idx, TProc(nil));   Next(idx);
   mem_submenu.Add('Strings'  , idx  , idx, TProc(nil));   Next(idx);
   mem_submenu.Add('Reset all', 'R'  , 'R', TProc(nil));
   // mem_submenu.Add('Settings' , 'S'  , 'S', nil);
   mem_submenu.Add('Back'     , 'ESC', OPTION_EXIT , TProc(nil));
   
   // Trigonometry specific menu - redirection in analysis of angle
   trig_redirmenu := TMenu.Create;                    idx := '1';
   trig_redirmenu.Title := 'Choose an option:';
   trig_redirmenu.Add('New angle'  , idx , idx, TProc(nil));
   trig_redirmenu.Add('Back'       , '<-', #8 , TProc(nil));
   
   trig_identitymenu := TMenu.Create;     idx := '1';
   trig_identitymenu.Title := 'Choose an option:';
   trig_identitymenu.Add('Analyse angle'  , idx , idx, TProc(@trig_analyse));
   trig_identitymenu.Add('Back', '<-', #8 , TProc(nil));
   
   // Random specific menu - redirection in viewing generated data
   random_redirmenu := TMenu.Create;                     idx := '1';
   random_redirmenu.Title := 'Choose an option:';
   random_redirmenu.Add('New random'  , idx , idx, TProc(nil));
   random_redirmenu.Add('Back'        , '<-', #8 , TProc(nil));
end;


procedure FreeUI;
(* Destroys every GUI elements. *)
begin
   mainmenu.Free;
   eqmenu.Free;
   listmenu.Free;
   trigmenu.Free;
   pluginmenu.Free;
   systemmenu.Free;
   helpmenu.Free;
   yesnomenu.Free;
   
   trig_redirmenu.Free;
   trig_identitymenu.Free;
   random_redirmenu.Free;
   
   mem_submenu.Free;
end;


procedure StaticMainMenu(item : word);
(* Shows the main menu, but it isn't functional. *)
begin
   clrscr;
   WriteAppMainInfo(2, 2);
   BuildMainScreen;
   mainmenu.Show(MAINMENU_POS_X, MAINMENU_POS_Y, item);
end;


procedure menu_calculator;
begin
   calc_prompt;
   StaticMainMenu(0);
end;


(* Implementation of generic and specific menus. *)
procedure menu_trigonometry;
var option : char;
    item : integer = 0;
begin
   repeat
      option := trigmenu.GetChoice(SUBMENU_POS_X, SUBMENU_POS_Y + 1, item);
      case option of
         #8 : trigmenu.Clear;
      else
         StaticMainMenu(1);
      end;
   until (option = #8) or _EXITING;
end;


procedure menu_list;
var option : char;
    item : integer = 0;
begin
   repeat
      option := listmenu.GetChoice(SUBMENU_POS_X, SUBMENU_POS_Y + 3, item);
      case option of
         #8 : listmenu.Clear;
      else
         StaticMainMenu(3);
      end;
   until (option = #8) or _EXITING;
end;


procedure menu_equation;
var option : char;
    item : integer = 0;
begin
   repeat
      option := eqmenu.GetChoice(SUBMENU_POS_X, SUBMENU_POS_Y + 2, item);
      case option of
         #8 : eqmenu.Clear;
      else
         StaticMainMenu(2);
      end;
   until (option = #8) or _EXITING;
end;


procedure menu_random;
begin
   random_sim;
   StaticMainMenu(4);
end;


procedure menu_tools;
var option : char;
    item : integer = 0;
begin
   repeat
      option := pluginmenu.GetChoice(SUBMENU_POS_X, SUBMENU_POS_Y + 5, item);
      case option of
         #8 : pluginmenu.Clear;
      else
         StaticMainMenu(5);
      end;
   until (option = #8) or _EXITING;
end;


const
   HELPPOSCOLLAB : record
      x, y1 : byte;
   end = (x:48; y1:11);

procedure menu_help;
var option : char;
    item : integer = 0;
begin
   repeat
      // WriteXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1  , '> In collaboration with:', 8);
      // WriteXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1+1, '  Nuno Picado', 8);
      option := helpmenu.GetChoice(SUBMENU_POS_X, SUBMENU_POS_Y + 7, item);
      case option of
         #8 :
            begin
               helpmenu.Clear;
               GotoXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1); clreol;
               GotoXY(HELPPOSCOLLAB.x, HELPPOSCOLLAB.y1+1); clreol;
            end;
      else
         StaticMainMenu(7);
      end;
   until (option = #8) or _EXITING;
end;


procedure menu_system;
var option : char;
    item : integer = 0;
begin
   repeat
      option := systemmenu.GetChoice(SUBMENU_POS_X, SUBMENU_POS_Y + 6, item);
      case option of
         #8 : systemmenu.Clear;
      else
         StaticMainMenu(6);
      end;
   until (option = #8) or _EXITING;
end;
