(* === Mini Calc for Linux ===
 * Ver: 1.2.3
 * June 1st, 2015
 *  By: Igor Nunes
 * The Minimalist Calculator for GNU/Linux. *)

{$mode objfpc}
{$unitpath lib}
{$unitpath plugin/lib}

program mcalc;
// {$R res/mc4.res}

uses
   // RTL Free Pascal
   crt, sysutils, strutils, classes, math, dynlibs,
   // FCL Free Pascal
   fpexprpars,
   // Own generic units
   UMenu, UNavigation, UMath, UAlpha, UExcept,
   // Mini Calc specific units
   mcGUI, mcIO, mcCalc, mcEq, mcTrig, mcRand, mcVLM, mcStat, mcMem,
   // Extensions
   mcIntf,
   // *Nix specific units
   { nixParam, } nixShell;

const
   OPTION_EXIT  = #27;  // Esc
   SCREENWIDTH  = 80;
   SCREENHEIGHT = 25;
   LIST_MSGPOS  = 23;
   LIST_KEYTXT  = 18;
   // CRLF = #13+#10;
   
   MEM_PAR_NAME = 'par.mc4p';
   MEM_FN_NAME  = 'fn.mc4f';
   MEM_VLM_NAME = 'vlm.mc4l';
   
   DEFCOLOR_SELECTED_TEXT = 15;
   DEFCOLOR_SELECTED_BACK = 4;
   DEFCOLOR_REGULAR_TEXT = 7;
   DEFCOLOR_REGULAR_BACK = 0;

var
   _EXITING : boolean = false;     // Internal flag - order to exit
   MEM_PAR : string;
   MEM_FN  : string;
   MEM_VLM : string;

(* <INCLUDE> *)
   (* Generic features *)
   {$i inc/preamble.inc}   // Lists all public procedures and functions present in the *.inc files.
   {$i inc/app.inc}        // Application information (name, version, author...).
   {$i inc/gui.inc}        // Everything related with GUI (navigation, not presentation of information).
   {$i inc/io.inc}         // Specific methods for I/O handling.

   (* Specific features and systems *)
   {$i inc/calc.inc}       // Calculator
   {$i inc/eq.inc}         // Equations
   {$i inc/form.inc}       // Formulas (e.g., quadratic)
   {$i inc/list.inc}       // Lists (VLM)
   {$i inc/stat.inc}       // Statistics
   {$i inc/trig.inc}       // Trigonometry
   {$i inc/random.inc}     // Random
   {$i inc/mem.inc}        // Memory Manager
   {$i inc/help.inc}       // Help
   
   (* *Nix features *)
   {$i inc/shell.inc}
   
   (* Extensions/plugins *)
   {$i inc/IntfHost.inc}   // Implementation of host side of interface
(* </INCLUDE> *)

(* Main block *)
var 
   option : char;
   previous_item : integer = 0;
   _PLUGSTAT : byte;
   _SHELL :
      record
         intercept : boolean;
         valid     : boolean;
      end;

begin
   // _SHELL := ShellIntercept;
   _SHELL.intercept := ParamValidation(_SHELL.valid);
   // writeln(_SHELL.intercept, '.', _SHELL.valid);
   // Halt;
   if _SHELL.intercept then begin
      if _SHELL.valid then begin
         mem_init;
         ExecShell;
         mem_final;
      end;
      Exit;
   end;
   
   // Plugin initialization - it is the first thing
   writeln('Plugin Initialization: ');
   pluginmenu := TMenu.Create;
   try
      _PLUGSTAT := PluginInitialization;
      writeln('[', _PLUGSTAT, ']');
   except
      on ex : Exception do begin
         _PLUGSTAT := PLUGIN_FATAL;
         pluginmenu.Add('Back', '<-', #8, TProc(nil));
         writeln('   [ERR] ', ex.classname, ', ', ex.message);
         writeln;
         writeln('Unloading all plugins...');
         try
            PluginFinalization;
         except
            on ex2 : Exception do
               writeln('  [ERR] ', ex2.classname, ', ', ex2.message);
         end;
         Pause('Press enter to continue...');
      end;
   end;
   
   
   try
      mem_init;
      LoadUI;     // Loads every GUI element of the program
      
      try
         clrscr;
         repeat   // Main menu
            WriteAppMainInfo(2, 2);
            BuildMainScreen;
            option := mainmenu.GetChoice(MAINMENU_POS_X, MAINMENU_POS_Y, previous_item);
         until (option = OPTION_EXIT) or _EXITING;
      except
         on ex : Exception do begin    // Handles unexpected exceptions
            write('[ERR] ', ex.classname, ', ', ex.message);
            Pause;
            (* After final build, use 'FatalInfo' from unit 'UExcept' *)
         end;
      end;
      
   finally
      FreeUI;  // Destroys every GUI element
      mem_final;
   end;
   
   // Plugin finalization
   clrscr;
   write('Plugin Finalization: ');
   if _PLUGSTAT <> PLUGIN_FATAL then
      try
         PluginFinalization;
         writeln('OK');
      except
         on ex2 : Exception do begin
            writeln('  [ERR] ', ex2.classname, ', ', ex2.message);
            Pause('Press enter to continue...');
         end;
      end
   else
      writeln('CODE 4, there was no initialization.');
end.
