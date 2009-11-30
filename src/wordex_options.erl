-module(spadinac_options).
-export([build_mopts/1,build_topts/1]).
-export([parse_cmdargs/1,parse_cmdstr/1]).

-include("spadinac.hrl").

build_mopts(Opts) ->
   resolve_implied_mopts(build_opts(Opts, #mopts{}, record_info(fields, mopts))).

resolve_implied_mopts(MOpts) ->
    ProjDir = resolve_projdir(MOpts), 
    ModDir  = resolve_moddir(MOpts, ProjDir),
    MOpts#mopts{ projdir = ProjDir, 
                 srcdir  = resolve_srcdir(MOpts, ProjDir), 
                 moddir  = ModDir,
                 modpath = resolve_modpath( MOpts, ModDir),
                 targdir = resolve_targdir(MOpts, ProjDir) }.

%%resolve_implied_mopts(#mopts{srcdir=SrcDir, moddir=ModDir, modpath=ModPath, targdir=TargDir}) ->
%%    MOpts#mopts{ projdir=ProjDir2, srcdir=SrcDir2, moddir=ModDir2,
%%                 modpath=ModPath2, targdir=TargDir2 }.

resolve_projdir( #mopts{projdir=ProjDir} ) ->
   filename:absname(ProjDir).

resolve_srcdir( #mopts{srcdir=implied}, ProjDir ) ->
   filename:absname("src", ProjDir);
resolve_srcdir( #mopts{srcdir=SrcDir}, _ ) ->
   filename:absname(SrcDir).

resolve_moddir( #mopts{moddir=implied}, ProjDir ) ->
   filename:absname("mod", ProjDir);
resolve_moddir( #mopts{moddir=ModDir}, _ ) ->
   filename:absname(ModDir).

resolve_modpath( #mopts{modpath=implied}, ModDir ) ->
   [ModDir];
resolve_modpath( #mopts{modpath=ModPath}, ModDir ) ->
   [ModDir|ModPath].

resolve_targdir( #mopts{targdir=implied}, ProjDir ) ->
   filename:absname("target", ProjDir);
resolve_targdir( #mopts{targdir=TargDir}, _ ) ->
   filename:absname(TargDir).

build_topts(Opts) ->
   build_opts(Opts, #topts{}, record_info(fields, topts)).

build_opts(Opts, DefRec, Fields) ->
   OptsDict = dict:from_list(Opts),
   [RTag|DefVals] = tuple_to_list(DefRec),
   build_opts(OptsDict, [RTag], DefVals, Fields). 

 
build_opts(_OptsDict, OptsAcc, [], []) ->
   list_to_tuple(lists:reverse(OptsAcc));
build_opts(OptsDict, OptsAcc, [DefVal|DVT], [Field|FT]) ->
   case dict:find(Field, OptsDict) of
      {ok, Value} ->
         build_opts(OptsDict, [Value|OptsAcc], DVT, FT);
      _ ->
         build_opts(OptsDict, [DefVal|OptsAcc], DVT, FT)
   end.


parse_cmdstr(CmdStr) ->
   parse_cmdargs(string:tokens(CmdStr," "), [], [], []).

parse_cmdargs(Strings) ->
   parse_cmdargs(Strings, [], [], []).

parse_cmdargs([], MOptAcc, TargAcc, SrcAcc) ->
   { 
      build_mopts( [{targets, lists:reverse(TargAcc)}|MOptAcc] ),
      lists:reverse(SrcAcc)
   };
parse_cmdargs([[$-|[$-|Arg]]|Rest], MOptAcc, TargAcc, SrcAcc) ->
   parse_longarg(Arg, Rest, MOptAcc, TargAcc, SrcAcc);
parse_cmdargs([[$-|Arg]|Rest], MOptAcc, TargAcc, SrcAcc) ->
   parse_shortarg(Arg, Rest, MOptAcc, TargAcc, SrcAcc);
parse_cmdargs([Arg|Rest], MOptAcc, TargAcc, SrcAcc) ->
   parse_cmdargs(Rest, MOptAcc, TargAcc, [Arg|SrcAcc]).
   
parse_longarg(Arg, Args, MOptAcc, TargAcc, SrcAcc) ->
   parse_cmdargs( Args,
                  [opt_for_longarg(Arg)|MOptAcc], 
                  TargAcc, 
                  SrcAcc).

opt_for_longarg(Arg) ->
   [Tag|Values] = string:tokens(Arg,"=:;"), 
   opt_for_longarg( list_to_atom(Tag), Values).

opt_for_longarg(srcdir, [Value]) when is_list(Value) ->
   {srcdir, Value};
opt_for_longarg(srcdir, Values) ->
   throw({badarg, srcdir, Values}); 

opt_for_longarg(moddir, [Value]) when is_list(Value) ->
   {moddir, Value};
opt_for_longarg(moddir, Values) ->
   throw({badarg, moddir, Values}); 

opt_for_longarg(modpath, Values) when is_list(Values) ->
   {modpath, Values};
opt_for_longarg(modpath, Values) ->
   throw({badarg, modpath, Values}); 

opt_for_longarg(targdir, [Value]) when is_list(Value) ->
   {targdir, Value};
opt_for_longarg(targdir, Values) ->
   throw({badarg, targdir, Values}); 

opt_for_longarg(recompile, ["true"]) ->
   {recompile, true};
opt_for_longarg(recompile, ["false"]) ->
   {recompile, false};
opt_for_longarg(recompile, []) ->
   {recompile, true};
opt_for_longarg(recompile, Values) ->
   throw({badarg, recompile, Values}). 


parse_shortarg(ShortArg, [Value|Args], MOptAcc, TargAcc, SrcAcc) ->
   parse_cmdargs(Args, [{tag_for_shortarg(ShortArg), Value}|MOptAcc], TargAcc, SrcAcc).

tag_for_shortarg("p") ->
   projdir;
tag_for_shortarg("s") ->
   srcdir;
tag_for_shortarg("m") ->
   moddir;
tag_for_shortarg("mp") ->
   modpath;
tag_for_shortarg("t") ->
   targdir;
tag_for_shortarg("T") ->
   targets;
tag_for_shortarg(T) ->
   throw({ badarg, T }).



