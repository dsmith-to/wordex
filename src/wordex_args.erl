-module(spadinac_args).
-export([parse_cmdargs/1,parse_cmdstr/1]).

-include("spadinac.hrl").

build_mopts(Opts) ->
   resolve_implied_mopts(build_opts(Opts, #mopts{}, record_info(fields, mopts))).

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



