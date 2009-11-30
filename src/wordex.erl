-module(wordex).

-export([start/0, stop/0]).
-export([compile/1,compile/2]).
-export([scan_and_parse/1,scan/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called to start the decimal application.
%% It initializes the elacs application as well as all dependant 
%% applications.
%%
%% @spec start() -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @doc
%% This function is called to stop the decimal application.
%% It stops erlacs and all depenancies.
%%
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    application:stop(?MODULE).


compile(Source) ->
   compile( Source, #mopts{}).

compile(Source, MOpts) ->
   case compile_modules(Source, MOpts) of
      {ok, _} ->
         compile_targets(MOpts);
      Error ->
         Error
   end.

compile_modules(Source, #mopts{srcdir=SrcDir, moddir=ModDir, recompile=Recompile}) ->
   assert_dir(ModDir),
   lists:foldl( fun(SourceFile, ResultAcc) ->
                   compile_module(SourceFile, ModDir, Recompile, ResultAcc)
                end,
                [],
                expand_source(Source, SrcDir)
              ).

compile_module( SrcFile, ModDir, Recompile, ResultAcc ) ->
   case compile_module( SrcFile, module_filename(SrcFile, ModDir), Recompile ) of 
      ok ->
         [{SrcFile, ok}|ResultAcc];
      Error ->
         [{SrcFile, Error}|ResultAcc]
   end.

compile_module( SrcFile, ModFile, true ) ->
   compile_module( SrcFile, ModFile );
compile_module( SrcFile, ModFile, false ) ->
   case filelib:last_modified(SrcFile) >= filelib:last_modified(ModFile) of
      true ->
         compile_module( SrcFile, ModFile );
      false ->
         ok
   end.

compile_module( SrcFile, ModFile ) ->
   case scan_and_parse( SrcFile ) of
      {ok, Result} ->
         file:write_file( ModFile,
                          term_to_binary(Result) );
      Error ->
         Error
   end.

expand_source(Source, SrcDir) ->
   expand_source(Source, [], SrcDir).

expand_source([], Acc, _SrcDir)->
   Acc;
expand_source([H|T], Acc, SrcDir) when is_list(H)->
   expand_source(T, filelib:wildcard(H, SrcDir) ++ Acc, SrcDir). 

module_filename(SrcFile, ModDir) ->
   filename:join(ModDir, filename:basename(SrcFile,".sp") ++ ".spm").

compile_targets(_MOpts) ->
   ok.

scan_and_parse( File ) ->
   case scan(File) of 
      {ok, Tokens, _TokenCount} ->
         spadinac_parse:parse(Tokens);
      Error ->
         Error
   end.

scan(File) -> 
   case read_file(File) of
      {ok, Str} ->
         spadinac_scan:string(Str);
      Error ->
         Error
   end.


read_file(File) ->
   case file:read_file(File) of
      {ok, Bin} -> 
         asn1rt:utf8_binary_to_list(Bin);
      Error ->
         Error
   end.

assert_dir(Dir) ->
   case  filelib:ensure_dir(Dir) of
      {error, _} = Error ->
         throw( Error );
      _ ->
         ok
   end.
