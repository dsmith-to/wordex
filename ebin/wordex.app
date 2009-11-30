%% This is the application resource file (.app file) for the wordex,
%% application.
{application, wordex, 
  [{description, "Word indexing module."},
   {vsn, "0.1.0"},
   {modules, [wordex,
              wordex_app,
              wordex_sup,
              wordex_args,
              wordex_options,
              wordex_parse]},
   {registered,[wordex_sup]},
   {applications, [kernel, stdlib]},
   {mod, {wordex_app,[]}},
   {start_phases, []}]}.

