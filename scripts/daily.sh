#!/usr/bin/env /Users/evabihari/.kerl/r18/bin/escript
%% -*- erlang -*-
%%! -smp enable -mnesia debug verbose dir 'Mnesia.nonode@nohost'
-include_lib("../include/records.hrl").
main(_) ->
        shell_default:cd("/Users/evabihari/Erlang_apps/vienna_life_value"),
	io:format("shell_default~n",[]),
        Script_dir=filename:dirname(escript:script_name()),
        true = code:add_pathz(Script_dir ++ "/../ebin"),
	io:format("ebin~n",[]),

        true = code:add_pathz(Script_dir ++ "/../deps/mochiweb/ebin"),
	io:format("mochiweb~n",[]),

        true = code:add_pathz(Script_dir ++ "/../deps/mochiweb_xpath/ebin"),
	io:format("xpath~n",[]),

        file:read_link_info("records.hrl"),
        try
        %%     mnesia:restore("data/backup.bup",[{default_op, recreate_tables}])
               mnesia:load_textfile("data/mnesia.txt")
        catch
            {aborted,enoent} ->
                io:format("Backup does not exist, starting with clean database",[]);
            {error,open} -> io:format("File does not exist, starting with clean database",[])
        end,
	      io:format("before arfolyam:start() ~n",[]),

	      arfolyam:start(),
	      io:format("after arfolyam:start()~n",[]),

        arfolyam:read_portfolio(),
        io:format("read_portfolio() done~n",[]),
	      arfolyam:calculate_portfolio(),
        io:format("calculate_portfolio() done~n",[]),
        utils:dump_daily_values_table(),
        mnesia:dump_to_textfile("data/mnesia.txt"),
        mnesia:backup("data/backup.bup").
