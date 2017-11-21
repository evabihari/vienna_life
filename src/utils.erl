%%%-------------------------------------------------------------------
%%% @author eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%% @copyright (C) 2016, eva.bihari
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(utils).
-include("../include/records.hrl").
%% API
-export([calculate_portfolio/0,
         encode_name/1,
         create_date/3,
         dump_daily_values_table/0,
	       dump_to_csv/1,
	       create_date_list/1,
	       get_and_sort_dates/0,
	       collect_paper_types/0,
	       calculate_historical_values/0,
	       sum_csv/1]).

-define (GEN_GP,"scripts/generic.gp").
-define (SPEC_GP,"scripts/specific.gp").
-define(GOOGLE_PATH,"/Users/evabihari/Google Drive/Portfolio/").

%%%===================================================================
%%% API
%%%===================================================================
calculate_portfolio()->
    {{Y,M,D},_}=calendar:local_time(),
    Today=int_to_string(Y)++"-"++int_to_string(M)++"-"++int_to_string(D),
    case mnesia:dirty_match_object(daily_values,
                       #daily_value{date_currency_type={Today,'_','_'},_='_'}) of
        [] -> ok;
        Results -> remove_old_data(Results)
    end,
    FirstKey=mnesia:dirty_first(portfolio),
    calculate_portfolio2(FirstKey,Today).

calculate_portfolio(Date)->
    case mnesia:dirty_match_object(daily_values,
                       #daily_value{date_currency_type={Date,'_','_'},_='_'}) of
        [] -> ok;
        Results -> remove_old_data(Results)
    end,
    FirstKey=mnesia:dirty_first(portfolio),
    calculate_portfolio2(FirstKey,Date).


calculate_historical_values() ->
    {{Y,M,D},_}=calendar:local_time(),
    Today=calendar:date_to_gregorian_days(Y,M,D),
    Start=calendar:date_to_gregorian_days(2016,2,1),
    calculate_historical_values(Start,Today).
calculate_historical_values(Today,Today) ->
    ok;
calculate_historical_values(GrDay,Today) ->
    {Y,M,D}=calendar:gregorian_days_to_date(GrDay),
    Date=int_to_string(Y)++"-"++int_to_string(M)++"-"++int_to_string(D),
    case mnesia:dirty_match_object(exchanges,
                       #exchange{name_and_date={'_',Date},_='_'}) of
        [] -> ok;
        _Results -> calculate_portfolio(Date)
    end,
    calculate_historical_values(GrDay+1,Today).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
calculate_portfolio2('$end_of_table',Today) ->
    sum_daily_values(Today),
    ok;
calculate_portfolio2(Key,Today) ->
    {Name,_Type}=Key,
    Value=find_daily_value(Name,Today),
    [Portfolio]=mnesia:dirty_read(portfolio,Key),
    agregate_daily_values(Portfolio,Value,Today),
    calculate_portfolio2(mnesia:dirty_next(portfolio,Key),Today).

find_daily_value("Cash",_Date) ->
    io:format("Cash ~n",[]),
    1;
find_daily_value(_Name,"2010-01-01") ->
    0;
find_daily_value(Name,Date) ->
    case mnesia:dirty_read(exchanges,{Name,Date}) of
        [] ->
            Date1=less(Date),
            find_daily_value(Name,Date1);
        [Record] ->
            Record#exchange.value
    end.

less(Date) ->
    [Y,M,D]=string:tokens(Date,"-"),
    {Year,_}=string:to_integer(Y),
    {Month,_}=string:to_integer(M),
    {Day,_}=string:to_integer(D),
    less(Year,Month,Day).
less(Y,M,D) when (D==1) ->
    {Year,Month,Day} =case M of
                  1 -> {Y-1,12,31};
                  2 -> {Y,1,31};
                  3 -> {Y,2,29};
                  4 -> {Y,3,31};
                  5 -> {Y,4,30};
                  6 -> {Y,5,31};
                  7 -> {Y,6,30};
                  8 -> {Y,7,30};
                  9 -> {Y,8,31};
                  10 ->{Y,9,30};
                  11 ->{Y,10,31};
                  12 ->{Y,11,30}
                      end,
    int_to_string(Year)++"-"++int_to_string(Month)++"-"++int_to_string(Day);
less(Y,M,D) ->
    int_to_string(Y)++"-"++int_to_string(M)++"-"++int_to_string(D-1).

int_to_string(Int) when Int<10 ->
    "0"++integer_to_list(Int);
int_to_string(Int) ->
    integer_to_list(Int).

remove_old_data([]) ->
    ok;
remove_old_data([DV|List]) ->
    mnesia:dirty_delete(daily_values,DV#daily_value.date_currency_type),
    remove_old_data(List).

agregate_daily_values(Portfolio,Value,Date) ->
    {_Name,Type}=Portfolio#paper.name_and_type,
    case _Name of
      "Cash" -> io:format ("agregate_daily_values for Cash, Date=~p~n",[Date]);
      _-> ok
    end,
    Number=Portfolio#paper.number,
    Currency=Portfolio#paper.currency,
    Val=Number*Value,
    New_DV=case mnesia:dirty_read(daily_values,{Date,Currency,Type}) of
               [] ->   #daily_value{
                        date_currency_type={Date,Currency,Type},
                        value=Val};
               [DV|_] -> case DV#daily_value.date_currency_type of
                             %% {_,_,"SUM"} ->
                             %%     #daily_value{
                             %%        date_currency_type={Date,Currency,Type},
                             %%        value=Val};
                             {_,_,_Other} ->
                                 OldV=DV#daily_value.value,
                                 NewV=OldV+Val,
                                 DV#daily_value{value=NewV}
                         end
           end,
           case Type of
             "EQUILOR TBSZ" -> io:format ("agregate_daily_values for Cash, New_DV=~p Type=~p ~n",[New_DV, Type]);
             _-> ok
           end,
    mnesia:dirty_write(daily_values,New_DV).

sum_daily_values(Date) ->
    sum_daily_values(Date,0,0,0,mnesia:dirty_first(daily_values)).
sum_daily_values(Date,EUR_SUM, HUF_SUM, HUF1_SUM,'$end_of_table') ->
    EUR_DV=#daily_value{
              date_currency_type={Date,"EUR","SUM"},
              value=EUR_SUM},
    mnesia:dirty_write(daily_values,EUR_DV),
    HUF_DV=#daily_value{
              date_currency_type={Date,"HUF","SUM"},
              value=HUF_SUM},
    mnesia:dirty_write(daily_values,HUF_DV),
    HUF1_DV=#daily_value{
              date_currency_type={Date,"HUF1","SUM"},
              value=HUF1_SUM},
    mnesia:dirty_write(daily_values,HUF1_DV);
sum_daily_values(Date,EUR_SUM, HUF_SUM, HUF1_SUM, Key) ->
   {New_EUR_SUM,NEW_HUF_SUM,NEW_HUF1_SUM} = case Key of
                                   {Date,"EUR","SUM"} -> {EUR_SUM,HUF_SUM,HUF1_SUM};
                                   {Date,"HUF","SUM"} -> {EUR_SUM,HUF_SUM,HUF1_SUM};
                                   {Date,"HUF1","SUM"} -> {EUR_SUM,HUF_SUM,HUF1_SUM};
                                   {Date,"EUR",_} ->
                                       [Record]=mnesia:dirty_read(daily_values,Key),
                                       {EUR_SUM+Record#daily_value.value,HUF_SUM,HUF1_SUM};
                                   {Date,"HUF",_} ->
                                       [Record]=mnesia:dirty_read(daily_values,Key),
                                       {EUR_SUM,HUF_SUM+Record#daily_value.value,HUF1_SUM};
                                   {Date,"HUF1",_} ->
                                       [Record]=mnesia:dirty_read(daily_values,Key),
                                       {EUR_SUM,HUF_SUM,HUF1_SUM+Record#daily_value.value};
                                   _ ->
                                        {EUR_SUM,HUF_SUM,HUF1_SUM}
                               end,
    New_key=mnesia:dirty_next(daily_values,Key),
    sum_daily_values(Date,New_EUR_SUM,NEW_HUF_SUM,NEW_HUF1_SUM,New_key).

encode_name([]) -> "";
encode_name([225|Name])->
    "a"++encode_name(Name);
encode_name([246|Name])->
    "o"++encode_name(Name);
encode_name([195,188|Name])->
    "u"++encode_name(Name);
encode_name([195,186|Name])->
    "u"++encode_name(Name);
encode_name([195,161|Name])->
    "a"++encode_name(Name);
encode_name([195,169|Name])->
    "e"++encode_name(Name);
encode_name([195,179|Name])->
    "o"++encode_name(Name);
encode_name([195,173|Name])->
    "i"++encode_name(Name);
encode_name([195,182|Name])->
    "o"++encode_name(Name);
encode_name([195,129|Name])->
    "A"++encode_name(Name);
encode_name([197,177|Name])->
    "u"++encode_name(Name);
encode_name([197,145|Name])->
    "o"++encode_name(Name);
encode_name([233|Name])->
    "e"++encode_name(Name);
encode_name([193|Name])->
    "A"++encode_name(Name);
encode_name([252|Name])->
    "u"++encode_name(Name);
encode_name([237|Name])->
    "i"++encode_name(Name);
encode_name([250|Name])->
    "u"++encode_name(Name);
encode_name([369|Name])->
    "u"++encode_name(Name);
encode_name([Ch|Name]) when Ch>127 ->
    encode_name(Name);
encode_name([Ch|Name]) ->
    lists:flatten([Ch|encode_name(Name)]).

create_date(Y,M,D) when D<0 ->
    create_date(Y,M-1,30);
create_date(Y,M,D) when (M<10),(D<10) ->
    integer_to_list(Y)++"-0"++integer_to_list(M)++"-0"++integer_to_list(D);
create_date(Y,M,D) when M<10  ->
    integer_to_list(Y)++"-0"++integer_to_list(M)++"-"++integer_to_list(D);
create_date(Y,M,D) when D<10  ->
    integer_to_list(Y)++"-"++integer_to_list(M)++"-0"++integer_to_list(D);
create_date(Y,M,D) ->
    integer_to_list(Y)++"-"++integer_to_list(M)++"-"++integer_to_list(D).

dump_daily_values_table() ->
    {{Y,M,D},_}=calendar:local_time(),
    DateString=integer_to_list(Y)++"-"++integer_to_list(M)++"-"++integer_to_list(D),
 %   dump_daily_values_table("EUR",DateString),
 %   dump_daily_values_table("HUF",DateString).
    dump_daily_values_table("EUR",DateString),
    dump_daily_values_table("HUF",DateString),
    dump_daily_values_table("HUF1",DateString).

dump_daily_values_table(Currency,DateString) ->
    Name="data/"++DateString++"-"++Currency++".dat",
    io:format("dump_daily_values_table, Name=~p, DateString=~p~n",[Name,DateString]),
    NameIn="data/"++DateString++"-"++Currency++"unsorted"++".dat",
    io:format("dump_daily_values_table, NameIn=~p~n",[NameIn]),
    {ok,F} = file:open(NameIn,[read,write]),
    Sets=store_values(F,ets:match(daily_values,{'_',{'$1',Currency,'$2'},'$3'}),sets:new()),
    file:close(F),
    sort(NameIn,Name),
    io:format("dump_daily_values_table, after sort , Sets=~p~n",[Sets]),
    % file:delete(NameIn),
    draw_diagram(Name,sets:to_list(Sets),Currency).
    % file:delete(Name).

store_values(_F,[],Types) ->
    Types;
store_values(F,[Value|VList],Set) ->
    [Date, Type, Money]=Value,
    String=Date ++ "|" ++ Type ++ "|" ++ io_lib:write(Money) ++ "\n",
    file:write(F,String),
    NewSet=sets:add_element(Type,Set),
    store_values(F,VList,NewSet).

draw_diagram(FileName,Types,Currency) ->
    file:copy(?GEN_GP,?SPEC_GP),
    {ok,Target}=file:open(?SPEC_GP,[read,append]),
    String="set title \"" ++ FileName ++ " egyenlegek \" \n",
    ok=file:write(Target, String),
    FName= FileName--"data/",
    add_diagram(Target,FileName,Types,Currency),
    file:close(Target),
    Cmd="/usr/local/bin/gnuplot "++ ?SPEC_GP,
    io:format("draw_diagram, call Cmd=~p~n",[Cmd]),
    Result=os:cmd(Cmd),
    io:format("os:cmd Result=~pN`",[Result]),
    GoogleFile=?GOOGLE_PATH ++FName++".pdf",
    %% Wait until gnuplot will be finished
    timer:sleep(3000),
    file:copy(FileName++".pdf",GoogleFile).


add_diagram(Target,FileName,Types,"HUF") ->
    io:format("add_diagram(Target,FileName,Types,'HUF') ~n",[]),
    %%% SUM should use y2 axis and modify some parameters...
    Line1="set y2tics \n",
    ok=file:write(Target,Line1),
    Line2="set yrange   [900000 :*] \n",
    ok=file:write(Target,Line2),
    String="plot "++ "\""++FileName++"\""++" using 1:(stringcolumn(2) eq "++"\""++"SUM"++"\""++
        "? column(3):1/0) with boxes title "++"\""++"SUM"++"\""++" lc rgb \"orange\"  axes x1y2" ++ "\n",
    ok=file:write(Target,String),
    add_diagrams(Target,FileName,lists:delete("SUM",Types),"HUF",1);

add_diagram(Target,FileName,Types,"HUF1") ->
    io:format("add_diagram(Target,FileName,Types,'HUF1') ~n",[]),
    %%% SUM should use y2 axis and modify some parameters...
    Line1="set y2tics \n",
    ok=file:write(Target,Line1),
    Line2="set yrange   [600000 : *] \n",
    ok=file:write(Target,Line2),
    Line3="set y2range   [4500000 :*] \n",
    ok=file:write(Target,Line3),
    String="plot "++ "\""++FileName++"\""++" using 1:(stringcolumn(2) eq "++"\""++"SUM"++"\""++
        "? column(3):1/0) with boxes title "++"\""++"SUM"++"\""++" lc rgb \"orange\"  axes x1y2" ++ "\n",
    ok=file:write(Target,String),
    io:format("add_diagram, HUF1, Types=~p~n",[Types]),
    add_diagrams(Target,FileName,lists:delete("SUM",Types),"HUF1",1);

add_diagram(Target,FileName,[Type|Types],Currency) ->
    io:format("add_diagram(Target=~p,FileName=~p,[Type=~p|Types],Currency=~p) ~n",[Target,FileName,Type,Currency]),
    String = "plot "++generate_string(FileName,Type,1),
    ok=file:write(Target,String),
    add_diagrams(Target,FileName,Types,Currency,2).

add_diagrams(Target,FN,[],_Currency,_N) ->
    io:format("add_diagram(Target,FN,[],_Currency,_N) ~n",[]),
    {ok,Cwd}=file:get_cwd(),
    io:format("Current directory is~p~n",[Cwd]),
    String = "set output '| /usr/local/bin/ps2pdf - " ++ Cwd++"/"++FN ++ ".pdf \n",
    ok=file:write(Target,String),
    S2= "set size 1,1 \n" ++
        "set term post portrait color \"Times-Roman\" 12 \n" ++
        "replot \n",
    ok=file:write(Target,S2),
    io:format("add_diagrams, after file:write, Target=~p~n",[Target]),
    ok;

add_diagrams(Target,FileName,[Type|Types],Currency,N) ->
    io:format("add_diagram(Target,FileName,[Type|Types],Currency,N=~p) ~n",[N]),
    String = "replot "++generate_string(FileName,Type,N),
    ok=file:write(Target,String),
    add_diagrams(Target,FileName,Types,Currency,N+1).

generate_string(FileName,Type,N) ->
    "\""++FileName++"\""++" using 1:(stringcolumn(2) eq "++"\""++Type++"\""++
        "? column(3):1/0) title "++"\""++Type++"\""++" lc rgb \""++ map_to_color(N) ++ "\"\n".

map_to_color(1) ->
    "blue";
map_to_color(2) ->
    "red";
map_to_color(3) ->
    "yellow";
map_to_color(4) ->
    "green";
map_to_color(_N) ->
    "black".
dump_to_csv(FileName) ->
    mnesia:start(),
    mnesia:load_textfile("data/mnesia.txt"),
    {ok,Target}=file:open(FileName,[write]),
    csv_gen:row(Target, ["Currency","Type","Date","Value"]),
    ets:new(dump_table,[named_table,bag]),
    write_record(Target, ets:first(daily_values)),
    ets:delete(dump_table),
    file:close(Target).

write_record(Target,'$end_of_table') ->
    convert_ets_to_csv(Target);
write_record(Target,Key) ->
    io:format("~p~n",[Key]),
    [Data]=ets:lookup(daily_values,Key),
    case Key of
	{Date,Currency,Type} ->
	    Value=Data#daily_value.value,
	    ets:insert(dump_table,{Date,[Currency,Type,Value]});
	_ -> no_tuple
    end,
    NewKey=ets:next(daily_values,Key),
    write_record(Target,NewKey).


convert_ets_to_csv(Target) ->
    Result=sort_table(),
    write_result(Target,Result).

sort_table() ->
    Orig_rows=ets:tab2list(dump_table),
    F=fun(X,Y) ->
	      {DateX,[CurrX,TypeX,_]}=X,
	      {DateY,[CurrY,TypeY,_]}=Y,
	      CurrX++TypeX++DateX<CurrY++TypeY++DateY
      end,
    lists:sort(F,Orig_rows).

write_result(_Target,[]) ->
    ok;
write_result(Target,[{Date,[Currency,Type,Value]}|List])->
    csv_gen:row(Target, [Currency,Type,Date,Value]),
    write_result(Target,List).


create_date_list('$end_of_table')->
    [];
create_date_list({Date,Currency,Type}) ->
    NewKey=ets:next(daily_values,{Date,Currency,Type}),
    [Date|create_date_list(NewKey)];
create_date_list(Key) ->
    NewKey=ets:next(daily_values,Key),
    create_date_list(NewKey).


sort_date_list(DateList) ->
    One={1,0,0},
    F=fun(X,Y) -> X_Date=string_to_date(X),
		  Y_Date=string_to_date(Y),
		  calendar:datetime_to_gregorian_seconds({X_Date,One}) < calendar:datetime_to_gregorian_seconds({Y_Date,One})
      end,
    lists:sort(F,DateList).

string_to_date(DateString) ->
    [Y,M,D]=string:tokens(DateString,"-"),
    {to_int(Y),to_int(M),to_int(D)}.

to_int(IntString)->
    {Int,_}=string:to_integer(IntString),
    Int.

get_and_sort_dates()->
%    Dates=create_date_list(ets:first(daily_values)),
    Dates=go_through(1,ets:first(daily_values)),
    sort_date_list(lists:usort(Dates)).

collect_paper_types() ->
    Types=go_through2(2,3,ets:first(daily_values)),
    lists:usort(Types).

go_through(_Element,'$end_of_table') ->
    [];
go_through(Element,Key) when is_tuple(Key)->
    Item=element(Element,Key),
    [Item|go_through(Element,ets:next(daily_values,Key))];
go_through(Element,Key) ->
    go_through(Element,ets:next(daily_values,Key)).

go_through2(_Element,_,'$end_of_table') ->
    [];
go_through2(E1,E2,Key) when is_tuple(Key)->
    Item1=element(E1,Key),
    Item2=element(E2,Key),
    [{Item1,Item2}|go_through2(E1,E2,ets:next(daily_values,Key))];
go_through2(E1,E2,Key) ->
    go_through2(E1,E2,ets:next(daily_values,Key)).

sum_csv(FileName) ->
    mnesia:start(),
    mnesia:load_textfile("data/mnesia.txt"),
    {ok,Target}=file:open(FileName,[write]),
    Paper_Types=collect_paper_types(),
    Type_header=skip_currency( Paper_Types),
    csv_gen:row(Target, ["Date"]++Type_header),
    Dates=get_and_sort_dates(),
    write_rows(Target,Paper_Types,Dates),
    file:close(Target).


write_rows(_File,_Types,[]) ->
    ok;
write_rows(File,Types,[Date|Dates]) ->
    Data=generate_row(Types, Date,[]),
    csv_gen:row(File,[Date]++format_data(Data)),
    write_rows(File,Types,Dates).

generate_row([],_Date,ValueList) ->
    lists:reverse(ValueList);
generate_row([{Curr,Type}|Types], Date, ValueList) ->
    Value=find_value({Curr,Type},Date),
    generate_row(Types, Date, [{Curr,Value}|ValueList]).

find_value({Curr,Type},Date) ->
    case ets:match(daily_values,{daily_value,{Date,Curr,Type},'$2'}) of
	[] -> 0.0;
	[[Value]|_] -> Value
    end.

skip_currency([]) ->
    [];
skip_currency([{_Curr,Type}|List]) ->
    io_lib:fwrite("~s",[Type])++skip_currency(List).


%% convert [{"EUR",9988.4972},{"HUF",12000.3}|List] to ["9988.5 EUR","12000.3 HUF"|List]
format_data([]) ->
    [];
format_data([{_Currency, " "}|List]) ->
    [" "|format_data(List)];
format_data([{Currency, Value}|List]) ->
    [SValue]=io_lib:fwrite("~.2f",[Value]),
    [SValue ++" "++ Currency|format_data(List)].

sort(In,Out)->
    {ok,F}=file:open(In,[read]),
    Lines=read(F,file:read_line(F)),
    NewLines=lists:sort(Lines),
    {ok,O}=file:open(Out,[write]),
    write(O,NewLines).

read(_F,eof) ->
    [];
read(F,{ok,Data}) ->
    [Data|read(F,file:read_line(F))].

write(O,[]) ->
    file:close(O);
write(O,[H|T]) ->
    file:write(O,H),
    write(O,T).
