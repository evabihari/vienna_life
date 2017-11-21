%%%-------------------------------------------------------------------
%%% @author eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%% @copyright (C) 2016, eva.bihari
%%% @doc
%%%
%%% @end
%%% Created :  9 Feb 2016 by eva.bihari <evabihari@Evas-MacBook-Pro.local>
%%%-------------------------------------------------------------------
-module(arfolyam).
-export [start/0,start/1,
         read_portfolio/0,
         read_portfolio/1,
         calculate_portfolio/0,
         mkb/0,
         get_value/1,
         init/0,
	 equilor/0].
-define (VL_URL,"https://www.viennalife.hu/befektetes/eszkozalapok/napi-arfolyam").
-define (ML_URL,"http://www.metlifehungary.hu/portfoliok").
-define (MKB_URL,"https://alapkezelo.mkb.hu/arfolyamok_es_hozamok/arfolyam_tablazat/index.html").
-define (PORTFOLIO_FILE,"data/portfolio.txt").
-define (EQUILOR_PRIMUS,"http://befektetesi.alapozo.hu/befektetesi-alapok/633-equilor-primus-alapok-alapja").
-define (CONCORDE_VM,"http://befektetesi.alapozo.hu/befektetesi-alapok/368-concorde-vm-abszolut-szarmaztatott-alap").
-define(ENEFI,"http://privatbankar.hu/reszvenyarfolyamok").
-define(AEGON,"https://www.aegonalapkezelo.hu").
-include("../include/records.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start() ->
    start(?VL_URL),
    mkb(),
    equilor(),
    aegon(),
    start(?ML_URL,ok).

start(Url) ->
    start(Url,init()).

read_portfolio()->
    read_portfolio(?PORTFOLIO_FILE).
read_portfolio(Name) ->
    {ok,F} = file:open(Name,[read]),
    read_papers(F).

calculate_portfolio()->
    utils:calculate_portfolio().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
init() ->
    %% code:add_pathz("/Users/evabihari/external/mochiweb/ebin"),
    %% code:add_pathz("/Users/evabihari/external/mochiweb_xpath/ebin"),
    error_logger:tty(false),
    error_logger:logfile({open, log_report}),
    inets:start(),
    ssl:start(),
    %% lager:start(),
    mnesia:create_schema([node()]),
    mnesia:start(),
    create_portfolio_table(),
    create_daily_values_table(),
    case mnesia:create_table(exchanges,
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, record_info(fields, exchange)},
                              {record_name, exchange}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,tickets}} ->
            error_logger:info_msg("exchanges table already_exists");
        Other ->
            error_logger:error_msg("exchanges table creation failed , reason = ~p~n",[Other])
    end.

start(Url,ok) ->
    %% httpc:set_options([{cookies,verify}]),
    _Method=get,
    %% Header=[],
    _Type = "application/x-www-form-urlencoded",
    %% Body = "username=enzian1&password=ethebi",
    _HTTPOptions=[],
    _Options=[],
    %% R = httpc:request(_Method, {Url, Header, _Type, Body}, _HTTPOptions,_Options),
    try httpc:request(Url) of
      R ->
        {ok, {{"HTTP/1.1",ReturnCode, _State}, _Head, Body}} = R,
        case ReturnCode of
          200 ->
            {ok,F} = file:open("page.txt",[read,write]),
            file:write(F,Body),
            %% tokenize and then transform the token stream into a HTML tree.
            Struct=mochiweb_html:parse(Body),
            {ok,F2} = file:open("structs.txt",[read,write]),
            file:write(F2,io_lib:print(Struct)),
            Date=extract_date(Struct,Url),
            extract_and_store_rates(Struct,Date,Url);
          OtherCode ->
            io:format("Request to Url=~p Resulted in ReturnCode=~p~n",[Url,OtherCode])
        end
      catch
        _E:Reason -> io:format("Error, curl to Url=~p can not be decoded, Reason=~p ~n",[Url,Reason])
    end;
start(_Url,Other) ->
    io:format("Init failed with reason = ~p~n",[Other]).

extract_and_store_rates(Struct,Date,?VL_URL) ->
    Path="html/body/div/div/div/div/div/ul",
    Results = mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("values.txt",[read,write]),
    file:write(File,io_lib:print(Results)),
    decode_types(Results,Date);
extract_and_store_rates(Struct,Date,?ML_URL) ->
    Path="html/body/div/div/div/div/div/div/table/tbody/tr",
    Results = mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("ML_values.txt",[read,write]),
    file:write(File,io_lib:print(Results)),
    decode_ml_types(Results,Date).

decode_types([],_) ->
    ok;
decode_types([Result|_List],Date) ->
    Path="ul/li/p",
    R = mochiweb_xpath:execute(Path,Result),
    decode_values(R,Date).
    %%    only the 1st part is interesting at this moment
    %%    decode_types(List).

decode_values([],_) ->
    ok;
decode_values([Value|List],Date) ->
    PName="p/span[position() = 1]",
    PValue="p/span[position() = 2]",

    [{_,_,[RName]}] =  mochiweb_xpath:execute(PName,Value),
    Name=binary_to_list(RName),
    _RValue = case mochiweb_xpath:execute(PValue,Value) of
                 [{_,_,[RV]}] -> Rate=binary_to_list(RV),
                                 create_record_and_store(Name, Rate, Date),
                                 Rate;
                 _ -> dont_know
             end,
    decode_values(List,Date).

extract_date(Struct,?VL_URL)->
    Path="html/body/div/div/div/div/div/h5[position() = 1]",
    R=mochiweb_xpath:execute(Path,Struct),
    case R of
        [{_,_,[BinString]}|_] ->
            String=binary_to_list(BinString),
            SList=string:tokens(String,"()"),
            DateString=lists:last(SList),
            [Y,M,D]=string:tokens(DateString,". "),
            Y++"-"++M++"-"++D;
        _ -> {{Y,M,D},_}=calendar:local_time(),
             utils:create_date(Y,M,D)
    end;
extract_date(Struct,?ML_URL) ->
    Path="html/body/div/div/div/div/div/div/table/thead/tr[position() = 2]",
    [R]=mochiweb_xpath:execute(Path,Struct),
    [R1]=mochiweb_xpath:execute("tr/td[position() = 2]",R),
    [{_,_,[DateBin]}]=mochiweb_xpath:execute("td/div/span[position() =2]",R1),
    %% [{<<"span">>,[{<<"class">>,<<"data">>}],[<<"2016.02.01">>]}]
    re:replace(binary_to_list(DateBin),"\\Q.\\E","-",[global,{return,list}]).

create_record_and_store(Name, RateInfo, Date) ->
    [ValueString,Currency]=case string:tokens(RateInfo," ") of
			       [A,B|_] -> [A,B];
			       [A] -> ["0",A]   % daily value is missing from the table
			   end,
    {Value,_}=string:to_float(ValueString),
    Record=#exchange{name_and_date={utils:encode_name(string:strip(Name,both)),
                                    Date},
                     value=Value,
                     currency=Currency},
    mnesia:dirty_write(exchanges,Record).

decode_ml_types([],_Date) ->
    ok;
decode_ml_types([Result|List],Date) ->
    NamePart=mochiweb_xpath:execute("tr/td/a",Result),
    Name=extract_value(NamePart),
    %% [{<<"a">>,
    %%   [{<<"href">>,<<"/portfoliok/Y1/">>}],
    %%   [<<"Ázsiai ingatlan részvény eszközalap (HUF)"/utf8>>]}]
    ValuePart=mochiweb_xpath:execute("tr/td[position() = 2]",Result),
    %% [{<<"td">>,
    %%   [{<<"class">>,<<"number number-1">>}],
    %%   [<<"1,13703 HUF">>]}]
    ValueString=extract_value(ValuePart),
    create_record_and_store(Name, ValueString,Date),
    decode_ml_types(List,Date).

extract_value([{_,_,[ValueString]}]) ->
    binary_to_list(ValueString).

extract_div({<<"div">>,_,[ValueString]})->
    binary_to_list(ValueString).

read_papers(F) ->
    case file:read_line(F) of
        {ok,Data} ->
            handle_data(Data,F);
        eof ->
            ok;
        {error,Reason} ->
            {error,Reason}
    end.

handle_data(Data,F) ->
    [Name,Number,Currency,Type|_]=string:tokens(Data,"|"),
    {N,_}=string:to_integer(string:strip(Number)),
    Name1=string:strip(Name,both),
    Record=#paper{name_and_type={Name1,Type},
                  number=N,
                  currency=Currency},
    NewRecord= case mnesia:dirty_read(portfolio,{Name1,Type}) of
                   [] -> Record;
                   [Old_rec] -> %same type of paper already in the portfolio
                       Old_number=Old_rec#paper.number,
                       Record#paper{number=Old_number+N}
               end,
    mnesia:dirty_write(portfolio,NewRecord),
    read_papers(F).

create_daily_values_table()->
    case mnesia:create_table(daily_values,
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes,
                               record_info(fields, daily_value)},
                              {record_name, daily_value}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,daily_values}} ->
            error_logger:info_msg("daily_values table already_exists");
        Other ->
            error_logger:error_msg("daily_values table creation failed ,
                                    reason = ~p~n",[Other])
    end.

create_portfolio_table()->
    mnesia:delete_table(portfolio),
    case mnesia:create_table(portfolio,
                             [{disc_copies,[node()]},
                              {type, ordered_set},
                              {attributes, record_info(fields, paper)},
                              {record_name, paper}]) of
        {atomic,ok} -> ok;
        {aborted,{already_exists,portfolio}} ->
            error_logger:info_msg("portfolio table already_exists");
        Other ->
            error_logger:error_msg("portfolio table creation failed , reason = ~p~n",[Other])
    end.


mkb()->
    R=httpc:request(?MKB_URL),
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}} = R,
    Struct=mochiweb_html:parse(Body),
    Path="html/body/div/div/div/table/tr/td",
    ResultList=mochiweb_xpath:execute(Path,Struct),
    % io:format("ResultList: ~p~n",[ResultList]),
    {ok,File} = file:open("MKB_results.txt",[read,write]),
    file:write(File,io_lib:print(ResultList)),
    decode_mkb_results(ResultList).




decode_mkb_results([])->
    [];
decode_mkb_results([R1,R2,R3,R4,R5|List]) ->
    [Name1,Date,_Netto_value,Rate,_Year_profit]=
        lists:map(fun arfolyam:get_value/1,[R1,R2,R3,R4,R5]),
    % io:format("~s ~p ~p ~p ~p ~n",[Name1,Date,Netto_value,Rate,Year_profit]),
    case Name1 of
        [] -> ok;
        Name ->
            Value = case string:to_float(Rate) of
                        {error,_} -> 0;
                        {V,[]} -> V;
                        {_V1,_Remaining} ->
                            [S1,S2,S3|_]=string:tokens(Rate,",."),
                            N2=length(S2),
                            N3=length(S3),
                            list_to_integer(S1)*math:pow(10,N2)+list_to_integer(S2)+
                                list_to_integer(S3)*math:pow(10,-1*N3)
            end,
            Record=#exchange{name_and_date=
                                 {utils:encode_name(string:strip(Name,both)),Date},
                              value=Value,
                              currency="HUF"},
            mnesia:dirty_write(exchanges,Record),
            decode_mkb_results(List)
     end;
decode_mkb_results(_) ->
    io:format("??~n",[]),
    ok.

equilor() ->
    equilor(?EQUILOR_PRIMUS),
    equilor(?CONCORDE_VM),
    enefi().

equilor(Url)->
    R=httpc:request(Url),
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}} = R,
    Struct=mochiweb_html:parse(Body),
    Path="html/body/div/div/div/div/div/div/div/div/section/div/div/table/tbody/tr/td",
    ResultList=mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("EQ_Primus_results.txt",[read,write]),
    file:write(File,io_lib:print(ResultList)),
    decode_equilor(ResultList,Url).

decode_equilor([],_) ->
    [];
decode_equilor([DT,VT|List],Url) ->
						%  example: {<<"td">>,[],[<<"2016-12-13">>]},{<<"td">>,[],[<<"1,188894">>]}
    Name=case Url of
	     ?EQUILOR_PRIMUS ->"Equilor Primus Alapok Alapja";
	     ?CONCORDE_VM -> "Concorde VM Abszolut Szarmaztatott Alap"
    end,
    Date=binary_to_list(lists:nth(1,erlang:element(3,DT))),
    Value=list_to_float(binary_to_list(lists:nth(1,erlang:element(3,VT)))),
    Record=#exchange{name_and_date=
			 {utils:encode_name(string:strip(Name,both)),Date},
		     value=Value,
		     currency="HUF1"},
    mnesia:dirty_write(exchanges,Record),
    decode_equilor(List,Url).
  aegon() ->
    aegon(?AEGON).

  aegon(Url) ->
    Result=httpc:request(get, {Url, [{"User-Agent","curl/7.54.0"}]},[], []),
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}} = Result,
    Struct=mochiweb_html:parse(Body),
    Path = "html/body//div/div/div/div/div/div/div/div/div/div[position() = 3]/div/div/div/div",
    ResultList=mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("Aegon_arfolyam_results.txt",[read,write]),
    file:write(File,io_lib:print(ResultList)),
    Date=find_date(Struct),
    decode_aegon(Date,ResultList,Url).

  find_date(Struct)->
    Path = "html/body//div/div/div/div/div/div/div/div/div/div[position() = 2]",
    ResultList=mochiweb_xpath:execute(Path,Struct),
      % example:
      % [{<<"div">>,
      % [{<<"class">>,<<"row no-margin-padding">>}],
      % [{<<"h4">>,
      %   [{<<"class">>,<<"arfolyam-text">>}],
      %   [<<"Árfolyamok - "/utf8>>,
      %    {<<"span">>,
      %     [{<<"class">>,<<"arfolyamWidgetDate">>}],
      %     [<<"2017.11.17">>]}]}]}]
      [{_,_,H4}]=ResultList,
      [{_,_,DatePart}]=H4,
      [_,{_,_,DateBinList}]=DatePart,
      [DateBin|_]=DateBinList,
      re:replace(binary_to_list(DateBin),"\\Q.\\E","-",[global,{return,list}]).

  decode_aegon(_,[],_) ->
      [];
  decode_aegon(Date,[Data|List],Url) ->
  						% example:
              % Data={<<"div">>,
              % [{<<"class">>,<<"row arfolyam-box">>},{<<"style">>,<<>>}],
              % [{<<"div">>,
              %   [{<<"class">>,<<"arfolyam-border">>}],
              %   [{<<"div">>,[{<<"class">>,<<"arfolyam_name">>}],[<<"Alfa">>]},
              %    {<<"div">>,
              %     [{<<"class">>,<<"arfolyam-percent">>}],
              %     [{<<"span">>,[{<<"class">>,<<"iconset green">>}],[<<"+0.016%">>]}]},
              %    {<<"div">>,
              %     [{<<"class">>,<<"arfolyam_torl">>}],
              %     [{<<"span">>,[{<<"class">>,<<"iconset green">>}],[<<238,128,160>>]}]},
              %    {<<"div">>,[{<<"class">>,<<"arfolyam_foly">>}],[<<"2.688306">>]},
              %    {<<"div">>,[{<<"class">>,<<"cl">>}],[]}]}]}
    {<<"div">>,_A,B}=Data,
    [{<<"div">>,_A1,B1}]=B,
    [NameDiv,_PercentDiv,_TorlDiv,ArfDiv|_]=B1,

    io:format("NameDiv=~p, ArfDiv=~p~n",[NameDiv,ArfDiv]),

    Name=extract_div(NameDiv),
    Arfolyam=extract_div(ArfDiv),
    Record=#exchange{name_and_date=
       {utils:encode_name(string:strip(Name,both)),Date},
         value=string_to_num(Arfolyam),
         currency="HUF"},
    mnesia:dirty_write(exchanges,Record),
    decode_aegon(Date,List,Url).

  string_to_num(N) ->
      case string:to_float(N) of
          {error,no_float} -> list_to_integer(N);
          {F,_Rest} -> F
      end.

enefi() ->
    Url=?ENEFI,
    R=httpc:request(Url),
    {ok, {{"HTTP/1.1",_ReturnCode, _State}, _Head, Body}} = R,
    Struct=mochiweb_html:parse(Body),
    Path="html/body/div/div/div/div/div/ul/li/div/div/table/tbody/tr",
    ResultList=mochiweb_xpath:execute(Path,Struct),
    {ok,File} = file:open("ENEFI_results.txt",[read,write]),
    file:write(File,io_lib:print(ResultList)),
    decode_enefi(ResultList).

decode_enefi([])->
    [];
decode_enefi([{<<"tr">>,
	       [{<<"class">>,_C},
		{<<"data-title">>,<<"enefi">>}],Rest}|List]) ->
    [_TH,Price,_Min,_Max,_Diff,_BuyOffer,_SellOffer,_,DateElement,_Time|_]=Rest,
    Value=list_to_integer(get_value(Price)),
    Date=get_value(DateElement),
    Name="ENEFI reszveny",
    Record=#exchange{name_and_date=
			 {utils:encode_name(string:strip(Name,both)),Date},
		     value=Value,
		     currency="HUF1"},
    mnesia:dirty_write(exchanges,Record),
    decode_enefi(List);
decode_enefi([_H|List]) ->
    decode_enefi(List).

get_value({<<"td">>,[],[{<<"a">>,_,_}|_]}) ->
    [];
get_value({<<"td">>,_,[BinValue|_]}) ->
    re:replace(binary_to_list(BinValue), "(^\\s+)|(\\s+$)", "", [global,{return,list}]).

         %% {daily_values,[{record_name,daily_value},
         %%                {attributes,[date_currency_type,value]}]},
         %% {exchanges,[{record_name,exchange},
         %%             {attributes,[name_and_date,value,currency]}]}]}.
