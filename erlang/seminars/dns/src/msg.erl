%% Decoding and encodng of DNS messages. 


-module(msg).

-compile(export_all).  %% during development 


decode(<<Id:16, Flags:2/binary, QDC:16, ANC:16, NSC:16, ARC:16, Body/binary>>=Raw) ->
    <<QR:1, Op:4, AA:1, TC:1, RD:1, RA:1, _:3, Resp:4>> = Flags,
    Decoded = decode_body(QDC, ANC, NSC, ARC, Body, Raw),
    {Id, QR, Op, AA, TC, RD, RA, Resp, Decoded}.

%%% 
%%% Id - identifier
%%% QR - query (0) or response (1)
%%% OP - opcode, regular (0), inverser (1), status (2)
%%% AA - Autorative Answer 
%%% TC - 
%%% RD - recursive desired
%%% RA - recursive available 
%%% Resp

decode_body(QDC, ANC, NSC, ARC, Body0, Raw) ->
    {Query, Body1} = decode_query(QDC, Body0, Raw),
    {Answer, Body2} = decode_answer(ANC, Body1, Raw),
    {Authority, Body3} = decode_answer(NSC, Body2, Raw),
    {Additional, _} = decode_answer(ARC, Body3, Raw),
    {Query, Answer, Authority, Additional}.



decode_query(0, Body, _) ->
    {[], Body};
decode_query(N, Queries, Raw) ->
    {Name, <<QType:16, QClass:16, Next/binary>>} = decode_name(Queries, Raw),
    {Decoded, Body} = decode_query(N-1, Next, Raw),
    {[{Name, QType, QClass} | Decoded], Body}.


decode_answer(0, Body, _) ->
    {[], Body};
decode_answer(N, Answers, Raw) ->
    {Name, <<Type:16, Class:16, TTL:32, RDLength:16, Rest/binary>>} = decode_name(Answers, Raw),
    {RData, Next} =   decode_rdata(RDLength, Rest),
    Record = decode_record(Type, Class, RData, Raw),
    {Decoded, Body} = decode_answer(N-1, Next, Raw),
    {[{Name, TTL, Record} | Decoded], Body}.


decode_rdata(N, Rest) ->
    <<RData:N/binary, Next/binary>> = Rest,
    {RData, Next}.


%%% The most common records

-define(A, 1).
-define(NS, 2).
-define(CNAME, 5).
-define(SOA, 6).
-define(PTR, 12).
-define(MX, 15).
-define(TXT, 16).
-define(AAAA, 28).

decode_record(?A, _, <<I1:8, I2:8, I3:8, I4:8>>, _) ->
    {a, {I1, I2, I3, I4}};
decode_record(?NS, _, RData, Raw) ->
    {Name, _} = decode_name(RData, Raw),
    {ns, Name};
decode_record(?CNAME, _, RData, Raw) ->
    {Name, _} = decode_name(RData, Raw),
    {cname, Name};
decode_record(?SOA, _, RData, Raw) ->
    {Primary, RD1} = decode_name(RData, Raw),
    {Admin, RD2} = decode_name(RD1, Raw),
    <<Serial:32, Refr:32, Retr:32, Exp:32, TTL:32>> = RD2,
    {soa, Primary, Admin, Serial, Refr, Retr, Exp, TTL};
decode_record(?MX, _, <<Pred:16, RData/binary>>, Raw) ->
    {Name, _} = decode_name(RData, Raw),
    {mx, Pred, Name};
decode_record(?PTR, _, RData, Raw) ->
    {Name, _} = decode_name(RData, Raw),
    {ptr, Name};
decode_record(?TXT, _, RData, _) ->
    {txt, binary_to_list(RData)};
decode_record(?AAAA, _, RData, _) ->
    {ipv6, RData};
decode_record(Type, Class, RData, _) ->
    {Type, Class, RData}.


decode_name(Label, Raw) ->
    decode_name(Label, [], Raw).

decode_name(<<0:1, 0:1, _:6, _/binary>>=Label, Names, Raw) -> 
    %% regular name encoding
    decode_label(Label, Names, Raw);
decode_name(<<1:1, 1:1, N:14, Rest/binary>>, Names, Raw) ->
    %% offset encoding
    Offset = 8*N,    
    <<_:Offset, Section/binary>> = Raw,
    {Name, _} = decode_label(Section, Names, Raw),
    {Name, Rest}.


decode_label(<<0:8, Rest/binary>>, Names, _) ->
    {lists:reverse(Names), Rest};
decode_label(<<N:8, Rest/binary>>, Names, Raw) ->
    decode_label(N, Rest, [], Names, Raw).

decode_label(0, Rest, Sofar, Names, Raw) ->
    decode_name(Rest, [lists:reverse(Sofar)|Names], Raw);

decode_label(N, <<Char:8, Rest/binary>>, Sofar, Names, Raw) ->
    decode_label(N-1, Rest, [Char|Sofar], Names, Raw).

