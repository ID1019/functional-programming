-module(transport).

-export([start/2]).

-include("transport.hrl").

start(To, Size) ->
    spawn(fun() -> init(To, Size) end).

init(To, Size) ->
    receive
	{connect, Net} ->
	    Receiver = spawn_link(fun() -> receiver(To, 0, Size, [], Net) end),
	    Sender = spawn_link(fun() -> sender(To, 0, 1, [], Net) end),
	    transport(Sender, Receiver)
    end.

transport(Sender, Receiver) -> 
    receive
	{send, Msg} ->
	    Sender ! {send, Msg},
	    transport(Sender, Receiver);
	{read, Pid} ->
	    Receiver ! {read, Pid},
	    transport(Sender, Receiver);
	{msg,  #trp{}=Data} ->
	    Receiver ! Data,
	    transport(Sender, Receiver);
	{msg,  #ack{}=Ack} ->
	    Receiver ! Ack,
	    transport(Sender, Receiver)
    end.

%% The receiver process will receive messages from the remote sender
%% process. Messages are numbered and will be delivered in order. The
%% receiving buffer size is communicated to the sender in ack-messages. 

receiver(To, N, 0, [{_, Msg}|Rest], Net) ->
    %% The buffer is full, we wait for a read operation. 
    receive 
	{read, Pid} ->
	    Pid ! {msg, Msg},
	    Net ! #ack{size=1},
	    receiver(To, N, 1, Rest, Net)
    end;

receiver(To, N, S, [], Net) ->
    %% The buffer is empty, we will only accept datagrams.
    receive
	#trp{id=N, data=Msg} ->
	    Net ! {send, To, #ack{id=N, size=(S-1)}},	    
	    receiver(To, N+1, S-1, [{N, Msg}], Net)
    end;

receiver(To, N, S, [{_, Msg}|Rest]=Buffer, Net) ->
    %% Normal operation
    receive
	{read, Pid} ->
	    Pid ! {msg, Msg},
	    Net ! {send, To, #ack{size=S+1}},
	    receiver(To, N, S+1, Rest, Net);

	#trp{id=N, data=Msg} ->
	    Net ! {send, To, #ack{id=N, size=(S-1)}},	    
	    receiver(To, N+1, S-1, Buffer++[{N, Msg}], Net)
    end.
    
%% The sender process will send messages to the remote receiver. It
%% will assume a sending window of 1 and then adapt to the receiver.

sender(To, N, 0, [{A, _}|Rest], Net) ->
    receive
	#ack{id=A, size=S} ->
	    sender(To, N, S, Rest, Net)
    end;
sender(To, N, S, [{A, _}|Rest]=Sent, Net) ->
    receive 
	#ack{id=A, size=Si} ->
	    sender(To, N, Si, Rest, Net);
	{send, Msg} ->
	    Net ! {send, To, #trp{id=N, data=Msg}},
	    sender(To, N+1, S-1, Sent++[{N, Msg}], Net)
    end.



    
    



	    
			  
	    
    
