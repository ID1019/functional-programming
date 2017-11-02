-module(gui).
-export([start/1, init/1]).
-include_lib("wx/include/wx.hrl").

start(Name) ->
	spawn_link(gui, init, [Name]).

init(Name) ->
	Width = 200,
	Height = 200,
	Server = wx:new(),  %Server will be the parent for the Frame
    	Frame = wxFrame:new(Server, -1, Name, [{size,{Width, Height}}]), 
	wxFrame:show(Frame),
	loop(Frame).
	

loop(Frame)->
	receive
		waiting ->
			wxFrame:setBackgroundColour(Frame, {255, 255, 0}),	%wxYELLOW doesn't exist in wx/include/wx.hrl
			loop(Frame);
		enter ->
			wxFrame:setBackgroundColour(Frame, ?wxRED),
			loop(Frame);
		leave ->
			wxFrame:setBackgroundColour(Frame, ?wxBLUE),
			loop(Frame);
		abort ->
			wxFrame:setBackgroundColour(Frame, ?wxBLACK),
			loop(Frame);
		stop ->
			ok;
		Error ->
			io:format("gui: strange message ~w ~n", [Error]),
			loop(Frame)
	end.


