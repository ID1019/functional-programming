-module(test).

-compile(export_all).

-define(Walk, 1000).

start() ->
    {ok, Shop} = shop:start(2),
    {ok, _} = customer:start("Joe", Shop),
    {ok, _} = customer:start("Bill", Shop),
    {ok, _} = customer:start("Jack", Shop),    
    {ok, _} = customer:start("Ron", Shop),
    {ok, _} = customer:start("Tom", Shop),
    {ok, _} = customer:start("Zeb", Shop),
    register(shop, Shop).

close() ->    
    shop ! close.

	    
		    

	
	    

    
