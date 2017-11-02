-module(data).

-compile(export_all).

sample(File, N) ->
    {ok, Fd} = file:open(File, [read, binary]),
    {ok, Binary} = file:read(Fd, N),
    file:close(Fd),
    unicode:characters_to_list(Binary, utf16).
