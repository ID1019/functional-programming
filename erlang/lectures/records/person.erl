-module(person).
-compile(export_all).

-record(person, {name, age, ref}).

new(Name, Age) ->
  #person{name=Name, age=Age, ref= make_ref()}.

name(#person{name=Name}) -> Name.

age(#person{age=Age}) -> Age.

