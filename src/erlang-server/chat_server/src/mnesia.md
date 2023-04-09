Nodo 1:
```
	erl -sname nodo1 -setcookie cookie
```

Nodo 2:
```
	erl -sname nodo2 -setcookie cookie
```

Su nodo 1 eseguire le seguenti istruzioni Erlang:
```
	
	mnesia:create_schema([node(), 'nodo2@riccardo-linux']).
	mnesia:start().
	mnesia:info().
	mnesia:create_table(prova, [{type, bag}, {ram_copies, [node(), 'nodo2@riccardo-linux']}]).
```

Su nodo 2:
```
	mnesia:start().
	mnesia:info().
```
