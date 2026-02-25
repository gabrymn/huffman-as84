/*
	Insieme di funzionalità per generare una lista di coppie simbolo-frequenza
	L = { [<S-L>] : S è un simbolo, L è un peso come frequenza}
	
	il punto di partenza è una stringa (verrà usata come array di char) qualsiasi 
	"ciao come va..." che può arrivare da un messaggio in input o dal contenuto 
	di un file, che in genere è di testo ma può essere qualsiasi file, dato che 
	un file in generale è una sequenza di byte
*/
%-------------------------------------------------------------------------------

read_file(File, String) :-
    open(File, read, Stream),
    read_string(Stream, _, String),
    close(Stream).

update_symbol_w([], C, [C-1]).

update_symbol_w([C-F|Resto], C, [C-NF|Resto]) :- 
    NF is F + 1.

update_symbol_w([AltF|Resto], C, [AltF|NuovoResto]) :-
    AltF = D-_,
    C \= D,
    update_symbol_w(Resto, C, NuovoResto).

count_freq([], []).
count_freq([C|Resto], Risultato) :-
    count_freq(Resto, Temp),
    update_symbol_w(Temp, C, Risultato).

gen_sw(StrOrPath, SWs, FileBool) :-
	get_string(StrOrPath, Str, FileBool),
	string_chars(Str, Chars),
	count_freq(Chars, SWs),
	!.

get_string(Str, Str, 0) :- string(Str), !.
get_string(FilePath, Str, 1) :- read_file(FilePath, Str), string(Str).

	
%-------------------------------------------------------------------------------




% UTILS
%-------------------------------------------------------------------------------

strlen(Str, L) :-
	string(Str),
	string_chars(Str, Chars),
	listlen(Chars, L).   
    
listlen([], 0).
listlen([_|T], N) :-
	listlen(T, Ns),
	N is Ns+1.	
	
%-------------------------------------------------------------------------------
	
