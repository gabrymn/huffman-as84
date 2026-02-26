% Test di accettazione
%-------------------------------------------------------------------------------
program1(File) :-
	hucodec_generate_sw(File, SWs, 1),
	hucodec_generate_huffman_tree(SWs, HT),
	hucodec_print_ht(HT).
%-------------------------------------------------------------------------------	


% Insieme di funzionalità per generare una lista di coppie simbolo-frequenza
% L = { sw(S,W) : S è un simbolo, W è un peso come frequenza}
% il punto di partenza è una lista di char o di byte, entrambi si possono
% ottenere da una stringa in input o dal contenuto di un file.
%-------------------------------------------------------------------------------
hucodec_generate_sw(StrOrPath, SWs, FileBool) :-
	get_chars(StrOrPath, Chars, FileBool),
	count_freq(Chars, SWs).

hucodec_generate_sw_byte(StrOrPath, SWs, FileBool) :-
	get_bytes(StrOrPath, Chars, FileBool),
	count_freq(Chars, SWs),!.

get_chars(Str, Chars, 0) :- 
	string(Str), 
	string_chars(Str, Chars),
	!.

get_chars(FilePath, Chars, 1) :- 
	read_file(FilePath, Str), 
	get_chars(Str, Chars, 0).

get_bytes(Str, Bytes, 0) :-
	string_codes(Str, Bytes).

get_bytes(FilePath, Bytes, 1) :-
    open(FilePath, read, Stream, [type(binary)]),
    read_stream_bytes(Stream, Bytes),
    close(Stream).

count_freq([], []).
count_freq([S|Tail], R) :-
    count_freq(Tail, Temp),
    update_symbol_w(Temp, S, R).

update_symbol_w([], S, [sw(S,1)]).

update_symbol_w([sw(S,F)|T], S, [sw(S,NF)|T]) :- 
    NF is F + 1.

update_symbol_w([AltF|Resto], S, [AltF|NuovoResto]) :-
    AltF = sw(Ss,_),
    S \= Ss,
    update_symbol_w(Resto, S, NuovoResto).

read_file(File, String) :-
    open(File, read, Stream),
    read_string(Stream, _, String),
    close(Stream).

read_stream_bytes(Stream, []) :-
    at_end_of_stream(Stream), !.
    
read_stream_bytes(Stream, [B|Rest]) :-
    get_byte(Stream, B),
    read_stream_bytes(Stream, Rest).

%-------------------------------------------------------------------------------


% Partendo da un albero di huffman genera una lista d {sb(S,B)} dove S è
% un simbolo e B è una sequenza di bit
%-------------------------------------------------------------------------------
hucodec_generate_symbol_bits_table(node(L, R, _), SBs) :-
	visit_ht(node(L, R, _), [], SBs).

visit_ht(leaf(S, _), Bits, [sb(S,Bits)]).

visit_ht(node(L, R, _), Bits, SBs) :-
	visit_ht(L, [0|Bits], SBs_left),
	visit_ht(R, [1|Bits], SBs_right),
	join(SBs_left, SBs_right, SBs).
	
join([], [], []).
join([], L, L).
join([H|T], L, [H|Tr]) :-
	join(T, L, Tr).
%-------------------------------------------------------------------------------


% Creazione dell'albero di huffman a partire da una lista di {sw(S,W)} con
% tutte le utils necessarie
%-------------------------------------------------------------------------------
hucodec_generate_huffman_tree(SWs, HT) :-
	init_tree(SWs, LFs),
	build_tree(LFs, HT).
	
init_tree([], []).
init_tree([sw(S,W)|T1], [leaf(S,W)|T2]) :-
	init_tree(T1, T2).

build_tree([], []).
build_tree([Tree], Tree).	
build_tree(NL, Tree) :-
	sort_elements(NL, [E1,E2|Tail]),
	weight(E1, W1),
	weight(E2, W2),
	W3 is W1+W2,
	build_tree([node(E2, E1, W3) | Tail], Tree).
	
weight(leaf(_, W), W).
weight(node(_, _, W), W).

to_pairs([], []).
to_pairs([N|T], [W-N|T2]) :-
    weight(N, W),
    to_pairs(T, T2).

from_pairs([], []).
from_pairs([_-N|T], [N|T2]) :-
    from_pairs(T, T2).

sort_elements(Elements, Sorted) :-
    to_pairs(Elements, Pairs),
    keysort(Pairs, SortedPairs),
    from_pairs(SortedPairs, Sorted).

%-------------------------------------------------------------------------------



% Print huffman tree
%-------------------------------------------------------------------------------
hucodec_print_ht(Tree) :-
    hucodec_print_ht(Tree, "", true).

hucodec_print_ht(leaf(Sym,W), Prefix, IsLast) :-
    branch(Prefix, IsLast, LinePrefix),
    format("~wLeaf: ~w (~w)~n", [LinePrefix, Sym, W]).

hucodec_print_ht(node(Left,Right,W), Prefix, IsLast) :-
    branch(Prefix, IsLast, LinePrefix),
    format("~wNode: weight ~w~n", [LinePrefix, W]),
    child_prefix(Prefix, IsLast, NewPrefix),
    print_children([Left,Right], NewPrefix).

branch(Prefix, true, LinePrefix) :- 
	format(string(LinePrefix), "~w└─", [Prefix]).
branch(Prefix, false, LinePrefix) :- 
	format(string(LinePrefix), "~w├─", [Prefix]).

child_prefix(Prefix, true, NewPrefix) :- 
	format(string(NewPrefix), "~w   ", [Prefix]).
child_prefix(Prefix, false, NewPrefix) :- 
	format(string(NewPrefix), "~w│  ", [Prefix]).

print_children([], _).
print_children([C], Prefix) :- hucodec_print_ht(C, Prefix, true).
print_children([C1,C2|Rest], Prefix) :-
    hucodec_print_ht(C1, Prefix, false),
    print_children([C2|Rest], Prefix).
%-------------------------------------------------------------------------------



% Generic Utils
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



	
