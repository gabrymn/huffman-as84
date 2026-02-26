consult('./huffman-codes.pl').

% Test di accettazione
%-------------------------------------------------------------------------------
program1(File) :-
	hucodec_generate_sw(File, SWs, 1),
	hucodec_generate_ht(SWs, HT),
	hucodec_print_ht(HT).

program2(Msg, Bits, HT, Save, SBs) :-
	hucodec_generate_sw(Msg, SWs, 0),
	hucodec_generate_ht(SWs, HT),
	huffman_encode(Msg, HT, Bits),
	hucodec_print_ht(HT),
	hucodec_generate_sb(HT, SBs),
	length(Bits, Len),
	get_chars(Msg, Chars, 0),
	length(Chars, NByte0),
	NByte1 is (Len + 7) // 8,
	Save is ((NByte0 - NByte1) / NByte0) * 100.

%-------------------------------------------------------------------------------



