%%%% -*- Mode: Prolog -*-
%%%% lmc.pl


%%% carica il programma letto da file in memoria

lmc_load(Filename, Mem) :-
    read_file(Filename, Result),
    string_lower(Result, ResultToLower),
    split_string(ResultToLower, "\n", " \n", ResultByLines),
    without_comments(ResultByLines, ResultWithoutComments),
    retractall(label(_, _)),
    label_loader(ResultWithoutComments, 0),
    check_duplicate_labels(),
    label_resolver(ResultWithoutComments, ResultWithoutLabels),
    initialize_memory(ResultWithoutLabels, MemInit),
    justify_memory(MemInit, Mem).


%%% prendendo in input un programma ed una lista di input
%%% esegue il programma dopo che viene caricato in memoria

lmc_run(Filename, Input, Output) :-
    lmc_load(Filename, Mem),
    is_list(Input),
    maplist(number, Input),
    maplist(between(0, 999), Input),
    execution_loop(state(0, 0, Mem, Input, [], noflag), Output).


op_code(1, "add").
op_code(2, "sub").
op_code(3, "sta").
op_code(5, "lda").
op_code(6, "bra").
op_code(7, "brz").
op_code(8, "brp").
op_code(901, "inp").
op_code(902, "out").
op_code(0, "hlt").
op_code(-1, "dat").


%%% riconosce il tipo d'istruzione de eseguire

fetch(state(_, Pc, Mem, _, _, _), Type, Addr) :-
    nth0(Pc, Mem, E),
    Code is div(E, 100),
    Code \= 9,
    !,
    Addr is mod(E, 100),
    op_code(Code, Type).

fetch(state(_, Pc, Mem, _, _, _), Type, _) :-
    nth0(Pc, Mem, E),
    Code is div(E, 100),
    Code = 9,
    !,
    op_code(E, Type).


%%% esegue un'istruzione

execute(state(Acc, Pc, Mem, In, Out, Flag),
        "hlt",
        _,
        halted_state(Acc, Pc, Mem, In, Out, Flag)).

execute(state(Acc, Pc, Mem, In, Out, _),
        "add",
        Addr,
        state(NewAcc, NewPc, Mem, In, Out, flag)) :-
    nth0(Addr, Mem, Val),
    Sum is Acc + Val,
    Sum >= 1000,
    !,
    NewAcc is mod(Sum, 1000),
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, Pc, Mem, In, Out, _),
        "add",
        Addr,
        state(NewAcc, NewPc, Mem, In, Out, noflag)) :-
    nth0(Addr, Mem, Val),
    Sum is Acc + Val,
    Sum < 1000,
    !,
    NewAcc is mod(Sum, 1000),
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, Pc, Mem, In, Out, _),
        "sub",
        Addr,
        state(NewAcc, NewPc, Mem, In, Out, noflag)) :-
    nth0(Addr, Mem, Val),
    Sub is Acc - Val,
    Sub >= 0,
    !,
    NewAcc is mod(Sub, 1000),
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, Pc, Mem, In, Out, _),
        "sub",
        Addr,
        state(NewAcc, NewPc, Mem, In, Out, flag)) :-
    nth0(Addr, Mem, Val),
    Sub is Acc - Val,
    Sub < 0,
    !,
    NewAcc is mod(Sub, 1000),
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, Pc, Mem, In, Out, Flag),
        "sta",
        Addr,
        state(Acc, NewPc, NewMem, In, Out, Flag)) :-
    !,
    nth0(Addr, Mem, _, R),
    nth0(Addr, NewMem, Acc, R),
    NewPc is mod(Pc + 1, 100).

execute(state(_, Pc, Mem, In, Out, Flag),
        "lda",
        Addr,
        state(NewAcc, NewPc, Mem, In, Out, Flag)) :-
    !,
    nth0(Addr, Mem, E),
    NewAcc is mod(E, 1000),
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, _, Mem, In, Out, Flag),
        "bra",
        Addr,
        state(Acc, Addr, Mem, In, Out, Flag)) :- !.

execute(state(0, _, Mem, In, Out, noflag),
        "brz",
        Addr,
        state(0, Addr, Mem, In, Out, noflag)) :- !.

execute(state(Acc, Pc, Mem, In, Out, Flag),
        "brz",
        _,
        state(Acc, NewPc, Mem, In, Out, Flag)) :-
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, _, Mem, In, Out, noflag),
        "brp",
        Addr,
        state(Acc, Addr, Mem, In, Out, noflag)) :- !.

execute(state(Acc, Pc, Mem, In, Out, flag),
        "brp",
        _,
        state(Acc, NewPc, Mem, In, Out, flag)) :-
    NewPc is mod(Pc + 1, 100).

execute(state(_, Pc, Mem, [X | Xs], Out, Flag),
        "inp",
        _,
        state(X, NewPc, Mem, Xs, Out, Flag)) :-
    !,
    NewPc is mod(Pc + 1, 100).

execute(state(Acc, Pc, Mem, In, Out, Flag),
        "out",
        _,
        state(Acc, NewPc, Mem, In, NewOut, Flag)) :-
    !,
    append(Out, [Acc], NewOut),
    NewPc is mod(Pc + 1, 100).


%%% dopo aver riconusciuto il tipo d'istruzione la esegue

one_instruction(State, NewState) :-
    fetch(State, Type, ValAddr),
    execute(State, Type, ValAddr, NewState).


%%% esegue tutte le istruzioni del programma

execution_loop(State, Out) :-
    functor(State, state, _),
    !,
    one_instruction(State, NewState),
    execution_loop(NewState, Out).

execution_loop(halted_state(_, _, _, _, Out, _), Out) :- !.


%%% inizializza la memoria

initialize_memory([], []) :- !.

initialize_memory([X | Xs], [R | Rs]) :-
    length(X, 1),
    !,
    nth0(0, X, E1),
    resolve_instruction(E1, R),
    initialize_memory(Xs, Rs).

initialize_memory([X | Xs], [R | Rs]) :-
    length(X, 2),
    !,
    nth0(0, X, E1),
    nth0(1, X, E2),
    resolve_instruction(E1, E2, R),
    initialize_memory(Xs, Rs).


%%% risolve l'istruzione in linguaggio assembly in un codice macchina

resolve_instruction("dat", 0) :- !.
resolve_instruction(I, R) :- op_code(R, I).
resolve_instruction("dat", A, A) :- !.
resolve_instruction(I, V, R) :-
    I \= "inp",
    I \= "out",
    !,
    op_code(Code, I),
    R is (Code * 100) + V.


%%% riconosco le etichette e le salvo in memoria

label_loader([], _) :- !.

label_loader([""], _) :- !.

label_loader([X | Xs], Index) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 1),
    !,
    nth0(0, NewX, E1),
    assert_label(E1, Index),
    NewIndex is Index + 1,
    label_loader(Xs, NewIndex).

label_loader([X | Xs], Index) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 2),
    !,
    nth0(0, NewX, E1),
    nth0(1, NewX, E2),
    assert_label(E1, E2, Index),
    NewIndex is Index + 1,
    label_loader(Xs, NewIndex).

label_loader([X | Xs], Index) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 3),
    !,
    nth0(0, NewX, E1),
    nth0(1, NewX, E2),
    nth0(2, NewX, E3),
    assert_label(E1, E2, E3, Index),
    NewIndex is Index + 1,
    label_loader(Xs, NewIndex).


%%% salvo in memoria le etichette
%%% associando il nome simbolico con l'indirizzo a cui puntano

assert_label(Instruction, _) :-
    op_code(_, Instruction).

assert_label(Instruction, _, _) :-
    op_code(_, Instruction),!.

assert_label(Label, Instruction, Index) :-
    op_code(_, Instruction),
    \+ op_code(_, Label),
    !,
    assert(label(Label, Index)).

assert_label(Label, Instruction, Addr, Index) :-
    op_code(_, Instruction),
    number_string(NumAddr, Addr),
    number(NumAddr), !,
    \+ op_code(_, Label),
    !,
    assert(label(Label, Index)).

assert_label(Label, Instruction, _, Index) :-
    op_code(_, Instruction),
    \+ op_code(_, Label),
    !,
    assert(label(Label, Index)).


%%% risolvo nelle istruzioni assembly le etichette

label_resolver([], []) :- !.

label_resolver([""], []) :- !.

label_resolver([X | Xs], [[R1] | Rs]) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 1),
    !,
    nth0(0, NewX, E1),
    resolve_label(E1, R1),
    label_resolver(Xs, Rs).

label_resolver([X | Xs], [[R1, R2] | Rs]) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 2),
    nth0(0, NewX, E1),
    \+ check_label(E1),
    !,
    nth0(1, NewX, E2),
    resolve_label(E1, E2, R1, R2),
    label_resolver(Xs, Rs).

label_resolver([X | Xs], [[R1] | Rs]) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 2),
    nth0(0, NewX, E1),
    check_label(E1),
    !,
    nth0(1, NewX, E2),
    resolve_label(E1, E2, R1),
    label_resolver(Xs, Rs).

label_resolver([X | Xs], [[R1, R2] | Rs]) :-
    split_string(X, " ", " ", NewX),
    length(NewX, 3),
    !,
    nth0(0, NewX, E1),
    nth0(1, NewX, E2),
    nth0(2, NewX,  E3),
    resolve_label(E1, E2, E3, R1, R2),
    label_resolver(Xs, Rs).


%%% risolvo le istruzioni assembly
%%% cambiando il nome simbolico con il loro indirizzo

resolve_label([], []) :- !.

resolve_label(I, I) :- !.

resolve_label(I, L, I, R) :-
    I \= "hlt",
    I \= "dat",
    I \= "inp",
    I \= "out",
    check_label(L),
    !,
    op_code(_, I),
    label(L, R).

resolve_label(I, A, I, NewA) :-
    op_code(_, I),
    string(A),
    number_string(NewA, A).

resolve_label(L, I, I) :-
    check_label(L).

resolve_label(L1, I, L2, I, R) :-
    I \= "hlt",
    I \= "dat",
    I \= "inp",
    I \= "out",
    check_label(L2),
    label(L2, R),
    !,
    \+ op_code(_, L1),
    op_code(_, I).

resolve_label(L1, I, A, I, NewA) :-
    check_label(L1),
    op_code(_, I),
    string(A),
    number_string(NewA, A).


%%% controllo che la stringa passata sia una label

check_label(L) :-
    \+ op_code(_, L),
    label(L, _).


%%% controllo che non esistano label duplicate

check_duplicate_labels() :-
    findall(L, label(L, _), Labels),
    sort(Labels, WithoutDuplicates),
    length(Labels, N1),
    length(WithoutDuplicates, N2),
    X is N1 - N2,
    X = 0.


%%% leggo il file in input

read_file(Filename, Result) :-
    read_file_to_string(Filename, Result, []).


%%% tolgo i commenti dal file letto

without_comments([], []) :- !.
without_comments([X | Xs], Rs) :-
    string_codes(X, [47, 47 | _]),
    !,
    without_comments(Xs, Rs).
without_comments([X | Xs], [R | Rs]) :-
    split_string(X, "//", "", [R | _]),
    !,
    without_comments(Xs, Rs).


%%% creo una lista di zeri con una data lunghezza

create_zeros_list(0, []) :- !.
create_zeros_list(Length, [0 | Rs]) :-
    number(Length),
    Length > 0,
    !,
    NewLength is Length - 1,
    create_zeros_list(NewLength, Rs).


%%% riempo la memoria generata con degli zeri fino ad arrivare a 100

justify_memory(Mem, NewMem) :-
    is_list(Mem),
    length(Mem, MemLength),
    MemLength =< 100,
    PaddingLength is 100 - MemLength,
    create_zeros_list(PaddingLength, PaddingList),
    append(Mem, PaddingList, NewMem).


%%%% end of lmc.pl