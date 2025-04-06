:- module(ranking, [atualizarRanking/2, mostrarRanking/0]).

:- use_module(library(readutil)).
:- use_module(library(lists)).

ranking_file('ranking.txt').

%% Atualiza o ranking com o nome do jogador e os pontos ganhos
atualizarRanking(Jogador, PontosGanhos) :-
    ranking_file(File),
    (   exists_file(File)
    ->  lerRanking(File, RankingAtual)
    ;   RankingAtual = []
    ),
    atualizarPontuacao(RankingAtual, Jogador, PontosGanhos, NovoRanking),
    salvarRanking(File, NovoRanking).

%% Mostra o ranking atual
mostrarRanking :-
    format('~n===== Ranking =====~n'),
    ranking_file(File),
    (   exists_file(File)
    ->  lerRanking(File, Ranking),
        sort(2, @>=, Ranking, RankingOrdenado),
        mostrarJogadores(RankingOrdenado)
    ;   format('Nenhum jogador no ranking ainda.~n')
    ),
    format('==================~n').

%% Lê o ranking do arquivo
lerRanking(File, Ranking) :-
    open(File, read, Stream),
    lerTermos(Stream, Ranking),
    close(Stream).

lerTermos(Stream, []) :-
    at_end_of_stream(Stream), !.
lerTermos(Stream, [Termo|Outros]) :-
    \+ at_end_of_stream(Stream),
    read(Stream, Termo),
    Termo \= end_of_file,
    lerTermos(Stream, Outros).
lerTermos(Stream, []) :-
    read(Stream, end_of_file).

%% Atualiza ou adiciona a pontuação do jogador
atualizarPontuacao([], Jogador, PontosGanhos, [jogador(Jogador, PontosGanhos)]).
atualizarPontuacao([jogador(Jogador, Pontos)|Resto], Jogador, PontosGanhos, [jogador(Jogador, NovoPontos)|Resto]) :-
    NovoPontos is Pontos + PontosGanhos.
atualizarPontuacao([Outro|Resto], Jogador, PontosGanhos, [Outro|NovoResto]) :-
    atualizarPontuacao(Resto, Jogador, PontosGanhos, NovoResto).

%% Salva o ranking no arquivo
salvarRanking(File, Ranking) :-
    open(File, write, Stream),
    salvarTermos(Stream, Ranking),
    close(Stream).

salvarTermos(_, []).
salvarTermos(Stream, [Termo|Resto]) :-
    writeq(Stream, Termo),
    write(Stream, '.\n'),
    salvarTermos(Stream, Resto).

%% Mostra todos os jogadores
mostrarJogadores([]).
mostrarJogadores([jogador(Nome, Pontos)|Resto]) :-
    format('~w - ~w pontos~n', [Nome, Pontos]),
    mostrarJogadores(Resto).
