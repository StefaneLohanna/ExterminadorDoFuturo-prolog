:- module(bot, [
    escolherJogadaBot/1,
    obtemCoordenadasValidasBot/6,
    escolherTempoBot/2
]).

:- dynamic jogador1_nome/1.
:- dynamic jogador2_nome/1.

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').
:- use_module('./src/Utils/Ranking.pl').

% Escolhe uma jogada para o bot ('m' para movimentar ou 'p' para plantar ou 'v' para viajar)
escolherJogadaBot(Jogada) :-
    random_member(Jogada, ['m', 'p', 'v']).

% Escolhe coordenadas válidas para jogar
obtemCoordenadasValidasBot(Tabuleiro, Jogador, Linha, Coluna, LinhaDestino, ColunaDestino) :-
    % Primeiro tenta encontrar jogada para matar
    jogadaParaMatar(Tabuleiro, Jogador, Linha, Coluna, LinhaDestino, ColunaDestino), !.

obtemCoordenadasValidasBot(Tabuleiro, Jogador, Linha, Coluna, LinhaDestino, ColunaDestino) :-
    % Caso não haja jogadas para matar, executa uma jogada aleatória válida
    findall(
        (L, C),
        (
            between(1, 4, L), between(1, 4, C),
            nth1(L, Tabuleiro, LinhaTab),
            nth1(C, LinhaTab, Peca),
            pertenceAoJogador(Peca, Jogador)
        ),
        PecasJogador
    ),
    PecasJogador \= [],
    random_member((Linha, Coluna), PecasJogador),
    findall(
        (L, C),
        (
            between(1, 4, L), between(1, 4, C),
            (
                (L =:= Linha, abs(C - Coluna) =:= 1)
                ;
                (C =:= Coluna, abs(L - Linha) =:= 1)
            ),
            nth1(L, Tabuleiro, LinhaTabDest),
            nth1(C, LinhaTabDest, PecaDest),
            (PecaDest = ' ' ; \+ pertenceAoJogador(PecaDest, Jogador))
        ),
        DestinosValidos
    ),
    DestinosValidos \= [],
    random_member((LinhaDestino, ColunaDestino), DestinosValidos).

% Define as direções válidas
direcao(cima, -1, 0).
direcao(baixo, 1, 0).
direcao(esquerda, 0, -1).
direcao(direita, 0, 1).

% Verifica se está dentro do tabuleiro
dentro_do_tabuleiro(L, C) :- between(1, 4, L), between(1, 4, C).

% Verifica se uma jogada pode matar
jogadaParaMatar(Tabuleiro, Jogador, Linha, Coluna, LinhaDestino, ColunaDestino) :-
    between(1, 4, Linha),
    between(1, 4, Coluna),
    nth1(Linha, Tabuleiro, LinhaTab),
    nth1(Coluna, LinhaTab, Peca),
    pertenceAoJogador(Peca, Jogador),
    direcao(_, DL, DC),
    LinhaViz is Linha + DL,
    ColunaViz is Coluna + DC,
    dentro_do_tabuleiro(LinhaViz, ColunaViz),
    nth1(LinhaViz, Tabuleiro, LinhaVizTab),
    nth1(ColunaViz, LinhaVizTab, Meio),
    pertenceAoAdversario(Meio, Jogador),
    LinhaFinal is Linha + 2 * DL,
    ColunaFinal is Coluna + 2 * DC,
    (
        (dentro_do_tabuleiro(LinhaFinal, ColunaFinal),
         nth1(LinhaFinal, Tabuleiro, LinhaFinalTab),
         nth1(ColunaFinal, LinhaFinalTab, Fim),
         (pertenceAoAdversario(Fim, Jogador) ; Fim == '\x1F333' ; Fim == '\x1F331'))
        ;
        \+ dentro_do_tabuleiro(LinhaFinal, ColunaFinal)
    ),
    LinhaDestino is Linha + DL,
    ColunaDestino is Coluna + DC.

% Verifica se a peça pertence ao jogador
pertenceAoJogador(Peca, Jogador) :-
    atom_chars(Peca, [Jogador|_]).

% Verifica se a peça pertence ao adversário
pertenceAoAdversario(Peca, Jogador) :-
    jogador1(Jogador), jogador2(Adversario), atom_chars(Peca, [Adversario|_]);
    jogador2(Jogador), jogador1(Adversario), atom_chars(Peca, [Adversario|_]).

% Escolhe aleatoriamente um tempo para viajar
escolherTempoBot(FocoAtual, NovoTempo) :-
    (FocoAtual == passado -> Tempos = [presente]
    ; FocoAtual == presente -> Tempos = [passado, futuro]
    ; FocoAtual == futuro -> Tempos = [presente]
    ; Tempos = []),
    Tempos \= [],
    random_member(NovoTempo, Tempos).
