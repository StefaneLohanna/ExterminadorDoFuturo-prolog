:- module(bot, [escolherJogadaBot/1, obtemCoordenadasValidasBot/6, escolherTempoBot/2]).

:- dynamic jogador1_nome/1.
:- dynamic jogador2_nome/1.

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').
:- use_module('./src/Utils/Ranking.pl').

/*
 * Escolhe uma jogada para o bot (aleatória ou baseada em prioridade)
 *
 * @param Tabuleiro O tabuleiro atual do jogo
 * @param Linha     A linha da posição do bot
 * @param Coluna    A coluna da posição do bot
 * @param Jogador   O símbolo do bot (X ou O)
 * @param Jogada    A jogada escolhida (saída)
 */
escolherJogadaBot(Jogada) :-
    random_member(Jogada, ['m', 'p']).

obtemCoordenadasValidasBot(Tabuleiro, Jogador, Linha, Coluna, LinhaDestino, ColunaDestino) :-
    % 1. Encontra todas as peças do jogador
    findall(
        (L, C),
        (between(1, 4, L), between(1, 4, C),
        nth1(L, Tabuleiro, LinhaTab),
        nth1(C, LinhaTab, Peca),
        pertenceAoJogador(Peca, Jogador)
    ),
    PecasJogador),
    
    PecasJogador \= [],  % Garante que há peças disponíveis
    
    % 2. Escolhe aleatoriamente uma peça do jogador
    random_member((Linha, Coluna), PecasJogador),
    
    % 3. Calcula destinos possíveis (apenas ortogonais: ↑ ↓ ← →)
    findall(
        (L, C),
        (
            between(1, 4, L), between(1, 4, C),
            (
                (L =:= Linha, abs(C - Coluna) =:= 1)  % Mesma linha, coluna adjacente
                ;
                (C =:= Coluna, abs(L - Linha) =:= 1)  % Mesma coluna, linha adjacente
            ),
            nth1(L, Tabuleiro, LinhaTabDest),
            nth1(C, LinhaTabDest, PecaDest),
            (PecaDest = ' ' ; \+ pertenceAoJogador(PecaDest, Jogador))  % Vazio ou peça adversária
        ),
        DestinosValidos
    ),
    
    DestinosValidos \= [],  % Garante que há movimentos possíveis
    
    % 4. Escolhe um destino aleatório
    random_member((LinhaDestino, ColunaDestino), DestinosValidos).

% Predicado auxiliar para verificar se uma peça pertence ao jogador
pertenceAoJogador(Peca, Jogador) :-
    jogador1(Jogador), atom_chars(Peca, [Jogador|_]);
    jogador2(Jogador), atom_chars(Peca, [Jogador|_]).

% Versão corrigida do destinosValidos/2
destinosValidos((Linha, Coluna), Destinos) :-
    findall(
        (L, C),
        (between(1, 4, L), between(1, 4, C),
        abs(L - Linha) =< 1, abs(C - Coluna) =< 1,
        (L \= Linha ; C \= Coluna)  % Não inclui a própria posição
        ),
        Destinos
    ).

escolherTempoBot(FocoAtual, NovoTempo) :-
    % Define as opções disponíveis baseadas no foco atual
    (FocoAtual == passado -> 
        Tempos = [presente]
    ; FocoAtual == presente -> 
        Tempos = [passado, futuro]
    ; FocoAtual == futuro -> 
        Tempos = [presente]
    ; 
        Tempos = []
    ),
    
    % Verifica se há opções disponíveis
    (Tempos \= [] ->
        % Escolhe aleatoriamente uma das opções
        random_member(NovoTempo, Tempos)
    ).