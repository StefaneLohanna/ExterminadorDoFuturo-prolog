:- module(jogador, [definirFoco/6, focoValido/5, obterLinha/1, obterColuna/1, escolherJogada/1, obtemCoordenadasValidas/4]).

:- use_module('./src/Jogo/Tabuleiro.pl').

/*
 * Define o foco do jogador para a próxima rodada.
 *
 * @param Jogador    O símbolo do jogador que está escolhendo o foco.
 * @param Passado    O tabuleiro representando o estado atual do passado.
 * @param Presente   O tabuleiro representando o estado atual do presente.
 * @param Futuro     O tabuleiro representando o estado atual do futuro.
 * @param FocoAtual  O foco atual do jogador.
 * @param NovoFoco   O novo foco definido pelo jogador.
 */
definirFoco(Jogador, Passado, Presente, Futuro, FocoAtual, NovoFoco) :-
    exibirMenuFoco,
    read(Escolha),
    (   traduzirEscolha(Escolha, FocoTentativa) ->
        (   focoValido(FocoTentativa, Jogador, Passado, Presente, Futuro) ->
            NovoFoco = FocoTentativa,
            format('~w definiu foco para: ~w~n', [Jogador, NovoFoco])
        ;
            writeln('Jogador não encontrado nesse tempo. Escolha outro foco.'),
            definirFoco(Jogador, Passado, Presente, Futuro, FocoAtual, NovoFoco)
        )
    ;   definirFoco(Jogador, Passado, Presente, Futuro, FocoAtual, NovoFoco)
    ).

/*
 * Exibe o menu de escolha de foco para o jogador.
 */
exibirMenuFoco :-
    writeln('Escolha o foco para suas próximas jogadas:'),
    writeln('(s) Passado'),
    writeln('(p) Presente'),
    writeln('(f) Futuro'),
    write('Digite sua escolha: ').

/*
 * Traduz a escolha do jogador para o respectivo foco.
 *
 * @param Escolha  Entrada do jogador ('s', 'p' ou 'f').
 * @param Foco     O foco correspondente (passado, presente ou futuro).
 */
traduzirEscolha('s', passado).
traduzirEscolha('p', presente).
traduzirEscolha('f', futuro).
traduzirEscolha(_, _) :- 
    writeln('Opção inválida! Use s, p ou f.'),
    fail.

/*
 * Verifica se o foco escolhido pelo jogador é válido, ou seja, se o jogador está presente no tabuleiro correspondente.
 *
 * @param Foco     O foco escolhido (passado, presente ou futuro).
 * @param Jogador  O símbolo do jogador.
 * @param Passado  O tabuleiro representando o estado atual do passado.
 * @param Presente O tabuleiro representando o estado atual do presente.
 * @param Futuro   O tabuleiro representando o estado atual do futuro.
 * @return         Verdadeiro se o jogador estiver presente no tabuleiro correspondente, falso caso contrário.
 */
focoValido(passado, Jogador, Passado, _, _) :- 
    existeJogador(Passado, Jogador).
focoValido(presente, Jogador, _, Presente, _) :- 
    existeJogador(Presente, Jogador).
focoValido(futuro, Jogador, _, _, Futuro) :- 
    existeJogador(Futuro, Jogador).

/*
 * Solicita ao jogador que escolha uma ação.
 *
 * @return Escolha  A ação escolhida pelo jogador: 
 *                  'm' para movimentar,
 *                  'p' para plantar,
 *                  'v' para viajar no tempo,
 *                  'r' para reiniciar o jogo.
 */
escolherJogada(Escolha) :-
    writeln("Escolha uma ação:"),
    writeln("(m) Movimentar"),
    writeln("(p) Plantar"),
    writeln("(v) Viajar no tempo"),
    writeln("(r) Reiniciar jogo"),
    write("Digite sua escolha: "),
    read(E),
    (
        member(E, [m, p, v, r]) ->
            Escolha = E
        ;
            writeln("Entrada inválida! Tente novamente."),
            escolherJogada(Escolha)
    ).

/*
 * Obtém coordenadas válidas em que o jogador está posicionado no tabuleiro.
 *
 * @param Tabuleiro O tabuleiro atual (passado, presente ou futuro).
 * @param Jogador   O símbolo do jogador.
 * @return Linha    A linha em que o jogador está localizado.
 * @return Coluna   A coluna em que o jogador está localizado.
 */
obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna) :-
    obterLinha(L),
    obterColuna(C),
    (
        verificarPosicaoTabuleiro(Tabuleiro, L, C, Jogador) ->
            Linha = L, Coluna = C
    ;
        format("Posição inválida! Insira uma posição em que a sua peça (~w) se encontre.\n", [Jogador]),
        obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna)
    ).

/*
 * Obtém coordenadas do jogador.
 *
 * @return Linha  A linha inserida pelo jogador.
 * @return Coluna A coluna inserida pelo jogador.
 */
/*obtemCoordenadas(Linha, Coluna) :-
    obterLinha(Linha),
    obterColuna(Coluna).*/

/*
 * Solicita ao jogador uma linha válida dentro dos limites do tabuleiro.
 *
 * @return Linha  A linha escolhida (de 1 a 4).
 */
obterLinha(Linha) :-
    write("Informe a linha (1-4): "), read(L),
    ( integer(L), L >= 1, L =< 4 ->
        Linha = L
    ;
        writeln("Linha inválida! Escolha um valor entre 1 e 4."),
        obterLinha(Linha) % Repete até obter um valor válido
    ).

/*
 * Solicita ao jogador uma coluna válida dentro dos limites do tabuleiro.
 *
 * @return Coluna  A coluna escolhida (de 1 a 4).
 */
obterColuna(Coluna) :-
    write("Informe a coluna (1-4): "), read(C),
    ( integer(C), C >= 1, C =< 4 ->
        Coluna = C
    ;
        writeln("Coluna inválida! Escolha um valor entre 1 e 4."),
        obterColuna(Coluna) % Repete até obter um valor válido
    ).