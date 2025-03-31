:- module(jogador, [definirFoco/6]).

:- use_module('./src/Jogo/tabuleiro.pl').

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