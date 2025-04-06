:- module(jogar, [iniciarJogo/0]).

:- dynamic jogador1_nome/1.
:- dynamic jogador2_nome/1.

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').
:- use_module('./src/Utils/Ranking.pl').

:- use_module(library(readutil)). % para ler linha completa com read_line_to_string
:- use_module(library(apply)).   % para maplist
:- use_module(library(strings)). % para string_lower (se necessário)

espacoVazio(_Ev).
iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inica o tabuleiro e faz a rodada.
    */
    registrarJogadores,
    jogador1_nome(N1),
    jogador1(J1),
    iniciarTabuleiros(Passado, Presente, Futuro),
    rodada(J1, N1, passado, futuro, Passado, Presente, Futuro).

formatar(Entrada, Saida) :-
    string_lower(Entrada, Lower),
    string_chars(Lower, Chars),
    exclude(=( ' ' ), Chars, SemEspacos),
    string_chars(Saida, SemEspacos).

registrarJogadores :-
    jogador1(J1),
    jogador2(J2),

    % Remove nomes antigos se existirem
    retractall(jogador1_nome(_)),
    retractall(jogador2_nome(_)),

    writeln("\nRegistro do jogadores!"),
    write("Jogador 1, digite seu nome: "),
    read_line_to_string(user_input, Nome1),
    formatar(Nome1, Nome1Min),
    assertz(jogador1_nome(Nome1Min)),
    format("Seu personagem será a ~w~n", [J1]),

    write("Jogador 2, digite seu nome: "),
    read_line_to_string(user_input, Nome2),
    formatar(Nome2, Nome2Min),
    assertz(jogador2_nome(Nome2Min)),
    format("Jogador 2 ficará com o ~w~n", [J2]).

registrarJogadorUunico :-
    jogador1(J1),

    % Remove nome antigo
    retractall(jogador1_nome(_)),

    writeln("\nRegistro do jogador!"),
    write("Digite seu nome: "),
    read_line_to_string(user_input, Nome),
    formatar(Nome, NomeMin),
    assertz(jogador1_nome(NomeMin)),
    format("Seu personagem será o ~w~n", [J1]).

/*
 * Controla a rodada do jogo, permitindo que cada jogador realize duas jogadas antes de alternar.
 *
 * @param Peca       Peça do jogador atual.
 * @param FocoJ1     Foco do jogador 1.
 * @param FocoJ2     Foco do jogador 2.
 * @param Passado    O tabuleiro representando o estado atual do passado.
 * @param Presente   O tabuleiro representando o estado atual do presente.
 * @param Futuro     O tabuleiro representando o estado atual do futuro.
 */

rodada(Peca, _, FocoJ1, FocoJ2, Passado, Presente, Futuro) :-
    jogador1(J1),
    jogador2(J2),

    %% Usa o predicado auxiliar para obter o nome correto
    nomeDoJogador(Peca, Nome),
    format("Vez do jogador: ~w (~w)~n", [Nome, Peca]),

    % Determina qual foco usar nesta rodada
    (Peca == J1 -> FocoAtual = FocoJ1 ; FocoAtual = FocoJ2),

    % Verifica se o jogador está presente no foco atual
    (focoValido(FocoAtual, Peca, Passado, Presente, Futuro) -> NovoFoco = FocoAtual;
        writeln('Jogador não está presente nesse foco. Escolha outro.'),
        definirFoco(Peca, Passado, Presente, Futuro, FocoAtual, NovoFoco)
    ),

    % Primeira jogada
    jogar(NovoFoco, Peca, Passado, Presente, Futuro, NovoPassado1, NovoPresente1, NovoFuturo1),

    % Verifica vitória após a primeira jogada
    (verificarVitoria(NovoPassado1, NovoPresente1, NovoFuturo1, Peca) ->
        exibirTabuleiros(NovoPassado1, NovoPresente1, NovoFuturo1),
        finalizarJogo(Nome)
    ;
        true
    ),

    % Segunda jogada
    jogar(NovoFoco, Peca, NovoPassado1, NovoPresente1, NovoFuturo1, NovoPassado2, NovoPresente2, NovoFuturo2),

    % Verifica vitória após a segunda jogada
    (verificarVitoria(NovoPassado2, NovoPresente2, NovoFuturo2, Peca) ->
        exibirTabuleiros(NovoPassado2, NovoPresente2, NovoFuturo2),
        finalizarJogo(Nome)
    ;
        true
    ),

    exibirTabuleiros(NovoPassado2, NovoPresente2, NovoFuturo2),

    % Define o foco para a próxima rodada
    (
        Peca == J1 -> 
        definirFoco(J1, NovoPassado2, NovoPresente2, NovoFuturo2, NovoFoco, NovoFocoJ1),
        NovoFocoJ2 = FocoJ2,
        NovaPeca = J2
        ;
        definirFoco(J2, NovoPassado2, NovoPresente2, NovoFuturo2, NovoFoco, NovoFocoJ2),
        NovoFocoJ1 = FocoJ1,
        NovaPeca = J1
    ),
    writeln("Mudando para o próximo jogador..."),
    % Chama a próxima rodada passando '_' no nome, pois ele será resolvido dentro da rodada
    rodada(NovaPeca, _, NovoFocoJ1, NovoFocoJ2, NovoPassado2, NovoPresente2, NovoFuturo2).

jogar(Foco, Jogador, Passado, Presente, Futuro, NovoPassado, NovoPresente, NovoFuturo) :-
    exibirTabuleiros(Passado, Presente, Futuro),

    escolherJogada(Escolha),


    ( Foco == passado -> Tabuleiro = Passado
    ; Foco == presente -> Tabuleiro = Presente
    ; Foco == futuro -> Tabuleiro = Futuro
    ),

    ( Escolha == 'm' ->
        obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna),
        movimento(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna, Jogador, NovoPassado, NovoPresente, NovoFuturo)

    ; Escolha == 'p' ->
        obterLinha(Linha),
        obterColuna(Coluna),
        plantarSemente(Passado, Presente, Futuro, Foco, Linha, Coluna, NovoPassado, NovoPresente, NovoFuturo)

    ; Escolha == 'v' ->
        writeln("Jogada 'Viajar no tempo' ainda não implementada."),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro

    ; Escolha == 'r' ->
        writeln("Reiniciando o jogo..."),
        iniciar_jogo(NovoPassado, NovoPresente, NovoFuturo)

    ; % fallback
        writeln("Escolha inválida inesperada!"),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro
    ).

/**
 * Retorna o nome do jogador com base na peça.
 *
 * @param Peca A peça do jogador.
 * @param Nome O nome correspondente ao jogador.
 */
nomeDoJogador(Peca, Nome) :-
    jogador1(J1),
    jogador2(J2),
    ( Peca == J1 -> jogador1_nome(Nome)
    ; Peca == J2 -> jogador2_nome(Nome)
    ).

finalizarJogo(NomeJogador) :-
    format('Parabéns, ~w! Você venceu!~n', [NomeJogador]),
    atualizarRanking(NomeJogador, 1),
    mostrarRanking,
    iniciarJogo.