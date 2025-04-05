% File: jogar.pl
:- module(jogar, [iniciarJogo/0]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').

/*iniciarJogo :-
    iniciarTabuleiros(Passado, Presente, Futuro),
    exibirTabuleiros(Passado, Presente, Futuro),
    writeln('\nPlantando na posição (2,3) do tabuleiro do presente:'),
    plantarSemente(Passado, Presente, Futuro, futuro, 2, 3, 
                NovoPassado, NovoPresente, NovoFuturo),

    exibirTabuleiros(NovoPassado, NovoPresente, NovoFuturo).*/
espacoVazio(Ev).
iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inica o tabuleiro e faz a rodada.
    */
    jogador1(J1),
    iniciarTabuleiros(Passado, Presente, Futuro),
    rodada(J1, passado, futuro, Passado, Presente, Futuro).

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
rodada(Peca, FocoJ1, FocoJ2, Passado, Presente, Futuro) :-
    jogador1(J1),
    jogador2(J2),
    format("Vez do jogador: ~w~n", [Peca]),

    % Determina qual foco usar nesta rodada
    (Peca == J1 -> FocoAtual = FocoJ1 ; FocoAtual = FocoJ2),

    % Verifica se o jogador está presente no foco atual
    (focoValido(FocoAtual, Peca, Passado, Presente, Futuro) -> NovoFoco = FocoAtual;
        writeln('Jogador não está presente nesse foco. Escolha outro.'),
        definirFoco(Peca, Passado, Presente, Futuro, FocoAtual, NovoFoco)
    ),

    jogar(NovoFoco, Peca, Passado, Presente, Futuro, NovoPassado1, NovoPresente1, NovoFuturo1),
    jogar(NovoFoco, Peca, NovoPassado1, NovoPresente1, NovoFuturo1, NovoPassado2, NovoPresente2, NovoFuturo2),
    
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
    writeln("Mudando para o próximo jogador."),
    % Alterna para o outro jogador (mantendo os focos atualizados)
    rodada(NovaPeca, NovoFocoJ1, NovoFocoJ2, NovoPassado2, NovoPresente2, NovoFuturo2).

jogar(Foco, Jogador, Passado, Presente, Futuro, NovoPassado, NovoPresente, NovoFuturo) :-
    exibirTabuleiros(Passado, Presente, Futuro),

    escolherJogada(Escolha),

    ( Foco == passado -> Tabuleiro = Passado
    ; Foco == presente -> Tabuleiro = Presente
    ; Foco == futuro -> Tabuleiro = Futuro
    ),

    ( Escolha == m ->
        obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna),
        movimento(Tabuleiro, Foco, Passado, Presente, Futuro, Linha, Coluna, Jogador, NovoTabuleiro),
        ( Foco == passado -> (NovoPassado = NovoTabuleiro, NovoPresente = Presente, NovoFuturo = Futuro)
        ; Foco == presente -> (NovoPassado = Passado, NovoPresente = NovoTabuleiro, NovoFuturo = Futuro)
        ; Foco == futuro -> (NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = NovoTabuleiro)
        )

    ; Escolha == p ->
        writeln("Digite a linha: "),
        read(Linha),
        writeln("Digite a coluna: "),
        read(Coluna),
        plantarSemente(Passado, Presente, Futuro, Foco, Linha, Coluna, NovoPassado, NovoPresente, NovoFuturo)

    ; Escolha == v ->
        writeln("Jogada 'Viajar no tempo' ainda não implementada."),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro

    ; Escolha == r ->
        writeln("Reiniciando o jogo..."),
        iniciar_jogo(NovoPassado, NovoPresente, NovoFuturo)

    ; % fallback
        writeln("Escolha inválida inesperada!"),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro
    ).