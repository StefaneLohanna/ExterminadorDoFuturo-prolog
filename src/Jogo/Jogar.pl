% File: jogar.pl
:- module(jogar, [iniciarJogo/0]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').
:- use_module('./src/Jogo/ViagemNoTempo.pl').

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
    rodada(J1, passado, futuro, 0, 0, Passado, Presente, Futuro).

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
rodada(Peca, FocoJ1, FocoJ2, ClonesJ1, ClonesJ2, Passado, Presente, Futuro) :-
    jogador1(J1),
    jogador2(J2),
    format("Vez do jogador: ~w~n", [Peca]),

    (Peca == J1 -> (FocoAtual = FocoJ1, CloneAtual = ClonesJ1) 
    ;              (FocoAtual = FocoJ2, CloneAtual = ClonesJ2)),

    jogar(FocoAtual, CloneAtual, Peca, Passado, Presente, Futuro,
          NovoPassado1, NovoPresente1, NovoFuturo1, NovoClone1, NovoFoco1),

    jogar(NovoFoco1, NovoClone1, Peca, NovoPassado1, NovoPresente1, NovoFuturo1,
          NovoPassado2, NovoPresente2, NovoFuturo2, NovoClone2, NovoFoco2),

    exibirTabuleiros(NovoPassado2, NovoPresente2, NovoFuturo2),

    (Peca == J1 ->
        definirFoco(J1, NovoPassado2, NovoPresente2, NovoFuturo2, FocoAtual, NovoFocoJ1),
        NovoFocoJ2 = FocoJ2,
        NovaPeca = J2,
        NovosClonesJ1 = NovoClone2,
        NovosClonesJ2 = ClonesJ2
    ;
        definirFoco(J2, NovoPassado2, NovoPresente2, NovoFuturo2, FocoAtual, NovoFocoJ2),
        NovoFocoJ1 = FocoJ1,
        NovaPeca = J1,
        NovosClonesJ1 = ClonesJ1,
        NovosClonesJ2 = NovoClone2
    ),

    writeln("Mudando para o próximo jogador."),
    rodada(NovaPeca, NovoFocoJ1, NovoFocoJ2, NovosClonesJ1, NovosClonesJ2, NovoPassado2, NovoPresente2, NovoFuturo2).

jogar(Foco, Clones, Jogador, Passado, Presente, Futuro, NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco) :-
    exibirTabuleiros(Passado, Presente, Futuro),
    writeln("Escolha uma ação:"),
    writeln("(m) Movimentar"),
    writeln("(p) Plantar"),
    writeln("(v) Viajar no tempo"),
    write("Digite sua escolha: "),
    read(Escolha),
    (
        Foco == passado -> Tabuleiro = Passado ;
        Foco == presente -> Tabuleiro = Presente ;
        Foco == futuro -> Tabuleiro = Futuro
    ),
    (
        Escolha == 'm' ->
            repeat,
            obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna),
            escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna),
            movimento(Tabuleiro, Foco, Passado, Presente, Futuro, Linha, Coluna, NovaLinha, NovaColuna, Jogador, NovoTabuleiro),
            (
                Foco == passado -> (NovoPassado = NovoTabuleiro, NovoPresente = Presente, NovoFuturo = Futuro) ;
                Foco == presente -> (NovoPassado = Passado, NovoPresente = NovoTabuleiro, NovoFuturo = Futuro) ;
                Foco == futuro -> (NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = NovoTabuleiro)
            ),
            NovoClones = Clones,
            NovoFoco = Foco  

        ; Escolha == 'p' ->
            writeln("Digite a linha: "),
            read(Linha),
            writeln("Digite a coluna: "),
            read(Coluna),
            plantarSemente(Passado, Presente, Futuro, Foco, Linha, Coluna, NovoPassado, NovoPresente, NovoFuturo),
            NovoClones = Clones,
            NovoFoco = Foco

        ; Escolha == 'v' ->
        obtemCoordenadasOrigemValidas(Tabuleiro, Jogador, Linha, Coluna),
        escolherTempo(TempoEscolhido),
        defineViagem(Foco, Clones, TempoEscolhido, Resultado),
        (
            Resultado == "viagem impossível" ->
                writeln("Viagem impossível. Tente outra jogada."),
                jogar(Foco, Clones, Jogador, Passado, Presente, Futuro,
                      NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco)
            ;
            (
                ( TempoEscolhido == "passado" -> TabDestino = Passado ;
                  TempoEscolhido == "presente" -> TabDestino = Presente ;
                  TempoEscolhido == "futuro" -> TabDestino = Futuro
                ),
                verificaPosicaoLivre(TabDestino, Linha, Coluna, Livre),
                (
                    Livre == true ->
                        viagem(Foco, Clones, TempoEscolhido, Linha, Coluna, Jogador,
                               Passado, Presente, Futuro,
                               NovoPassado, NovoPresente, NovoFuturo, NovoClones),
                        stringParaFoco(TempoEscolhido, NovoFoco)
                    ;
                        writeln("A posição no tempo de destino já está ocupada. Tente outra jogada."),
                        jogar(Foco, Clones, Jogador, Passado, Presente, Futuro,
                              NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco)
                )
            )
        )
        ; otherwise ->
            writeln("Ação inválida. Tente novamente."),
            jogar(Foco, Clones, Jogador, Passado, Presente, Futuro,
                NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco)
        
        ).