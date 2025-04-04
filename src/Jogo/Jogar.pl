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

    jogar(FocoAtual, Peca, Passado, Presente, Futuro, NovoPassado1, NovoPresente1, NovoFuturo1),
    jogar(FocoAtual, Peca, NovoPassado1, NovoPresente1, NovoFuturo1, NovoPassado2, NovoPresente2, NovoFuturo2),
    
    exibirTabuleiros(NovoPassado2, NovoPresente2, NovoFuturo2),
    
    % Define o foco para a próxima rodada
    (
        Peca == J1 -> 
        definirFoco(J1, NovoPassado2, NovoPresente2, NovoFuturo2, FocoAtual, NovoFocoJ1),
        NovoFocoJ2 = FocoJ2,
        NovaPeca = J2
        ;
        definirFoco(J2, NovoPassado2, NovoPresente2, NovoFuturo2, FocoAtual, NovoFocoJ2),
        NovoFocoJ1 = FocoJ1,
        NovaPeca = J1
    ),
    writeln("Mudando para o próximo jogador."),
    % Alterna para o outro jogador (mantendo os focos atualizados)
    rodada(NovaPeca, NovoFocoJ1, NovoFocoJ2, NovoPassado2, NovoPresente2, NovoFuturo2).

jogar(Foco, Jogador, Passado, Presente, Futuro, NovoPassado, NovoPresente, NovoFuturo) :-
    /* Pede pro jogador escolher a jogada que quer realizar e chama as funções correspondentes a ela. 

    Args: 
        Foco: foco do jogador Atual.
        Jogador: jogador atual. 
        Passado: tabuleiro do passado original.
        Presente: tabuleiro do presente original.
        Futuro: tabuleiro do futuro original. 

    Returns: 
        NovoPassado: tabuleiro do passado atualizado. 
        NovoPresente: tabuleiro do presente atualizado. 
        NovoFuturo: tabuleiro do futuro atualizado. 
    */
    exibirTabuleiros(Passado, Presente, Futuro),
    writeln("Escolha uma ação:"),
    writeln("(m) Movimentar"),
    writeln("(p) Plantar"),
    writeln("(v) Viajar no tempo"),
    write("Digite sua escolha: "),
    read(Escolha),
    ( Foco == passado -> Tabuleiro = Passado
    ; Foco == presente -> Tabuleiro = Presente
    ; Foco == futuro -> Tabuleiro = Futuro
    ),
    ( Escolha == 'm' ->
        obtemCoordenadas(Linha, Coluna),
        ( 
            verificarPosicaoTabuleiro(Tabuleiro, Linha, Coluna, Jogador) -> 
            % Caso verdadeiro: executa o movimento
            movimento(Tabuleiro, Foco, Passado, Presente, Futuro, Linha, Coluna, Jogador, NovoTabuleiro),
            ( Foco == passado -> (NovoPassado = NovoTabuleiro, NovoPresente = Presente, NovoFuturo = Futuro)
            ; Foco == presente -> (NovoPassado = Passado, NovoPresente = NovoTabuleiro, NovoFuturo = Futuro)
            ; Foco == futuro -> (NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = NovoTabuleiro)
            )
            ;
            
            % Caso falso: mostra mensagem e chama recursivamente
            format("Posição inválida! Insira uma posição em que a sua peça (~w) se encontre.\n", [PecaEsperada]),
            movimento(Tabuleiro, Foco, Passado, Presente, Futuro, Linha, Coluna, Jogador, NovoTabuleiro)
        ) 
    ; Escolha == 'p' -> 
        writeln("Digite a linha: "),
        read(Linha),
        writeln("Digite a coluna: "),
        read(Coluna),
        plantarSemente(Passado, Presente, Futuro, Foco, Linha, Coluna, NovoPassado, NovoPresente, NovoFuturo)
    ; Escolha == 'v' -> 
        writeln("Jogada 'Viajar no tempo' ainda não implementada."),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro
    ; 
        writeln("Escolha inválida! Por favor, escolha uma opção válida."),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro
    ).
obtemCoordenadas(Linha, Coluna) :-
    /* Obtém as coordenadas originais do jogador. 

    Args: 
        Linha: Linha atual.
        Coluna: Coluna atual. 

    Returns: A linha e a coluna inseridas. 
    */
    obterLinha(Linha),
    obterColuna(Coluna).


obterLinha(Linha) :-
    /* Obtém uma linha válida para o jogador jogar. 

    Args: 
        Linha: linha atual do jogador. 
    
    Returns: uma linha que atende os limites do tabuleiro. 
    */
    write("Informe a linha (1-4): "), read(L),
    ( integer(L), L >= 1, L =< 4 ->
        Linha = L
    ;
        writeln("Linha inválida! Escolha um valor entre 1 e 4."),
        obterLinha(Linha) % Repete até obter um valor válido
    ).


obterColuna(Coluna) :-
    /* Obtém uma coluna válida para o jogador jogar. 

    Args: 
        Coluna: coluna atual do jogador. 
    
    Returns: uma coluna que atende os limites do tabuleiro.
    */ 
    write("Informe a coluna (1-4): "), read(C),
    ( integer(C), C >= 1, C =< 4 ->
        Coluna = C
    ;
        writeln("Coluna inválida! Escolha um valor entre 1 e 4."),
        obterColuna(Coluna) % Repete até obter um valor válido
    ).

