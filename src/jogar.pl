% File: jogar.pl
:- module(jogar, [iniciarJogo/0]).
:- use_module(tabuleiro, [iniciarTabuleiros/3, exibirTabuleiros/3]).
:- use_module(controllerPlantas, [plantarSemente/9]).  % Import the 9-arity version
:- use_module(movimento, [movimento/6]).

iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inica o tabuleiro e faz a rodada.
    */
    iniciarTabuleiros(Passado, Presente, Futuro),
    rodada('\x26AA', passado, Passado, Presente, Futuro).


rodada(Peca, Foco, Passado, Presente, Futuro) :-
    /* Faz a rodada chamando jogar duas vezes para o mesmo jogador e depois trocando o jogador atual.

    Args: 
        Peca: peça do jogador atual. 
        Foco: foco do jogador atual. 
        Passado: tabuleiro do passado. 
        Presente: tabuleiro do presente.
        Futuro: tabuleiro do futuro. 

    Returns: chama recursivamente passando os tabuleiros atualizados.  
    */
    format("Vez do jogador: ~w~n", [Peca]),
    jogar(Foco, Peca, Passado, Presente, Futuro, NovoPassado1, NovoPresente1, NovoFuturo1),
    jogar(Foco, Peca, NovoPassado1, NovoPresente1, NovoFuturo1, NovoPassado2, NovoPresente2, NovoFuturo2),
    ( Peca == '\x26AA' -> NovoPeca = '\x26AB' ; NovoPeca = '\x26AA' ),
    writeln("Mudando para o próximo jogador."),
    rodada(NovoPeca, Foco, NovoPassado2, NovoPresente2, NovoFuturo2).



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
    ( Escolha == 'm' -> 
        movimento(Foco, Passado, Presente, Futuro, Jogador, NovoTabuleiro),
        ( Foco == passado -> (NovoPassado = NovoTabuleiro, NovoPresente = Presente, NovoFuturo = Futuro)
        ; Foco == presente -> (NovoPassado = Passado, NovoPresente = NovoTabuleiro, NovoFuturo = Futuro)
        ; Foco == futuro -> (NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = NovoTabuleiro)
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



