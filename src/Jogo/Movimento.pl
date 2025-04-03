:- module(movimento, [movimento/6]).
:- use_module(tabuleiro, [verificarPosicaoTabuleiro/4, moverPeca/7, exibirTabuleiros/3, semente/1, arbusto/1, arvore/1]).

movimento(Foco, Passado, Presente, Futuro, PecaEsperada, TabuleiroAtualizado) :-
    /* Efetua o movimento do jogador. 

    Args: 
        Foco: foco atual do jogador. 
        Passado: tabuleiro atual que representa o passado. 
        Presente: tabuleiro atual que representa o presente. 
        Futuro: tabuleiro atual que representa o futuro. 
        PecaEsperada: Peca do jogador. 
        TabuleiroAtualizado: Novo tabuleiro que foi modificado. 
    */
    ( Foco == passado -> Tabuleiro = Passado
    ; Foco == presente -> Tabuleiro = Presente
    ; Foco == futuro -> Tabuleiro = Futuro
    ),
    obtemCoordenadas(Linha, Coluna),
    ( verificarPosicaoTabuleiro(Tabuleiro, Linha, Coluna, PecaEsperada) ->
        escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna),
        moverPeca(Tabuleiro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado)
    ;
        format("Posição inválida! Insira uma posição em que a sua peça (~w) se encontre.\n", [PecaEsperada]),
        movimento(Foco, Passado, Presente, Futuro, PecaEsperada, TabuleiroAtualizado)
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

verificarPosicao(Tabuleiro, PecaEsperada) :-
    /* Verifica a posição escolhida pelo jogador e repete até encontrar uma válida.
        
    Args:
        Tabuleiro: o tabuleiro atual.
        PecaEsperada: a peça que o jogador deve selecionar.
    
    Returns: boolean informando se na posição havia a peça esperada ou não.
    */
    obtemCoordenadas(Linha, Coluna),
    ( verificarPosicaoTabuleiro(Tabuleiro, Linha, Coluna, PecaEsperada) -> true ;
        format("Posição inválida! Insira uma posição em que a sua peça (~w) se encontre.\n", [PecaEsperada]),
        verificarPosicao(Tabuleiro, PecaEsperada)
    ).

escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna) :-
    /* Pergunta ao jogador para onde deseja mover e calcula a nova posição, garantindo que esteja nos limites.
        
    Args:
        Linha: linha atual da peça.
        Coluna: coluna atual da peça.
        NovaLinha: linha após o movimento.
        NovaColuna: coluna após o movimento.

    Returns: a nova coordenada para que o jogador se mova.
    */
    writeln("Para onde deseja mover? (w = cima, a = esquerda, s = baixo, d = direita):"),
    read(Direcao),
    ( Direcao == w -> TempLinha is Linha - 1, TempColuna is Coluna
    ; Direcao == a -> TempLinha is Linha, TempColuna is Coluna - 1
    ; Direcao == s -> TempLinha is Linha + 1, TempColuna is Coluna
    ; Direcao == d -> TempLinha is Linha, TempColuna is Coluna + 1
    ; writeln("Direção inválida! Use apenas w, a, s ou d."), escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna)
    ),
    ( TempLinha >= 1, TempLinha =< 4, TempColuna >= 1, TempColuna =< 4 ->
    NovaLinha = TempLinha, NovaColuna = TempColuna
    ;
        writeln("Movimento inválido! Escolha uma direção que mantenha a peça dentro dos limites do tabuleiro."),
        escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna)
    ).
