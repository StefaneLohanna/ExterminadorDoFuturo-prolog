:- module(coordenadas, [
    obtemCoordenadas/2, 
    obterLinha/1, 
    obterColuna/1, 
    obterCoordenadasValidas/4, 
    escolherMovimento/4, 
    validarPosicao/4
]).

:- use_module('./src/Jogo/Tabuleiro.pl').

obtemCoordenadas(Linha, Coluna) :-
    obterLinha(Linha),
    obterColuna(Coluna).

obterLinha(Linha) :-
    write("Informe a linha (1-4): "), read(L),
    ( integer(L), L >= 1, L =< 4 ->
        Linha = L
    ;
        writeln("Linha inválida! Escolha um valor entre 1 e 4."),
        obterLinha(Linha)
    ).

obterColuna(Coluna) :-
    write("Informe a coluna (1-4): "), read(C),
    ( integer(C), C >= 1, C =< 4 ->
        Coluna = C
    ;
        writeln("Coluna inválida! Escolha um valor entre 1 e 4."),
        obterColuna(Coluna)
    ).

escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna) :-
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

validarPosicao(Tabuleiro, Linha, Coluna, Jogador) :-
    verificarPosicaoTabuleiro(Tabuleiro, Linha, Coluna, Jogador).

obterCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna) :-
    obterLinha(Linha),
    obterColuna(Coluna),
    ( validarPosicao(Tabuleiro, Linha, Coluna, Jogador) ->
        true
    ;
        writeln("Você não possui uma peça nessa posição. Tente novamente."),
        obterCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna)
    ).
