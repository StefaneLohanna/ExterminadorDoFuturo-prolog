:- module(movimento, [movimento/11]).
:- use_module('./Tabuleiro.pl', [verificarPosicaoTabuleiro/4, moverPeca/7, exibirTabuleiros/3, semente/1, arbusto/1, arvore/1]).

movimento(Tabuleiro, Foco, Passado, Presente, Futuro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado) :-
    /* Efetua o movimento do jogador com as coordenadas já escolhidas.

    Args: 
        Linha, Coluna: posição atual da peça.
        NovaLinha, NovaColuna: posição para onde mover.
        PecaEsperada: Peça do jogador. 
        TabuleiroAtualizado: Novo tabuleiro após a movimentação.
    */
    moverPeca(Tabuleiro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado).


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
