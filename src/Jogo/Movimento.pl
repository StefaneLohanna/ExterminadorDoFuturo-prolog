:- module(movimento, [movimento/11]).
:- use_module('./Tabuleiro.pl', [verificarPosicaoTabuleiro/4, moverPeca/7, exibirTabuleiros/3, semente/1, arbusto/1, arvore/1]).
:- use_module('./Coordenadas.pl').

movimento(Tabuleiro, Foco, Passado, Presente, Futuro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado) :-
    /* Efetua o movimento do jogador com as coordenadas já escolhidas.

    Args: 
        Linha, Coluna: posição atual da peça.
        NovaLinha, NovaColuna: posição para onde mover.
        PecaEsperada: Peça do jogador. 
        TabuleiroAtualizado: Novo tabuleiro após a movimentação.
    */
    moverPeca(Tabuleiro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado).

