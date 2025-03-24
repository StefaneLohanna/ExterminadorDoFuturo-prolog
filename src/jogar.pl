:- module(jogar, [iniciarJogo/0]).
:- use_module(tabuleiro, [iniciarTabuleiros/3, exibirTabuleiros/3]).

iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inica o tabuleiro e o exibe
    */
    iniciarTabuleiros(Passado, Presente, Futuro),
    exibirTabuleiros(Passado, Presente, Futuro).


