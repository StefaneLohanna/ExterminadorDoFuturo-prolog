:- use_module('../src/Jogo/Jogar.pl').

main :-
    /*
    Main para controle, atualmente só chama a função iniciarJogo.
    */
    iniciarJogo,
    halt.

:- initialization(main).
