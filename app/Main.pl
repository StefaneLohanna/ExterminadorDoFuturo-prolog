:- use_module('../src/Jogo/jogar.pl').

main :-
    /*
    Main para controle, atualmente só chama a função iniciarJogo.
    */
    iniciarJogo,
    halt.

:- initialization(main).
