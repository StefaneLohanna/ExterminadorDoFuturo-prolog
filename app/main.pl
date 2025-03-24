:- use_module('../src/jogar').

main :-
    /* Main para controle, atualmente só chama a função iniciarJogo.
    */
    iniciarJogo,
    halt.

:- initialization(main).
