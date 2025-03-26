:- module(jogar, [iniciarJogo/0]).
:- use_module(tabuleiro, [iniciarTabuleiros/3, exibirTabuleiros/3, plantar/5]).

iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inicia o tabuleiro e o exibe
    */
    iniciarTabuleiros(Passado, Presente, Futuro),
    exibirTabuleiros(Passado, Presente, Futuro),
    writeln('\nPlantando na posição (2,3) do tabuleiro do passado:'),
    plantar(Passado, '🌱', 2, 3, TabuleiroModificado),
    exibirTabuleiros(TabuleiroModificado, Presente, Futuro).



