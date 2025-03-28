% File: jogar.pl
:- module(jogar, [iniciarJogo/0]).
:- use_module(tabuleiro, [iniciarTabuleiros/3, exibirTabuleiros/3]).
:- use_module(controllerPlantas, [plantarSemente/9]).  % Import the 9-arity version

iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inica o tabuleiro e o exibe
        */
    iniciarTabuleiros(Passado, Presente, Futuro),
    exibirTabuleiros(Passado, Presente, Futuro),
    writeln('\nPlantando na posição (2,3) do tabuleiro do presente:'),
    plantarSemente(Passado, Presente, Futuro, passado, 2, 3, 
                NovoPassado, NovoPresente, NovoFuturo),

    exibirTabuleiros(NovoPassado, NovoPresente, NovoFuturo).



