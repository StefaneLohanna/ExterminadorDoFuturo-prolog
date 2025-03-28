% File: jogar.pl
:- module(jogar, [iniciarJogo/0]).
:- use_module(tabuleiro, [iniciarTabuleiros/3, exibirTabuleiros/3]).
:- use_module(controllerPlantas, [plantarSemente/9]).  % Import the 9-arity version

iniciarJogo :-
    iniciarTabuleiros(Passado, Presente, Futuro),
    exibirTabuleiros(Passado, Presente, Futuro),
    writeln('\nPlantando na posição (2,3) do tabuleiro do presente:'),
    plantarSemente(Passado, Presente, Futuro, futuro, 2, 3, 
                NovoPassado, NovoPresente, NovoFuturo),

    exibirTabuleiros(NovoPassado, NovoPresente, NovoFuturo).



