% File: controllerPlantas.pl
:- module(controllerPlantas, [plantarSemente/9]).
:- use_module(tabuleiro, [plantar/5, semente/1, arbusto/1, arvore/1]).  

plantarSemente(TPassado, TPresente, TFuturo, Tempo, Linha, Coluna, NovoTPassado, NovoTPresente, NovoTFuturo) :-
    semente(Semente),
    arbusto(Arbusto),
    arvore(Arvore),
    (Tempo == 'passado' ->
        plantar(TPassado, Semente, Linha, Coluna, TempPassado),
        plantar(TPresente, Arbusto, Linha, Coluna, TempPresente),
        plantar(TFuturo, Arvore, Linha, Coluna, TempFuturo),
        NovoTPassado = TempPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'presente' ->
        plantar(TPresente, Semente, Linha, Coluna, TempPresente),
        plantar(TFuturo, Arbusto, Linha, Coluna, TempFuturo),
        NovoTPassado = TPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'futuro' ->
        plantar(TFuturo, Semente, Linha, Coluna, TempFuturo),
        NovoTPassado = TPassado,
        NovoTPresente = TPresente,
        NovoTFuturo = TempFuturo
    ).
