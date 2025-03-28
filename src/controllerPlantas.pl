% File: controllerPlantas.pl
:- module(controllerPlantas, [plantarSemente/9]).  % Correctly export 9-arity predicate
:- use_module(tabuleiro, [plantar/5]).

plantarSemente(TPassado, TPresente, TFuturo, Tempo, Linha, Coluna, NovoTPassado, NovoTPresente, NovoTFuturo) :-
    (Tempo == 'passado' -> 
        plantar(TPassado, '🌰', Linha, Coluna, TempPassado),
        plantar(TPresente, '🌱', Linha, Coluna, TempPresente),
        plantar(TFuturo, '🌳', Linha, Coluna, TempFuturo),
        NovoTPassado = TempPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'presente' ->
        plantar(TPresente, '🌰', Linha, Coluna, TempPresente),
        plantar(TFuturo, '🌱', Linha, Coluna, TempFuturo),
        NovoTPassado = TPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'futuro' ->
        plantar(TFuturo, '🌰', Linha, Coluna, TempFuturo),
        NovoTPassado = TPassado,
        NovoTPresente = TPresente,
        NovoTFuturo = TempFuturo
    ).