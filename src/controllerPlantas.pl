% File: controllerPlantas.pl

% Módulo responsável pelo controle do plantio de sementes em diferentes períodos de tempo
:- module(controllerPlantas, [plantarSemente/9]).  % Exporta corretamente o predicado de aridade 9

% Importa o módulo tabuleiro e a função plantar/5, que modifica o estado do tabuleiro
:- use_module(tabuleiro, [plantar/5]).

/**
 * plantarSemente/9
 * 
 * Predicado responsável por plantar uma semente em um tabuleiro de um determinado período de tempo.
 * Dependendo do tempo escolhido ('passado', 'presente' ou 'futuro'), a semente evolui no tempo seguinte.
 * 
 * @param TPassado      Tabuleiro do passado antes da plantação
 * @param TPresente     Tabuleiro do presente antes da plantação
 * @param TFuturo       Tabuleiro do futuro antes da plantação
 * @param Tempo         Tempo onde a semente será plantada ('passado', 'presente' ou 'futuro')
 * @param Linha         Linha do tabuleiro onde a semente será plantada
 * @param Coluna        Coluna do tabuleiro onde a semente será plantada
 * @param NovoTPassado  Tabuleiro do passado após a plantação
 * @param NovoTPresente Tabuleiro do presente após a plantação
 * @param NovoTFuturo   Tabuleiro do futuro após a plantação
 */
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