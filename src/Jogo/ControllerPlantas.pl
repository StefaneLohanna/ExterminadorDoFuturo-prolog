:- module(controllerPlantas, [plantarSemente/9, removerSemente/9, plantarSementeBot/9]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').


/*
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
    semente(Semente),
    arbusto(Arbusto),
    arvore(Arvore),
    escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna),
    (Tempo == 'passado' ->
        plantar(TPassado, Semente, NovaLinha, NovaColuna, true, TempPassado, SucessoS),
        plantar(TPresente, Arbusto, NovaLinha, NovaColuna, SucessoS, TempPresente, SucessoP),
        plantar(TFuturo, Arvore, NovaLinha, NovaColuna, SucessoP, TempFuturo, _),
        NovoTPassado = TempPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'presente' ->
        plantar(TPresente, Arbusto, NovaLinha, NovaColuna, true, TempPresente, SucessoP),
        plantar(TFuturo, Arvore, NovaLinha, NovaColuna, SucessoP, TempFuturo, _),
        NovoTPassado = TPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'futuro' ->
        plantar(TFuturo, Arvore, NovaLinha, NovaColuna, true, TempFuturo, _),
        NovoTPassado = TPassado,
        NovoTPresente = TPresente,
        NovoTFuturo = TempFuturo
    ).

plantarSementeBot(TPassado, TPresente, TFuturo, Tempo, NovaLinha, NovaColuna, NovoTPassado, NovoTPresente, NovoTFuturo) :-
    semente(Semente),
    arbusto(Arbusto),
    arvore(Arvore),
    (Tempo == 'passado' ->
        plantar(TPassado, Semente, NovaLinha, NovaColuna, true, TempPassado, SucessoS),
        plantar(TPresente, Arbusto, NovaLinha, NovaColuna, SucessoS, TempPresente, SucessoP),
        plantar(TFuturo, Arvore, NovaLinha, NovaColuna, SucessoP, TempFuturo, _),
        NovoTPassado = TempPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'presente' ->
        plantar(TPresente, Semente, NovaLinha, NovaColuna, true, TempPresente, SucessoP),
        plantar(TFuturo, Arbusto, NovaLinha, NovaColuna, SucessoP, TempFuturo, _),
        NovoTPassado = TPassado,
        NovoTPresente = TempPresente,
        NovoTFuturo = TempFuturo
    ; Tempo == 'futuro' ->
        plantar(TFuturo, Semente, NovaLinha, NovaColuna, true, TempFuturo, _),
        NovoTPassado = TPassado,
        NovoTPresente = TPresente,
        NovoTFuturo = TempFuturo
    ).

removerSemente(TPassado, TPresente, TFuturo, Tempo, Linha, Coluna, NovoTPassado, NovoTPresente, NovoTFuturo) :-
    semente(Semente),
    arbusto(Arbusto),
    arvore(Arvore),
    (Tempo == 'passado' ->
        removerPlanta(TPassado, Semente, Linha, Coluna, TabuleiroAtualizado),
        removerPlanta(TPresente, Arbusto, Linha, Coluna, TabuleiroPresenteAtualizado),
        removerPlanta(TFuturo, Arvore, Linha, Coluna, TabuleiroFuturoAtualizado),

        NovoTPassado = TabuleiroAtualizado,
        NovoTPresente = TabuleiroPresenteAtualizado,
        NovoTFuturo = TabuleiroFuturoAtualizado

    ; Tempo == 'presente' ->
        removerPlanta(TPresente, Arbusto, Linha, Coluna, TabuleiroPresenteAtualizado),
        removerPlanta(TFuturo, Arvore, Linha, Coluna, TabuleiroFuturoAtualizado),

        NovoTPassado = TPassado,
        NovoTPresente = TabuleiroPresenteAtualizado,
        NovoTFuturo = TabuleiroFuturoAtualizado
    ; Tempo == 'futuro' ->
        removerPlanta(TFuturo, Arvore, Linha, Coluna, TabuleiroFuturoAtualizado),
        NovoTPassado = TPassado,
        NovoTPresente = TPresente,
        NovoTFuturo = TabuleiroFuturoAtualizado
    ).