:- module(movimento, [movimento/11, escolherMovimento/4, movimentoBot/13]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').


movimento(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna, PecaEsperada, NovoPassado, NovoPresente, NovoFuturo) :-
    /* Efetua o movimento do jogador. 

    Args: 
        Foco: foco atual do jogador. 
        Passado: tabuleiro atual que representa o passado. 
        Presente: tabuleiro atual que representa o presente. 
        Futuro: tabuleiro atual que representa o futuro. 
        PecaEsperada: Peca do jogador. 
        TabuleiroAtualizado: Novo tabuleiro que foi modificado. 
    */
    
    escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna),
    semente(Semente),
    nth1(NovaLinha, Tabuleiro, LinhaDestinoLista),
    nth1(NovaColuna, LinhaDestinoLista, ConteudoDestino),

    % Primeiro verifica e remove semente se necessário
    (ConteudoDestino == Semente ->  
        removerSemente(Passado, Presente, Futuro, Foco, NovaLinha, NovaColuna, TempPassado, TempPresente, TempFuturo)
    ;
        TempPassado = Passado,
        TempPresente = Presente,
        TempFuturo = Futuro
    ),
    
    moverPeca(Tabuleiro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado),
    
    (Foco == passado -> (NovoPassado = TabuleiroAtualizado, NovoPresente = TempPresente, NovoFuturo = TempFuturo)
    ; Foco == presente -> (NovoPassado = TempPassado, NovoPresente = TabuleiroAtualizado, NovoFuturo = TempFuturo)
    ; Foco == futuro -> (NovoPassado = TempPassado, NovoPresente = TempPresente, NovoFuturo = TabuleiroAtualizado)
    ).

movimentoBot(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, NovoPassado, NovoPresente, NovoFuturo) :-
    /* Efetua o movimento do jogador. 

    Args: 
        Foco: foco atual do jogador. 
        Passado: tabuleiro atual que representa o passado. 
        Presente: tabuleiro atual que representa o presente. 
        Futuro: tabuleiro atual que representa o futuro. 
        PecaEsperada: Peca do jogador. 
        TabuleiroAtualizado: Novo tabuleiro que foi modificado. 
    */

    semente(Semente),
    nth1(NovaLinha, Tabuleiro, LinhaDestinoLista),
    nth1(NovaColuna, LinhaDestinoLista, ConteudoDestino),

    % Primeiro verifica e remove semente se necessário
    (ConteudoDestino == Semente ->  
        removerSemente(Passado, Presente, Futuro, Foco, NovaLinha, NovaColuna, TempPassado, TempPresente, TempFuturo)
    ;
        TempPassado = Passado,
        TempPresente = Presente,
        TempFuturo = Futuro
    ),
    
    moverPeca(Tabuleiro, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, TabuleiroAtualizado),
    
    (Foco == passado -> (NovoPassado = TabuleiroAtualizado, NovoPresente = TempPresente, NovoFuturo = TempFuturo)
    ; Foco == presente -> (NovoPassado = TempPassado, NovoPresente = TabuleiroAtualizado, NovoFuturo = TempFuturo)
    ; Foco == futuro -> (NovoPassado = TempPassado, NovoPresente = TempPresente, NovoFuturo = TabuleiroAtualizado)
    ).
    