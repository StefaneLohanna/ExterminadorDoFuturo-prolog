:- module(movimento, [movimento/13]).
:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').

movimento(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna, NovaLinha, NovaColuna, PecaEsperada, NovoPassado, NovoPresente, NovoFuturo) :-
    /* Efetua o movimento do jogador com as coordenadas já escolhidas.
    Args: 
        Linha, Coluna: posição atual da peça.
        NovaLinha, NovaColuna: posição para onde mover.
        PecaEsperada: Peça do jogador. 
        TabuleiroAtualizado: Novo tabuleiro após a movimentação.
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
    

