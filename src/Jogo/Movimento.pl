:- module(movimento, [movimento/11, escolherMovimento/4, movimentoBot/13]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').


movimento(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna, PecaEsperada, NovoPassado, NovoPresente, NovoFuturo) :-
    /* Efetua o movimento do jogador com as coordenadas já escolhidas, verificando e removendo sementes se necessário
 
  * @param Tabuleiro Tabuleiro antes do movimento.
  * @param Passado Tabuleiro do passado.
  * @param Presente Tabuleiro do presente.
  * @param Futuro Tabuleiro do futuro.
  * @param Foco Tempo atual do jogador (passado, presente ou futuro).
  * @param Linha Linha atual da peça a ser movida.
  * @param Coluna Coluna atual da peça a ser movida.
  * @param PecaEsperada Peça esperada na posição de destino para validação do movimento.
  * @param NovoPassado Tabuleiro do passado atualizado após o movimento.
  * @param NovoPresente Tabuleiro do presente atualizado após o movimento.
  * @param NovoFuturo Tabuleiro do futuro atualizado após o movimento.
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
    /* Efetua o movimento do jogador com as coordenadas já escolhidas, verificando e removendo sementes se necessário
 
  * @param Tabuleiro Tabuleiro antes do movimento.
  * @param Passado Tabuleiro do passado.
  * @param Presente Tabuleiro do presente.
  * @param Futuro Tabuleiro do futuro.
  * @param Foco Tempo atual do bot (passado, presente ou futuro).
  * @param Linha Linha atual da peça a ser movida.
  * @param Coluna Coluna atual da peça a ser movida.
  * @param NovaLinha Linha de destino do movimento.
  * @param NovaColuna Coluna de destino do movimento.
  * @param PecaEsperada Peça esperada na posição de destino para validação do movimento.
  * @param NovoPassado Tabuleiro do passado atualizado após o movimento.
  * @param NovoPresente Tabuleiro do presente atualizado após o movimento.
  * @param NovoFuturo Tabuleiro do futuro atualizado após o movimento.
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
    