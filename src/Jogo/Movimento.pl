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

movimentoBot(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna,NovaLinha, NovaColuna, PecaEsperada, NovoPassado, NovoPresente, NovoFuturo) :-
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
    

/* Verifica a posição escolhida pelo jogador e repete até encontrar uma válida.
        
Args:
    Tabuleiro: o tabuleiro atual.
    PecaEsperada: a peça que o jogador deve selecionar.
    
Returns: boolean informando se na posição havia a peça esperada ou não.
*/
/*
verificarPosicao(Tabuleiro, PecaEsperada) :-

    obtemCoordenadas(Linha, Coluna),
    ( verificarPosicaoTabuleiro(Tabuleiro, Linha, Coluna, PecaEsperada) -> true ;
        format("Posição inválida! Insira uma posição em que a sua peça (~w) se encontre.\n", [PecaEsperada]),
        verificarPosicao(Tabuleiro, PecaEsperada)
    ).
*/

escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna) :-
    /* Pergunta ao jogador para onde deseja mover e calcula a nova posição, garantindo que esteja nos limites.
        
    Args:
        Linha: linha atual da peça.
        Coluna: coluna atual da peça.
        NovaLinha: linha após o movimento.
        NovaColuna: coluna após o movimento.

    Returns: a nova coordenada para que o jogador se mova.
    */
    writeln("Para onde deseja mover? (w = cima, a = esquerda, s = baixo, d = direita):"),
    read_line_to_string(user_input, Entrada),
    string_lower(Entrada, Lower),
    removerEspacos(Lower, EntradaSemEspaco),
    atom_string(Direcao, EntradaSemEspaco),
    ( Direcao == w -> TempLinha is Linha - 1, TempColuna is Coluna
    ; Direcao == a -> TempLinha is Linha, TempColuna is Coluna - 1
    ; Direcao == s -> TempLinha is Linha + 1, TempColuna is Coluna
    ; Direcao == d -> TempLinha is Linha, TempColuna is Coluna + 1
    ; writeln("Direção inválida! Use apenas w, a, s ou d."),
    escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna)
    ), 
    ( TempLinha >= 1, TempLinha =< 4, TempColuna >= 1, TempColuna =< 4 ->
        NovaLinha = TempLinha, NovaColuna = TempColuna
    ;
        writeln("Movimento inválido! Escolha uma direção que mantenha a peça dentro dos limites do tabuleiro."),
        escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna)
    ).
