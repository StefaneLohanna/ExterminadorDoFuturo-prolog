:- module(tabuleiro, [criarTabuleiro/1, exibirTabuleiros/3, iniciarTabuleiros/3, modificar_matriz/5, plantar/5,semente/1, arbusto/1, arvore/1, jogador1/1, jogador2/1, verificarPosicaoTabuleiro/4, moverPeca/7]).

%  Definindo os emojis dos jogadores
espacoVazio('\x1F533'). 
jogador1('\x26AA').
jogador2('\x26AB').
semente('\x1F330').
arbusto('\x1F331').
arvore('\x1F333').
caveira('\x1F480').
negado('\x274C').
exclamacao('\x2757').


criarTabuleiro(Tabuleiro) :-
    /* Cria o tabuleiro já colocando os jogadores nas extremidades. 
    
    Args: 
        Tabuleiro: tabuleiro a ser criado. 

    Returns: o tabuleiro gerado.
    */
    espacoVazio(Ev),
    jogador1(J1),
    jogador2(J2),
    Tabuleiro = [
        [J1, Ev, Ev, Ev],
        [Ev, Ev, Ev, Ev],
        [Ev, Ev, Ev, Ev],
        [Ev, Ev, Ev, J2]
    ].

exibirTabuleiros(Tab1, Tab2, Tab3) :-
    /* Exibe os três tabuleiros lado a lado.
    
    Args: 
        Tab1: tabuleiro um, tipicamente o passado.
        Tab2: tabuleiro dois, tipicamente o presente.
        Tab3: tabuleiro três, tipicamente o futuro.

    Returns: exibe os tabuleiros combinados lado a lado.
    */

    maplist(concatenaLinhas, Tab1, Tab2, Tab3, LinhasCombinadas),
    maplist(writeln, LinhasCombinadas).

concatenaLinhas(Linha1, Linha2, Linha3, LinhaCombinada) :-
    /* Concatena as linhas dos 3 tabuleiros diferentes uma ao lado da outra para permitir a impressão, adicionando | como delimitadores
    
    Args:  
        Linha1: linha do passado.
        Linha2: linha do presente.
        Linha3: linha do futuro.
        LinhaCombinada: índice da linha quq está sendo concatenada.7

    Returns: linhas concatenadas.  
    */
    atomic_list_concat(Linha1, ' ', Linha1FormatadaSemBordas),
    atomic_list_concat(Linha2, ' ', Linha2FormatadaSemBordas),
    atomic_list_concat(Linha3, ' ', Linha3FormatadaSemBordas),
    format(atom(Linha1Formatada), '|~w|', [Linha1FormatadaSemBordas]),
    format(atom(Linha2Formatada), '|~w|', [Linha2FormatadaSemBordas]),
    format(atom(Linha3Formatada), '|~w|', [Linha3FormatadaSemBordas]),
    atomic_list_concat([Linha1Formatada, Linha2Formatada, Linha3Formatada], '   ', LinhaCombinada).

iniciarTabuleiros(Tab1, Tab2, Tab3) :-
    /* Inicia os tabuleiros, criando o tabuleiro do presente, passado e futuro.
    
    Args: 
        Tab1: tabuleiro do passado.
        Tab2: tabuleiro do presente.
        Tab3: tabuleiro do futuro.
    */
    criarTabuleiro(Tab1),
    criarTabuleiro(Tab2),
    criarTabuleiro(Tab3).

    % Coloca uma planta em determinada posição do tabuleiro
plantar(Tabuleiro, Planta, Linha, Coluna, NovoTabuleiro) :-
    modificar_matriz(Linha, Coluna, Tabuleiro, Planta, NovoTabuleiro).

% Modifica uma posição na matriz
modificar_matriz(1, C, [Linha|Resto], NovoValor, [NovaLinha|Resto]) :-
    modificar_linha(C, Linha, NovoValor, NovaLinha).

modificar_matriz(L, C, [Linha|Resto], NovoValor, [Linha|NovoResto]) :-
    L > 1,
    L1 is L - 1,
    modificar_matriz(L1, C, Resto, NovoValor, NovoResto).

% Modifica uma posição na linha
modificar_linha(1, [_|Resto], NovoValor, [NovoValor|Resto]).
modificar_linha(C, [Elem|Resto], NovoValor, [Elem|NovoResto]) :-
    C > 1,
    C1 is C - 1,
    modificar_linha(C1, Resto, NovoValor, NovoResto).


verificarPosicaoTabuleiro(Tabuleiro, Linha, Coluna, PecaEsperada) :-
    /* Verifica se a peça correta está na posição escolhida.
        
    Args:
        Tabuleiro: o tabuleiro atual.
        Linha: linha escolhida pelo jogador.
        Coluna: coluna escolhida pelo jogador.
        PecaEsperada: a peça que o jogador deve selecionar.
        
    Returns:
        true se a posição for válida, false caso contrário.
    */
    nth1(Linha, Tabuleiro, LinhaLista), % Obtém a linha
    nth1(Coluna, LinhaLista, Peca), % Obtém a peça na posição escolhida
    Peca == PecaEsperada. % Retorna true se a peça for a esperada, false caso contrário

moverPeca(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado) :-
    /* Move a Peca com base na linha e a coluna.

    Args: 
        TabuleiroAntigo: tabuleiro original que vai ser atualizado. 
        LinhaOrigem: linha onde a peca estava anteriormente. 
        ColunaOrigem: coluna onde a peça estava anteriormente. 
        LinhaDestino: linha de destino da peça. 
        ColunaDestino: coluna de destino da peça. 
        Peca: peça do jogador atual em questão. 
    
    Returns: 
        TabuleiroAtualizado: tabuleiro depois que a peça se moveu.
    */
    espacoVazio(Ev),
    nth1(LinhaOrigem, TabuleiroAntigo, LinhaOrigemLista),
    replace(ColunaOrigem, LinhaOrigemLista, Ev, NovaLinhaOrigem),
    replace(LinhaOrigem, TabuleiroAntigo, NovaLinhaOrigem, TabuleiroIntermediario),
    nth1(LinhaDestino, TabuleiroIntermediario, LinhaDestinoLista),
    replace(ColunaDestino, LinhaDestinoLista, Peca, NovaLinhaDestino),
    replace(LinhaDestino, TabuleiroIntermediario, NovaLinhaDestino, TabuleiroAtualizado).

replace(Index, List, NewElement, NewList) :-
    /* Troca os elementos.
    */ 
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, NewElement, Rest).
