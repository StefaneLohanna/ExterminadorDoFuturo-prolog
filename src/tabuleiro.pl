:- module(tabuleiro, [criarTabuleiro/1, exibirTabuleiros/3, iniciarTabuleiros/3, modificar_matriz/5, plantar/5]).

% Cria o tabuleiro 4x4 com o unicode de um quadrado em cada casa
criarTabuleiro(Tabuleiro) :-
    Tabuleiro = [
        ['\x1F533', '\x1F533', '\x1F533', '\x1F533'],
        ['\x1F533', '\x1F533', '\x1F533', '\x1F533'],
        ['\x1F533', '\x1F533', '\x1F533', '\x1F533'],
        ['\x1F533', '\x1F533', '\x1F533', '\x1F533']
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

