:- module(tabuleiro, [criarTabuleiro/1, exibirTabuleiros/3, iniciarTabuleiros/3, modificar_matriz/5, plantar/5,semente/1, arbusto/1, arvore/1, jogador1/1, jogador2/1, verificarPosicaoTabuleiro/4, moverPeca/7, existeJogador/2]).

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

/*
 * Cria o tabuleiro já posicionando os jogadores nas extremidades.
 *
 * @param Tabuleiro tabuleiro a ser criado.
 * @return          O tabuleiro gerado.
 */
criarTabuleiro(Tabuleiro) :-
    espacoVazio(Ev),
    jogador1(J1),
    jogador2(J2),
    Tabuleiro = [
        [J1, Ev, Ev, Ev],
        [Ev, Ev, Ev, Ev],
        [Ev, Ev, Ev, Ev],
        [Ev, Ev, Ev, J2]].


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


replace(Index, List, NewElement, NewList) :-
    /* Troca os elementos.
    */ 
    nth1(Index, List, _, Rest),
    nth1(Index, NewList, NewElement, Rest).


/*moverPeca(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado) :-
    Move a Peca com base na linha e a coluna.
    Se a posição destino já estiver ocupada por outra peça, essa peça será movida para uma casa adjacente.
    
    espacoVazio(Ev),
    nth1(LinhaDestino, TabuleiroAntigo, LinhaDestinoLista),
    nth1(ColunaDestino, LinhaDestinoLista, ConteudoDestino),

    ( ConteudoDestino == Ev ->  
        % Se a posição destino está vazia, movemos normalmente.
        mover_simples(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ; 
        % Caso contrário, empurramos a peça ocupante.
        empurrar(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ).*/

remover_peca(Tabuleiro, Linha, Coluna, TabuleiroAtualizado) :-
    espacoVazio(Ev),
    nth1(Linha, Tabuleiro, LinhaLista),
    replace(Coluna, LinhaLista, Ev, NovaLinha),
    replace(Linha, Tabuleiro, NovaLinha, TabuleiroAtualizado).


moverPeca(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado) :-
   
    espacoVazio(Ev),
    arbusto(Arbusto),
    nth1(LinhaDestino, TabuleiroAntigo, LinhaDestinoLista),
    nth1(ColunaDestino, LinhaDestinoLista, ConteudoDestino),

    ( 
        ConteudoDestino == Ev ->  
        % Se a posição destino está vazia, movemos normalmente.
        mover_simples(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ; 
        ConteudoDestino == Peca ->
        remover_peca(TabuleiroAntigo, LinhaDestino, ColunaDestino, TabuleiroIntermediario),
        mover_simples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ;
        ConteudoDestino == Arbusto ->
        remover_peca(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, TabuleiroAtualizado)
    ;
        % Caso contrário, empurramos a peça ocupante.
        empurrar(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, ConteudoDestino, TabuleiroAtualizado)
    ).

mover_simples(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado) :-
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


empurrar(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, Empurrado, TabuleiroAtualizado) :- 
    espacoVazio(Ev),
    arbusto(Arbusto),
    arvore(Arvore),

    % Pegamos a peça ocupante na posição de destino
    nth1(LinhaDestino, TabuleiroAntigo, LinhaDestinoLista),
    nth1(ColunaDestino, LinhaDestinoLista, PecaOcupante),

    % Determinar a direção do movimento
    DirecaoLinha is LinhaDestino - LinhaOrigem,
    DirecaoColuna is ColunaDestino - ColunaOrigem,
    NovaLinhaOcupante is LinhaDestino + DirecaoLinha,
    NovaColunaOcupante is ColunaDestino + DirecaoColuna,

    % Verificar os limites do tabuleiro
    length(TabuleiroAntigo, NumLinhas),
    nth1(1, TabuleiroAntigo, PrimeiraLinha),
    length(PrimeiraLinha, NumColunas),

    (   
        % Caso: o empurrado sai do tabuleiro e morre
        (NovaLinhaOcupante < 1 ; NovaLinhaOcupante > NumLinhas ; 
         NovaColunaOcupante < 1 ; NovaColunaOcupante > NumColunas) ->
        writeln("Jogador morreu!"),
        remover_peca(TabuleiroAntigo, LinhaDestino, ColunaDestino, TabuleiroIntermediario),
        mover_simples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)
    ;
        % Se a nova posição está dentro dos limites
        nth1(NovaLinhaOcupante, TabuleiroAntigo, NovaLinhaLista),
        nth1(NovaColunaOcupante, NovaLinhaLista, ProximoConteudo),

        ( 
            % Caso: Paradoxo 
            (ProximoConteudo == Empurrado) ->
            paradoxo(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, Jogador, TabuleiroAtualizado)
        ; 
        
            (ProximoConteudo == Arbusto) ->
            mortePorArbusto(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)

        ;
            (ProximoConteudo == Arvore, Empurrado \= Arvore ) ->
            empurraArvore(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, Jogador, Empurrado, TabuleiroAtualizado) 
        ;

            Empurrado == Arvore  ->
            arvoreCai(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, Jogador, TabuleiroAtualizado) 

        ;
            % Caso: Casa vazia, empurra normalmente
            (ProximoConteudo == Ev) ->
            mover_simples(TabuleiroAntigo, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, PecaOcupante, TabuleiroIntermediario),
            mover_simples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)
        ;
            % Caso: Empurrão em cadeia
            (ProximoConteudo \= Ev)->
            empurrar(TabuleiroAntigo, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, PecaOcupante, Empurrado, NovoTabuleiro),
            mover_simples(NovoTabuleiro, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)

        )
    ).



paradoxo(Tabuleiro, LinhaEmpurrador, ColunaEmpurrador, LinhaEmpurrado, ColunaEmpurrado, NovaLinha, NovaColuna, JogadorEmpurrador, TabuleiroAtualizado) :-
    espacoVazio(Ev),

    % Remove a peça empurrada
    remover_peca(Tabuleiro, LinhaEmpurrado, ColunaEmpurrado, TabuleiroSemEmpurrado),
    
    % Remove a peça da nova posição (paradoxo)
    remover_peca(TabuleiroSemEmpurrado, NovaLinha, NovaColuna, TabuleiroSemAmbos),
    
    % Atualiza a linha onde estava o empurrado com o empurrador
    nth1(LinhaEmpurrado, TabuleiroSemAmbos, LinhaEmpurradoLista),
    replace(ColunaEmpurrado, LinhaEmpurradoLista, JogadorEmpurrador, NovaLinhaEmpurrado),
    replace(LinhaEmpurrado, TabuleiroSemAmbos, NovaLinhaEmpurrado, TabuleiroComEmpurrador),
    
    % Limpa a posição original do empurrador
    nth1(LinhaEmpurrador, TabuleiroComEmpurrador, LinhaEmpurradorLista),
    replace(ColunaEmpurrador, LinhaEmpurradorLista, Ev, NovaLinhaEmpurrador),
    replace(LinhaEmpurrador, TabuleiroComEmpurrador, NovaLinhaEmpurrador, TabuleiroAtualizado).


mortePorArbusto(Tabuleiro, LinhaEmpurrador, ColunaEmpurrador, LinhaEmpurrado, ColunaEmpurrado, JogadorEmpurrador, TabuleiroAtualizado) :-
    espacoVazio(Ev),
    % Remove a peça empurrada
    remover_peca(Tabuleiro, LinhaEmpurrado, ColunaEmpurrado, TabuleiroSemEmpurrado),

    % Atualiza a linha onde estava o empurrado com o empurrador
    nth1(LinhaEmpurrado, TabuleiroSemEmpurrado, LinhaEmpurradoLista),
    replace(ColunaEmpurrado, LinhaEmpurradoLista, JogadorEmpurrador, NovaLinhaEmpurrado),
    replace(LinhaEmpurrado, TabuleiroSemEmpurrado, NovaLinhaEmpurrado, TabuleiroComEmpurrador),
    
    % Limpa a posição original do empurrador
    nth1(LinhaEmpurrador, TabuleiroComEmpurrador, LinhaEmpurradorLista),
    replace(ColunaEmpurrador, LinhaEmpurradorLista, Ev, NovaLinhaEmpurrador),
    replace(LinhaEmpurrador, TabuleiroComEmpurrador, NovaLinhaEmpurrador, TabuleiroAtualizado).


arvoreCai(Tabuleiro, LinhaEmpurrador, ColunaEmpurrador, LinhaEmpurrado, ColunaEmpurrado, NovaLinha, NovaColuna, JogadorEmpurrador, TabuleiroAtualizado) :-
    espacoVazio(Ev),
    arbusto(Arbusto),
    semente(Semente),

    % Remove a árvore da posição original
    remover_peca(Tabuleiro, LinhaEmpurrado, ColunaEmpurrado, TabuleiroSemArvore),

    % Move o empurrador para a posição onde estava a árvore
    nth1(LinhaEmpurrado, TabuleiroSemArvore, LinhaEmpurradoLista),
    replace(ColunaEmpurrado, LinhaEmpurradoLista, JogadorEmpurrador, NovaLinhaEmpurrado),
    replace(LinhaEmpurrado, TabuleiroSemArvore, NovaLinhaEmpurrado, TabuleiroComEmpurrador),


    % Limpa a posição original do empurrador
    nth1(LinhaEmpurrador, TabuleiroComEmpurrador, LinhaEmpurradorVazia),
    replace(ColunaEmpurrador, LinhaEmpurradorVazia, Ev, NovaLinhaEmpurradorVazia),
    replace(LinhaEmpurrador, TabuleiroComEmpurrador, NovaLinhaEmpurradorVazia, TabuleiroMovido),

    % Verifica o conteúdo da nova posição (onde a árvore cairia)
    nth1(NovaLinha, Tabuleiro, LinhaDestinoLista),
    nth1(NovaColuna, LinhaDestinoLista, ConteudoNovo),

    (
        (ConteudoNovo \= Ev, ConteudoNovo \= Arbusto, ConteudoNovo \= Semente) ->
        % Há um jogador na posição, ele é eliminado (árvore caiu sobre ele)
        nth1(NovaLinha, TabuleiroMovido, LinhaDestinoAtual),
        replace(NovaColuna, LinhaDestinoAtual, Ev, LinhaAtualizada),
        replace(NovaLinha, TabuleiroMovido, LinhaAtualizada, TabuleiroAtualizado)
    ;
        % Caso contrário, mantém como está
        TabuleiroAtualizado = TabuleiroMovido
    ).

    
empurraArvore(Tabuleiro, LinhaEmpurrador, ColunaEmpurrador, LinhaEmpurrado, ColunaEmpurrado, NovaLinha, NovaColuna, JogadorEmpurrador, JogadorEmpurrado, TabuleiroAtualizado) :-
    espacoVazio(Ev),

    % Remove a árvore do destino
    remover_peca(Tabuleiro, NovaLinha, NovaColuna, TabuleiroSemArvore),

    % Move o empurrado para a posição onde estava a árvore
    nth1(NovaLinha, TabuleiroSemArvore, LinhaEmpurradoLista),
    replace(NovaColuna, LinhaEmpurradoLista, JogadorEmpurrado, NovaLinhaEmpurrado),
    replace(NovaLinha, TabuleiroSemArvore, NovaLinhaEmpurrado, TabuleiroComEmpurrado),

    % Move o empurrador para a posição onde estava o empurrado
    nth1(LinhaEmpurrado, TabuleiroComEmpurrado, LinhaEmpurradorLista),
    replace(ColunaEmpurrado, LinhaEmpurradorLista, JogadorEmpurrador, NovaLinhaEmpurrador),
    replace(LinhaEmpurrado, TabuleiroComEmpurrado, NovaLinhaEmpurrador, TabuleiroComJogadores),

    % Limpa a posição original do empurrador
    nth1(LinhaEmpurrador, TabuleiroComJogadores, LinhaEmpurradorVazia),
    replace(ColunaEmpurrador, LinhaEmpurradorVazia, Ev, NovaLinhaEmpurradorVazia),
    replace(LinhaEmpurrador, TabuleiroComJogadores, NovaLinhaEmpurradorVazia, TabuleiroAtualizado).

planta_certa(Planta, Elemento) :- Planta = Elemento.

/*
 * Verifica se um jogador está presente no tabuleiro.
 *
 * @param Tabuleiro O tabuleiro 4x4.
 * @param Jogador   O símbolo do jogador a ser verificado.
 * @return          Verdadeiro se o jogador estiver presente no tabuleiro, falso caso contrário.
 */
existeJogador(Tabuleiro, Jogador) :-
    member(Linha, Tabuleiro),
    member(Jogador, Linha).
