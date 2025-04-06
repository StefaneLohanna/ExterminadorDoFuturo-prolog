:- module(tabuleiro, [criarTabuleiro/1, exibirTabuleiros/3, iniciarTabuleiros/3, plantar/5,semente/1, arbusto/1, arvore/1, jogador1/1, jogador2/1, verificarPosicaoTabuleiro/4, moverPeca/7, existeJogador/2, removerPlanta/5, verificarVitoria/4, viagemNoTabuleiro/7, viagemNoTabuleiroClones/7, verificaPosicaoLivre/4, negado/1]).


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
    modificarMatriz(Linha, Coluna, Tabuleiro, Planta, NovoTabuleiro).

% Modifica uma posição na matriz
modificarMatriz(1, C, [Linha|Resto], NovoValor, [NovaLinha|Resto]) :-
    modificarLinha(C, Linha, NovoValor, NovaLinha).

modificarMatriz(L, C, [Linha|Resto], NovoValor, [Linha|NovoResto]) :-
    L > 1,
    L1 is L - 1,
    modificarMatriz(L1, C, Resto, NovoValor, NovoResto).

% Modifica uma posição na linha
modificarLinha(1, [_|Resto], NovoValor, [NovoValor|Resto]).
modificarLinha(C, [Elem|Resto], NovoValor, [Elem|NovoResto]) :-
    C > 1,
    C1 is C - 1,
    modificarLinha(C1, Resto, NovoValor, NovoResto).


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


removerPeca(Tabuleiro, Linha, Coluna, TabuleiroAtualizado) :-
    espacoVazio(Ev),
    nth1(Linha, Tabuleiro, LinhaLista),
    replace(Coluna, LinhaLista, Ev, NovaLinha),
    replace(Linha, Tabuleiro, NovaLinha, TabuleiroAtualizado).

removerPlanta(Tabuleiro, Planta, Linha, Coluna, TabuleiroAtualizado) :-
    % Verifica se a peça na posição é igual à planta especificada
    nth1(Linha, Tabuleiro, LinhaLista),
    nth1(Coluna, LinhaLista, PecaAtual),
    (PecaAtual == Planta ->
        % Se FOR a planta: remove (substitui por espaço vazio)
        espacoVazio(Ev),
        replace(Coluna, LinhaLista, Ev, NovaLinha),
        replace(Linha, Tabuleiro, NovaLinha, TabuleiroAtualizado)
    ;
        % Se NÃO FOR a planta: retorna o tabuleiro original sem alterações
        TabuleiroAtualizado = Tabuleiro
    ).

moverPeca(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado) :-
    espacoVazio(Ev),
    arbusto(Arbusto),
    semente(Semente),

    nth1(LinhaDestino, TabuleiroAntigo, LinhaDestinoLista),
    nth1(ColunaDestino, LinhaDestinoLista, ConteudoDestino),

    ( 
        ConteudoDestino == Ev ->  
        % Se a posição destino está vazia, movemos normalmente.
        moverSimples(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ; 
        ConteudoDestino == Peca ->
        removerPeca(TabuleiroAntigo, LinhaDestino, ColunaDestino, TabuleiroIntermediario),
        moverSimples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ;
        ConteudoDestino == Arbusto ->
        removerPeca(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, TabuleiroAtualizado)
    ;
        ConteudoDestino == Semente ->
        removerPeca(TabuleiroAntigo, LinhaDestino, ColunaDestino, TabuleiroSemSemente),
        moverSimples(TabuleiroSemSemente, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado)
    ;
        % Caso contrário, empurra a peça ocupante.
        empurrar(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, ConteudoDestino, TabuleiroAtualizado)
    ).

moverSimples(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Peca, TabuleiroAtualizado) :-
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


% Calcula a próxima posição na direção do empurrão, e verifica os limites do tabuleiro.
calculaPosicaoDestino(LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Tabuleiro, NovaLinha, NovaColuna) :-
    DirecaoLinha is LinhaDestino - LinhaOrigem,
    DirecaoColuna is ColunaDestino - ColunaOrigem,
    NovaLinha is LinhaDestino + DirecaoLinha,
    NovaColuna is ColunaDestino + DirecaoColuna,

    length(Tabuleiro, NumLinhas),
    nth1(1, Tabuleiro, PrimeiraLinha),
    length(PrimeiraLinha, NumColunas),

    % Garantir que as novas coordenadas estão dentro do tabuleiro
    NovaLinha >= 1, NovaLinha =< NumLinhas,
    NovaColuna >= 1, NovaColuna =< NumColunas.


empurrar(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, Empurrado, TabuleiroAtualizado) :- 
    espacoVazio(Ev),
    arbusto(Arbusto),
    arvore(Arvore),

    % Pega peça ocupante na posição de destino
    nth1(LinhaDestino, TabuleiroAntigo, LinhaDestinoLista),
    nth1(ColunaDestino, LinhaDestinoLista, PecaOcupante),

    (
        calculaPosicaoDestino(LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, TabuleiroAntigo, NovaLinhaOcupante, NovaColunaOcupante)
    ->
        nth1(NovaLinhaOcupante, TabuleiroAntigo, NovaLinhaLista),
        nth1(NovaColunaOcupante, NovaLinhaLista, ProximoConteudo),

        ( 
            (ProximoConteudo == Empurrado) ->
                paradoxo(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, Jogador, TabuleiroAtualizado)
        ; 
            (ProximoConteudo == Arbusto) ->
                mortePorArbusto(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)
        ;
            (ProximoConteudo == Arvore, Empurrado \= Arvore ) ->
                empurraArvore(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, Jogador, Empurrado, TabuleiroAtualizado)
        ;
            (Empurrado == Arvore)  ->
                arvoreCai(TabuleiroAntigo, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, Jogador, TabuleiroAtualizado)
        ;
            (ProximoConteudo == Ev) ->
                moverSimples(TabuleiroAntigo, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, PecaOcupante, TabuleiroIntermediario),
                moverSimples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)
        ;
            (ProximoConteudo \= Ev) ->
            empurrar(TabuleiroAntigo, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, PecaOcupante, ProximoConteudo, TabuleiroDepoisEmpurrar),
            moverSimples(TabuleiroDepoisEmpurrar, LinhaDestino, ColunaDestino, NovaLinhaOcupante, NovaColunaOcupante, PecaOcupante, TabuleiroIntermediario),
            moverSimples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)
        )
    ;
        writeln("Jogador morreu!"),
        removerPeca(TabuleiroAntigo, LinhaDestino, ColunaDestino, TabuleiroIntermediario),
        moverSimples(TabuleiroIntermediario, LinhaOrigem, ColunaOrigem, LinhaDestino, ColunaDestino, Jogador, TabuleiroAtualizado)
    ).


paradoxo(Tabuleiro, LinhaEmpurrador, ColunaEmpurrador, LinhaEmpurrado, ColunaEmpurrado, NovaLinha, NovaColuna, JogadorEmpurrador, TabuleiroAtualizado) :-
    espacoVazio(Ev),

    % Remove a peça empurrada
    removerPeca(Tabuleiro, LinhaEmpurrado, ColunaEmpurrado, TabuleiroSemEmpurrado),
    
    % Remove a peça da nova posição (paradoxo)
    removerPeca(TabuleiroSemEmpurrado, NovaLinha, NovaColuna, TabuleiroSemAmbos),
    
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
    removerPeca(Tabuleiro, LinhaEmpurrado, ColunaEmpurrado, TabuleiroSemEmpurrado),

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
    removerPeca(Tabuleiro, LinhaEmpurrado, ColunaEmpurrado, TabuleiroSemArvore),

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
            nth1(NovaLinha, TabuleiroMovido, LinhaDestinoAtual),
            replace(NovaColuna, LinhaDestinoAtual, Ev, LinhaAtualizada),
            replace(NovaLinha, TabuleiroMovido, LinhaAtualizada, TabuleiroAtualizado)
        ;
            TabuleiroAtualizado = TabuleiroMovido
    ).


empurraArvore(Tabuleiro, LinhaEmpurrador, ColunaEmpurrador, LinhaEmpurrado, ColunaEmpurrado,NovaLinha, NovaColuna, JogadorEmpurrador, JogadorEmpurrado, TabuleiroAtualizado) :- 
    espacoVazio(Ev),

    (
        calculaPosicaoDestino(LinhaEmpurrado, ColunaEmpurrado, NovaLinha, NovaColuna, Tabuleiro, LinhaArvoreCai, ColunaArvoreCai)
    ->
        nth1(LinhaArvoreCai, Tabuleiro, LinhaAlvo),
        replace(ColunaArvoreCai, LinhaAlvo, Ev, NovaLinhaAlvo),
        replace(LinhaArvoreCai, Tabuleiro, NovaLinhaAlvo, TabuleiroArvoreCaiAtualizado)
    ;
        TabuleiroArvoreCaiAtualizado = Tabuleiro
    ),

    % Remove a árvore do destino
    removerPeca(TabuleiroArvoreCaiAtualizado, NovaLinha, NovaColuna, TabuleiroSemArvore),

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


plantaCerta(Planta, Elemento) :- Planta = Elemento.

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


viagemNoTabuleiro(TabAtual, TabDestino, Linha, Coluna, Jogador, NovoTabAtual, NovoTabDestino) :-
/*
 * Viaja no tempo sem criar clones.
 * Move a peça do jogador da posição atual em TabAtual para a mesma posição em TabDestino.
 * Remove a peça da posição original e insere na nova.
 * 
 * @param TabAtual O tabuleiro atual (onde está a peça antes da viagem).
 * @param TabDestino O tabuleiro de destino (para onde a peça irá).
 * @param Linha A linha da posição da peça (1 a 4).
 * @param Coluna A coluna da posição da peça (1 a 4).
 * @param Jogador A peça do jogador.
 * @param NovoTabAtual Tabuleiro atualizado com a remoção da peça.
 * @param NovoTabDestino Tabuleiro de destino com a peça inserida.
 */
    espacoVazio(Ev),
    substituirCelula(TabAtual, Linha, Coluna, Ev, NovoTabAtual),
    substituirCelula(TabDestino, Linha, Coluna, Jogador, NovoTabDestino).

viagemNoTabuleiroClones(TabAtual, TabDestino, Linha, Coluna, Jogador, NovoTabAtual, NovoTabDestino) :-
/*
 * Viaja no tempo com criação de clone.
 * Apenas adiciona a peça na posição correspondente do tabuleiro de destino.
 * O tabuleiro atual permanece inalterado.
 * 
 * @param TabAtual O tabuleiro atual (permanece inalterado).
 * @param TabDestino O tabuleiro de destino (recebe a nova peça).
 * @param Linha A linha da posição da peça (1 a 4).
 * @param Coluna A coluna da posição da peça (1 a 4).
 * @param Jogador A peça do jogador.
 * @param NovoTabAtual Mesmo tabuleiro original (nenhuma alteração).
 * @param NovoTabDestino Tabuleiro de destino com a nova peça inserida.
 */
    NovoTabAtual = TabAtual,
    substituirCelula(TabDestino, Linha, Coluna, Jogador, NovoTabDestino).

substituirCelula(Tabuleiro, Linha1Base, Coluna1Base, NovoElem, NovoTabuleiro) :-
/*
 * Substitui um elemento em uma célula do tabuleiro (lista de listas).
 * 
 * @param Tabuleiro O tabuleiro original (lista de listas).
 * @param Linha1Base A linha (1 a 4) da célula a ser substituída.
 * @param Coluna1Base A coluna (1 a 4) da célula a ser substituída.
 * @param NovoElem O novo elemento que substituirá o antigo.
 * @param NovoTabuleiro O novo tabuleiro com a célula substituída.
 */ 
    LinhaIndex is Linha1Base - 1,
    ColunaIndex is Coluna1Base - 1,
    nth0(LinhaIndex, Tabuleiro, LinhaAtual),
    substituirEmLista(LinhaAtual, ColunaIndex, NovoElem, NovaLinha),
    substituirEmLista(Tabuleiro, LinhaIndex, NovaLinha, NovoTabuleiro).


substituirEmLista([_|T], 0, Elem, [Elem|T]).
/*
 * Caso base: substitui o elemento na posição 0 da lista.
 */
substituirEmLista([H|T], Index, Elem, [H|Resto]) :-
/*
 * Caso recursivo: percorre a lista até alcançar o índice desejado.
 * 
 * @param [H|T] Lista original.
 * @param Index Índice do elemento a ser substituído.
 * @param Elem Novo elemento a ser inserido.
 * @param [H|Resto] Lista resultante com substituição feita.
 */
    Index > 0,
    Index1 is Index - 1,
    substituirEmLista(T, Index1, Elem, Resto).

% Verifica se o jogador atual venceu o jogo
verificarVitoria(Passado, Presente, Futuro, JogadorAtual) :-
    % Determina o adversário
    jogador1(Jogador1),
    jogador2(Jogador2),
    (JogadorAtual = Jogador1 -> Adversario = Jogador2 ; Adversario = Jogador1),
    
    % Verifica em quais tabuleiros o adversário está presente
    (existeJogador(Passado, Adversario) -> PresenteNoPassado = 1 ; PresenteNoPassado = 0),
    (existeJogador(Presente, Adversario) -> PresenteNoPresente = 1 ; PresenteNoPresente = 0),
    (existeJogador(Futuro, Adversario) -> PresenteNoFuturo = 1 ; PresenteNoFuturo = 0),

    % Soma o total de tabuleiros onde o adversário está presente
    Soma is PresenteNoPassado + PresenteNoPresente + PresenteNoFuturo,

    % Se o adversário está em apenas um tabuleiro, o jogador atual vence
    Soma =:= 1.

verificaPosicaoLivre(Tabuleiro, Linha, Coluna, true) :-
    espacoVazio(Ev),
    nth1(Linha, Tabuleiro, LinhaTab),
    nth1(Coluna, LinhaTab, Casa),
    Casa == Ev.

verificaPosicaoLivre(Tabuleiro, Linha, Coluna, false) :-
    espacoVazio(Ev),
    nth1(Linha, Tabuleiro, LinhaTab),
    nth1(Coluna, LinhaTab, Casa),
    Casa \== Ev.
