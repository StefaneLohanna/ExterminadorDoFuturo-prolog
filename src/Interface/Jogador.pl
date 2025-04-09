:- module(jogador, [definirFoco/6, focoValido/5, obterLinha/1, obterColuna/1, escolherJogada/1, obtemCoordenadasValidas/4, obtemCoordenadasOrigemValidas/4, escolherTempo/1, stringParaFoco/2, removerEspacos/2,escolherJogada/1,escolherOpcaoMenu/1,exibirOpcaoMenu/1, escolherModoDeJogo/1, exibirHistoria/0, exibirDelimitadorFinal/0,exibirDelimitadorInicial/0,exibirFimDeJogo/0, escolherMovimento/4]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Utils/ImprimirTxt.pl').
:- use_module('./src/Utils/Ranking.pl').

/*
 * Define o foco do jogador para a próxima rodada.
 *
 * @param Jogador    O símbolo do jogador que está escolhendo o foco.
 * @param Passado    O tabuleiro representando o estado atual do passado.
 * @param Presente   O tabuleiro representando o estado atual do presente.
 * @param Futuro     O tabuleiro representando o estado atual do futuro.
 * @param FocoAtual  O foco atual do jogador.
 * @param NovoFoco   O novo foco definido pelo jogador.
 */
definirFoco(Jogador, Passado, Presente, Futuro, FocoAtual, NovoFoco) :-
    exibirMenuFoco,
    read_line_to_string(user_input, Entrada),
    string_lower(Entrada, Lower),
    removerEspacos(Lower, EntradaSemEspaco),
    atom_string(Escolha, EntradaSemEspaco),
    (   traduzirEscolha(Escolha, FocoTentativa) ->
        (   focoValido(FocoTentativa, Jogador, Passado, Presente, Futuro) ->
            NovoFoco = FocoTentativa,
            format('~w definiu foco para: ~w~n', [Jogador, NovoFoco])
        ;
            writeln('Jogador não encontrado nesse tempo. Escolha outro foco.'),
            definirFoco(Jogador, Passado, Presente, Futuro, FocoAtual, NovoFoco)
        )
    ;   definirFoco(Jogador, Passado, Presente, Futuro, FocoAtual, NovoFoco)
    ).


/*
 * Traduz a escolha do jogador para o respectivo foco.
 *
 * @param Escolha  Entrada do jogador ('s', 'p' ou 'f').
 * @param Foco     O foco correspondente (passado, presente ou futuro).
 */
traduzirEscolha('s', passado).
traduzirEscolha('p', presente).
traduzirEscolha('f', futuro).
traduzirEscolha(_, _) :- 
    negado(Negado),
    format("~w Opção inválida! Use s, p ou f.~n", [Negado]),
    %writeln('Opção inválida! Use s, p ou f.'),
    fail.

/*
 * Verifica se o foco escolhido pelo jogador é válido, ou seja, se o jogador está presente no tabuleiro correspondente.
 *
 * @param Foco     O foco escolhido (passado, presente ou futuro).
 * @param Jogador  O símbolo do jogador.
 * @param Passado  O tabuleiro representando o estado atual do passado.
 * @param Presente O tabuleiro representando o estado atual do presente.
 * @param Futuro   O tabuleiro representando o estado atual do futuro.
 * @return         Verdadeiro se o jogador estiver presente no tabuleiro correspondente, falso caso contrário.
 */
focoValido(passado, Jogador, Passado, _, _) :- 
    existeJogador(Passado, Jogador).
focoValido(presente, Jogador, _, Presente, _) :- 
    existeJogador(Presente, Jogador).
focoValido(futuro, Jogador, _, _, Futuro) :- 
    existeJogador(Futuro, Jogador).

/*
 * Solicita ao jogador que escolha uma ação.
 *
 * @return Escolha  A ação escolhida pelo jogador: 
 *                  'm' para movimentar,
 *                  'p' para plantar,
 *                  'v' para viajar no tempo,
 *                  'r' para reiniciar o jogo.
 */
escolherJogada(Escolha) :-
    exibirMenuJogadas,
    negado(Negado),
    read_line_to_string(user_input, Entrada),
    string_lower(Entrada, Lower),
    removerEspacos(Lower, EntradaSemEspaco),
    atom_string(EscolhaConvertida, EntradaSemEspaco),
    (
        member(EscolhaConvertida, [m, p, v, r]) ->
            Escolha = EscolhaConvertida
        ;
            format("~w Entrada inválida! Tente novamente.~n", [Negado]),
            %writeln("Entrada inválida! Tente novamente."),
            escolherJogada(Escolha)
    ).


exibirHistoria :-
/*
 * Exibe o contexto do jogo.
 */
    imprimirTxt('src/Interface/textosDeExibicao/exterminadorDoFuturo.txt').



exibirMenuJogadas :-
/*
 * Exibe o menu de jogadas que um jogador pode realizar.
 */
    imprimirTxt('src/Interface/menus/jogadas.txt').


exibirMenuFoco :-
/*
 * Exibe o menu de escolha de foco para o jogador.
 */
    imprimirTxt('src/Interface/menus/foco.txt').

exibirModoJogo :-
/*
 * Exibe o menu de modos de jogar.
 */
    imprimirTxt('src/Interface/textosDeExibicao/modoDeJogo.txt').

exibirDetalhamento :-
/*
 * Exibe o menu do detalhamento de jogo.
 */
    imprimirTxt('src/Interface/textosDeExibicao/detalhamentoJogo.txt').

exibirMenu :-
/*
 * Exibe o menu do jogo.
 */
    imprimirTxt('src/Interface/menus/menu.txt').

exibirDelimitadorFinal :-
/*
 * Exibe o delimitador final.
 */
 imprimirTxt('src/Interface/textosDeExibicao/delimitadorFinal.txt').

exibirDelimitadorInicial :-
/*
 * Exibe o delimitador inicial.
 */
 imprimirTxt('src/Interface/textosDeExibicao/delimitadorInicial.txt').

exibirFimDeJogo :-
/*
 * Exibe o texto de fim de jogo.
 */
 imprimirTxt('src/Interface/textosDeExibicao/fimDeJogo.txt').


escolherOpcaoMenu(EscolhaFinal) :-
    exibirMenu,
    negado(Negado),
    read_line_to_string(user_input, Entrada),
    string_lower(Entrada, Lower),
    removerEspacos(Lower, EntradaSemEspaco),
    atom_string(Escolha, EntradaSemEspaco),
    (
        member(Escolha, [d, s, m, j, r]) ->
            EscolhaFinal = Escolha 
        ;
        format("~w Entrada inválida!~n", [Negado]),
        escolherOpcaoMenu(EscolhaFinal)
    ).


exibirOpcaoMenu(Opcao) :-
    ( 
        Opcao == r ->
        mostrarRanking
    ;   

        Opcao == d ->
        exibirDetalhamento
    ;
        Opcao == m ->
        exibirModoJogo
    ;
        Opcao == j ->
        true
    ;
        Opcao == s ->
        true
    
    ).

/*
 * Obtém coordenadas válidas em que o jogador está posicionado no tabuleiro.
 *
 * @param Tabuleiro O tabuleiro atual (passado, presente ou futuro).
 * @param Jogador   O símbolo do jogador.
 * @return Linha    A linha em que o jogador está localizado.
 * @return Coluna   A coluna em que o jogador está localizado.
 */
obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna) :-
    obterLinha(L),
    obterColuna(C),
    (
        verificarPosicaoTabuleiro(Tabuleiro, L, C, Jogador) ->
            Linha = L, Coluna = C
    ;
        format("Posição inválida! Insira uma posição em que a sua peça (~w) se encontre.\n", [Jogador]),
        obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna)
    ).

/*
 * Obtém coordenadas do jogador.
 *
 * @return Linha  A linha inserida pelo jogador.
 * @return Coluna A coluna inserida pelo jogador.
 */
/*obtemCoordenadas(Linha, Coluna) :-
    obterLinha(Linha),
    obterColuna(Coluna).*/


/*
 * Solicita ao jogador uma linha válida dentro dos limites do tabuleiro.
 *
 * @return Linha  A linha escolhida (de 1 a 4).
 */
% obterLinha(Linha) :-
%     write("Informe a linha (1-4): "), read(L),
%     ( integer(L), L >= 1, L =< 4 ->
%         Linha = L
%     ;
%         %writeln("Linha inválida! Escolha um valor entre 1 e 4."),
%         obterLinha(Linha) % Repete até obter um valor válido
%     ).

 
obterLinha(Linha) :-
    negado(Negado),
    write("Informe a linha (1-4): "),
    read_line_to_codes(user_input, Codes),
    string_codes(String, Codes),
    normalize_space(string(EntradaSemEspaco), String),
    ( number_string(Numero, EntradaSemEspaco),
      integer(Numero), Numero >= 1, Numero =< 4 ->
        Linha = Numero
    ;
        format("~w Linha inválida! Escolha um valor entre 1 e 4.~n", [Negado]),
         %writeln("Linha inválida! Escolha um valor entre 1 e 4."),
        obterLinha(Linha)  % Repete até obter um valor válido
    ).



/*
 * Solicita ao jogador uma coluna válida dentro dos limites do tabuleiro.
 *
 * @return Coluna  A coluna escolhida (de 1 a 4).
 */
obterColuna(Coluna) :-
    negado(Negado),
    write("Informe a coluna (1-4): "),
    read_line_to_codes(user_input, Codes),
    string_codes(String, Codes),
    normalize_space(string(EntradaSemEspaco), String),
    ( number_string(Numero, EntradaSemEspaco),
      integer(Numero), Numero >= 1, Numero =< 4 ->
        Coluna = Numero
    ;
        format("~w Coluna inválida! Escolha um valor entre 1 e 4.~n", [Negado]),
        %writeln("Coluna inválida! Escolha um valor entre 1 e 4."),
        obterColuna(Coluna)  % Repete até obter um valor válido
    ).


escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna) :-
    /* Pergunta ao jogador para onde deseja mover e calcula a nova posição, garantindo que esteja nos limites.
        
    Args:
        Linha: linha atual da peça.
        Coluna: coluna atual da peça.
        NovaLinha: linha após o movimento.
        NovaColuna: coluna após o movimento.

    Returns: a nova coordenada para que o jogador se mova.
    */
    lerDirecaoValida(Direcao),
    ( Direcao == w -> TempLinha is Linha - 1, TempColuna is Coluna
    ; Direcao == a -> TempLinha is Linha,     TempColuna is Coluna - 1
    ; Direcao == s -> TempLinha is Linha + 1, TempColuna is Coluna
    ; Direcao == d -> TempLinha is Linha,     TempColuna is Coluna + 1
    ),
    ( TempLinha >= 1, TempLinha =< 4, TempColuna >= 1, TempColuna =< 4 ->
        NovaLinha = TempLinha, NovaColuna = TempColuna
    ;
        negado(Negado),
        format("~w Movimento inválido! Escolha uma direção que mantenha a peça dentro dos limites do tabuleiro.", [Negado]),
        escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna)
    ).

lerDirecaoValida(Direcao) :-
    negado(Negado),
    writeln("Para onde deseja mover? (w = cima, a = esquerda, s = baixo, d = direita):"),
    read_line_to_string(user_input, Entrada),
    string_lower(Entrada, Lower),
    removerEspacos(Lower, EntradaSemEspaco),
    atom_string(DirecaoAtom, EntradaSemEspaco),
    ( member(DirecaoAtom, [w, a, s, d]) ->
        Direcao = DirecaoAtom
    ;
        format("~w Direção inválida! Use apenas w, a, s ou d.", [Negado]),
        lerDirecaoValida(Direcao)
    ).
    
/*
 * Obtém coordenadas de origem válidas para o jogador, ou seja,
 * verifica se existe uma peça do jogador naquela posição.
 *
 * @param Tabuleiro O tabuleiro atual (passado, presente ou futuro).
 * @param Jogador   O símbolo do jogador.
 * @param Linha     A linha onde a peça do jogador está.
 * @param Coluna    A coluna onde a peça do jogador está.
 */
obtemCoordenadasOrigemValidas(Tabuleiro, Jogador, Linha, Coluna) :-
    obterLinha(L),
    obterColuna(C),
    (
        verificarPosicaoTabuleiro(Tabuleiro, L, C, Jogador) ->
            Linha = L, Coluna = C
    ;
        format("Não há peça sua na posição (~d, ~d). Tente novamente.\n", [L, C]),
        obtemCoordenadasOrigemValidas(Tabuleiro, Jogador, Linha, Coluna)
    ).


exibirMenuViagem :-
    imprimirTxt('src/Interface/menus/viagem.txt').

escolherTempo(TempoEscolhido) :-
    /*writeln("Escolha para qual tempo deseja viajar:"),
    writeln("(s) passado"),
    writeln("(p) presente"),
    writeln("(f) futuro"),
    read(Entrada),*/
    exibirMenuViagem,
    read_line_to_string(user_input, Escolha),
    string_lower(Escolha, Lower),
    removerEspacos(Lower, EntradaSemEspaco),
    atom_string(Entrada, EntradaSemEspaco),

    ( Entrada == s -> TempoEscolhido = "passado"
    ; Entrada == p -> TempoEscolhido = "presente"
    ; Entrada == f -> TempoEscolhido = "futuro"
    ; writeln("Opção inválida, tente novamente."), escolherTempo(TempoEscolhido)
    ).

stringParaFoco("passado", passado).
stringParaFoco("presente", presente).
stringParaFoco("futuro", futuro).

removerEspacos(Str, SemEspacos) :-
    split_string(Str, " ", "", Lista),
    atomic_list_concat(Lista, "", SemEspacos).

escolherModoDeJogo(Escolha):-
        imprimirTxt('src/Interface/menus/escolherModoDeJogo.txt'),
        read_line_to_string(user_input, Entrada),
        string_lower(Entrada, Lower),
        removerEspacos(Lower, EntradaLimpa),
        atom_string(EscolhaConvertida, EntradaLimpa),
        (
            member(EscolhaConvertida, [a, d]) ->
                Escolha = EscolhaConvertida
            ;
                writeln("Entrada inválida! Tente novamente."),
                escolherModoDeJogo(Escolha)
        ).
