:- module(jogar, [iniciarJogo/0]).

:- dynamic jogador1_nome/1.
:- dynamic jogador2_nome/1.

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Jogo/Movimento.pl').
:- use_module('./src/Jogo/ControllerPlantas.pl').
:- use_module('./src/Interface/Jogador.pl').
:- use_module('./src/Utils/Ranking.pl').
:- use_module('./src/Jogo/ViagemNoTempo.pl').

:- use_module(library(readutil)). % para ler linha completa com read_line_to_string
:- use_module(library(apply)).   % para maplist
:- use_module(library(strings)). % para string_lower (se necessário)

espacoVazio(_Ev).
iniciarJogo :-
    /* Função temporária para iniciar o jogo, atualmente só inica o tabuleiro e faz a rodada.
    */
    escolherOpcaoMenu(OpcaoMenu),
    exibirOpcaoMenu(OpcaoMenu),
    (
        OpcaoMenu == j ->
        registrarJogadores,
        jogador1_nome(N1),
        jogador1(J1),
        iniciarTabuleiros(Passado, Presente, Futuro),
        rodada(J1, N1, passado, futuro, 0, 0, Passado, Presente, Futuro)  
        ;
        OpcaoMenu == s ->
        halt
        ;
        iniciarJogo
    ).

formatar(Entrada, Saida) :-
/*
Converte para minúsculo e remove os espaços em branco

@param Entrada String original a ser formatada
@param Saida Resultado da formatação da string
*/
    string_lower(Entrada, Lower),
    string_chars(Lower, Chars),
    exclude(=( ' ' ), Chars, SemEspacos),
    string_chars(Saida, SemEspacos).

registrarJogadores :-
/* 
Realiza o registro dos jogadores
*/
    jogador1(J1),
    jogador2(J2),

    % Remove nomes antigos se existirem
    retractall(jogador1_nome(_)),
    retractall(jogador2_nome(_)),

    writeln("\nRegistro do jogadores!"),
    write("Jogador 1, digite seu nome: "),
    read_line_to_string(user_input, Nome1),
    formatar(Nome1, Nome1Min),
    assertz(jogador1_nome(Nome1Min)), %armazena os nomes como fatos dinamicos (jogador1_nome, jogador2_nome)
    format("Seu personagem será a ~w~n", [J1]),

    write("Jogador 2, digite seu nome: "),
    read_line_to_string(user_input, Nome2),
    formatar(Nome2, Nome2Min),
    assertz(jogador2_nome(Nome2Min)),
    format("Jogador 2 ficará com o ~w~n", [J2]).

registrarJogadorUunico :-
/* 
Realiza o registro do jogador que jogará contra o bot
*/
    jogador1(J1),

    % Remove nome antigo
    retractall(jogador1_nome(_)),

    writeln("\nRegistro do jogador!"),
    write("Digite seu nome: "),
    read_line_to_string(user_input, Nome),
    formatar(Nome, NomeMin),
    assertz(jogador1_nome(NomeMin)),
    format("Seu personagem será o ~w~n", [J1]).

/*
 * Controla a rodada do jogo, permitindo que cada jogador realize duas jogadas antes de alternar.
 *
 * @param Peca       Peça do jogador atual.
 * @param FocoJ1     Foco do jogador 1.
 * @param FocoJ2     Foco do jogador 2.
 * @param Passado    O tabuleiro representando o estado atual do passado.
 * @param Presente   O tabuleiro representando o estado atual do presente.
 * @param Futuro     O tabuleiro representando o estado atual do futuro.
 */

rodada(Peca, _, FocoJ1, FocoJ2, ClonesJ1, ClonesJ2, Passado, Presente, Futuro) :-
    jogador1(J1),
    jogador2(J2),
    exclamacao(Exclamacao),
    %% Usa o predicado auxiliar para obter o nome correto
    nomeDoJogador(Peca, Nome),
    format("Vez do jogador: ~w (~w)~n", [Nome, Peca]),

    (Peca == J1 ->
        (FocoAtual = FocoJ1, CloneAtual = ClonesJ1)
    ;
        (FocoAtual = FocoJ2, CloneAtual = ClonesJ2)
    ),
            

    % Verifica se o jogador está presente no foco atual
    (focoValido(FocoAtual, Peca, Passado, Presente, Futuro) -> NovoFoco = FocoAtual;
        format("~w Jogador não está presente nesse foco. Escolha outro. ~n", [Exclamacao]),
        %writeln('Jogador não está presente nesse foco. Escolha outro.'),
        definirFoco(Peca, Passado, Presente, Futuro, FocoAtual, NovoFoco)
    ),

    % Primeira jogada
    jogar(NovoFoco, Peca, CloneAtual, Passado, Presente, Futuro, NovoPassado1, NovoPresente1, NovoFuturo1, NovoClone1, NovoFoco1),

    % Verifica vitória após a primeira jogada
    (verificarVitoria(NovoPassado1, NovoPresente1, NovoFuturo1, Peca) ->
        exibirTabuleiros(NovoPassado1, NovoPresente1, NovoFuturo1),
        finalizarJogo(Nome)
    ;
        true
    ),

    % Segunda jogada
    jogar(NovoFoco1, Peca, NovoClone1, NovoPassado1, NovoPresente1, NovoFuturo1, NovoPassado2, NovoPresente2, NovoFuturo2, NovoClone2, _),

    % Verifica vitória após a segunda jogada
    (verificarVitoria(NovoPassado2, NovoPresente2, NovoFuturo2, Peca) ->
        exibirTabuleiros(NovoPassado2, NovoPresente2, NovoFuturo2),
        finalizarJogo(Nome)
    ;
        true
    ),
    
    exibirTabuleiros(NovoPassado2, NovoPresente2, NovoFuturo2),
    
    % Define o foco para a próxima rodada
    (
        Peca == J1 -> 
        definirFoco(J1, NovoPassado2, NovoPresente2, NovoFuturo2, NovoFoco, NovoFocoJ1),
        NovoFocoJ2 = FocoJ2,
        NovaPeca = J2, 
        NovosClonesJ1 = NovoClone2,
        NovosClonesJ2 = ClonesJ2,
        jogador2_nome(NovoNome)
        ;
        definirFoco(J2, NovoPassado2, NovoPresente2, NovoFuturo2, NovoFoco, NovoFocoJ2),
        NovoFocoJ1 = FocoJ1,
        NovaPeca = J1,
        NovosClonesJ1 = ClonesJ1, 
        NovosClonesJ2 = NovoClone2,
        jogador1_nome(NovoNome)
    ),
    writeln("Mudando para o próximo jogador."),
    % Alterna para o outro jogador (mantendo os focos atualizados)
    rodada(NovaPeca, NovoNome, NovoFocoJ1, NovoFocoJ2, NovosClonesJ1, NovosClonesJ2, NovoPassado2, NovoPresente2, NovoFuturo2).

jogar(Foco, Jogador, Clones, Passado, Presente, Futuro, NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco) :-
    exibirTabuleiros(Passado, Presente, Futuro),
    negado(Negado),
    escolherJogada(Escolha),


    ( Foco == passado -> Tabuleiro = Passado
    ; Foco == presente -> Tabuleiro = Presente
    ; Foco == futuro -> Tabuleiro = Futuro
    ),

    ( Escolha == 'm' ->
        repeat,
        obtemCoordenadasValidas(Tabuleiro, Jogador, Linha, Coluna),
        escolherMovimento(Linha, Coluna, NovaLinha, NovaColuna),
        movimento(Tabuleiro, Passado, Presente, Futuro, Foco, Linha, Coluna, NovaLinha, NovaColuna, Jogador, NovoPassado, NovoPresente, NovoFuturo),
        NovoClones = Clones,
        NovoFoco = Foco  

    ; Escolha == 'p' ->
        obterLinha(Linha),
        obterColuna(Coluna),
        plantarSemente(Passado, Presente, Futuro, Foco, Linha, Coluna, NovoPassado, NovoPresente, NovoFuturo),
        NovoClones = Clones,
        NovoFoco = Foco

    ; Escolha == 'v' ->
    obtemCoordenadasOrigemValidas(Tabuleiro, Jogador, Linha, Coluna),
    escolherTempo(TempoEscolhido),
    defineViagem(Foco, Clones, TempoEscolhido, Resultado),
    (
        Resultado == "viagem impossível" ->
            format("~w Viagem impossível. Tente outra jogada.~n", [Negado]),
            %writeln("Viagem impossível. Tente outra jogada."),
            jogar(Foco, Jogador, Clones, Passado, Presente, Futuro,
                    NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco)

        ;
        (
            ( TempoEscolhido == "passado" -> TabDestino = Passado ;
              TempoEscolhido == "presente" -> TabDestino = Presente ;
              TempoEscolhido == "futuro" -> TabDestino = Futuro
            ),
            verificaPosicaoLivre(TabDestino, Linha, Coluna, Livre),
            (
                Livre == true ->
                    viagem(Foco, Clones, TempoEscolhido, Linha, Coluna, Jogador,
                           Passado, Presente, Futuro,
                           NovoPassado, NovoPresente, NovoFuturo, NovoClones),
                    stringParaFoco(TempoEscolhido, NovoFoco)
                ;
                    format("~w A posição no tempo de destino já está ocupada. Tente outra jogada.~n", [Negado]),
                    %writeln("A posição no tempo de destino já está ocupada. Tente outra jogada."),
                    jogar(Foco, Jogador, Clones, Passado, Presente, Futuro,
                        NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco)

            )
        )
    )

    ; Escolha == 'r' ->
        writeln("Reiniciando o jogo..."),
        iniciarJogo
    ; % fallback
        format("~w Escolha inválida inesperada!~n", [Negado]),
        %writeln("Escolha inválida inesperada!"),
        NovoPassado = Passado, NovoPresente = Presente, NovoFuturo = Futuro
    ).

/**
 * Retorna o nome do jogador com base na peça.
 *
 * @param Peca A peça do jogador.
 * @param Nome O nome correspondente ao jogador.
 */
nomeDoJogador(Peca, Nome) :-
    jogador1(J1),
    jogador2(J2),
    ( Peca == J1 -> jogador1_nome(Nome)
    ; Peca == J2 -> jogador2_nome(Nome)
    ).

finalizarJogo(NomeJogador) :-
    format('Parabéns, ~w! Você venceu!~n', [NomeJogador]),
    atualizarRanking(NomeJogador, 1),
    mostrarRanking,
    iniciarJogo.