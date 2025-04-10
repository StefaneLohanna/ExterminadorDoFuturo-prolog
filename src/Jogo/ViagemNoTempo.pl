:- module(viagemNoTempo, [defineViagem/4, viagem/13, viagemBot/13]).

:- use_module('./src/Jogo/Tabuleiro.pl').
:- use_module('./src/Interface/Jogador.pl').


viagem(FocoAtual, Clones, NovoTempoStr, Linha, Coluna, Jogador,
       Passado, Presente, Futuro,
       NovoPassado, NovoPresente, NovoFuturo, NovoClones) :-
/*
 * Realiza a viagem no tempo, atualizando os tabuleiros conforme a movimentação do jogador.
 * Cria clones se a viagem for do futuro para o presente ou do presente para o passado.
 * 
 * @param FocoAtual Tempo atual do jogador (passado, presente ou futuro).
 * @param Clones Quantidade atual de clones do jogador.
 * @param NovoTempoStr Tempo de destino como string ("passado", "presente", "futuro").
 * @param Linha Linha da posição onde está a peça.
 * @param Coluna Coluna da posição onde está a peça.
 * @param Jogador A peça do jogador.
 * @param Coluna Coluna da posição onde está a peça
 * @param Passado Tabuleiro do passado.
 * @param Presente Tabuleiro do presente.
 * @param Futuro Tabuleiro do futuro.
 * @param NovoPassado Tabuleiro do passado atualizado após a viagem.
 * @param NovoPresente Tabuleiro do presente atualizado após a viagem.
 * @param NovoFuturo Tabuleiro do futuro atualizado após a viagem.
 * @param NovoClones Nova quantidade de clones após a viagem.
 */

    ( FocoAtual == passado -> TabAtual = Passado
    ; FocoAtual == presente -> TabAtual = Presente
    ; FocoAtual == futuro -> TabAtual = Futuro
    ),

    ( NovoTempoStr == "passado" -> TabDestino = Passado
    ; NovoTempoStr == "presente" -> TabDestino = Presente
    ; NovoTempoStr == "futuro" -> TabDestino = Futuro
    ),

    (
        ( (FocoAtual == futuro, NovoTempoStr == "presente") ;
          (FocoAtual == presente, NovoTempoStr == "passado") ) ->
            viagemNoTabuleiroClones(TabAtual, TabDestino, Linha, Coluna, Jogador, NovoTabAtual, NovoTabDestino),
            NovoClones is Clones + 1
        ;
            viagemNoTabuleiro(TabAtual, TabDestino, Linha, Coluna, Jogador, NovoTabAtual, NovoTabDestino),
            NovoClones = Clones
    ),

    atualizaTabuleiros(FocoAtual, NovoTempoStr, Passado, Presente, Futuro, NovoTabAtual, NovoTabDestino,
                       NovoPassado, NovoPresente, NovoFuturo).


atualizaTabuleiros(FocoAtual, NovoTempoStr, Passado, Presente, Futuro, NovoTabAtual, NovoTabDestino,
                   NovoPassado, NovoPresente, NovoFuturo) :-
/*
 * Atualiza os tabuleiros após a viagem no tempo, substituindo apenas os afetados.
 *
 * @param FocoAtual Tempo de origem (átomo).
 * @param NovoTempoStr Tempo de destino (string).
 * @param Passado Tabuleiro original do passado.
 * @param Presente Tabuleiro original do presente.
 * @param Futuro Tabuleiro original do futuro.
 * @param NovoTabAtual Tabuleiro atualizado com a peça removida.
 * @param NovoTabDestino Tabuleiro atualizado com a peça adicionada.
 * @param NovoPassado Tabuleiro do passado atualizado.
 * @param NovoPresente Tabuleiro do presente atualizado.
 * @param NovoFuturo Tabuleiro do futuro atualizado.
 */
    ( FocoAtual == passado -> T1 = NovoTabAtual ; T1 = Passado ),
    ( NovoTempoStr == "passado" -> NovoPassado = NovoTabDestino ; NovoPassado = T1 ),

    ( FocoAtual == presente -> T2 = NovoTabAtual ; T2 = Presente ),
    ( NovoTempoStr == "presente" -> NovoPresente = NovoTabDestino ; NovoPresente = T2 ),

    ( FocoAtual == futuro -> T3 = NovoTabAtual ; T3 = Futuro ),
    ( NovoTempoStr == "futuro" -> NovoFuturo = NovoTabDestino ; NovoFuturo = T3 ).


defineViagem(Foco, Clones, NovoTempo, Resultado) :-
/*
 * Define se uma viagem no tempo é possível e retorna o resultado.
 * Valida as regras do jogo quanto à direção da viagem e criação de clones.
 *
 * @param Foco Tempo atual do jogador (passado, presente ou futuro).
 * @param Clones Quantidade de clones que o jogador já criou.
 * @param NovoTempo Tempo de destino desejado (string).
 * @param Resultado Resultado da tentativa de viagem (string).
 *                  Pode ser "viagem impossível" ou o próprio NovoTempo se for válida.
 */
	atom_string(Foco, TempoAtualStr),
	
	(
		NovoTempo == TempoAtualStr ->
			writeln("Lembre-se que não podemos viajar para o tempo que estamos atualmente"),
			Resultado = "viagem impossível"

	;   (TempoAtualStr == "passado", NovoTempo == "futuro") ->
			writeln("Lembre-se que não podemos viajar direto do passado para o futuro"),
			Resultado = "viagem impossível"

	;   (TempoAtualStr == "futuro", NovoTempo == "passado") ->
			writeln("Lembre-se que não podemos viajar direto do futuro para o passado"),
			Resultado = "viagem impossível"

	;   (TempoAtualStr == "futuro", NovoTempo == "presente") ->
			(Clones >= 4 ->
				writeln("Você já tem 4 clones e não pode criar mais."),
				Resultado = "viagem impossível"
			;
				Resultado = NovoTempo
			)

	;   (TempoAtualStr == "presente", NovoTempo == "passado") ->
			(Clones >= 4 ->
				writeln("Você já tem 4 clones e não pode criar mais."),
				Resultado = "viagem impossível"
			;
				Resultado = NovoTempo
			)

	;   (TempoAtualStr == "passado", NovoTempo == "presente") ->
			Resultado = NovoTempo

	;   (TempoAtualStr == "presente", NovoTempo == "futuro") ->
			Resultado = NovoTempo

	;   writeln("Opção Inválida"),
		Resultado = "viagem impossível"
	).


viagemBot(Foco, Clones, Linha, Coluna, Jogador,
          Passado, Presente, Futuro,
          NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco) :-

    % Tenta escolher um tempo diferente do foco atual
    findall(T, (member(T, ["passado", "presente", "futuro"]), T \= Foco), TemposDisponiveis),
    random_member(TempoEscolhido, TemposDisponiveis),

    defineViagem(Foco, Clones, TempoEscolhido, Resultado),
        (Resultado == "viagem impossível" ->
		%format("~w Viagem impossível. Tente outra jogada.~n", [Negado]),
            writeln("Viagem impossível para o bot."),
            jogarBot(Foco, Jogador, Clones, Passado, Presente, Futuro,
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
                    stringParaFoco(TempoEscolhido, NovoFoco),
                    writeln("Viagem realizada com sucesso"),
                    writeln("saiu em viagem")
                ;
                    writeln("Posição ocupada no destino."),
                    jogarBot(Foco, Jogador, Clones, Passado, Presente, Futuro,
                    NovoPassado, NovoPresente, NovoFuturo, NovoClones, NovoFoco)
			))
    ).
