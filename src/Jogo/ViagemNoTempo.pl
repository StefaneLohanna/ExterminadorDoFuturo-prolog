:- module(viagemNoTempo, [defineViagem/8]).

:- use_module('./src/Jogo/Tabuleiro.pl').

defineViagem(Foco, TempoDestino, Linha, Coluna, Passado, Presente, Futuro, Resultado) :-
    mudancaDeTempoValida(Foco, TempoDestino, Passado, Presente, Futuro),
    obterTabuleiroPorTempo(TempoDestino, Passado, Presente, Futuro, TabuleiroDestino),
    posicaoLivreNoDestino(TabuleiroDestino, Linha, Coluna),
    writeln("Viagem realizada com sucesso!"),
    Resultado = sucesso.

% 1. Verifica se a mudança de tempo é válida
mudancaDeTempoValida(Foco, TempoDestino, Passado, Presente, Futuro) :-
    (Foco == Passado -> TempoAtual = "passado"
    ; Foco == Presente -> TempoAtual = "presente"
    ; Foco == Futuro -> TempoAtual = "futuro"
    ),

    (TempoAtual == TempoDestino ->
        writeln("Você já está nesse tempo! Escolha outro."),
        fail
    ; ( (TempoAtual == "passado", TempoDestino == "futuro")
      ; (TempoAtual == "futuro", TempoDestino == "passado") ) ->
        writeln("Não é possível viajar diretamente do passado para o futuro ou vice-versa."),
        fail
    ; true
    ).

% 2. Verifica se a posição no tabuleiro de destino está livre
posicaoLivreNoDestino(Tabuleiro, Linha, Coluna) :-
    nth1(Linha, Tabuleiro, LinhaLista),
    nth1(Coluna, LinhaLista, Conteudo),
    (Conteudo == ' ' ->
        true
    ; 
        format("A posição (~w,~w) no tempo escolhido está ocupada.~n", [Linha, Coluna]),
        fail
    ).

% 3. Obtém o tabuleiro de acordo com o tempo destino
obterTabuleiroPorTempo(passado, Passado, _, _, Passado).
obterTabuleiroPorTempo(presente, _, Presente, _, Presente).
obterTabuleiroPorTempo(futuro, _, _, Futuro, Futuro).
