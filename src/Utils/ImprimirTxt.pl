:- module(imprimirTxt, [imprimirTxt/1]).

/*
 * Imprime o conteúdo de um arquivo.
 *
 * @param CaminhoArquivo: Caminho para o arquivo.
 */
imprimirTxt(CaminhoArquivo) :-
    open(CaminhoArquivo, read, Stream, [encoding(utf8)]),
    imprimirLinhas(Stream),
    close(Stream).

/*
 * Lê e imprime cada linha do Stream até o fim do arquivo.
 *
 * @param Stream: Fluxo de entrada do arquivo.
 */
imprimirLinhas(Stream) :-
    read_line_to_string(Stream, Linha),
    (Linha \= end_of_file -> 
        writeln(Linha),
        imprimirLinhas(Stream)
    ; true
    ).