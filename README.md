# ExterminadorDoFuturo-prolog

Para rodar execute:
```
chmod +x run.sh && ./run.sh
```

- Windows
```
./run.bat
```
# O Exterminador do Futuro
Projeto desenvolvido com foco na programação lógica, utilizando Prolog, para a disciplina Paradigmas de Linguagem de Programação da Universidade Federal de Campina Grande (UFCG).


## 🎮 Descrição 
Trata-se da implementação lógica de um jogo de tabuleiro interativo em terminal, baseado no board game *That Time You Killed Me*, em que dois jogadores disputam pelo reconhecimento da criação da máquina do tempo.

## ⚙️ Executar o projeto
Para executar o projeto, siga os seguintes passos:
1. Instale e assegure-se de que o stack está funcionando em sua máquina
   - [Guia de instalação do stack](https://www.swi-prolog.org/Download.html)
  
2. Clone este repositório em sua máquina
    ```
    git clone https://github.com/StefaneLohanna/ExterminadorDoFuturo-prolog
    ```
3. Entre no diretório do projeto
    ```
    cd ExterminadorDoFuturo-prolog
    ```
4. (Apenas para usuários Windows) Configurar o terminal para exibir caracteres Unicode corretamente

   O jogo utiliza caracteres Unicode para representar algumas peças no tabuleiro. No entanto, o terminal do Windows pode não exibi-los corretamente. Para evitar esse problema, execute o seguinte comando antes de rodar o jogo:  
    ```
    chcp 65001
    ```
    Esse comando altera a página de código do terminal para UTF-8, garantindo a exibição correta dos caracteres.
   
5. Rode o projeto
    ```
    swipl -q -s app/Main.pl -t main
    ```
## 🔍 Regras do Jogo
O jogo começa com a junção de 3 tabuleiros (como está representado na imagem acima), representando, da esquerda para direita, os tempos passado, presente e futuro. O jogador da peça branca, primeiro a jogar, começa no tabuleiro do passado e o jogador da peça preta, começa no tabuleiro do futuro. Eles irão batalhar entre si, com a finalidade de extinguir todas peças do seu oponente em, no mínimo, 2 tabuleiros. 

As seguintes ações (2 ações para cada jogador por rodada) são permitidas:
- Mover 1 casa por vez;
- Plantar uma semente, produzindo um arbusto e/ou uma árvore (a depender do tempo escolhido para plantação);
- Viajar no tempo.
  
Durante o jogo, o jogador pode eliminar o seu adversário através dos seguintes modos: 
- Empurrar o seu oponente para as extremidades do tabuleiro;
- Empurrar o seu oponente para uma casa em que há um arbusto;
- Empurrar uma árvore para a casa em que o seu oponente está;
- Paradoxo: empurrar uma das duas peças do seu oponente que estão em casas adjacentes, matando-as.

## 📌 Equipe
- [Ana Paula Berto](https://github.com/anapaulab3rto)
- [Bruna Letícia](https://github.com/brunaletsleticia)
- [Eyshila Buriti](https://github.com/eyshilaburiti)
- [Stefane Lohanna](https://github.com/StefaneLohanna)
