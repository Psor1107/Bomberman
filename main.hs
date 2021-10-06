{-

Gabriel Luchetti Garcia Sanchez - 12011BCC015
Silvano Martins da Silva Junior - 12011BCC042

-}

data ItemJogador = Patins | Bomba | Arremesso
data Item = Grama | ItemJogador | Parede | Pedra | Jogador deriving Eq

type Celula = [Item]
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)
type CapacidadeJogador = ((ItemJogador, Int) , (ItemJogador, Int), (ItemJogador, Int))
type Cordenada = (Int, Int)
type Jogador = (Int, Cordenada, Char, CapacidadeJogador)

Arremesso :: Tabuleiro -> Jogador -> Tabuleiro
Arremesso Tabuleiro (a, b, c, d)
  | c == 'S' && Bomba `elem` (Tabuleiro !! fst b+1) !! snd b  = 
  | c == 'N' && Bomba `elem` (Tabuleiro !! fst b-1) !! snd b =
  | c == 'L' && Bomba `elem` (Tabuleiro !! fst b) !! snd b+1 =
  | c == 'O' && Bomba `elem` (Tabuleiro !! fst b) !! snd b-1 =