{-

Gabriel Luchetti Garcia Sanchez - 12011BCC015
Silvano Martins da Silva Junior - 12011BCC042

-}

data ItemJogador = Patins | Bomba | Arremesso
data Item = Grama | ItemJogador | Parede | Pedra | Jogador Int deriving Eq

type Celula = [Item]
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha)
type CapacidadeJogador = ((ItemJogador, Int) , (ItemJogador, Int), (ItemJogador, Int))
type Cordenada = (Int, Int)
data Direcao = N | S | L | O
type Jogador = (Int, Cordenada, Direcao, CapacidadeJogador)


regrasCelula :: Celula -> Bool
regrasCelula celula
  | length celula > 1 && last celula /= Grama = False
  | length celula == 1 && (head celula /= Grama || head celula /= Pedra) = False
  | length celula == 2 && (head celula == ItemJogador && last celula /= Grama) = False
  | length celula == 2 && (head celula == Parede && last celula /= Grama) = False
  | length celula == 3 && (head celula == Parede && head (tail celula) /= ItemJogador)  = False
  | length celula == 2 && (head celula == Jogador 1 && last celula /= Grama) = False
  | length celula == 2 && (head celula == Jogador 2 && last celula /= Grama) = False
  | length celula == 2 && (head celula == Jogador 3 && last celula /= Grama) = False
  | length celula == 2 && (head celula == Jogador 4 && last celula /= Grama) = False
  | length celula > 2 && elem (Jogador 1) celula = False
  | length celula > 2 && elem (Jogador 2) celula = False
  | length celula > 2 && elem (Jogador 3) celula = False
  | length celula > 2 && elem (Jogador 4) celula = False
  | length celula > 4 = False
  | otherwise = True

linhaValida :: Linha -> Bool
linhaValida (c1,c2,c3,c4,c5,c6,c7,c8)
  | regrasCelula c1 && regrasCelula c2 && regrasCelula c3 && regrasCelula c4 && regrasCelula c5 && regrasCelula c6 && regrasCelula c7 && regrasCelula c8 = True
  | otherwise = False


constroi_tabuleiro :: Tabuleiro -> Tabuleiro
constroi_tabuleiro (l1,l2,l3,l4,l5,l6,l7,l8)
 | linhaValida l1 && linhaValida l2 && linhaValida l3 && linhaValida l4 && linhaValida l5 && linhaValida l6 && linhaValida l7 && linhaValida l8 = (l1,l2,l3,l4,l5,l6,l7,l8)
 | otherwise = error "Tabuleiro Inválido!"

Arremesso :: Tabuleiro -> Jogador -> Tabuleiro
Arremesso Tabuleiro (a, b, c, d)
  | c == 'S' && Bomba `elem` (Tabuleiro !! fst b+1) !! snd b  = 
  | c == 'N' && Bomba `elem` (Tabuleiro !! fst b-1) !! snd b =
  | c == 'L' && Bomba `elem` (Tabuleiro !! fst b) !! snd b+1 =
  | c == 'O' && Bomba `elem` (Tabuleiro !! fst b) !! snd b-1 =