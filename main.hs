import Data.Graph (Table)
{-

Gabriel Luchetti Garcia Sanchez - 12011BCC015
Silvano Martins da Silva Junior - 12011BCC042

-}

data ItemJogador = Patins | Bombas | Arremesso deriving (Eq, Show)
data Item = Grama | Presente ItemJogador | Parede | Bomba | Pedra | Jogador Int deriving (Eq, Show)

type Celula = [Item]
type Linha = [Celula]
type Tabuleiro = [Linha]
type CapacidadeJogador = ((ItemJogador, Int) , (ItemJogador, Int), (ItemJogador, Int))
type Cordenada = (Int, Int)
data Direcao = N | S | L | O deriving (Eq,Show)
type Jogador = (Int, Cordenada, Direcao, CapacidadeJogador)

cel1 :: Celula
cel1 = [Pedra]
cel2 :: Celula
cel2 = [Grama]
cel3 :: Celula
cel3 = [Parede, Grama]
cel4 :: Celula
cel4 = [Presente Patins, Grama]
cel5 :: Celula
cel5 = [Presente Bombas, Grama]
cel6 :: Celula
cel6 = [Presente Arremesso, Grama]
cel7 :: Celula
cel7 = [Jogador 1, Grama]
cel8 :: Celula
cel8 = []

lin1 :: Linha
lin1 = [cel1, cel1, cel1, cel1, cel1, cel1, cel1, cel1]
lin2 :: Linha
lin2 = [cel1, cel7, cel2, cel2, cel2, cel2, cel2, cel1]
lin3 :: Linha
lin3 = [cel1, cel2, cel3, cel2, cel2, cel3, cel2, cel1]
lin4 :: Linha
lin4 = [cel1, cel2, cel2, cel2, cel2, cel2, cel2, cel1]
lin5 :: Linha
lin5 = [cel1, cel2, cel2, cel2, cel2, cel2, cel2, cel1]
lin6 :: Linha
lin6 = [cel1, cel2, cel3, cel2, cel2, cel3, cel2, cel1]
lin7 :: Linha
lin7 = [cel1, cel2, cel2, cel2, cel2, cel2, cel2, cel1]
lin8 :: Linha
lin8 = [cel1, cel1, cel1, cel1, cel1, cel1, cel1, cel1]

tabuleiroPadrao :: Tabuleiro
tabuleiroPadrao = [lin1, lin2, lin3, lin4, lin5, lin6, lin7, lin8]

regrasCelula :: Celula -> Bool
regrasCelula celula
  | length celula > 1 && last celula /= Grama = False
  | length celula == 1 && (head celula /= Grama && head celula /= Pedra) = False
  | length celula == 2 && (head celula == Presente Patins && last celula /= Grama) = False
  | length celula == 2 && (head celula == Presente Arremesso && last celula /= Grama) = False
  | length celula == 2 && (head celula == Presente Bombas && last celula /= Grama) = False
  | length celula == 2 && (head celula == Parede && last celula /= Grama) = False
  | length celula == 3 && (head celula == Parede && head (tail celula) /= Presente Patins)  = False
  | length celula == 3 && (head celula == Parede && head (tail celula) /= Presente Bombas)  = False
  | length celula == 3 && (head celula == Parede && head (tail celula) /= Presente Arremesso)  = False
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
linhaValida [c1,c2,c3,c4,c5,c6,c7,c8]
  | regrasCelula c1 && regrasCelula c2 && regrasCelula c3 && regrasCelula c4 && regrasCelula c5 && regrasCelula c6 && regrasCelula c7 && regrasCelula c8 = True
  | otherwise = False

constroiTabuleiro :: Tabuleiro -> Tabuleiro
constroiTabuleiro [l1,l2,l3,l4,l5,l6,l7,l8]
 | linhaValida l1 && linhaValida l2 && linhaValida l3 && linhaValida l4 && linhaValida l5 && linhaValida l6 && linhaValida l7 && linhaValida l8 = [l1,l2,l3,l4,l5,l6,l7,l8]
 | otherwise = error "Tabuleiro Inválido!"

jogador :: Item -> Jogador
jogador jogador
  | jogador == Jogador 1 = (1, (2,2), S, ((Patins,1), (Bombas, 1), (Arremesso,0))) 
  | otherwise = error "Jogador não encontrado"

checaLinha :: Linha -> Item -> Celula
checaLinha linha jogador
  | null linha = []
  | head (head linha) == jogador = head linha
  | otherwise = checaLinha(tail linha) jogador

achaJogador :: Tabuleiro -> Item -> Celula
achaJogador tabuleiro jogador
  | checaLinha (head tabuleiro) jogador /= [] = checaLinha (head tabuleiro) jogador
  | otherwise = achaJogador (tail tabuleiro) jogador

movimentarS :: Jogador -> Jogador
movimentarS (a, (b1, b2), c, d)
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) /= Parede && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) /= Bomba = (a, (b1, b2 + 1), c, d)
  | otherwise = error "Movimento Inválido"

movimentarN :: Jogador -> Jogador
movimentarN (a, (b1, b2), c, d)
  | b2 > 2 && head (pegaLinha tabuleiroPadrao (b2 - 1) b1) /= Parede && head (pegaLinha tabuleiroPadrao (b2 - 1) b1) /= Bomba = (a, (b1, b2 - 1), c, d)
  | otherwise = error "Movimento Inválido"

movimentarO :: Jogador -> Jogador
movimentarO (a, (b1, b2), c, d)
  | b1 > 2 && head (pegaLinha tabuleiroPadrao b2 (b1 - 1)) /= Parede && head (pegaLinha tabuleiroPadrao b2 (b1 - 1)) /= Bomba = (a, (b1 - 1, b2), c, d)
  | otherwise = error "Movimento Inválido"

movimentarL :: Jogador -> Jogador
movimentarL (a, (b1, b2), c, d)
  | b1 < 7 && head (pegaLinha tabuleiroPadrao b2 (b1 + 1)) /= Parede && head (pegaLinha tabuleiroPadrao b2 (b1 + 1)) /= Bomba = (a, (b1 + 1, b2), c, d)
  | otherwise = error "Movimento Inválido"

movimentar :: Jogador -> Direcao -> Jogador
movimentar jogador direcao
  | direcao == S = movimentarS jogador
  | direcao == N = movimentarN jogador
  | direcao == O = movimentarO jogador
  | direcao == L = movimentarL jogador

pegaCelula :: Linha -> Int -> Celula
pegaCelula linha cel
  | cel == 1 = head linha
  | otherwise = pegaCelula (tail linha) (cel-1)

pegaLinha :: Tabuleiro -> Int -> Int -> Celula
pegaLinha tabuleiro linha cel
  | linha == 1 = pegaCelula (head tabuleiro) cel
  | otherwise = pegaLinha (tail tabuleiro) (linha-1) cel


colocaBomba :: Item -> Celula -> Celula
colocaBomba x l
  | x == Bomba && l == [Grama] = x:l
  | l == [] = []
  | otherwise = l


contadorCasas :: Tabuleiro -> Direcao -> Cordenada -> Int
contadorCasas t d c
  | d == N = contadorCasasN t c
  | d == S = contadorCasasS t c
  | d == L = contadorCasasL t c
  | d == O = contadorCasasO t c


contadorCasasN :: Tabuleiro -> Cordenada -> Int
contadorCasasN t (x,y)
  | x == 2 = 0
  | otherwise = 1 + contadorCasasN t (x-1,y)

contadorCasasS :: Tabuleiro -> Cordenada -> Int
contadorCasasS t (x,y)
  | x == 7 = 0
  | otherwise = 1 + contadorCasasS t (x+1,y)

contadorCasasL :: Tabuleiro -> Cordenada -> Int
contadorCasasL t (x,y)
  | y == 7 = 0
  | otherwise = 1 + contadorCasasL t (x,y+1)

contadorCasasO :: Tabuleiro -> Cordenada -> Int
contadorCasasO t (x,y)
  | y == 2 = 0
  | otherwise = 1 + contadorCasasO t (x,y-1)



arremessaBomba :: Tabuleiro -> Jogador -> Int
arremessaBomba t (a, (b1, b2), c, d)
  | c == S = arremessaBombaS t (a, (b1, b2), c, d)
  | c == N = arremessaBombaN t (a, (b1, b2), c, d)
  | c == O = arremessaBombaO t (a, (b1, b2), c, d)
  | c == L = arremessaBombaL t (a, (b1, b2), c, d)


arremessaBombaS :: Tabuleiro -> Jogador -> Int
arremessaBombaS t (a, (b1, b2), c, (_,(bomb, qntd),_))
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) == Bomba && qntd > (contadorCasas t S (b1,b2)) = contadorCasas t S (b1,b2)
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) == Bomba && qntd <= contadorCasas t S (b1,b2) = qntd
  | otherwise = error "Não foi possivel arremessar."

arremessaBombaN :: Tabuleiro -> Jogador -> Int
arremessaBombaN t (a, (b1, b2), c, (_,(bomb, qntd),_))
  | b2 > 2 && head (pegaLinha tabuleiroPadrao (b2 - 1) b1) == Bomba && qntd > (contadorCasas t N (b1,b2)) = contadorCasas t N (b1,b2)
  | b2 > 2 && head (pegaLinha tabuleiroPadrao (b2 - 1) b1) == Bomba && qntd <= (contadorCasas t N (b1,b2)) = qntd
  | otherwise = error "Não foi possivel arremessar."

arremessaBombaO :: Tabuleiro -> Jogador -> Int
arremessaBombaO t (a, (b1, b2), c, (_,(bomb, qntd),_))
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) == Bomba && qntd > (contadorCasas t O (b1,b2)) = contadorCasas t O (b1,b2)
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) == Bomba && qntd <= (contadorCasas t O (b1,b2)) = qntd
  | otherwise = error "Não foi possivel arremessar."

arremessaBombaL :: Tabuleiro -> Jogador -> Int
arremessaBombaL t (a, (b1, b2), c, (_,(bomb, qntd),_))
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) == Bomba && qntd > (contadorCasas t L (b1,b2)) = contadorCasas t L (b1,b2)
  | b2 < 7 && head (pegaLinha tabuleiroPadrao (b2 + 1) b1) == Bomba && qntd <= (contadorCasas t L (b1,b2)) = qntd
  | otherwise = error "Não foi possivel arremessar."

