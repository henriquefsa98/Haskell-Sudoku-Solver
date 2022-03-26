module SudokuSolversLib where

import Data.List
import Control.Monad
import Data.Function
import Data.Maybe
import Data.Functor


-- Nome: Henrique Fantato          RA: 21053916
-- Paradigmas da Programação   -   UFABC

-- Projeto SudokuSolver - Biblioteca auxiliar de Sudoku

--  Baseado em conceitos obtidos em aulas durente o curso de Paradigmas de Programaçao, 
-- bem como aulas disponibilizadas sobre Sudoku do professor Graham Houton : https://www.youtube.com/c/GrahamHuttonNotts/featured

-- Artigo auxiliar com conceitos sobre sudokus: https://www.csc.kth.se/utbildning/kth/kurser/DD143X/dkand12/Group6Alexander/final/Patrik_Berggren_David_Nilsson.report.pdf
-- Material de referencia sobre estrategias de resolucao de sudokus: https://www.sudokudragon.com/sudokustrategy.htm

-- Definição dos tipos necessários para o SudokuSolver

type Sudoku = Matriz Valor

type Matriz a = [Linhas a]

type Linhas a = [a]

type Valor = Char

type Tentativas = [Valor]


-- Definicoes auxiliares

tamanhocaixa :: Int
tamanhocaixa = 3

valores :: [Valor]
valores = ['1'..'9']

vazio :: Valor -> Bool
vazio = (=='.')

unico :: [a] -> Bool
unico [_] = True
unico _ = False



-- Funções

linhas :: Matriz a -> [Linhas a]
linhas = id

colunas :: Matriz a -> [Linhas a]
colunas = transpose


caixas :: [[a]] -> [[a]]
caixas as = juntaCaixas(combinaTriadesEmCaixas(divideEmTriades as))
    where
    combinaTriadesEmCaixas = map transpose

    juntaCaixas a = map concat (concat a)

    divideEmTriades a = divideLinhasEm3(map divideLinhasEm3 a)

    divideLinhasEm3 [] = []
    divideLinhasEm3 a = take tamanhocaixa a: divideLinhasEm3(drop tamanhocaixa a)


algumElemento :: (Eq a) => [a] -> [a] -> [a]
algumElemento [] ys = []
algumElemento (x:xs) ys
    | elem x ys = [x]
    | otherwise = algumElemento xs ys


-- Funcao que valida se um determinado estado do sudoku eh a solucao
valida :: Sudoku -> Bool
valida a = all semDuplicadas (linhas a )  &&
           all semDuplicadas (colunas a)  &&
           all semDuplicadas (caixas a )
        where
            semDuplicadas [] = True
            semDuplicadas (a:as) =  not(elem a as) && semDuplicadas as


-- Funcao que recebe um sudoku inicial, e gera todos os dominios das celulas vazias, sem nenhum filtro
possibilidades :: Sudoku -> [[[Valor]]]
possibilidades a = map (map chute) a
    where
        chute b = if b == '.'
                  then valores
                  else [b]

-- Funcao que expande as possibilidades geradas pela funcao possibilidades. Expande muitas possibilidades, precisa de tratamento para ser viavel na prática!
expandePossibilidades :: Matriz [a] -> [Matriz a]
expandePossibilidades s = expandeParcial (map expandeParcial s)
    where
        expandeParcial [] = [[]]
        expandeParcial (bs:bss) = [a:as | a <- bs, as <- expandeParcial bss]


-- Funcao que filtra os sudokus gerados, com base nos dominios das linhas, colunas e caixas
peneiraPossibilidades :: Matriz Tentativas -> Matriz Tentativas
peneiraPossibilidades = peneira caixas . peneira  colunas . peneira linhas
                        where
                            peneira tipo = tipo . map limpar . tipo
                            limpar bss = [if unico xs then xs else xs \\ (celulasUnicas)| xs <- bss]
                                          where
                                              celulasUnicas = concat (filter unico bss)


-- Funcao que filtra os sudokus gerados, com base nos dominios das linhas, colunas e caixas, e tambem com os elementos unicos escondidos!
peneiraPossibilidadesSmart :: Matriz Tentativas -> Matriz Tentativas -- Isso aqui é muito power!
peneiraPossibilidadesSmart = peneira caixas . peneira  colunas . peneira linhas
                        where
                            peneira tipo = tipo . map limpar . tipo
                            limpar bss = [if unico xs then xs else if not(null(unicoEscondido bss)) && not(null(algumElemento (unicoEscondido bss) xs)) then algumElemento (unicoEscondido bss) xs else (xs \\ celulasUnicas) | xs <- bss]
                                          where
                                              celulasUnicas = concat (filter unico bss)
                                              unicoEscondido bs = z
                                                where
                                                    y = [x | b <- bs, (x) <- b, notElem x (delete x (concat bs))]
                                                    z = y \\ celulasUnicas


-- Funcao que recebe uma funcao filtro, e itera ela ate o filtro nao surtir mais efeito
peneiraMaxima :: Eq t => (t -> t) -> t -> t
peneiraMaxima f x = if x == fx then x
                    else peneiraMaxima f fx
                    where fx = f x


-- Procura por sudokus que tem alguma célula sem solução possível!
sudokuBuraco :: Matriz Tentativas -> Bool
sudokuBuraco = any(any null)


-- Procura por sudokus que tem células unicas repetidas indo contra a regra do sudoku!
sudokuCelulaUnicaRepetida :: Eq a => Matriz [a] -> Bool
sudokuCelulaUnicaRepetida a = any unicaRepetida (linhas a) ||
                              any unicaRepetida (colunas a) ||
                              any unicaRepetida (caixas a)

                              where
                                  unicaRepetida bss = duplicada celulasUnicas
                                                      where
                                                          celulasUnicas = concat (filter unico bss)
                                                          duplicada [] = False
                                                          duplicada (a:as) =  elem a as || duplicada as


-- Procura por sudokus que não possuem solução
sudokuBloqueado :: Matriz Tentativas -> Bool
sudokuBloqueado s = sudokuBuraco s || (sudokuCelulaUnicaRepetida s)


-- Funcao que procura a solucao do sudoku, expandindo as celulas nao preenchidas e filtrando os sudokus que nao sao solucoes
procura :: Matriz Tentativas -> [Sudoku]
procura s
    | sudokuBloqueado s = []
    | all (all unico) s = expandePossibilidades s
    | otherwise = [g | m <- expand s, g <- procura (peneiraPossibilidades m)]


-- Funcao que expande as possibilidades da primeira celula nao preenchida do sudoku
expand                :: Matriz Tentativas -> [Matriz Tentativas]
expand m              =
   [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
   where
      (rows1,row:rows2) = break (any (not . unico)) m
      (row1,cs:row2)    = break (not . unico) row


procura2 :: Matriz Tentativas -> [Sudoku]
procura2 s
    | sudokuBloqueado s = []
    | all (all unico) s = expandePossibilidades s
    | otherwise = [g | m <- smartExpand s (encontraMelhorCelula s), g <- procura2 (peneiraPossibilidadesSmart m)]


procura3 :: Matriz Tentativas -> Int -> [(Sudoku, Int)]
procura3 s i
    | sudokuBloqueado s = [([], (i+1))]
    | all (all unico) s = [(head(expandePossibilidades s), (i+1))]
    | otherwise = [g | m <-  (smartExpand s (encontraMelhorCelula s)), g <- procura3 (peneiraPossibilidadesSmart m) (i+1) ]


-- Tentativa de implementar par unico
procura4 :: Matriz Tentativas -> Int -> [(Sudoku, Int)]
procura4 s i
    | sudokuBloqueado s = [([], (i+1))]
    | all (all unico) s = [(head(expandePossibilidades s), (i+1))]
    | otherwise = [g | m <-  (smartExpand s (encontraMelhorCelula s)), g <- procura4 (peneiraMaxima (filtraDuplasUnicas . peneiraPossibilidadesSmart) m) (i+1) ]


filtraDuplasUnicas :: Matriz Tentativas -> Matriz Tentativas
filtraDuplasUnicas = peneiraDuplas caixas . peneiraDuplas  colunas . peneiraDuplas linhas
                        where
                            peneiraDuplas tipo = tipo . map limpar . tipo
                            limpar bss = if null (duplaUnica bss) then bss else [if unico xs || ( not(null(duplaUnica bss)) && xs == (duplaUnica bss) ) then xs else xs \\ (duplaUnica bss) | xs <- bss]
                                          where
                                              celulasUnicas = concat (filter unico [bss])
                                              duplaUnica bs = z
                                                where
                                                    y = [x | x <- bs, elem x (delete x (bs)) && length x == length (filter (x==) (delete x bs)) + 1 ]  -- ta errado aqui!
                                                    w = y \\ celulasUnicas
                                                    z      --if length w == length (head w) then head w else []
                                                        | null w = []
                                                        | length w == length (head w) = head w
                                                        | otherwise = []

-- Funcao de heuristica que encontra a melhor celula a ser preenchida, a celula com menos possibilidades
encontraMelhorCelula :: (Foldable t1, Foldable t2) => t1 [t2 a] -> (Maybe Int, Maybe Int)
encontraMelhorCelula s = z
    where

        a = s
        c = menosPossibilidades a
            where
                menosPossibilidades a = concatMap (map length) a
        d =  foldl (\x y->if (x<y && (x /= 1)) then x else if y/= 1 then y else x) 1 c
        (Just x) = (elemIndex d c)
        z =  (Just (div x 9), Just (mod x 9))


-- Funcao para exibir as melhores celulas de um sudoku
melhoresCelulas :: (Foldable t1, Foldable t2) => [t1 [t2 a]] -> String
melhoresCelulas a = z
    where
        b = map encontraMelhorCelula a
        c = map unlines [[show b]]
        d = unlines c
        z = d

-- Funcao que expande as possibilidades da melhor celula informada
smartExpand :: Matriz Tentativas -> (Maybe Int, Maybe Int) -> [Matriz Tentativas]
smartExpand m (i, j) =
    [inicio ++ [row1 ++ [c] : row2] ++ resto | c <- colunaIdeal]
        where
        (inicio, linhaIdeal:resto) = break ((isNotRowIndex i)) m
            where
                isNotRowIndex i x = (elemIndex x m) == i

        (row1,colunaIdeal:row2)    = break (isNotRowIndex j) linhaIdeal
            where
                isNotRowIndex j x = elemIndex x linhaIdeal == j



-- Alguns Sudokus pré definidos!


facil :: Sudoku
facil = ["2....1.38",
        "........5",
        ".7...6...",
        ".......13",
        ".981..257",
        "31....8..",
        "9..8...2.",
        ".5..69784",
        "4..25...."]


sudokuVazio :: Sudoku
sudokuVazio = replicate  9 (replicate 9 '.')

sudokuMedio :: Sudoku
sudokuMedio           =  [".1.42...5",
                          "..2.71.39",
                          ".......4.",
                          "2.71....6",
                          "....4....",
                          "6....74.3",
                          ".7.......",
                          "12.73.5..",
                          "3...82.7."]


sudokuDificil :: Sudoku
sudokuDificil         =  [".9.7..86.",
                          ".31..5.2.",
                          "8.6......",
                          "..7.5...6",
                          "...3.7...",
                          "5...1.7..",
                          "......1.9",
                          ".2.6..35.",
                          ".54..8.7."]

-- Requer obrigatoriamente chutes
sudokuMuitoDificil    :: Sudoku
sudokuMuitoDificil    =  ["1..9.7..3",
                          ".8.....7.",
                          "..9...6..",
                          "..72.94..",
                          "41.....95",
                          "..85.43..",
                          "..3...7..",
                          ".5.....4.",
                          "2..8.6..9"]

-- teste para filtro duplas unicas
sudokuAntiAC3 :: Sudoku
sudokuAntiAC3 = [".......23",
                 "6..7.....",
                 ".......8.",
                 "....385..",
                 "2.....8..",
                 "....1....",
                 "...2..64.",
                 "..34.....",
                 ".1......."]


-- Solvers!

-- Primeiro solver, porém só funciona com sudokus Fáceis, trava com qualquer outra dificuldade.
semiSolver :: Sudoku -> [Sudoku]
semiSolver = filter valida . expandePossibilidades . peneiraMaxima peneiraPossibilidades. possibilidades


-- Segundo solver, agora capaz de resolver qualquer sudoku, porem ainda eh lento
solver :: Sudoku -> [Sudoku]
solver = procura . peneiraPossibilidades . possibilidades


-- Terceiro solver, rapido, utiliza heuristica de melhor casa e filtro de unico escondido
solver2 :: Sudoku -> [Sudoku]
solver2 a = procura2 (peneiraPossibilidadesSmart (possibilidades a))


-- Quarto solver, semelhante ao terceiro, mas agora retorna a quantidade de iteraçoes necessárias para resolver o sudoku.
solver3 :: Sudoku -> [(Sudoku, Int)]
solver3 a = procura3 (peneiraMaxima peneiraPossibilidadesSmart (possibilidades a)) 0


-- Quarto solver, semelhante ao quarto, mas agora com tentativa de pair unico
solver4 :: Sudoku -> [(Sudoku, Int)]
solver4 a = procura4 (peneiraMaxima (peneiraPossibilidadesSmart . filtraDuplasUnicas) (possibilidades a)) 0

