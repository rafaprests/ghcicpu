-- Define a memoria como uma colecao de tuplas (endereco,valor)
-- [(Int,Int)]
-- Exemplo:
-- 0 LOD 240
-- 2 ADD 241
-- 4 STO 251
-- 6 HLT NOP
-- let prog1 = [(0,2),(1,240),(2,14),(3,241),(4,4),(5,251),(6,20),(7,18),(240,0),(241,1),(251,0)]

-- Executar um programa
-- Recebe uma memória e retorna uma memória resultante da execução do programa carregado na memória
-- Assume que o programa começa no endereço 0
-- executar(memoria)=memoria
-- executar prog1
executar :: [(Int,Int)] -> [(Int,Int)]

-- Lembrete: uma CPU funciona no ciclo "busca, decodificação e execução de instruções".
-- Portanto: modelar e construir funções para cada uma das fases desse ciclo.

-- Cada instrução da CPU pode ser modelada como uma função sobre o estado atual da computação.
-- Instrução NOP
-- execNOP(memoria,acc,eqz)=(memoria,acc,eqz)
execNOP :: ([(Int,Int)], Int, Int) -> ([(Int,Int)], Int, Int)
execNOP (mem, acc, eqz) = (mem, acc, eqz)
-- Intrução LOD
-- execLOD(endereco,memoria,acc,eqz)=(memoria,acc,eqz)
execLOD :: Int -> ([(Int,Int)], Int, Int) -> ([(Int,Int)], Int, Int)
execLOD end (mem, acc, eqz) = (mem, readMem mem end, eqz)

-- Várias funções auxiliares a seguir:

-- Ler a memória
-- Retornar o conteúdo do endereço de memória
-- readMem(memoria,endereco)=conteudo
-- Exemplo:
-- readMem [(0,10),(1,3),(2,23),(10,100)] 1 = 3
readMem :: [(Int,Int)] -> Int -> Int
readMem (m:ms) e
    | e == fst m = snd m
    | e /= fst m = readMem ms e

-- Escrever na memória
-- Armazenar o conteúdo em um endereço de memória
-- writeMem(memoria,endereço,conteudo)=memoria
-- Exemplo:
-- wirteMem [(0,10),(1,3),(2,23),(10,100)] 1 6 = [(0,10),(1,6),(2,23),(10,100)]
