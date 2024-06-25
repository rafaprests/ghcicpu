-- Trabalho 1 - Programacao Funcional
-- Nome: Rafael Brondani Prestes
-- Data: 25/06/2024

-- declaracao de tipos
type Endereco = Int
type Valor = Int

data Estado = Estado {
    memoria :: [(Endereco, Valor)],
    acc :: Valor,
    eqz :: Bool,
    pc :: Endereco,
    ir :: (Int, Int)
} deriving (Show)

-- Valores iniciais para o estado do computador
estadoInicial :: [(Endereco, Valor)] -> Estado
estadoInicial mem = Estado {
    memoria = mem,
    acc = 0,
    eqz = False,
    pc = 0,
    ir = (0, 0)
}

-- Busca um valor na memória pelo endereço
buscarMemoria :: Endereco -> [(Endereco, Valor)] -> Valor
buscarMemoria end mem = case lookup end mem of
    Just val -> val
    Nothing -> 0

-- Atualiza um valor na memória
atualizarMemoria :: Endereco -> Valor -> [(Endereco, Valor)] -> [(Endereco, Valor)]
atualizarMemoria end val mem = (end, val) : filter ((/= end) . fst) mem

-- Carrega a instrução atual no registrador de instruções (IR)
carregarInstrucao :: Estado -> Estado
carregarInstrucao estado =
    let pcVal = pc estado
        instrucao = buscarMemoria pcVal (memoria estado)
        endereco = buscarMemoria (pcVal + 1) (memoria estado)
    in estado { ir = (instrucao, endereco), pc = pcVal + 2 }

executarInstrucao :: Estado -> Estado
executarInstrucao estado =
    let (instrucao, endereco) = ir estado
    in case instrucao of
        2 -> lod endereco estado
        4 -> sto endereco estado
        6 -> jmp endereco estado
        8 -> jmz endereco estado
        10 -> cpe endereco estado
        14 -> add endereco estado
        16 -> sub endereco estado
        18 -> nop estado
        20 -> hlt estado
        _ -> error "Instrução inválida"

-- instrucoes

-- 02 LOD 
-- Carrega o conteúdo do endereço de memória <end> no 
-- registrador acumulador (ACC).
lod :: Endereco -> Estado -> Estado
lod end estado =
    let val = buscarMemoria end (memoria estado)
    in estado { acc = val, eqz = val == 0 }

-- 04 STO 
-- Armazena o conteúdo do registrador acumulador (ACC) no 
-- endereço de memória 
sto :: Endereco -> Estado -> Estado
sto end estado = estado { memoria = atualizarMemoria end (acc estado) (memoria estado) }

-- 06 JMP 
-- Desvio incondicional: carrega no contador de instruções o valor 
-- <end> forçando com que a próxima instrução a ser executada 
-- seja a que se encontra no endereço de memória <end>
jmp :: Endereco -> Estado -> Estado
jmp end estado = estado { pc = end }

-- 08 JMZ
-- Desvio condicional: funcionamento análogo ao da instrução JMP 
-- com a diferença que a carga do contador de instruções só ocorre 
-- se o valor do acumulador for igual a zero (de acordo com a flag 
-- EQZ)
jmz :: Endereco -> Estado -> Estado
jmz end estado = if eqz estado then estado { pc = end } else estado

-- 10 CPE
-- Se o conteúdo do endereço <end> for igual ao acumulador, 
-- coloca 0 no acumulador, caso contrário coloca 1.
cpe :: Endereco -> Estado -> Estado
cpe end estado =
    let val = buscarMemoria end (memoria estado)
    in estado { acc = if val == acc estado then 0 else 1, eqz = if val == acc estado then True else False }

-- 14 ADD 
-- Adiciona o conteúdo do endereço de memória <end> ao 
-- conteúdo armazenado no acumulador (ACC) e armazena a 
-- resposta no próprio acumulador
add :: Endereco -> Estado -> Estado
add end estado =
    let val = buscarMemoria end (memoria estado)
        resultado = (acc estado + val) `mod` 256
    in estado { acc = resultado, eqz = resultado == 0 }

-- 16 SUB 
-- Subtrai o conteúdo do endereço de memória <end> do conteúdo 
-- do acumulador (ACC) e armazena a resposta no próprio 
-- acumulador.
sub :: Endereco -> Estado -> Estado
sub end estado =
    let val = buscarMemoria end (memoria estado)
        resultado = (acc estado - val) `mod` 256
    in estado { acc = resultado, eqz = resultado == 0 }

-- 18 NOP Não executa ação nenhuma (No OPeration).
nop :: Estado -> Estado
nop estado = estado

-- 20 HLT Encerra o ciclo de execução do processador (HaLT).
hlt :: Estado -> Estado
hlt estado = estado -- Parada, não faz nada



simular :: Estado -> Estado
simular estado =
    let estadoComInstrucao = carregarInstrucao estado
    in if fst (ir estadoComInstrucao) == 20
        then estadoComInstrucao
        else simular (executarInstrucao estadoComInstrucao)

-- Programa 1: Resp = A + B - 2
programa1 :: [(Endereco, Valor)]
programa1 = [
    (0, 2), (1, 240), -- LOD 240
    (2, 14), (3, 241), -- ADD 241
    (4, 16), (5, 245), -- SUB 245 (onde 245 contém 2)
    (6, 4), (7, 251), -- STO 251
    (8, 20), (9, 0), -- HLT
    -- Valores iniciais das variáveis
    (240, 10), (241, 20), (245, 2)
    ]


-- Estado inicial para o programa 1
estadoInicial1 = estadoInicial programa1

-- Executar a simulação
main :: IO ()
main = do
    let resultado1 = simular estadoInicial1
    print resultado1
