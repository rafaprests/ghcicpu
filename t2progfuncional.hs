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

-- Resp = 251
-- A = 240
-- B = 241
-- C = 242
-- D = 243
-- E = 244
-- CONTANTES >= 245


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

-- Programa 2: Resp = A * B    
programa2 :: [(Endereco, Valor)]
programa2 = [
    (0, 2), (1, 240), -- LOD 240 Carrega A no ACC
    (2, 4), (3, 242), -- STO 242 Armazena A no endereço 242 (multiplicador)
    (4, 2), (5, 241), -- LOD 241 Carrega B no ACC
    (6, 4), (7, 243), -- STO 243 Armazena B no endereço 243 (multiplicand
    (8, 2), (9, 244), -- LOD 244 Carrega 0 no ACC (inicializa a soma)
    (10, 4), (11, 251), -- STO 251 Armazena 0 no endereço 251 (inicializa Resp)
    (12, 2), (13, 243), -- LOD 243 Carrega o multiplicando (B) no ACC
    (14, 8), (15, 30),-- JMZ 30 Se o multiplicando for 0, pula para HLT
    (16, 2), (17, 251), -- LOD 251 Carrega o valor de Resp no ACC
    (18, 14), (19, 242), -- ADD 242 Soma o multiplicador (A) ao ACC
    (20, 4), (21, 251), --STO 251 Armazena o resultado de volta em Resp
    (22, 2), (23, 243), --LOD 243 Carrega o multiplicando no ACC
    (24, 16), (25, 245), --SUB 245 Subtrai 1 do multiplicando
    (26, 4), (27, 243), --STO 243 Armazena o novo valor do multiplicando
    (28, 6), (29, 12), --JMP 12 Volta para o início do loop
    (30, 20), (31, 0), --HLT Finaliza a execução 
    -- valores iniciais das variaveis
    (240, 10), (241, 10),
    (244, 0), (245, 1)
    ]

-- Programa 3: A=0; Resp=1; while(A<5){A=A+1; Resp=Resp+2;}
programa3 :: [(Endereco, Valor)]
programa3 = [
    (0, 2), (1, 244), -- LOD 244 Carrega 0 no ACC (inicializa A)
    (2, 4), (3, 240), -- STO 240 Armazena 0 no endereço 240 (A)
    (4, 2), (5, 245), -- LOD 245 Carrega 1 no ACC (inicializa Resp)
    (6, 4), (7, 251), -- STO 251 Armazena 1 no endereço 251 (Resp)
    (8, 2), (9, 240), -- LOD 240 Carrega o valor de A no ACC
    (10, 16), (11, 246), -- SUB 246 Subtrai 5 do ACC
    (12, 8), (13, 28), -- JMZ 28 Se ACC for 0 (A == 5), pula para HLT
    (14, 2), (15, 240), -- LOD 240 Carrega o valor de A no ACC
    (16, 14), (17, 247), -- ADD 247 Soma 1 ao ACC (incrementa A)
    (18, 4), (19, 240), -- STO 240 Armazena o novo valor de A
    (20, 2), (21, 251), -- LOD 251 Carrega o valor de Resp no ACC
    (22, 14), (23, 248), -- ADD 248 Soma 2 ao ACC (incrementa Resp)
    (24, 4), (25, 251), -- STO 251 Armazena o novo valor de Resp
    (26, 6), (27, 8), -- JMP 8 Volta para o início do loop
    (28, 20), (29, 0), -- HLT Finaliza a execução

    -- valores iniciais das variáveis e constantes
    (240, 0),  -- A = 0
    (244, 0),  -- Inicializa A com 0
    (245, 1),  -- Inicializa Resp com 1
    (246, 5),  -- Constante 5 para comparação
    (247, 1),  -- Incremento de A
    (248, 2)   -- Incremento de Resp
    ]


-- Estado inicial para o programa 1
estadoInicial1 = estadoInicial programa1

-- Estado inicial para o programa 2
estadoInicial2 = estadoInicial programa2

-- Estado inicial para o programa 3
estadoInicial3 = estadoInicial programa3


-- Executar a simulação
main :: IO ()
main = do
    let resultado1 = simular estadoInicial1
    print resultado1
    let resultado2 = simular estadoInicial2
    print resultado2
    let resultado3 = simular estadoInicial3
    print resultado3
