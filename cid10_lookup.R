# =============================================================================
# CID10_LOOKUP.R — Tabela de referência CID-10 (3 dígitos) para o SIM
# Fonte: CID10_3D__.CNV (DATASUS)
# 15ª Regional de Saúde — Maringá/PR | SCVGE
# =============================================================================
# Uso: source("cid10_lookup.R")
# Gera o objeto `cid10` (data.frame) com colunas:
#   codigo        — código CID-10 com 3 dígitos (ex: "I50")
#   descricao     — descrição abreviada em português (ex: "Insuf cardiaca")
#   capitulo_num  — número romano do capítulo (ex: "IX")
#   capitulo_nome — nome do capítulo (ex: "Circulatório")
#   grupo_mapa    — categoria simplificada para uso no mapa coroplético
# =============================================================================

# -----------------------------------------------------------------------------
# 1. LEITURA E PARSING DO ARQUIVO CNV
# -----------------------------------------------------------------------------

# Caminho para o arquivo CNV
caminho_cnv <- "/Users/valentimsalajunior/Documents/SIM-Dashboard/CID10_3D__.cnv"

# Verificar existência — mova o arquivo para a pasta SIM/DBF se necessário
if (!file.exists(caminho_cnv)) {
  stop(
    "Arquivo CNV não encontrado em: ", caminho_cnv,
    "\nMova o arquivo CID10_3D__.CNV para: ",
    "/Users/valentimsalajunior/Desktop/R_SIM/SIM/DBF/"
  )
}

# Ler todas as linhas (encoding DATASUS = latin1)
linhas <- readLines(caminho_cnv, encoding = "latin1", warn = FALSE)

# Filtrar apenas linhas de dados (começam com espaços + número sequencial)
linhas_dados <- linhas[grepl("^\\s+[0-9]+\\s+[A-Za-z]", linhas)]

# Parsear: extrair código (posição fixa após o número) e descrição
cid10_raw <- do.call(rbind, lapply(linhas_dados, function(linha) {
  # Remover \r (arquivo Windows)
  linha <- gsub("\r", "", linha)
  # Dividir por espaços múltiplos
  partes <- strsplit(trimws(linha), "\\s{2,}")[[1]]
  if (length(partes) < 3) return(NULL)
  # partes[1] = número sequencial, partes[2] = código, partes[3] = descrição
  # O código CID aparece repetido no final — ignorar
  codigo <- trimws(partes[2])
  desc   <- trimws(partes[3])
  # Remover o código CID repetido no final da descrição (com ou sem vírgula)
  desc <- gsub(paste0("\\s+", codigo, "[,\\s]*$"), "", desc, ignore.case = TRUE)
  # Remover qualquer código CID solto no final (padrão letra+2dígitos)
  desc <- gsub("\\s+[A-Z][0-9]{2,3}[,\\s]*$", "", desc)
  desc <- trimws(desc)
  data.frame(codigo = codigo, descricao = desc, stringsAsFactors = FALSE)
}))

# Manter apenas códigos CID válidos (letra + 2 dígitos)
cid10_raw <- cid10_raw[grepl("^[A-Za-z][0-9]{2}$", cid10_raw$codigo), ]
cid10_raw$codigo <- toupper(cid10_raw$codigo)

message("Registros CID-10 carregados: ", nrow(cid10_raw))

# -----------------------------------------------------------------------------
# 2. CLASSIFICAÇÃO POR CAPÍTULO CID-10
# Baseada no código numérico extraído do CID (letras A-Z + 00-99)
# -----------------------------------------------------------------------------

classificar_capitulo_cid <- function(codigo) {
  # Extrair letra e número
  letra <- substr(codigo, 1, 1)
  num   <- as.integer(substr(codigo, 2, 3))

  dplyr::case_when(
    letra %in% c("A", "B")                        ~ list("I",    "Infecciosas e parasitárias"),
    letra == "C" | (letra == "D" & num <= 48)      ~ list("II",   "Neoplasias"),
    letra == "D" & num >= 50 & num <= 89           ~ list("III",  "Doenças do sangue"),
    letra == "E"                                   ~ list("IV",   "Endócrinas e metabólicas"),
    letra == "F"                                   ~ list("V",    "Transtornos mentais"),
    letra == "G"                                   ~ list("VI",   "Sistema nervoso"),
    letra == "H" & num <= 59                       ~ list("VII",  "Doenças do olho"),
    letra == "H" & num >= 60                       ~ list("VIII", "Doenças do ouvido"),
    letra == "I"                                   ~ list("IX",   "Circulatório"),
    letra == "J"                                   ~ list("X",    "Respiratório"),
    letra == "K"                                   ~ list("XI",   "Digestivo"),
    letra == "L"                                   ~ list("XII",  "Pele e subcutâneo"),
    letra == "M"                                   ~ list("XIII", "Osteomuscular"),
    letra == "N"                                   ~ list("XIV",  "Geniturinário"),
    letra == "O"                                   ~ list("XV",   "Gravidez e parto"),
    letra == "P"                                   ~ list("XVI",  "Período perinatal"),
    letra == "Q"                                   ~ list("XVII", "Malformações congênitas"),
    letra == "R"                                   ~ list("XVIII","Causas mal definidas"),
    letra %in% c("S", "T")                         ~ list("XIX",  "Lesões e envenenamentos"),
    letra %in% c("V", "W", "X", "Y")              ~ list("XX",   "Causas externas"),
    letra == "Z"                                   ~ list("XXI",  "Fatores de saúde"),
    letra == "U"                                   ~ list("XXII", "Códigos especiais (U)"),
    TRUE                                           ~ list("—",    "Não classificado")
  )
}

# Grupo simplificado para mapa (10 categorias — legível visualmente)
grupo_mapa_cid <- function(codigo) {
  letra <- substr(codigo, 1, 1)
  num   <- as.integer(substr(codigo, 2, 3))
  dplyr::case_when(
    letra %in% c("A", "B")           ~ "Infecciosas e parasitárias",
    letra == "C" | (letra == "D" & num <= 48) ~ "Neoplasias",
    letra == "E"                      ~ "Endócrinas / Diabetes",
    letra == "I"                      ~ "Circulatório",
    letra == "J"                      ~ "Respiratório",
    letra == "K"                      ~ "Digestivo",
    letra %in% c("F", "G")           ~ "Neurológico / Mental",
    letra == "R"                      ~ "Mal definidas",
    letra %in% c("S","T","V","W","X","Y") ~ "Causas externas",
    TRUE                              ~ "Outras"
  )
}

# Aplicar classificações
cap_list <- lapply(cid10_raw$codigo, classificar_capitulo_cid)

cid10 <- cid10_raw
cid10$capitulo_num  <- sapply(cap_list, function(x) x[[1]])
cid10$capitulo_nome <- sapply(cap_list, function(x) x[[2]])
cid10$grupo_mapa    <- grupo_mapa_cid(cid10$codigo)

# -----------------------------------------------------------------------------
# 3. FUNÇÃO DE CONSULTA — uso no script principal
# -----------------------------------------------------------------------------

# Adiciona descrição e capítulo a um vetor de códigos CAUSABAS
# Aceita tanto 3 quanto 4 dígitos (trunca para 3 se necessário)
enriquecer_causabas <- function(vec_causabas) {
  codigo3 <- toupper(substr(trimws(vec_causabas), 1, 3))
  resultado <- dplyr::left_join(
    data.frame(codigo = codigo3, stringsAsFactors = FALSE),
    cid10,
    by = "codigo"
  )
  resultado
}

# -----------------------------------------------------------------------------
# 4. VERIFICAÇÃO
# -----------------------------------------------------------------------------

message("\nExemplo — primeiros registros do lookup:")
print(head(cid10, 10))

message("\nDistribuição por capítulo:")
print(table(cid10$capitulo_nome))
