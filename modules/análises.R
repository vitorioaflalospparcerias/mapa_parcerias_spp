# ===================================================================================
# SCRIPT DE PRÉ-PROCESSAMENTO DE DADOS
# ===================================================================================

# --- CARREGAMENTO DE BIBLIOTECAS ---
library(shiny)
library(leaflet)
library(sf)
library(tidyverse)
library(readxl)
library(htmltools)

# --- ETAPA 1: PROCESSAMENTO DA CAMADA DE DISTRITOS DE SÃO PAULO ---
distritos <- st_read("DADOS/distritos_sao_paulo/SP_distritos_CD2022.shp", quiet = TRUE)
distritos <- distritos %>%
  filter(NM_MUN == "São Paulo")
distritos <- st_transform(distritos, crs = 4326)

# --- ETAPA 2: PROCESSAMENTO DA BASE UNIFICADA (EXCEL) ---
projetos <- read_excel("DADOS/parcerias_lat_long.xlsx")

projetos <- projetos %>%
  rename(
    ppp_nome = ppp,
    lote = Parceria,
    ppp_area = `ppp_area m2`,
    ppp_link1 = ppp_link
  ) %>%
  mutate(
    ppp_invest = as.numeric(ppp_invest),
    ppp_area = as.numeric(ppp_area),
    ppp_assina = as.character(ppp_assina),
    
    geometry_clean = str_replace_all(geometry, "[^0-9.-]", " "),
    geometry_clean = str_squish(geometry_clean)
  ) %>%
  separate(geometry_clean, into = c("lat_str", "lon_str"), sep = " ") %>%
  mutate(
    lat_num = as.numeric(lat_str),
    lon_num = as.numeric(lon_str)
  )

projetos <- st_as_sf(
  projetos,
  coords = c("lon_num", "lat_num"), 
  crs = 4326,
  remove = FALSE
)

projetos_para_excluir <- c("Terminal Parada Clínicas", "Terminal Parada Eldorado")
projetos <- projetos %>% filter(!ppp_nome %in% projetos_para_excluir)

# --- ETAPA 3: JUNÇÃO ESPACIAL E LIMPEZA DE DADOS ---
projetos <- st_join(projetos, distritos %>% select(NM_DIST))

projetos <- projetos %>%
  mutate(
    ppp_conced = trimws(ppp_conced),
    ppp_modali = replace_na(ppp_modali, "Não Especificado"),
    NM_DIST = str_to_title(trimws(NM_DIST)),
    
    ppp_contra = str_remove(ppp_contra, regex("^CONTRATO.*?N[.°ºo]+\\s*", ignore_case = TRUE)),
    ppp_contra = str_remove(ppp_contra, regex("^CONTRATO\\s+DE\\s+[A-ZÀ-ŸÇ]+\\s+", ignore_case = TRUE)),
    ppp_contra = str_remove(ppp_contra, regex("^CONTRATO\\s+", ignore_case = TRUE)),
    ppp_contra = trimws(ppp_contra)
  )

projetos <- projetos %>%
  mutate(
    ppp_conced = str_replace_all(ppp_conced, regex("São Paulo Regula", ignore_case = TRUE), "SP Regula"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secretaria Municipal De Mobilidade E Trânsito", ignore_case = TRUE), "SMT"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secretaria Municipal Do Verde E Meio Ambiente", ignore_case = TRUE), "SVMA"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secretaria Muncipal De Subprefeituras", ignore_case = TRUE), "SMSUB"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secreatria Municipal De Esportes E Lazer", ignore_case = TRUE), "SEME"),
    ppp_conced = str_replace_all(ppp_conced, regex("Subprefeitura Da Lapa", ignore_case = TRUE), "Sub Lapa"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secretaria Municipal De Educação", ignore_case = TRUE), "SME"),
    ppp_conced = str_replace_all(ppp_conced, regex("Sp Turis", ignore_case = TRUE), "SP Turis"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secretaria Municipal De Cultura", ignore_case = TRUE), "SMC"),
    ppp_conced = str_replace_all(ppp_conced, regex("Secretaria Municipal Da Pessoa Com Deficiência", ignore_case = TRUE), "SMPED")
  ) %>%
  mutate(lote = str_replace_all(lote, c(
    "CEUs - IB" = "CEUs MROSC"
  )))

projetos <- projetos %>%
  mutate(
    label_html = pmap_chr(
      list(ppp_nome, ppp_modali, ppp_conced, ppp_conces, ppp_assina, ppp_contra, ppp_invest, ppp_area, ppp_link1),
      function(nome, modali, conced, conces, assina, contra, invest, area, link1) {
        info_lista <- list(
          sprintf("<strong>PPP:</strong> %s", nome),
          if (!is.na(modali) && modali != "") sprintf("<strong>Modalidade:</strong> %s", modali) else NULL,
          if (!is.na(conced) && conced != "") sprintf("<strong>Poder Concedente:</strong> %s", conced) else NULL,
          if (!is.na(conces) && conces != "") sprintf("<strong>Parceiro:</strong> %s", conces) else NULL,
          if (!is.na(assina) && assina != "") sprintf("<strong>Ano de Assinatura:</strong> %s", assina) else NULL,
          if (!is.na(contra) && contra != "") sprintf("<strong>Contrato:</strong> %s", contra) else NULL,
          if (!is.na(invest)) sprintf("<strong>Investimento (R$):</strong> %s", format(invest, big.mark = ".", decimal.mark = ",", nsmall = 2)) else NULL,
          if (!is.na(area)) sprintf("<strong>Área (m²):</strong> %s", format(area, big.mark = ".", decimal.mark = ",", nsmall = 0)) else NULL,
          if (!is.na(link1) && link1 != "") sprintf("<strong>Link:</strong> <a href='%s' target='_blank'>Saiba Mais</a>", link1) else NULL
        )
        paste(compact(info_lista), collapse = "<br>")
      }
    )
  )

# --- ETAPA 4: MAPEAMENTO DE ZONAS E CONSOLIDAÇÃO FINAL ---
zonas_df <- read_csv("DADOS/de_para_zonas.csv", show_col_types = FALSE)

distritos <- distritos %>%
  mutate(NM_DIST = str_to_title(trimws(NM_DIST))) %>%
  left_join(zonas_df, by = "NM_DIST")

projetos <- projetos %>%
  left_join(st_drop_geometry(distritos %>% select(NM_DIST, zona)), by = "NM_DIST")

ordem_colunas <- c(
  "ppp_nome", "geometry", "ppp_modali", "ppp_conced", "ppp_area", 
  "ppp_conces", "ppp_assina", "ppp_contra", "ppp_invest", "ppp_link1",
  "lote", "NM_DIST", "zona", "label_html"
)
colunas_existentes <- intersect(ordem_colunas, names(projetos))
projetos <- projetos[, colunas_existentes]

# --- ETAPA 5: SALVAR OS OBJETOS PROCESSADOS ---
saveRDS(distritos, file = "DADOS/distritos_processados.rds")
saveRDS(projetos, file = "DADOS/projetos.rds")

# --- ETAPA FINAL: IDENTIFICAR E SALVAR DISTRITOS SEM EQUIPAMENTOS ---
distritos_com_equipamentos <- unique(na.omit(projetos$NM_DIST))

distritos_sem_equipamentos <- distritos %>%
  filter(!NM_DIST %in% distritos_com_equipamentos)

saveRDS(distritos_sem_equipamentos, file = "DADOS/distritos_sem_equipamentos.rds")

# --- LIMPEZA DE MEMÓRIA ---
rm(list = ls())
gc()