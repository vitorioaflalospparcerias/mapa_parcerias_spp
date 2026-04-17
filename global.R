# ===================================================================================
# OBJETIVO PRINCIPAL: CARREGAMENTO ÚNICO DE DEPENDÊNCIAS E DADOS (GLOBAL)
# ===================================================================================

# --- CARREGAMENTO DE BIBLIOTECAS ---
library(shiny)
library(shinyjs)
library(bslib)
library(dplyr)
library(sf)
library(highcharter)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(shinycssloaders)
library(stringr)
library(writexl)
library(zip)

# --- CARREGAR MÓDULOS ---
source("modules/mapa.R")
source("modules/filtros.R")
source("modules/graficos.R")

# --- CARREGAR DADOS ---
projetos <- readRDS("DADOS/projetos.rds")
distritos <- readRDS("DADOS/distritos_processados.rds")
distritos_sem_equipamentos <- readRDS("DADOS/distritos_sem_equipamentos.rds")