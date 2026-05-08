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

# --- CONFIGURAÇÕES ESTÁTICAS ---
CORES_VALIDAS <- c('orange', 'darkblue', 'cadetblue', 'darkred', 'purple', 'green', 'lightred', 'lightblue', 'darkgreen', 'pink', 'beige', 'gray')
CORES_HEX <- c(
  'orange' = '#F08200', 'darkblue' = '#023047', 'cadetblue' = '#436978', 
  'darkred' = '#A23336', 'purple' = '#8E44AD', 'green' = '#27AE60', 
  'lightred' = '#E74C3C', 'lightblue' = '#3498DB', 'darkgreen' = '#1E8449', 
  'pink' = '#D252B9', 'beige' = '#F39C12', 'gray' = '#7F8C8D'
)

# --- CARREGAR MÓDULOS ---
source("modules/mapa.R")
source("modules/filtros.R")
source("modules/graficos.R")

# --- CARREGAR DADOS ---
projetos <- readRDS("DADOS/projetos.rds")
distritos <- readRDS("DADOS/distritos_processados.rds")
distritos_sem_equipamentos <- readRDS("DADOS/distritos_sem_equipamentos.rds")