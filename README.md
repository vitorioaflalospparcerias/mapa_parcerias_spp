# Mapa das Parcerias - São Paulo

## 📝 Descrição

Aplicação interativa desenvolvida em **R/Shiny** para a visualização, monitoramento e análise de equipamentos públicos sob regimes de concessão e parceria no município de São Paulo. O sistema integra dados tabulares e geoespaciais para oferecer uma visão clara da distribuição territorial e dos indicadores financeiros e operacionais dos projetos.

## 🚀 O que é mostrado

-   **Mapa Interativo**: Localização georreferenciada de equipamentos com agrupamento (*cluster*) e pop-ups contendo informações detalhadas (modalidade, parceiro, investimento, contrato e link oficial).
-   **Filtros Dinâmicos**: Navegação hierárquica por Zona, Distrito, Lote de Parceria, Equipamento, Modalidade e Poder Concedente.
-   **Painel de Dashboards**: KPIs dinâmicos (Maior área, Distritos em destaque) e gráficos de barras interativos que se ajustam aos filtros aplicados.
-   **Camada de Análise Territorial**: Identificação visual de distritos com e sem equipamentos (camada cinza).
-   **Módulo de Downloads**: Exportação da base filtrada em formatos Excel (.xlsx) e Shapefile (.shp).

## 🛠️ Ferramentas e Técnicas

### Linguagem e Frameworks

-   **R (Shiny)**: Estrutura principal da aplicação.
-   **ShinyJS & Bslib**: Customização de interface, controle de menus flutuantes e temas.
-   **CSS & JavaScript**: Estilização avançada, fontes personalizadas e manipulação do DOM para comportamento responsivo.

### Processamento de Dados (ETL)

-   **Tidyverse (dplyr, tidyr, stringr)**: Manipulação e limpeza de dados.
-   **SF (Simple Features)**: Processamento de dados geoespaciais e operações de junção espacial (*spatial join*).
-   **Vetorização**: Uso de `purrr::pmap_chr` para geração eficiente de strings HTML em larga escala.
-   **Desacoplamento**: Uso de dicionários externos (CSV) para zoneamento, facilitando a manutenção.

### Visualização

-   **Leaflet**: Renderização de mapas e camadas espaciais.
-   **Highcharter**: Gráficos de barra dinâmicos com paletas customizadas.

## 📂 Estrutura do Projeto

-   `ui.R` / `server.R`: Definição da interface e lógica do servidor.
-   `global.R`: Carregamento de dependências e fontes de dados rds.
-   `analises.R`: Script de pré-processamento (ETL). Transforma dados brutos (.xlsx e .shp) em objetos otimizados (.rds).
-   `modules/`: Scripts contendo módulos isolados de Mapa, Filtros e Gráficos.
-   `www/`: Recursos estáticos (estilos CSS, ícones, logos e scripts JS).
-   `DADOS/`: Diretório contendo as fontes de dados e dicionários.

## 🔄 Fluxo de Atualização

Para inserir novos dados: 1. Atualize o arquivo `parcerias_lat_long.xlsx` na pasta `DADOS/`. 2. Certifique-se de que a coluna `geometry` segue o padrão `POINT(LATITUDE, LONGITUDE)`. 3. Execute o script `analises.R` para regenerar os arquivos de sistema (`.rds`). 4. Reinicie a aplicação Shiny.
