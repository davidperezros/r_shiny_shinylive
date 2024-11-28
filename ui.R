# Libraries used

library(shiny)
library(readxl)
library(shinydashboard)
library(bslib)
library(dplyr)
library(sf) # read_sf
library(leaflet) # map
library(plotly)
# ineapir -------------------------------------------------------------------

# DATOS ECOICOP
# Obtener ids y nombres comu autonomas para selector
ids2 <- read_excel("excels/ids.xlsx", col_types = c("text", "text", "text", "text"))

ids <- ids2 %>%
  filter(Fk_Variable %in% c(70, 349)) %>%
  pull(Id)



nombres <- ids2 %>%
  filter(Fk_Variable %in% c(70, 349)) %>%
  pull(Nombre)

# Crear estructura para el selector de años
selector_lugar <- setNames(ids, nombres)










# UI ----------------------------------------------------------------------

ui <- page_sidebar(
  includeCSS("www/style.css"),
  title = tags$a(href = "https://www.ine.es/", target = "_blank", tags$img(src = "ine_logo.svg", alt = "Logo", class = "logo")),
  tags$div(
    class = "logo-container2", tags$a(
      href = "https://github.com/davidperezros/r_shiny", # Cambia por tu enlace a GitHub
      target = "_blank", # Se abrirá en una nueva pestaña
      icon("github"), # Utiliza un ícono de Font Awesome
      class = "logo2"
    )
  ),
  sidebar = sidebar(
    selectizeInput(
      "x", "Ambito geográfico",
      selector_lugar,
      selected = 9002
    ),
    selectizeInput(
      "anyo", "Año",
      choices = as.character(seq(2010, 2022, 1)),
      selected = 2022
    ),
    tooltip(
      span(
        "Fuente de datos: INE",
        bsicons::bs_icon("info-circle")
      ),
      "Para más ifnormación que la mostrada debjo, visitar la web del INE.",
      placement = "bottom"
    ),
    tags$div(
      class = "fuentes",
      tags$span("Fuentes de datos:"), # Título de la sección
      tags$ul(
        # Lista con los hipervínculos
        tags$li("Tamaño población, ", tags$a(href = "https://www.ine.es/jaxiT3/Tabla.htm?t=10262&L=0", "https://www.ine.es/jaxiT3/Tabla.htm?t=10262&L=0")),
        tags$li("Datos ECOICOP, ", tags$a(href = "https://www.ine.es/jaxiT3/Tabla.htm?t=25143&L=0", "https://www.ine.es/jaxiT3/Tabla.htm?t=25143&L=0")),
        tags$li("Datos sueldo medio, ", tags$a(href = "https://www.ine.es/jaxiT3/Tabla.htm?t=28191&L=0", "https://www.ine.es/jaxiT3/Tabla.htm?t=28191&L=0")),
        tags$li("Contornos comunidades, ", tags$a(href = "https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70", "https://www.ine.es/wstempus/geojs/ES/CONTORNOS/70"))
      )
    )
  ),
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Número de habitantes", value = textOutput("poblacion22"), theme = value_box_theme(
        bg = "#FFFFFF",
        fg = "#457E76"
      ), showcase = fontawesome::fa_i("people-group"),
      showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
      height = NULL
    ),
    value_box(
      title = "Sueldo medio bruto", value = textOutput("sueldomedio"), theme = value_box_theme(
        bg = "#FFFFFF",
        fg = "#457E76"
      ), showcase = bsicons::bs_icon("currency-euro"),
      showcase_layout = "left center", full_screen = FALSE, fill = TRUE,
      height = NULL
    ),
    value_box(
      title = "Porcenetaje de la población total", value = textOutput("poblacion_porcentaje"),
      theme = value_box_theme(bg = "#FFFFFF", fg = "#457E76"),
      showcase = bsicons::bs_icon("percent"), showcase_layout = "left center",
      full_screen = FALSE, fill = TRUE, height = NULL
    )
  ),
  layout_column_wrap(
    width = 1 / 2,
    heigth = 400,
    card(
      full_screen = TRUE,
      card_header("Ambito geográfico"),
      leafletOutput("map")
    ),
    layout_column_wrap(
      width = 1,
      heights_equal = "row",
      card(
        full_screen = TRUE,
        card_header("Distribución del gasto medio por persona"),
        plotlyOutput("ecoicop")
      )
    )
  )
)
