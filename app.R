# =============================================
# *** Econometría 1 - UNCP
# *** Tema  : Dashboard Gapminder
# *** Modelo: Desarrollo mundial 1952-2007
# =============================================

# 1. INSTALAR PAQUETES (solo primera vez) -------------------------------
install.packages(c("shiny", "shinydashboard", "gapminder","ggplot2", "plotly", "dplyr", "DT"))

# 2. CARGAR LIBRERÍAS ---------------------------------------------------
library(shiny)
library(shinydashboard)
library(gapminder)
library(ggplot2)
library(plotly)
library(dplyr)
library(DT)

# -- COLORES POR CONTINENTE ---------------------------------------------
colores <- c(Africa   = "#D85A30",
             Americas = "#1D9E75",
             Asia     = "#378ADD",
             Europe   = "#7F77DD",
             Oceania  = "#D4537E")

# ----------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "???? Gapminder Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Burbuja interactiva", tabName = "bubble",  icon = icon("circle")),
      menuItem("Mapa mundial",        tabName = "mapa",    icon = icon("globe")),
      menuItem("Tendencias",          tabName = "trend",   icon = icon("chart-line")),
      menuItem("Regresión",           tabName = "reg",     icon = icon("chart-bar")),
      menuItem("Proyección",          tabName = "proj",    icon = icon("arrow-right")),
      menuItem("Tabla de datos",      tabName = "tabla",   icon = icon("table"))
    ),
    hr(),
    # Filtros globales
    sliderInput("year", "Año:",
                min = 1952, max = 2007, value = 2007, step = 5,
                animate = animationOptions(interval = 800)),
    selectInput("region", "Región:",
                choices = c("Todas"    = "all",
                            "África"   = "Africa",
                            "Américas" = "Americas",
                            "Asia"     = "Asia",
                            "Europa"   = "Europe",
                            "Oceanía"  = "Oceania")),
    selectInput("xvar", "Variable X:",
                choices = c("PIB per cápita" = "gdpPercap",
                            "Esperanza vida" = "lifeExp",
                            "Población"      = "pop")),
    selectInput("yvar", "Variable Y:",
                choices = c("Esperanza vida" = "lifeExp",
                            "PIB per cápita" = "gdpPercap",
                            "Población"      = "pop"))
  ),
  
  dashboardBody(
    # Métricas superiores
    fluidRow(
      valueBoxOutput("box_vida",   width = 3),
      valueBoxOutput("box_gdp",    width = 3),
      valueBoxOutput("box_pop",    width = 3),
      valueBoxOutput("box_paises", width = 3)
    ),
    
    tabItems(
      
      # -- 1. BURBUJA --------------------------------------------------
      tabItem("bubble",
              fluidRow(
                box(width = 12,
                    title = "Gráfico de burbuja interactivo — tamaño = población",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("bubblePlot", height = "500px"))
              )
      ),
      
      # -- 2. MAPA -----------------------------------------------------
      tabItem("mapa",
              fluidRow(
                box(width = 12,
                    title = "Mapa mundial — esperanza de vida por país",
                    status = "info", solidHeader = TRUE,
                    plotlyOutput("mapPlot", height = "500px"))
              )
      ),
      
      # -- 3. TENDENCIAS -----------------------------------------------
      tabItem("trend",
              fluidRow(
                box(width = 6,
                    title = "Esperanza de vida por región 1952–2007",
                    status = "success", solidHeader = TRUE,
                    plotlyOutput("trendLife", height = "300px")),
                box(width = 6,
                    title = "PIB per cápita por región 1952–2007",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("trendGDP", height = "300px"))
              ),
              fluidRow(
                box(width = 12,
                    title = "Evolución de la población mundial total",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("trendPop", height = "250px"))
              )
      ),
      
      # -- 4. REGRESIÓN ------------------------------------------------
      tabItem("reg",
              fluidRow(
                box(width = 8,
                    title = "Regresión lineal simple con IC 95%",
                    status = "danger", solidHeader = TRUE,
                    plotlyOutput("regPlot", height = "400px")),
                box(width = 4,
                    title = "Resultados del modelo",
                    status = "danger", solidHeader = TRUE,
                    verbatimTextOutput("regStats"))
              )
      ),
      
      # -- 5. PROYECCIÓN -----------------------------------------------
      tabItem("proj",
              fluidRow(
                box(width = 12,
                    title = "Proyección esperanza de vida hasta 2032 — regresión lineal por región",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("projPlot", height = "450px"))
              )
      ),
      
      # -- 6. TABLA ----------------------------------------------------
      tabItem("tabla",
              fluidRow(
                box(width = 12,
                    title = "Datos completos Gapminder",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("tabla"))
              )
      )
    )
  )
)

# ----------------------------------------------------------------------
# SERVER
# ----------------------------------------------------------------------
server <- function(input, output, session) {
  
  # -- DATOS FILTRADOS REACTIVOS ------------------------------------
  datos <- reactive({
    df <- gapminder %>% filter(year == input$year)
    if (input$region != "all") df <- df %>% filter(continent == input$region)
    df
  })
  
  # -- VALUE BOXES --------------------------------------------------
  output$box_vida <- renderValueBox({
    valueBox(round(mean(datos()$lifeExp), 1),
             "Esperanza de vida promedio (años)",
             icon = icon("heartbeat"), color = "green")
  })
  output$box_gdp <- renderValueBox({
    valueBox(paste0("$", format(round(mean(datos()$gdpPercap)), big.mark = ",")),
             "PIB per cápita promedio",
             icon = icon("dollar-sign"), color = "blue")
  })
  output$box_pop <- renderValueBox({
    valueBox(paste0(round(sum(datos()$pop) / 1e9, 2), "B"),
             "Población total",
             icon = icon("users"), color = "yellow")
  })
  output$box_paises <- renderValueBox({
    valueBox(nrow(datos()), "Países",
             icon = icon("flag"), color = "red")
  })
  
  # -- 1. BURBUJA ---------------------------------------------------
  output$bubblePlot <- renderPlotly({
    df <- datos()
    xv <- input$xvar
    yv <- input$yvar
    
    plot_ly(df,
            x        = ~get(xv),
            y        = ~get(yv),
            size     = ~pop,
            color    = ~continent,
            colors   = colores,
            text     = ~paste0("<b>", country, "</b><br>",
                               "Esperanza vida: ", round(lifeExp, 1), " años<br>",
                               "PIB per cápita: $", format(round(gdpPercap), big.mark = ","), "<br>",
                               "Población: ", format(pop, big.mark = ",")),
            hoverinfo = "text",
            type      = "scatter",
            mode      = "markers",
            marker    = list(sizemode = "diameter", opacity = 0.7)) %>%
      layout(
        xaxis = list(title = xv,
                     type  = if (xv == "gdpPercap") "log" else "linear"),
        yaxis = list(title = yv),
        showlegend = TRUE
      )
  })
  
  # -- 2. MAPA ------------------------------------------------------
  output$mapPlot <- renderPlotly({
    df <- gapminder %>% filter(year == input$year)
    
    plot_ly(df,
            type         = "choropleth",
            locations    = ~country,
            locationmode = "country names",
            z            = ~lifeExp,
            text         = ~paste0(country, ": ", round(lifeExp, 1), " años"),
            colorscale   = "Blues",
            colorbar     = list(title = "Esperanza\nde vida")) %>%
      layout(geo = list(showframe      = FALSE,
                        showcoastlines = TRUE,
                        projection     = list(type = "natural earth")))
  })
  
  # -- 3. TENDENCIAS ------------------------------------------------
  trend_base <- reactive({
    df <- gapminder
    if (input$region != "all") df <- df %>% filter(continent == input$region)
    df %>%
      group_by(continent, year) %>%
      summarise(lifeExp   = mean(lifeExp),
                gdpPercap = mean(gdpPercap),
                pop       = sum(pop),
                .groups   = "drop")
  })
  
  output$trendLife <- renderPlotly({
    plot_ly(trend_base(),
            x      = ~year, y = ~lifeExp,
            color  = ~continent, colors = colores,
            type   = "scatter", mode = "lines+markers",
            line   = list(width = 2)) %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list(title = "Años"))
  })
  
  output$trendGDP <- renderPlotly({
    plot_ly(trend_base(),
            x      = ~year, y = ~gdpPercap,
            color  = ~continent, colors = colores,
            type   = "scatter", mode = "lines+markers",
            line   = list(width = 2)) %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list(title = "USD"))
  })
  
  output$trendPop <- renderPlotly({
    pop_total <- gapminder %>%
      group_by(year) %>%
      summarise(pop = sum(pop) / 1e9)
    
    plot_ly(pop_total,
            x         = ~year, y = ~pop,
            type      = "scatter", mode = "lines+markers",
            fill      = "tozeroy",
            line      = list(color = "#378ADD", width = 2),
            fillcolor = "rgba(55,138,221,0.15)") %>%
      layout(xaxis = list(title = "Año"),
             yaxis = list(title = "Billones de personas"))
  })
  
  # -- 4. REGRESIÓN -------------------------------------------------
  output$regPlot <- renderPlotly({
    df  <- datos()
    xv  <- input$xvar
    yv  <- input$yvar
    mod <- lm(df[[yv]] ~ df[[xv]])
    df$fitted <- fitted(mod)
    
    plot_ly() %>%
      add_markers(data      = df,
                  x         = ~get(xv), y = ~get(yv),
                  color     = ~continent, colors = colores,
                  text      = ~country,
                  hoverinfo = "text+x+y",
                  marker    = list(size = 8, opacity = 0.75)) %>%
      add_lines(data = df %>% arrange(get(xv)),
                x    = ~get(xv), y = ~fitted,
                line = list(color = "#E24B4A", width = 2),
                name = "Regresión lineal",
                showlegend = TRUE) %>%
      layout(xaxis = list(title = xv),
             yaxis = list(title = yv))
  })
  
  output$regStats <- renderPrint({
    df  <- datos()
    mod <- lm(df[[input$yvar]] ~ df[[input$xvar]])
    cat("========== MODELO ==========\n")
    cat("Y:", input$yvar, "\n")
    cat("X:", input$xvar, "\n\n")
    print(summary(mod))
    cat("\nIntervalos de confianza 95%:\n")
    print(confint(mod))
  })
  
  # -- 5. PROYECCIÓN ------------------------------------------------
  output$projPlot <- renderPlotly({
    hist_data <- gapminder
    if (input$region != "all")
      hist_data <- hist_data %>% filter(continent == input$region)
    
    hist_avg <- hist_data %>%
      group_by(continent, year) %>%
      summarise(lifeExp = mean(lifeExp), .groups = "drop")
    
    future_yrs <- c(2007, 2012, 2017, 2022, 2027, 2032)
    continents <- unique(hist_avg$continent)
    
    p <- plot_ly()
    
    for (cont in continents) {
      sub  <- hist_avg %>% filter(continent == cont)
      mod  <- lm(lifeExp ~ year, data = sub)
      proj <- data.frame(
        year    = future_yrs,
        lifeExp = pmin(90, predict(mod, newdata = data.frame(year = future_yrs)))
      )
      col <- colores[cont]
      
      p <- p %>%
        add_lines(data = sub,
                  x    = ~year, y = ~lifeExp,
                  name = cont,
                  line = list(color = col, width = 2)) %>%
        add_lines(data = proj,
                  x    = ~year, y = ~lifeExp,
                  name = paste(cont, "(proyec.)"),
                  line = list(color = col, width = 2, dash = "dash"),
                  showlegend = FALSE)
    }
    
    p %>% layout(
      xaxis  = list(title = "Año", range = c(1952, 2032)),
      yaxis  = list(title = "Esperanza de vida (años)", range = c(30, 90)),
      shapes = list(list(type = "line",
                         x0 = 2007, x1 = 2007,
                         y0 = 30,   y1 = 90,
                         line = list(dash = "dot", color = "gray")))
    )
  })
  
  # -- 6. TABLA -----------------------------------------------------
  output$tabla <- renderDT({
    gapminder %>%
      filter(if (input$region != "all") continent == input$region else TRUE) %>%
      mutate(gdpPercap = round(gdpPercap, 2),
             lifeExp   = round(lifeExp, 2)) %>%
      datatable(filter  = "top",
                options = list(pageLength = 15))
  })
}

# -- LANZAR LA APP ------------------------------------------------------
shinyApp(ui, server)