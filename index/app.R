# app.R - Simulación Interactiva del Coeficiente de Correlación de Pearson

# Cargar librerías
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

# Tabla de interpretación del coeficiente de correlación de Pearson
tabla_interpretacion <- data.frame(
  "Valor" = c("-1", "-0.90 a -0.99", "-0.70 a -0.89", "-0.40 a -0.69", 
              "-0.20 a -0.39", "-0.01 a -0.19", "0", "0.01 a 0.19", 
              "0.20 a 0.39", "0.40 a 0.69", "0.70 a 0.89", "0.90 a 0.99", "1"),
  "Clasificación" = c(
    "Correlación negativa grande y perfecta",
    "Correlación negativa muy alta",
    "Correlación negativa alta", 
    "Correlación negativa moderada",
    "Correlación negativa baja",
    "Correlación negativa muy baja",
    "Correlación nula",
    "Correlación positiva muy baja",
    "Correlación positiva baja",
    "Correlación positiva moderada",
    "Correlación positiva alta",
    "Correlación positiva muy alta",
    "Correlación positiva grande y perfecta"
  )
)

# CSS personalizado
css <- HTML("
<style>
body {
  background: linear-gradient(135deg, #667eea, #764ba2);
  min-height: 100vh;
}
.main-container {
  background: rgba(255, 255, 255, 0.95);
  border-radius: 15px;
  box-shadow: 0 20px 40px rgba(0,0,0,0.1);
  margin: 20px;
  padding: 30px;
}
.card {
  background: linear-gradient(145deg, #ffffff, #f8f9ff);
  border: none;
  border-radius: 12px;
  box-shadow: 0 8px 25px rgba(0,0,0,0.08);
  padding: 25px;
  margin: 15px 0;
}
.slider-container {
  background: linear-gradient(135deg, #4facfe, #00f2fe);
  padding: 25px;
  border-radius: 15px;
  margin: 20px 0;
  box-shadow: 0 10px 30px rgba(79, 172, 254, 0.3);
}
.slider-container .form-group {
  margin-bottom: 0;
}
.slider-container label {
  color: white !important;
  font-weight: 600;
  font-size: 18px;
  margin-bottom: 15px;
  display: block;
}
.slider-container .help-block {
  color: rgba(255,255,255,0.9) !important;
  font-style: italic;
  margin-top: 10px;
}
h1 {
  background: linear-gradient(135deg, #667eea, #764ba2);
  -webkit-background-clip: text;
  -webkit-text-fill-color: transparent;
  text-align: center;
  font-weight: 700;
  margin-bottom: 30px;
}
h4 {
  color: #2c3e50;
  font-weight: 600;
  margin-bottom: 20px;
}
.content-wrapper {
  max-width: 1200px;
  margin: 0 auto;
}
</style>
")

# UI
ui <- fluidPage(
  tags$head(css),
  
  div(class = "main-container",
      div(class = "content-wrapper",
          h1("Simulación Interactiva del Coeficiente de Correlación de Pearson"),
          
          # Control Interactivo
          h3("Control Interactivo"),
          div(class = "slider-container",
              sliderInput("cor", 
                          "Coeficiente de correlación (r):",   
                          min = -1, 
                          max = 1, 
                          value = 0, 
                          step = 0.01,
                          width = "100%"),
              helpText("Mueve la barra para observar cómo cambia el gráfico de dispersión según el valor de r.")
          ),
          
          # Visualización de Datos
          h3("Visualización de Datos"),
          div(class = "card",
              plotOutput("scatterPlot", height = "600px")
          ),
          
          # Interpretación del Coeficiente
          h3("Interpretación del Coeficiente"),
          div(class = "card",
              h4("Tabla de interpretación del coeficiente de correlación de Pearson"),
              DTOutput("tabla")
          )
      )
  )
)

# Server
server <- function(input, output, session) {
  
  output$scatterPlot <- renderPlot({
    set.seed(123)
    n <- 100
    r <- input$cor
    
    # Generar datos con correlación r
    x <- rnorm(n)
    y <- r * scale(x) + sqrt(1 - r^2) * rnorm(n)
    y <- as.numeric(scale(y))
    df <- data.frame(x, y)
    
    # Calcular R²
    r_squared <- r^2
    
    # Crear el gráfico
    p <- ggplot(df, aes(x, y)) +
      geom_point(color = "#3498db", alpha = 0.7, size = 3, stroke = 0.5) +
      geom_smooth(method = "lm", se = TRUE, color = "#e74c3c", linewidth = 1.2, 
                  alpha = 0.3, fill = "#e74c3c") +
      labs(
        x = "Variable X",
        y = "Variable Y",
        title = "Gráfico de Dispersión - Correlación de Pearson"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 18, hjust = 0.5, face = "bold", 
                                  color = "#2c3e50", margin = margin(b = 20)),
        axis.title = element_text(size = 14, face = "bold", color = "#34495e"),
        axis.text = element_text(size = 12, color = "#7f8c8d"),
        panel.grid.major = element_line(color = "grey90", linewidth = 0.5),
        panel.grid.minor = element_line(color = "grey95", linewidth = 0.25),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.border = element_rect(color = "#ecf0f1", fill = NA, linewidth = 1)
      ) +
      coord_cartesian(xlim = c(-3, 3), ylim = c(-3, 3))
    
    # Añadir cuadro de información en esquina superior derecha
    info_text <- paste0(
      "r = ", sprintf("%.3f", r), "\n",
      "r² = ", sprintf("%.3f", r_squared)
    )
    
    p + annotate("rect", xmin = 1.5, xmax = 2.8, ymin = 2, ymax = 2.8,
                 fill = "white", color = "#34495e", linewidth = 1, alpha = 0.95) +
      annotate("text", x = 2.15, y = 2.4, label = info_text,
               size = 5, fontface = "bold", color = "#2c3e50",
               hjust = 0.5, vjust = 0.5)
  })
  
  output$tabla <- renderDT({
    # Gradiente de colores
    colores_gradiente <- c(
      "#f44336",  # Rojo (correlación perfecta negativa)
      "#ef5350",  # Rojo claro
      "#e57373",  # Rosa
      "#ef9a9a",  # Rosa medio
      "#ffcdd2",  # Rosa claro
      "#ffebee",  # Rosa muy claro
      "#ffffff",  # Blanco (correlación nula)
      "#c8e6c9",  # Verde muy claro
      "#a5d6a7",  # Verde claro
      "#81c784",  # Verde medio claro
      "#66bb6a",  # Verde medio
      "#4caf50",  # Verde
      "#2e7d32"   # Verde oscuro (correlación perfecta positiva)
    )
    
    datatable(tabla_interpretacion,
              options = list(
                dom = 't', 
                paging = FALSE,
                ordering = FALSE,
                columnDefs = list(
                  list(targets = "_all", className = "dt-center")
                )
              ),
              rownames = FALSE) %>%
      formatStyle(
        columns = 1:2,
        backgroundColor = styleEqual(
          tabla_interpretacion$Valor,
          colores_gradiente
        )
      )
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)