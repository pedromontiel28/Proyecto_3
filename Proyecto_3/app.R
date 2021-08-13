library(readxl)
library(janitor)
library(tidyverse)
library(gglorenz)

UNAM <- read_excel("data/UNAM_Remuneracion-profesores_2021-08-12_15.39.38.xlsx") 

UNAM_mod <- UNAM %>% 
  clean_names() %>%
  mutate(remuneracion_neta=parse_number(remuneracion_neta)) %>%
  mutate(remuneracion_bruta=parse_number(remuneracion_bruta)) %>%
  mutate(monto_total_percibido=parse_number(monto_total_percibido)) %>% 
  mutate(tipo_o_nivel_de_contratacion=str_replace(tipo_o_nivel_de_contratacion,
                                                  "[:alpha:][:digit:][:digit:][:digit:][:digit:] ",
                                                  "")) %>% 
  mutate(tipo_contra=word(tipo_o_nivel_de_contratacion, 1))

promedio_montos <- UNAM_mod %>% 
  group_by(unidad_academica) %>% 
  summarise(Promedio = mean(monto_total_percibido)) %>% 
  ungroup() %>% 
  arrange(Promedio)

top_veinte <- promedio_montos[c(1:10,131:140),] %>% 
  as_tibble() %>%
  mutate(tipo = c(rep("Los 10 peor pagados", times = 10),  rep("Los 10 mejor pagados", times = 10)))

#----Aplicación----
ui <- fluidPage(
  # Application title
  titlePanel("Desigualdad: Ingresos en la UNAM."),
  navlistPanel(
    tabPanel("Los 20 más…", "Mejores y peores ingresos en la UNAM con base en unidad académica y
             tipo de contratación.", 
             plotOutput("Gráfica"),
             plotOutput("Top20")), 
    tabPanel("Tipo de Contratación", "Curva de Lorenz. Entre mayor área en el gráfico, mayor desigualdad
             salarial.", 
             plotOutput("Lorenz")), 
    tabPanel("Tipo de Remuneración", "Definida en cuanto al tipo de contratación.")
  ), 
  mainPanel(
    
  )
)
server <- function(input, output) {
  output$Gráfica <- renderPlot({
    UNAM_mod %>% select(tipo_o_nivel_de_contratacion, remuneracion_neta) %>% 
      count(tipo_o_nivel_de_contratacion, remuneracion_neta ) %>% 
      arrange(n, .by_group = TRUE) %>% 
      ggplot( aes(n, remuneracion_neta, colour = tipo_o_nivel_de_contratacion)) +
      geom_point() +
      scale_x_continuous(breaks = scales::breaks_extended(n = 12)) + 
      theme(legend.position="bottom")
    
  }) 
  output$Top20 <- renderPlot({
    top_veinte %>% 
      ggplot(aes(
        x = reorder(unidad_academica, -Promedio), 
        y = Promedio, 
        fill = tipo
      )) +
      geom_bar(stat = "identity") +
      labs(title = "Relación entre la Unidad Académica y el Promedio del Monto Total Percibido") +
      labs(x = "Unidad Académica", y = "Promedio del Monto Acumulado (MXN)") +
      theme_bw() +
      theme(legend.title = element_blank()) +
      theme(
        legend.box.background = element_rect(),
        legend.box.margin = margin(6, 6, 6, 6)
      ) +
      theme(panel.grid = element_blank()) +
      scale_y_continuous(breaks = 10000*1:5) +
      coord_flip()
  })
  output$Lorenz({
    UNAM_mod %>% 
      #filter(tipo_o_nivel_de_contratacion==) %>% 
      ggplot(aes(monto_total_percibido, fill=tipo_contra)) +
      stat_lorenz(geom = "polygon", alpha = 0.65) +
      geom_abline(linetype = "dashed") +
      coord_fixed() +
      labs(x = "Porcentaje acumulado de trabajadores",
           y = "Porcentaje acumulado de ingresos",
           title = "Desigualdad de ingresos en la UNAM",
           caption = "Source: http://www.transparencia.unam.mx/obligaciones/consulta/remuneracion-profesores (rev ago2021)")
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
