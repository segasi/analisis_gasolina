### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, lubridate, scales, readxl, tidyverse, treemapify, wesanderson, zoo)


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 28, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 16, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 15),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size = 16, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 14, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text = element_text(size = 16, face = "bold", family="Didact Gothic Regular"))

tema_hm <-  theme_minimal() +
  theme(text = element_text(family="Didact Gothic Regular", color = "grey35"),
        plot.title = element_text(size = 32, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 25, face = "bold", colour = "#666666", margin = margin(0, 0, 20, 0), family="Didact Gothic Regular"),
        plot.caption = element_text(hjust = 0, size = 25),
        panel.grid = element_line(linetype = 2), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.88, -0.12),
        legend.direction = "horizontal",
        legend.title = element_text(size = 20, face = "bold", family="Trebuchet MS Bold"),
        legend.text = element_text(size = 18, family="Didact Gothic Regular"),
        legend.title.align = 0.5,
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 20),
        axis.title = element_text(size = 18, hjust = 1, face = "bold", margin = margin(0,0,0,0), family="Didact Gothic Regular"),
        axis.text.x = element_text(size = 16, face = "bold", family="Didact Gothic Regular"),
        axis.text.y = element_text(size = 12, face = "bold", family="Didact Gothic Regular"))


### Importar datos ----

# Fuente: Sistema de Información Energetica | http://sie.energia.gob.mx/bdiController.do?action=cuadro&cvecua=VIGSL_PSP

demanda <- read_excel("01_datos/eqitvzqcet_VIGSL_PSP_15012019_20_11.xls", range = "A7:CA46") # Solo importamos los calumnas que incluyen valores numéricos. 


### Renombrar primera columna y filtrar renglones vacíos o con datos nacional y regionales -----
d_edos <- 
  demanda %>% 
  rename(edo = "..1") %>% 
  filter(!edo %in% c("Nacional", "Noroeste", "Noreste", "Centro-Occidente", "Centro", "Sur-Sureste"),
         !is.na(edo))


### Transformar estructura de datos -----
d_edos <- 
  d_edos %>% 
  gather(key = mes_año,
         value = demanda,
         -edo)


### Transformar datos ----
d_edos <- 
  d_edos %>% 
  separate(mes_año, c("mes_texto", "año")) %>%     # Separar columna mes_año
  mutate(mes = case_when(mes_texto == "Ene" ~ 1,   # Generar columna numérica de mes
                         mes_texto == "Feb" ~ 2,
                         mes_texto == "Mar" ~ 3,
                         mes_texto == "Abr" ~ 4,
                         mes_texto == "May" ~ 5,
                         mes_texto == "Jun" ~ 6,
                         mes_texto == "Jul" ~ 7,
                         mes_texto == "Ago" ~ 8,
                         mes_texto == "Sep" ~ 9,
                         mes_texto == "Oct" ~ 10,
                         mes_texto == "Nov" ~ 11,
                         mes_texto == "Dic" ~ 12),
         mest_texto = fct_relevel(mes_texto,       # Redefinir orden de niveles de meses    
                                  "Ene", "Feb", "Mar",
                                  "Abr", "May", "Jun",
                                  "Jul", "Ago", "Sep",
                                  "Oct", "Nov", "Dic"), 
         año = as.numeric(año),                     # Cambiar tipo de dato de año
         fecha = make_date(año, mes))               # Generar variable de fecha




