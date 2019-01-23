### Paquetes ----
library(pacman)
p_load(ggrepel, janitor, lubridate, scales, readxl, tidyverse, treemapify, wesanderson, zoo)


### Setup ----
Sys.setlocale("LC_ALL", "es_ES.UTF-8") # Cambiar locale para prevenir problemas con caracteres especiales
options(scipen=999) # Prevenir notación científica


### Definir tema de gráficas ----
tema <-  theme_minimal() +
  theme(text = element_text(family = "Didact Gothic Regular", color = "grey35"),
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


### Generar dataframe con datos estatales -----
d_edos <- 
  demanda %>% 
  rename(edo = "..1") %>% 
  filter(!edo %in% c("Nacional", "Noroeste", "Noreste", "Centro-Occidente", "Centro", "Sur-Sureste"),
         !is.na(edo))

### Generar dataframe con datos nacionales y por región -----
d_nal_reg <- 
  demanda %>% 
  rename(nivel = "..1") %>% # Renombrar primera columna
  filter(nivel %in% c("Nacional", "Noroeste", "Noreste", "Centro-Occidente", "Centro", "Sur-Sureste"), # Filtrar renglones con datos estatales 
         !is.na(nivel)) # Filtrar renglones con NAs


### Transformar estructura de datos -----

# Estados
d_edos <- 
  d_edos %>% 
  gather(key = mes_año,
         value = demanda,
         -edo)

# Nacional y regiones
d_nal_reg <- 
  d_nal_reg %>% 
  gather(key = mes_año,
         value = demanda,
         -nivel)

### Transformar datos ----
# Estados
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
         mes_texto = fct_relevel(mes_texto,       # Redefinir orden de niveles de meses    
                                  "Ene", "Feb", "Mar",
                                  "Abr", "May", "Jun",
                                  "Jul", "Ago", "Sep",
                                  "Oct", "Nov", "Dic"), 
         año = as.numeric(año),                     # Cambiar tipo de dato de año
         fecha = make_date(año, mes))               # Generar variable de fecha


# Nacional y regiones
d_nal_reg <- 
  d_nal_reg %>% 
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
         mes_texto = fct_relevel(mes_texto,       # Redefinir orden de niveles de meses    
                                 "Ene", "Feb", "Mar",
                                 "Abr", "May", "Jun",
                                 "Jul", "Ago", "Sep",
                                 "Oct", "Nov", "Dic"), 
         año = as.numeric(año),                     # Cambiar tipo de dato de año
         fecha = make_date(año, mes))   

### Gráfica: demanda mensual de gasolina por año y estado, 2014-2017
d_edos %>% 
  filter(año < 2018) %>% 
  ggplot(aes(mes_texto, demanda, group = año)) +
  geom_line(size = 1, alpha = 0.7, color = "grey50") +
  stat_summary(aes(group = edo), fun.y = mean, geom = "line", colour = "salmon", size = 1) +
  facet_wrap(~ edo, ncol = 8) +
  labs(title = str_wrap(str_to_upper("demanda mensual de gasolina por año y estado, 2012-2017"), width = 80),
       subtitle = "Las líneas grises indican la demanda mensual de gasolina a lo largo de cada año. La línea roja representa el promedio de la demana para el mes y estado correspondiente.",
       x = NULL,
       y = "Miles de barriles diarios\n",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SIE, url: bit.ly/2RBtZlu. Consultado el 17 de enero de 2019.") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 22),
        plot.subtitle = element_text(size = 22),
        plot.caption = element_text(size = 22)) +
  ggsave(filename = "demanda_mensual_gasolina_año_estado_2012_2017.png", path = "03_graficas", width = 24, height = 15, dpi = 200) 


### Gráfica: demanda mensual de gasolina a nivel nacional, 2012-2017 ----
d_nal_reg %>% 
  filter(nivel == "Nacional",
         año < 2018) %>% 
  group_by(mes_texto) %>% 
  mutate(demanda_promedio = mean(demanda)) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(aes(mes_texto, demanda, group = año), size = 1, alpha = 0.7, color = "grey50") +
  geom_line(aes(mes_texto, demanda_promedio, group = 1), color = "salmon", size = 2, alpha = 0.9) +
  annotate(geom = "segment", x = 1.2, xend = 1.8, y = 885.5, yend = 885.5, color = "salmon", size = 2, alpha = 0.9) +
  annotate(geom = "text", label = "Demanda promedio mensual", x = 2, y = 885.5, color = "grey30", size = 6, hjust = 0, family = "Didact Gothic Regular") +
  scale_y_continuous(breaks = seq(750, 900, 25)) +
  labs(title = str_wrap(str_to_upper("demanda mensual de gasolina a nivel nacional, 2012-2017"), width = 80),
       subtitle = str_wrap("Las líneas grises indican la demanda mensual de gasolina a lo largo del año correspondiente. La línea roja representa la demanda promedio de gasolina para ese mes en el período analizado.", width = 130),
       x = NULL,
       y = "Miles de barriles diarios\n",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SIE, url: bit.ly/2RBtZlu. Consultado el 17 de enero de 2019.") +
  tema +
  ggsave(filename = "demanda_mensual_gasolina_año_2012_2017.png", path = "03_graficas", width = 15, height = 10, dpi = 200) 



