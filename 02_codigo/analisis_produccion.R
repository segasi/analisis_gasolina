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

# Fuente: http://estadisticashidrocarburos.energia.gob.mx/Datos_semana.aspx

bd_produccion <- read_excel("01_datos/EstadÃ_sticas EnergÃ©ticas.xlsx", sheet = "Produccion")


### Transformación de datos ----

# Limpiaer datos ----
bd_produccion <- clean_names(bd_produccion)

# Cambiar tipo de variables y reordenar niveles de la variable mes ----
bd_produccion <- 
  bd_produccion %>%
  mutate(valor = as.numeric(valor), # Transformar tipo de variable a numeric
         fecha = dmy_hms(fecha), # Transformar tipo de variable a dttm
         mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) 


### Gráfica: producción semanal de gasolina a nivel nacional entre el 6 de abril de 2018 y el 4 de enero de 2019 ----
bd_produccion %>% 
  filter(tipo == "Gasolina",
         fecha >= as_datetime("2018-03-31 12:00:00")) %>% 
  group_by(fecha) %>% 
  summarise(produccion_semanal = sum(valor)) %>% 
  ungroup() %>% 
  arrange(fecha) %>%   
  mutate(promedio_movil = rollmean(x = produccion_semanal, 4, align = "right", fill = NA)) %>%
  ggplot() +
  geom_line(aes(fecha, produccion_semanal, group = 1), size = 1, alpha = 0.7, color = "grey50") +
  geom_line(aes(fecha, promedio_movil, group = 1), size = 2, alpha = 0.9, color = "salmon") +
  annotate(geom = "segment", x = as_datetime("2018-10-10 12:00:00"), xend = as_datetime("2018-10-20 12:00:00"), y = 375, yend = 375, color = "salmon", size = 2, alpha = 0.9) +
  annotate(geom = "text", label = "Promedio móvil de cuatro semanas", x = as_datetime("2018-10-23 12:00:00"), y = 375, color = "grey30", size = 6, hjust = 0, family = "Didact Gothic Regular") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_continuous(breaks = seq(100, 400, 50), limits = c(90, 400)) +
  labs(title = str_wrap(str_to_upper("producción semanal de gasolina a nivel nacional, 6/4/2018 al 4/1/2019"), width = 80),
       subtitle = str_wrap("La línea gris indica la producción semanal de gasolina. La línea roja muestra el promedio móvil de cuatro semanas.", width = 140),
       x = NULL,
       y = "Miles de barriles diarios\n",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 10 de enero de 2018.\n La serie comienza el 6 de abril de 2018 porque ese es el primer mes para el cual la base de datos tiene información para todas las semanas.") +
  tema +
  theme(plot.title = element_text(size = 26, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(filename = "produccion_semanal_nacional_gasolina_2018.png", path = "03_graficas", width = 15, height = 10, dpi = 200) 



### Gráfica: producción semanal de gasolina a nivel regional entre el 6 de abril de 2018 y el 4 de enero de 2019 ----
bd_produccion %>% 
  filter(tipo == "Gasolina",
         fecha >= as_datetime("2018-03-31 12:00:00")) %>% 
  group_by(fecha, region) %>% 
  summarise(produccion_semanal = sum(valor)) %>% 
  ungroup() %>% 
  arrange(region, fecha) %>%  
  group_by(region) %>% 
  mutate(promedio_movil = rollmean(x = produccion_semanal, 4, align = "right", fill = NA)) %>%
  ungroup() %>% 
  ggplot() +
  geom_line(aes(fecha, produccion_semanal, group = 1), size = 1, alpha = 0.7, color = "grey50") +
  geom_line(aes(fecha, promedio_movil, group = 1), size = 2, alpha = 0.9, color = "salmon") +
  scale_x_datetime(breaks = seq(as_datetime("2018-04-06 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "2 week"), limits = c(as_datetime("2018-03-20 12:00:00"), as_datetime("2019-01-14 12:00:00")), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 125)) +
  facet_wrap(~ region, scale = "free_x") +
  labs(title = str_wrap(str_to_upper("producción semanal de gasolina a nivel regional, 6/4/2018 al 4/1/2019"), width = 80),
       subtitle = str_wrap("Las líneas grises indican la producción semanal de gasolina. Las líneas rojas muestran el promedio móvil de cuatro semanas.", width = 140),
       x = NULL,
       y = "Miles de barriles diarios\n",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 10 de enero de 2018.\nLa serie comienza el 6 de abril de 2018 porque ese es el primer mes para el cual la base de datos tiene información para todas las semanas.") +
  tema +
  theme(plot.title = element_text(size = 35, face = "bold", margin = margin(10,0,20,0), family="Trebuchet MS Bold", color = "grey25"),
        plot.subtitle = element_text(size = 24), 
        plot.caption = element_text(size = 22), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.title.y = element_text(size = 20),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 22)) +
  ggsave(filename = "produccion_semanal_region_gasolina_2018.png", path = "03_graficas", width = 20, height = 15, dpi = 200)
