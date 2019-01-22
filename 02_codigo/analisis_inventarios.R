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

# Fuente: http://estadisticashidrocarburos.energia.gob.mx/Datos_semana.aspx
# bd_semanal <- read_excel("01_datos/EstadÃ_sticas EnergÃ©ticas.xlsx", sheet = "Inventarios") # Base con corte al 4 de enero de 2019

bd_semanal <- read_excel("01_datos/EstadÃ_sticas EnergÃ©ticas copy.xlsx", sheet = "Inventarios") # Base con corte al 4 de enero de 2019


### Transformaciones varias ----

# "Limpiar" nombres de variables ----
bd_semanal <- clean_names(bd_semanal)

# Cambiar tipo de variables y crear nuevas ----
bd_semanal <- bd_semanal %>% 
  mutate(mb = as.numeric(mb), # Transformar tipo de variable a numeric
         semana = dmy_hms(semana), # Transformar tipo de variable a dttm
         mes = fct_relevel(mes, "Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")) 


### Gráfica: inventario semanal de gasolina en 75 terminales de almacenamiento en México, 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento") %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1) +
  annotate(geom = "text", x = as_datetime("2018-12-18 00:00:00"), y = 67, label = "AMLO", fontface = "bold", size = 11, color = "grey90") +
  annotate(geom = "text", x = as_datetime("2018-11-22 00:00:00"), y = 67, label = "EPN", fontface = "bold", size = 11, color = "grey90") +
  scale_fill_gradient(low = "white", high = "#ae052b", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina en 75 terminales de almacenamiento en méxico, 2018"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj.\nConsultado el 10 de enero de 2018. Debido al sesgo en la distribución del inventario de gasolina, usamos la\n versión logarítmica de esta variable.") +
  tema_hm +
  theme(legend.position = c(0.88, -0.16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(filename = "niveles_semanales_de_inventarios_de_gasolinas_por_terminal_log.png", path = "03_graficas/", width = 23, height = 18, dpi = 200)


### Promedio anual de los inventarios nacionales semanales hasta el 30 de noviembre de 2018 ----
bd_semanal %>%
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento", 
         semana < as_date("2018-12-07")) %>% 
  # group_by(mes = floor_date(semana, "month")) %>% 
  group_by(semana) %>% 
  summarise(mb_nacional = sum(mb)) %>% 
  ungroup() %>%
  summarise(mb_promedio_semanal = mean(mb_nacional))  


### Inventarios nacionales semanales de gasolina, 2018 ----
bd_semanal %>%
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento") %>% 
  # group_by(mes = floor_date(semana, "month")) %>% 
  group_by(semana) %>% 
  summarise(mb_nacional = sum(mb)) %>% 
  ungroup() %>%
  mutate(mb_promedio_semanal = mean(mb_nacional)) %>% 
  print(n = Inf)
ggplot() +
  geom_line(aes(semana, mb_nacional, group = 1)) +
  geom_line(aes(semana, mb_promedio_semanal, group = 1), color = "red") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2018-12-28 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_continuous() +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 30))