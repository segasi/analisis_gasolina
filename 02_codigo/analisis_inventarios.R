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


### Gráfica: inventario semanal de gasolina en 75 terminales de almacenamiento en México, 2018 y 2019 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento") %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1) +
  annotate(geom = "text", x = as_datetime("2018-12-22 00:00:00"), y = 67, label = "AMLO", fontface = "bold", size = 11, color = "grey90") +
  annotate(geom = "text", x = as_datetime("2018-11-22 00:00:00"), y = 67, label = "EPN", fontface = "bold", size = 11, color = "grey90") +
  scale_fill_gradient(low = "white", high = "#ae052b", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10)) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de gasolina en 75 terminales de almacenamiento, 2018-2019"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más rojo el recuadro, mayor el inventario de gasolina en dicha semana. Los recuadros grises indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj.\nConsultado el 21 de enero de 2019. Debido al sesgo en la distribución del inventario de gasolina, usamos la\n versión logarítmica de esta variable.") +
  tema_hm +
  theme(legend.position = c(0.88, -0.16),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(filename = paste("niveles_semanales_de_inventarios_de_gasolinas_por_terminal_log", Sys.Date(), ".png", sep = "_"), path = "03_graficas/", width = 23, height = 18, dpi = 200)


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


### Inventario Total de Gasolina en las 75 TARs, por semana, 2018-2019. semanales de 2018 y 2019 ----
bd_semanal %>%
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento") %>% 
  group_by(semana) %>% 
  summarise(mb_nacional = sum(mb)) %>% 
  ungroup() %>%
  ggplot() +
  geom_line(aes(semana, mb_nacional, group = 1), color = "grey50", size = 1) +
  geom_vline(xintercept = as_datetime("2018-12-01 00:00:00"), color = "salmon", size = 0.8, linetype = 2) +
  annotate(geom = "text", x = as_datetime("2018-12-21 12:00:00"), y = 3125, label = "AMLO", fontface = "bold", size = 8, color = "grey50") +
  annotate(geom = "text", x = as_datetime("2018-11-15 00:00:00"), y = 3125, label = "EPN", fontface = "bold", size = 8, color = "grey50") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), limits = c(as_datetime("2018-01-01 12:00:00"), as_datetime("2019-01-09 12:00:00")), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_continuous(breaks = seq(1500, 3500, 250), label = comma) +
  labs(title = str_wrap(str_to_upper("inventario semanal total de gasolina en las 75 TARs, 2018-2019"), width = 55),
       subtitle = "Datos con corte al 4 de enero de 2019", 
       x = NULL,
       y = "Miles de barriles\n",
       caption =  "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 21\nde enero de 2019") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 30)) +
  ggsave(filename = paste("niveles_totales_de_inventarios_de_gasolinas_tars_por_semana", Sys.Date(), ".png", sep = "_"), path = "03_graficas/", width = 13, height = 9, dpi = 200)


### Inventario nacional de gasolina por tipo de terminal y semana, 2018-2019 ----
bd_semanal %>%
  filter(producto == "Gasolina") %>% 
  group_by(semana, tipo_de_terminal) %>% 
  summarise(mb_nacional = sum(mb)) %>% 
  ungroup() %>%
  ggplot() +
  geom_area(aes(semana, mb_nacional, fill = tipo_de_terminal)) +
  scale_fill_manual(values = c("grey50", "#ae052b")) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), limits = c(as_datetime("2018-01-01 12:00:00"), as_datetime("2019-01-09 12:00:00")), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_continuous(breaks = seq(0, 5000, 500), label = comma, expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("inventario nacional de gasolina por tipo de terminal y semana, 2018-2019"), width = 55),
       subtitle = "Datos con corte al 4 de enero de 2019", 
       x = NULL,
       y = "Miles de barriles\n",
       fill = "Tipo de terminal",
       caption =  "\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 21 de enero de 2019") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = c(0.1, 0.86),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 30)) +
  ggsave(filename = paste("nivel_nacional_de_inventarios_de_gasolinas_por_tipo_terminal_por_semana", Sys.Date(), ".png", sep = "_"), path = "03_graficas/", width = 13, height = 9, dpi = 200)


### Distribución proporcional del inventario nacional de gasolina en terminales de almacenamiento y marítimas por semana, 2018-2019 ----
bd_semanal %>%
  filter(producto == "Gasolina") %>% 
  group_by(semana, tipo_de_terminal) %>% 
  summarise(mb_nacional = sum(mb)) %>% 
  ungroup() %>%
  ggplot() +
  geom_area(aes(semana, mb_nacional, fill = tipo_de_terminal), position = "fill") +
  geom_hline(yintercept = seq(0, 1, 0.25), color = "white", alpha = 0.3, linetype = 2) +
  scale_fill_manual(values = c("grey50", "#ae052b")) +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), limits = c(as_datetime("2018-01-01 12:00:00"), as_datetime("2019-01-09 12:00:00")), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("distribución proporcional del inventario nacional de gasolina en terminales de almacenamiento y marítimas por semana, 2018-2019"), width = 55),
       subtitle = "Datos con corte al 4 de enero de 2019", 
       x = NULL,
       y = "Proporción\n",
       fill = "Tipo de terminal",
       caption =  "\nSebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj.\nConsultado el 21 de enero de 2019\n") +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 15),
        legend.position = c(0.88, -0.24),
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 30)) +
  ggsave(filename = paste("nivel_nacional_de_inventarios_de_gasolinas_por_tipo_terminal_por_semana_proporciones", Sys.Date(), ".png", sep = "_"), path = "03_graficas/", width = 13, height = 9, dpi = 200)


### Gráfica: número semanal de terminales de almacenamiento con cero barriles de inventario, por tipo de combustible ----
bd_semanal %>% 
  filter(producto %in% c("Gasolina", "Diésel"),
         tipo_de_terminal == "Almacenamiento", 
         mb == 00,
         semana > as_datetime("2018-11-01 12:00:00")) %>%
  group_by(semana, producto) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(etiqueta_grandes = ifelse(n > 3, n, ""),
         etiqueta_chicos = ifelse(n <= 3, n, "")) %>% 
  ggplot(aes(semana, n, fill = producto)) +
  geom_col(alpha = 0.9) +
  geom_text(aes(label = etiqueta_grandes), size = 6, color = "white", fontface = "bold", vjust = 1.7) +
  geom_text(aes(label = etiqueta_chicos), size = 6, color = "grey50", fontface = "bold", vjust = -0.6) +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 0.8, linetype = 3) +
  annotate(geom = "text", x = as_datetime("2018-12-10 12:00:00"), y = 19, label = "AMLO", fontface = "bold", size = 6, color = "grey50") +
  annotate(geom = "text", x = as_datetime("2018-11-29 00:00:00"), y = 19, label = "EPN", fontface = "bold", size = 6, color = "grey50") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  scale_fill_manual(values = c("grey10", "#ae052b", "#00B573", "#FF424E")) +
  facet_wrap(~ producto, ncol = 2) +
  labs(title = str_wrap(str_to_upper("número semanal de terminales de almacenamiento con inventario de cero barriles, por tipo de combustible"), width = 55),
       x = "\nFecha del corte semanal de información", 
       y = NULL,
       caption =  "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 21\nde enero de 2019") +
  tema +
  theme(panel.grid.major = element_blank(),
        axis.text.y = element_blank(), 
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = "none",
        strip.background = element_rect(color = "grey60", fill = "grey60"),
        strip.text = element_text(color = "white", size = 30)) +
  ggsave(filename = paste("num_tars_inventario_por_tipo_combustible_vacio", Sys.Date(), ".png", sep = "_"), path = "03_graficas/", width = 12, height = 8, dpi = 200) 


### Gráfica: inventario semanal de DIESEL en 75 terminales de almacenamiento en méxico, 2018 ----
bd_semanal %>% 
  filter(producto == "Diésel",
         tipo_de_terminal == "Almacenamiento") %>%
  group_by(terminal) %>% 
  mutate(suma_mb = sum(mb)) %>% 
  ungroup() %>%
  ggplot(aes(semana, fct_rev(terminal), fill = log(mb))) +
  geom_tile(color = "white") +
  geom_vline(xintercept = as_datetime("2018-12-04 00:00:00"), color = "black", size = 1) +
  annotate(geom = "text", x = as_datetime("2018-12-22 00:00:00"), y = 64, label = "AMLO", fontface = "bold", size = 11, color = "white") +
  annotate(geom = "text", x = as_datetime("2018-11-22 00:00:00"), y = 64, label = "EPN", fontface = "bold", size = 11, color = "white") +
  scale_fill_gradient(low = "white", high = "grey10", guide = guide_colorbar(barwidth = 12, nbins = 10), breaks = pretty_breaks(n = 10), na.value="salmon") +
  scale_x_datetime(breaks = seq(as_datetime("2018-01-05 12:00:00"), as_datetime("2019-01-04 12:00:00"), by = "1 week"), expand = c(0, 0),  date_labels = ("%b-%d")) +
  labs(title = str_wrap(str_to_upper("inventario semanal de diesel en 75 terminales de almacenamiento, 2018-2019"), width = 85), 
       subtitle = str_wrap("Cada recuadro representa el número de miles de barriles (log) en el inventario de cada terminal en la semana correspondiente. Mientras más negro el recuadro, mayor el inventario de diesel en dicha semana. Los recuadros rojos indican semanas en las que el inventario de la respectiva terminal de almacenamiento era de cero barriles.", width = 135),
       x = "\n", 
       y = NULL, 
       fill = "Miles de   \n barriles (log)   ",
       caption = str_wrap("\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 21 de enero de 2019. Debido al sesgo en la distribución del inventario de diesel, uso la versión logarítmica de esta variable.", width = 110)) +
  tema_hm +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  ggsave(filename = paste("niveles_semanales_de_inventarios_de_diesel_por_terminal_log", Sys.Date(), ".png", sep = "_"), path = "03_graficas", width = 23, height = 18, dpi = 200)



### Gráfica: Número de semanas que TARs tuvieron inventarios de cero barriles de gasolina en las seis últimas semanas de 2018 ----
bd_semanal %>% 
  filter(producto == "Gasolina",
         tipo_de_terminal == "Almacenamiento",
         semana >= as_datetime("2018-11-23 12:00:00"),
         mb == 0) %>% 
  group_by(terminal) %>% 
  summarise(num_semanas_cero = n()) %>% 
  ungroup() %>% 
  ggplot(aes(fct_reorder(terminal, num_semanas_cero), num_semanas_cero)) +
  geom_col(fill = "#ae052b", alpha = 0.9) +
  geom_text(aes(label = num_semanas_cero), size = 8, color = "white", fontface = "bold", vjust = 1.7) +
  scale_y_continuous(expan = c(0, 0)) +
  labs(title = str_wrap(str_to_upper("número de semanas que la terminal de almacenamiento ___ tuvo inventario de cero barriles de gasolina durante las siete semanas entre el 23/11/18 y el 04/01/19"), width = 65),
       x = "\n", 
       y = NULL,
       caption = str_wrap("\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SENER, url: bit.ly/2FsYvqj. Consultado el 21 de enero de 2019.", width = 150)) +
  tema +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggsave(filename = paste("num_semanas_inventario_gasolina_vacio_por_tar", Sys.Date(),".png", sep = "_"), path = "03_graficas/", width = 15, height = 10, dpi = 200)
