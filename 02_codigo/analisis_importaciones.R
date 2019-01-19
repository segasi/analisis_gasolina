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

# Fuente: Sistema de Información Energetica | http://sie.energia.gob.mx/bdiController.do?action=cuadro&cvecua=SSHDGPC22. Por default, el SIE muestra los datos de 2018. Para ampliar la serie especificamos el rango temporal usando el botón de "Opciones".

nom_var <- read_excel("01_datos/inqzalqdqy_SSHDGPC22_18012019_10_37.xls", range = "A8:CG8") # Importar nombres de variables

bd_imports <- read_excel("01_datos/inqzalqdqy_SSHDGPC22_18012019_10_37.xls", range = "A19:CG20", col_names =F) # Importar renglones correspondientes a importación de gasolina. Solo importamos los renglones correspondientes a Pemex y Otras empresas, pero no el total


### Transformar datos ----

# Agregar nombres de columnas ----
colnames(bd_imports) <- colnames(nom_var)
bd_imports

# Transformar estructura de base de datos -----
bd_imports <- 
  bd_imports %>% 
  rename(fuente = "..1") %>% # Renombrar primera columna
  gather(key = mes_año,
         value = importaciones,
         -fuente)


# Generar columna de fecha ----
bd_imports <- 
  bd_imports %>% 
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



# En la columna "importaciones", convertir N/D en NAs y cambiar tipo de variable a numeric ----
bd_imports <- 
  bd_imports %>% 
  mutate(importaciones = ifelse(importaciones == "N/D", NA, importaciones),
         importaciones = as.numeric(importaciones))


### Gráfica: importaciones mensuales 2013-2018 ----
bd_imports %>% 
  filter(año > 2012, 
         fuente == "PEMEX") %>% 
  mutate(importaciones = ifelse(fecha == as_date("2018-12-01"), 559, importaciones),
         color_2018 = ifelse(año == 2018, "2018", "Otro")) %>% 
  group_by(mes_texto) %>% 
  mutate(importaciones_promedio = mean(importaciones, na.rm = T)) %>% 
  ungroup() %>%  
  ggplot() +
  geom_line(aes(mes_texto, importaciones, group = año, color = color_2018), size = 1, alpha = 0.9) +
  geom_line(aes(mes_texto, importaciones_promedio, group = 1), color = "salmon", size = 2, alpha = 0.9) +
  scale_color_manual(values = c("steelblue", "grey80")) +
  annotate(geom = "segment", x = 1.2, xend = 1.8, y = 685.5, yend = 685.5, color = "salmon", size = 2, alpha = 0.9) +
  annotate(geom = "text", label = "Promedio de importaciones mensuales", x = 2, y = 685.5, color = "grey30", size = 6, hjust = 0, family = "Didact Gothic Regular") +
  annotate(geom = "segment", x = 1.2, xend = 1.8, y = 685.5, yend = 685.5, color = "salmon", size = 2, alpha = 0.9) +
  annotate(geom = "text", label = "2018", x = 2, y = 665.5, color = "grey30", size = 6, hjust = 0, family = "Didact Gothic Regular") +
  annotate(geom = "segment", x = 1.2, xend = 1.8, y = 665.5, yend = 665.5, color = "steelblue", size = 2, alpha = 0.9) +
  labs(title = str_wrap(str_to_upper("importaciones mensuales de gasolina a nivel nacional, 2013-2018*"), width = 80),
       subtitle = str_wrap("Las líneas grises indican el volumen de importaciones mensuales de gasolina a lo largo del año correspondiente. La línea roja representa el promedio de importaciones de gasolina para ese mes en el período analizado. La línea azul resalta los valores de 2018", width = 130),
       x = NULL,
       y = "Miles de barriles diarios\n",
       caption = "\nJorge A. Castañeda / @jorgeacast / Sebastián Garrido de Sierra / @segasi / Fuente: SIE, url: bit.ly/2RBtZlu. Consultado el 17 de enero de 2018. *La cifra de diciembre de 2018 es preliminar y proviene de la información publicada por la Secretaría de Energía.") +
  tema +
  theme(legend.position = "none") +
  ggsave(filename = "importaciones_mensual_gasolina_año_2012_2017.png", path = "03_graficas", width = 15, height = 10, dpi = 200) 




