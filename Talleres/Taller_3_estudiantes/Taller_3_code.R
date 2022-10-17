########################################################################/
#### Escuela de Negocios Universidad Adolfo Ibañez 2022 #####
##### Aproximaciones a las políticas públicas desde los datos ##########
########### Taller 3: Visualización de datos ######################
########################################################################/
# José Daniel Conejeros - jdconejeros@uc.cl
# Naim Bro -  naim.bro@gmail.com
# Material: https://github.com/JDConejeros/APPDD_UAI_V2022/tree/main/Talleres
# Programa del curso: https://naimbro.github.io/programa_diplomado_2022.html
########################################################################/

# En este taller nos enfocaremos:

# 1. Introducción de visualización de datos

# 2. Ajustar parámetros en ggplot

# 3. Tipos de gráficos y ejemplos concretos en ggplot

# Librerías de análisis
install.packages("rio") # Importar bases de datos
install.packages("tidyverse") # Librerías de procesamiento y visualización
install.packages("ggthemes") # Temas 
install.packages("RColorBrewer") # Colores
install.packages("corrplot") # Gráfico de correlación 
install.packages("GGally") # Panel de asociaciones
intall.packages("ggpubr")  # Unir plots
intall.packages("patchwork") # Unir plots
install.packages("stringr") # Procesamiento de texto
install.packages("lubridate") # Procesamiento de fecha
install.packages("gganimate") # Animación más info en: https://gganimate.com/
install.packages("gifski") # Funciones para animaciones

library(rio)
library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(corrplot)
library(GGally)
library(ggpubr) 
library(patchwork)
library(stringr)
library(lubridate)
library(gganimate)
library(gifski)



########################################################################/
# 1. Trabajo con bases de datos -------------
########################################################################/

# Veamos algunas configuraciones preliminares
rm(list=ls()) # Limpiamos la memoria

# Opciones de formato
options(scipen=999) # Desactiva la notación científica.
options(max.print = 99999999) # Nos permite ver más resultados en la consola.

# Leemos los datos (vamos a ocupar una extracción de spotify)
data_spotify <- rio::import("03_data/extraccion_spotify.csv")

# Realicemos unos ajustes preliminares a los datos 
glimpse(data_spotify)

# Modifiquemos la data
data_spotify <- data_spotify %>% 
  mutate(dur=duration_ms/60000, # Transforma la duración en minutos
         # Transformamos los valores en mayusculas y factores
         genero=factor(stringr::str_to_title(playlist_genero)), 
         subgenero=factor(stringr::str_to_title(playlist_subgenero))) 

# Algunas variables importantes: 
# Acousticness: numérica, medida de confianza de 0,0 a 1,0 de si la pista es acústica. 1.0 representa una alta confianza en que la pista es acústica.
# Danceability: numérica, la bailabilidad describe qué tan adecuada es una pista para bailar en función de una combinación de elementos musicales que incluyen tempo, estabilidad del ritmo, fuerza del ritmo y regularidad general. Un valor de 0,0 es menos bailable y 1,0 es más bailable.
# Duration_ms: numérico, la duración de la pista en milisegundos.
# Duration_min: Numérico, la duración de la pista en minutos.
# Energy: numérica, la energía es una medida de 0,0 a 1,0 y representa una medida perceptiva de intensidad y actividad. Por lo general, las pistas enérgicas se sienten rápidas, fuertes y ruidosas. Por ejemplo, el death metal tiene mucha energía, mientras que un preludio de Bach tiene una puntuación baja en la escala. Las características perceptivas que contribuyen a este atributo incluyen el rango dinámico, el volumen percibido, el timbre, la tasa de inicio y la entropía general.
# Explicit: categórico, ya sea que la pista tenga o no una letra explícita (verdadero = sí, lo tiene; falso = no, no O desconocido).
# Instrumentalness: numérica, predice si una pista no contiene voces. Los sonidos “Ooh” y “aah” se tratan como instrumentales en este contexto. Las pistas de rap o de palabras habladas son claramente "vocales". Cuanto más cerca esté el valor de instrumentalidad de 1,0, mayor será la probabilidad de que la pista no contenga contenido vocal. Los valores superiores a 0,5 pretenden representar pistas instrumentales, pero la confianza es mayor a medida que el valor se acerca a 1,0.
# Liveness: Numérico, detecta la presencia de una audiencia en la grabación. Los valores de vivacidad más altos representan una mayor probabilidad de que la pista se interprete en vivo. Un valor superior a 0,8 proporciona una gran probabilidad de que la pista esté activa.
# Loudness: numérica, sonoridad general de una pista en decibelios (dB). Los valores de sonoridad se promedian en toda la pista y son útiles para comparar la sonoridad relativa de las pistas. El volumen es la cualidad de un sonido que es el principal correlato psicológico de la fuerza física (amplitud). Los valores típicos oscilan entre -60 y 0 db.
# Mode: Numérico, el modo indica la modalidad (mayor o menor) de una pista, el tipo de escala de la que se deriva su contenido melódico. Mayor está representado por 1 y menor es 0.
# Popularity: numérica, la popularidad de una pista es un valor entre 0 y 100, siendo 100 la más popular. La popularidad se calcula mediante un algoritmo y se basa, en su mayor parte, en el número total de reproducciones que ha tenido la pista y cuán recientes son esas reproducciones.
# Speechiness: numérico, Speechiness detecta la presencia de palabras habladas en una pista. Cuanto más parecida a la voz sea la grabación (por ejemplo, programa de entrevistas, audiolibro, poesía), más cerca de 1,0 será el valor del atributo. Los valores superiores a 0,66 describen pistas que probablemente estén formadas en su totalidad por palabras habladas. Los valores entre 0,33 y 0,66 describen pistas que pueden contener tanto música como voz, ya sea en secciones o en capas, incluidos casos como la música rap. Los valores por debajo de 0,33 probablemente representen música y otras pistas que no sean de voz.
# Tempo: numérico, tempo general estimado de una pista en pulsaciones por minuto (BPM). En terminología musical, el tempo es la velocidad o ritmo de una pieza dada y se deriva directamente de la duración promedio del tiempo.
# Valence: numérica, medida de 0,0 a 1,0 que describe la positividad musical transmitida por una pista. Las pistas con una valencia alta suenan más positivas (p. ej., felices, alegres, eufóricas), mientras que las pistas con una valencia baja suenan más negativas (p. ej., tristes, deprimidas, enfadadas). 

########################################################################/
# 2. Argumentos básicos -------------
########################################################################/

# Datos:input de información para generar la figura. 
# Geometrías (geom_): forma geométrica que representaran los datos,
# Estética (aes()): estética de los objetos geométricos y estadísticos, 
# Escalas (scale): mapas entre los datos y las dimensiones estéticas, 
# Transformaciones estadísticas (stat_): resúmenes estadísticos de los datos
# Sistemas de coordenadas (coord_): mapear los datos del gráfico.
# Facetas:la disposición de los datos en una cuadrícula de gráficos.
# Temas (theme()): ajuste visuales del gráfico (incorpora variados elementos adicionales).

#install.packages("ggplot2")
library(ggplot2) # Librería con herramientas para visualizar datos
ggplot(data=data_spotify) # Fondo en blanco para comenzar a trabajar

# Lo más importante es enteder que la información se agrega por capas
ggplot(data=data_spotify, aes(x=dur, y=energy)) #Agrego ejes: capa2

# Sumamos capas
ggplot(data=data_spotify, aes(x=dur, y=energy)) + 
  geom_point()  #Agrego geometría: points. capa3

# También podemos operar agregando capas sobre un objeto 
g1 <- ggplot(data=data_spotify, aes(x=dur, y=energy)) #Guardo mi gráfico en un objeto
g1

# Agregamos la capa de puntos
g1 + geom_point()

# Agregamos capas
ggplot(data=data_spotify, aes(x=dur, y=energy)) + 
  geom_point() +  #Agrego geometría: points. capa3
  geom_line()  #Agrego geometría: lines. capa3

# geom_point() 
ggplot(data=data_spotify, aes(x=dur, y=energy))  + 
  geom_point(color="steelblue", shape="triangle", size=0.5, alpha=0.5)  # Ajusto parámetros de la capa

# geom_line()
ggplot(data=data_spotify, aes(x=dur, y=energy)) + 
  geom_line(color = "firebrick", linetype = "dotted", size = 0.3)  # Ajusto parámetros de la capa

# Podemos mejorar nuestra visualización
ggplot(data=data_spotify[data_spotify$playlist_subgenero=="reggaeton" & data_spotify$dur<=4,],
       aes(x=dur, y=energy))  + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") # Agregamos etiquetas

# Podemos agregar un filtro dentro de la figura 
data_spotify %>% filter(track.artist=="Wisin & Yandel") %>% 
  ggplot(aes(x=dur, y=energy)) +
  geom_point() +
  geom_smooth()

ggplot(data=data_spotify[data_spotify$track.artist=="Wisin & Yandel",], aes(x=dur, y=energy))  + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  geom_smooth(method = "lm") + # Agregar una línea de tendencia / Modifique el argumento loess
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") # Agregamos etiquetas

########################################################################/
# 3. Ajustando parámetros de un ggplot -------------
########################################################################/

## 3.1 Temas --------

# Podemos agregar temas
# Podemos dejar fijo un tema con el siguiente código:
theme_set(theme_bw())

# theme_bw()
# theme_classic()
# theme_light()
# theme_minimal()

# La función theme() nos permite mejorar múltiples opciones de visualización

ggplot(data=data_spotify, aes(x=dur, y=energy, color=factor(genero))) + 
  geom_point(size=0.2) + # Puedo ver lo que ocurre por grupos 
  scale_x_continuous(n.breaks = 10, limits = c(0,5)) + 
  labs(color = "Genero musical") + # Una etiqueta a la legenda
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") + # Agregamos etiquetas
  #theme_light(base_size=11) +
  #theme_classic(base_size=11) +
  #theme_minimal(base_size=11) +
  theme(plot.title = element_text(size = 15, face="bold"), # Título del gráfico
        axis.text.x=element_text(size=5), # Texto eje x
        axis.text.y=element_text(size=5), # Texto eje y
        axis.title.x=element_text(size=10), # Título x
        axis.title.y=element_text(size=10), # Título y
        legend.position = "top", # Posición de leyenda
        legend.title = element_text(size=10)) # Título de la leyenda 

## 3.2 Facetas ----

# facet_wrap(): matriz de paneles útil para variables discretas y continuas
# facet_grid(): matriz de paneles útil para dos variables discretas

ggplot(data=data_spotify, aes(x=dur, y=energy, color=factor(subgenero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  scale_x_continuous(n.breaks = 10) + 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") + # Agregamos etiquetas
  labs(color = "Subgenero") +
  theme_light() +
  theme(plot.title = element_text(size = 15, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10),
        legend.position = "right",
        legend.title = element_text(size=10),
        strip.text.y = element_text(angle = 0)
        ) +
  facet_grid(genero~., scales="fixed")
  #facet_wrap(~genero, ncol = 3, scales="fixed") 

## 3.3 Guardar gráficos  ----------------------------------------------------------------

# Podemos construir nuestro gráfico en un objeto
g1 <- ggplot(data=data_spotify, aes(x=dur, y=energy, color=factor(subgenero))) + 
  geom_point(size=0.5) + # Puedo ver lo que ocurre por grupos 
  scale_x_continuous(n.breaks = 10) + 
  labs(title = "Mi primer ggplot", x="Duración del tema (mins)", y="Indicador de Energía") + # Agregamos etiquetas
  labs(color = "Subgenero") +
  theme_light() +
  theme(plot.title = element_text(size = 15, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.x=element_text(size=10), 
        axis.title.y=element_text(size=10),
        legend.position = "right",
        legend.title = element_text(size=10)) +
  facet_wrap(~genero, nrow = 3) 

# Guardar gráficos (ajustar a ruta específica)
ggsave("04_output/mi_primer_plot.png", plot=g1)

########################################################################/
# 4. Tipos de gráficos -------------
########################################################################/

## 4.1 Histogramas ----------------------------------------------------------------

# Ejemplo para ver la densidad de una variable continua
# aplicamos geom_histogram y sus argumentos
ggplot(data=data_spotify, aes(x=dur)) +
  geom_histogram(aes(y=..density..),
                 position = "identity", binwidth = 0.1,
                 colour="black", fill="white") +
  scale_x_continuous(n.breaks=20) +
  labs(title="Distribución por género",
       x="Energía", y = "Densidad") +
  geom_density(col="red") + 
  facet_wrap(~genero, nrow = 3) +
  theme_bw()

# Veamos todas las áreas
caract <- colnames(select(data_spotify, danceability:dur, -duration_ms))
caract # Vector con el nombre de las características

data_spotify %>%
  select(c(subgenero, caract)) %>%
  pivot_longer(cols = caract) %>%
  ggplot(aes(x = value)) +
  geom_density(aes(color = subgenero), alpha = 0.5) +
  facet_wrap(~stringr::str_to_title(name), ncol = 3, scales = 'free') +
  labs(title = 'Características de las canciones por subgenero',
       x = NULL, y = "Densidad", color="Subgenero") +
  theme_light() +
  theme(plot.title = element_text(size = 10, face="bold"),
        axis.text.x=element_text(size=5),
        axis.text.y=element_text(size=5),
        axis.title.y=element_text(size=10),
        legend.position = "top")

## 4.2 Gráficos de Barras ----------------------------------------------------------------

# En general es mejor trabajar con datos resumidos pues hacen más eficiente la ejecusión de gráficos
tabla <- data_spotify %>% 
  group_by(subgenero) %>% 
  summarise(popu=mean(track.popularity, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(subgenero=ordered(subgenero, levels=c("Classic Rock", "Hard Rock",
                                               "Hip Hop", "Trap",
                                               "Reggaeton", "Tropical")))
tabla 
levels(tabla$subgenero)

ggplot(tabla, aes(x=subgenero, y=popu, fill=subgenero)) +         
  geom_bar(stat = "identity", position = "dodge", width = 0.75) +
  scale_y_continuous(limits = c(0,100),
                     n.breaks = 5) +
  geom_text(aes(label=format(round(popu,1), decimal.mark=",")), 
            vjust=-0.75,
            size=3,
            color="gray40",
            fontface="bold") +
  labs(title = "Gráfico de popularidad según subgenero",
       subtitle = "Extracciones de Spotify",
       caption = "Elaboración propia.",
       y="Índice de popularidad") +
  scale_fill_brewer(palette = "Set2") +
  geom_vline(xintercept = 2.5, col='red', lwd=0.5, linetype="dotted") + 
  geom_vline(xintercept = 4.5, col='red', lwd=0.5, linetype="dotted") + 
  theme_bw() + 
  theme(plot.title = element_text(size = 10, face="bold"),
        plot.subtitle = element_text(size = 8, color="gray40"),
        plot.caption = element_text(size = 8, color="gray40"),
        axis.text.x=element_text(size=8),
        axis.text.y=element_text(size=8),
        axis.title.y=element_text(size=10),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.grid = element_line(color="white"),
        legend.position = "none")

## 4.3 Gráficos de torta ----------------------------------------------------------------

# Construimos una tabla con las proporciones
pie_info <- data_spotify %>%
  filter(!is.na(subgenero)) %>% 
  dplyr::group_by(subgenero) %>%
  dplyr::arrange(subgenero) %>% 
  dplyr::summarize(n_all = n()) %>% 
  dplyr::mutate(prop=(n_all/sum(n_all)*100))

# Definimos colores
colores <- c("#0073C2FF", "#EFC000FF", "#868686FF",
             "#0073C2FF", "#EFC000FF", "#868686FF")

# install.packages("RColorBrewer")
library(RColorBrewer)
RColorBrewer::display.brewer.all()

ggplot(pie_info, aes(x = "", y = prop, fill = subgenero)) +
  geom_bar(stat = "identity", color = "white", width = 1) +
  coord_polar(theta = "y", start = 0) + # Nos permite generar una circunferencia 
  geom_text(aes(x=1, label = paste0(round(prop,1), "%")), position = position_stack(vjust = .5), 
            color = "white", size=5, fontface = "bold") +
  labs(title="Gráfico de torta", caption = "Fuente: Elaboración propia") +
  scale_fill_manual(values = colores, name="Subgenero") +
  theme_void() 

# Es más útil trabajar en un formato tipo rosquilla
# Generamos los datos 

 rosquilla_info  <- data_spotify  %>%                               
  filter(!is.na(subgenero)) %>% 
  group_by(subgenero) %>%
  dplyr::summarize(count= n()) %>%
  mutate(categoria=as.factor(subgenero)) %>%
  mutate(fraction=count/sum(count)) %>%
  mutate(percent=round((fraction*100),2)) %>%
  mutate(ymax=cumsum(fraction)) %>% # Nueva variable con la suma acumulada
  mutate(ymin=c(0, head(ymax, n=-1))) %>%
  mutate(labelPosition=(ymax + ymin)/2) %>% # Etiqueta de los valores
  mutate(label= paste0(round(percent,1), "%"))

ggplot(rosquilla_info, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=forcats::fct_inorder(categoria))) +
  geom_rect() +
  ggrepel::geom_label_repel(x=3.5, aes(label = paste(percent,"%"), x = 100, y = (ymin + ymax)/2),inherit.aes = F, show.legend = F, size = 4) +
  scale_fill_brewer(palette=1) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  labs(fill=" ", size= 3,
       title="Distribución por subgenero")+
  theme_void() +
  theme(legend.position = "top", 
        legend.text=element_text(size=10),
        plot.title = element_text(hjust = 0.15, vjust = 3))

## 4.4 Box-Plots ----------------------------------------------------------------

# install.packages("ggthemes")
library(ggthemes)

RColorBrewer::display.brewer.all()
paleta <- RColorBrewer::brewer.pal(6, "Set1")

box <- ggplot(data=data_spotify, aes(x=subgenero, y=dur, fill=subgenero)) + 
  geom_boxplot(width=0.5, alpha=0.75) + 
  scale_fill_manual(values=paleta) +
  labs(title = "Gráficos de caja según género",
       subtitle = "Extracciones de spotify",
       caption = "Elaboración propia",
       y="Duración") +
  facet_wrap(~genero, scales = "free_x") + 
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank())
box

box + geom_jitter(width = .3, alpha = .3)
box + geom_violin(fill = "gray80", size = 1, alpha = .5)

# Combinemos
box + geom_violin(fill = "gray80", size = 1, alpha = .5) +
  geom_jitter(alpha = .05, width = .1) +
  coord_flip()

## 4.5 Correlaciones ----------------------------------------------------------------

# Preparamos los datos
tabla <- data_spotify %>%
  filter(genero=="Rock") %>% # Filtramos por el género en particular
  select(caract) %>% # Seleccionamos las características de interés
  scale() %>% # dejamos todo en puntajes z
  cor() # Estimamos correlaciones 

tabla

# La librería que permite hacer gráficos de correlaciones es corrplot
#install.packages("corrplot")
library(corrplot)

# Vamos a guardar nuestro gráfico en un objeto
corrplot(tabla, method = 'color',
         order = 'hclust',
         type = 'upper',
         diag = FALSE,
         addCoef.col = "grey30",
         number.cex = 0.6,
         tl.col = "gray40",
         tl.srt=45,
         tl.cex = 0.7,
         mar = c(rep(0.25, 4)))

## 4.6 Paneles de asociación -----

# Ajustamos los datos
data <- data_spotify %>%
  filter(genero=="Rock") %>% # Filtramos por el género en particular
  select(caract) %>% # Seleccionamos las características de interés
  scale() %>%  # dejamos todo en puntajes z
  as.data.frame()

# Ajustamos el parámetro que define la línea de tendencia
smooth <- function(data, mapping, method = "lm", ...) {
  p <- ggplot(data = data, mapping = mapping) +
    geom_point(colour = "gray50",  alpha = 0.3, size=0.5) +
    geom_smooth(method = method, color = "gray30", size=0.5, ...)
  p
}

library(GGally)
# Generamos nuestro plot
ggpairs(data, method = "pearson",
        lower = list(continuous = wrap(smooth, alpha = 0.3, size=0.5)),
        #lower = list(continuous = wrap("points", alpha = 0.3, size=0.5)),
        diag = list(continuous = wrap("barDiag", bins = 24, fill = "black", color="gray90", alpha=0.3))) +
  theme_stata(scheme = "s1mono") +
  theme(axis.text = element_text(size=6),
        panel.grid.major = element_blank(), 
        panel.grid = element_blank(),
        strip.background = element_rect(fill="white", color="white"), 
        strip.text.x = element_text(size=11, hjust = 0),
        strip.text.y = element_text(size=10, hjust = 0))

# Guardamos nuestros resultados
ggsave(filename = "04_output/Panel_asociaciones.png",
       res = 300,
       width = 25,
       height = 25,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)

########################################################################/
# 5. Unir gráficos -------------
########################################################################/

# Podemos unir varios gráficos en una misma figura, para esto podemos usar: 
# intall.packages("ggpubr") # o también
# intall.packages("patchwork")

library(ggpubr) # Mayor detalle en: https://rpkgs.datanovia.com/ggpubr/reference/ggarrange.html
ggarrange(g1, box, nrow = 2)
ggarrange(g1, box, ncol = 2)

library(patchwork) # Mayor detalle en: https://patchwork.data-imaginist.com/
g1+box

g1|box

g1/box

# Para todos los casos necesitamos los objetos guardados (gg)

########################################################################/
# 6. Ejemplo extrayendo datos de COVID -------------
########################################################################/

#install.packages("stringr")
#install.packages("lubridate")
library(stringr)
library(lubridate)

# Cargamos la BBDD: 
# Reporte de casos nuevos COVID. Datos en formato ancho:
data <- read.csv("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto13/CasosNuevosCumulativo.csv")
data %>% glimpse()
View(data)

# Ajustamos como serie de tiempo
data_figura <- data %>% slice(-17) %>% 
  pivot_longer(cols=!Region, names_to="date", values_to = "casos") %>% # Pivoteamos la BBDD  
  mutate(date=str_replace_all(date, c("[X]"="", "[.]"="-")),           # Reemplazamos valores a partir de una expresión regular
         date=lubridate::ymd(date))                                    # Ajustamos la fecha

# Verifiquemos la cantidad de filas
data_figura %>% head(n=30)
unique(data_figura$Region) # Veamos los valores para las regiones

reg <- unique(data_figura$Region)
reg

# Graficamos
# Podemos fijar una paleta de colores:  http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
display.brewer.all(colorblindFriendly = TRUE)
colores <- brewer.pal(n=4, name="Set2")

g1 <- data_figura %>% 
  ggplot(aes(x=date, y=casos, color=Region)) +
  geom_line() +
  #scale_colour_manual(values=paleta) +
  #scale_color_viridis(discrete = TRUE, option = "D") +
  scale_x_date(date_labels = "%b %Y",
               date_breaks = "12 week", 
               guide = guide_axis(n.dodge = 2)) +
  labs(x="Semanas del año", y = "Casos de Covid-19",
       color = "Región")  +
  geom_vline(xintercept=ymd("2021-02-01"), linetype="dotdash") +
  #annotate("text", x = ymd("2021-02-01")+70, y = 5, label = 'Inicio vacunación', size=3, fontface="bold") + 
  facet_wrap(~factor(Region, levels=reg), ncol = 4, scale = "free") +
  geom_label(aes(x = ymd("2021-02-01"), y = 10, label = "Inicio vacunación"),
             color = "white", size = 2, hjust = "middle", label.size = NA, fill = "#d36f6f") + 
  theme_light(base_size = 12) + 
  theme(axis.text=element_text(size=8),
        axis.text.x = element_text(size=6),
        #axis.text.x = element_text(angle=45, vjust = 0.5, hjust=0.05),
        legend.position="none",
        strip.text = element_text(size = 10, face = "bold", color = "gray40"),
        strip.background = element_rect(fill="white", colour="gray", linetype="dashed"),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour="grey", linetype="dotted", size=0.3)) 

g1 # Revisamos nuestro resultado

# Podemos guardar la figura con varios ajustes de formato
ggsave(plot = g1,
       filename = "04_output/Visualización_covid.png",
       res = 300,
       width = 30,
       height = 20,
       units = 'cm',
       scaling = 0.7,
       device = ragg::agg_png)

########################################################################/
# 7. Animar figuras -------------
########################################################################/

# Podemos crear un gif animado a partir de la librería gganimate
install.packages("gganimate") # más info en: https://gganimate.com/
library(gganimate)
install.packages("gifski")
library(gifski)

# Construímos la animación
ganimado <- g1 + transition_reveal(date) + 
  labs(title = "Casos de Covid-19 para el día: {frame_along}")  + 
  ease_aes('linear')

# Generamos la animación
animate(ganimado, width = 1200, height = 900, fps = 30, duration = 30, 
        rewind = FALSE, renderer = gifski_renderer("04_output/gganim_covid.gif"))


########################################################################/
# FIN TALLER 3 -------------
########################################################################/
