### Histogramas de frecuencias

library(ggplot2)
library(dplyr)
library(ggthemes)


### ### Histograma de frecuencias apiladas

Etiquetas <- c(rep("Competencias",55), rep("Conocimientos",55),rep("Funciones",55))
Puntuaciones <- c(programadores$M1, programadores$M2, programadores$M3)

datos <- data.frame(Etiquetas, Puntuaciones)

datos  %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limit=c(0,50)) + geom_histogram(bins=10, color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= 
 "Número de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Módulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=10, hjust=0))


### ### Histograma de frecuencias 

programadores  %>% ggplot(aes(FINAL)) + scale_x_continuous(limit=c(0,100), breaks = c(0,10,20,30,40,50,60,70,80,90,100)) + 
 geom_histogram(bins=10, color = "black", fill = "darkviolet", alpha=0.7) + stat_bin(bins = 10, geom="text", aes(label=..count..), vjust = -0.6) +
 geom_vline(aes(xintercept=78), col = "dodgerblue4", size = 1, linetype = "longdash") +
 xlab("Puntuaciones finales")+ylab("Número de empleados")+ labs(title ="Puntuaciones finales", subtitle= 
 "Número de empleados por intervalo de puntuaciones") + theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=10, hjust=0)) 

### ### Histograma de frecuencias facet_grid

Nombre.Modulos <- c(rep("Competencias", 55), rep("Conocimientos", 55), rep("Funciones", 55))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)
Nivel.Requerido <- c(rep(30,55), rep(30,55),rep(30,55))

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Modulos)) + 
 scale_x_continuous(limit=c(0,100), breaks = c(0,10,20,30,40,50,60,70,80,90,100)) +
 geom_histogram( fill = Nombre.Modulos, bins = 10, alpha = 0.25, color = "black") + 
 stat_bin(bins = 10, geom="text", aes(label=..count..), vjust = -0.5) +
 geom_vline(aes(xintercept=Nivel.Requerido), col = "dodgerblue4", alpha = 0.7, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Modulos~.) +
 labs(title="Competencias, conocimientos y funciones ", subtitle="Número de empleados por intervalos de puntuaciones",
 x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=12, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=10), legend.position="none")
