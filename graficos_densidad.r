### Gráficos de densidad

library(ggplot2)
library(dplyr)
library(ggthemes)

### ### Densidad por competencias, conocimientos o funciones 

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento analítico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Solución de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5)
Nivel.Requerido <- c(rep(3,55), rep(3,55),rep(3,55),rep(4,55),rep(3,55))

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), hjust = -0.5) +
 geom_vline(aes(xintercept=Nivel.Requerido), col = "dodgerblue4", alpha = 0.7, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Competencias~.) +
 labs(title="Competencias", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=12, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=10), legend.position="none")

### ### Densidad de competencias apiladas

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento analítico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Solución de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5)

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias)
 
datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + 
 geom_density( alpha = 0.6, bw = 0.5, color = "white", position="stack") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias", subtitle= "Comparación de número de empleados por puntuación") +
 scale_fill_discrete(name = "Competencias") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=10, hjust=0),
 strip.text = element_text(color = "dodgerblue4", size=9))

### ### Densidad de Módulos apiladas y rango eje Y fijo

Nombre.Modulos <- c(rep("Competencias", 55), rep("Conocimientos", 55), rep("Funciones", 55))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos)
 
datos %>% ggplot(aes(Puntuaciones.Modulos, ..count.., fill = Nombre.Modulos)) + 
 geom_density( alpha = 0.6, bw = 0.7, color = "white", position="stack") + scale_y_continuous(limit=c(0,20))+
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= "Número de empleados por puntuación") +
 scale_fill_discrete(name = "Módulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=10, hjust=0),
 strip.text = element_text(color = "dodgerblue4", size=9))


