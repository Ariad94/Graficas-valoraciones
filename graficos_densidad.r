### Gr�ficos de densidad

library(ggplot2)
library(dplyr)
library(ggthemes)

### ### Densidades con facet_grid 

#Competencias

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento anal�tico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Soluci�n de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1II,programadores$C2II,programadores$C3II,programadores$C4II,programadores$C5II)

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias)

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), hjust = -0.5) +  
 facet_grid(Nombre.Competencias~.) +
 labs(title="Competencias", subtitle="N�mero de empleados por puntuaci�n", x = "Puntuaciones", y = "N�mero de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=10), legend.position="none")

#Conocimientos

Nombre.Conocimientos <- c(rep("Lenguajes de programaci�n", 55), rep("Herramientas de desarrollo (id)", 55))
Puntuaciones.Conocimientos <- c(programadores$CN1,programadores$CN2)

datos <- data.frame(Nombre.Conocimientos, Puntuaciones.Conocimientos)

datos %>% ggplot(aes(Puntuaciones.Conocimientos, ..count.., fill = Nombre.Conocimientos)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), hjust = -0.5) +  
 facet_grid(Nombre.Conocimientos~.) +
 labs(title="Conocimientos", subtitle="N�mero de empleados por puntuaci�n", x = "Puntuaciones", y = "N�mero de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=15), legend.position="none")

#Competencias

Nombre.Funciones <- c(rep("Programar aplicaciones", 55), rep("Manejar bases de datos", 55), rep("Asesorar a los usuarios", 55),
 rep("Documentar los m�dulos", 55), rep("Aplicar las normas", 55))
Puntuaciones.Funciones <- c(programadores$F1,programadores$F2,programadores$F3,programadores$F4,programadores$F5)

datos <- data.frame(Nombre.Funciones, Puntuaciones.Funciones)

datos %>% ggplot(aes(Puntuaciones.Funciones, ..count.., fill = Nombre.Funciones)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), hjust = -0.5) +  
 facet_grid(Nombre.Funciones~.) +
 labs(title="Funciones", subtitle="N�mero de empleados por puntuaci�n", x = "Puntuaciones", y = "N�mero de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=10), legend.position="none")



### ### Densidades con facet_grid y nivel requerido
#Competencias

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento anal�tico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Soluci�n de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5)
Nivel.Requerido <- c(rep(3,55), rep(3,55),rep(3,55),rep(4,55),rep(3,55))

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), hjust = -0.5) +
 geom_vline(aes(xintercept=Nivel.Requerido), col = "dodgerblue4", alpha = 0.7, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Competencias~.) +
 labs(title="Competencias", subtitle="N�mero de empleados por puntuaci�n", x = "Puntuaciones", y = "N�mero de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=12, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=10), legend.position="none")

### ### Densidad de competencias apiladas

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento anal�tico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Soluci�n de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5)

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias)
 
datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + 
 geom_density( alpha = 0.6, bw = 0.5, color = "white", position="stack") + 
 xlab("Puntuaciones")+ylab("N�mero de empleados")+ labs(title ="Competencias", subtitle= "Comparaci�n de n�mero de empleados por puntuaci�n") +
 scale_fill_discrete(name = "Competencias") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=10, hjust=0),
 strip.text = element_text(color = "dodgerblue4", size=9))

### ### Densidad de variables apiladas 
#Modulos

Nombre.Modulos <- c(rep("Competencias", 55), rep("Conocimientos", 55), rep("Funciones", 55))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)

opuesto <-  function(x){return(-median(x))}

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos)
 
datos %>% mutate(Nombre.Modulos = reorder(Nombre.Modulos, Puntuaciones.Modulos, FUN = opuesto)) %>% 
  ggplot(aes(Puntuaciones.Modulos, ..count.., fill = Nombre.Modulos)) + 
 geom_density( alpha = 0.6, bw = 0.7, color = "white", position="stack")+
 xlab("Puntuaciones")+ylab("N�mero de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= "N�mero de empleados por puntuaci�n") +
 scale_fill_discrete(name = "M�dulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0))



