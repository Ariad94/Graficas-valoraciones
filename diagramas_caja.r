### Diagramas de cajas

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de cajas de una variable por factores
# Para UN, UT

programadores %>% ggplot(aes(UN, FINAL)) + geom_boxplot(fill = c("yellowgreen","sandybrown")) +
 xlab("")+ylab("")+ggtitle("Puntuaciones Finales: \nCuartiles, máximo, mínimo y rango intercuartílico") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0)) 

### ### Diagrama de cajas de una variable por factores y por colores
# Para empleado-valorador, valorador-aprobador

Factor.Empleado <- rep(as.character(programadores$Nombre.Empleado), 12)
Puntuaciones <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5,programadores$CN1,programadores$CN2,
 programadores$F1,programadores$F2,programadores$F3,programadores$F4,programadores$F5)
Valoradores <- rep(as.character(programadores$Nombre.Valorador),12)

datos <- data.frame(Factor.Empleado, Valoradores, Puntuaciones)

datos %>% mutate(Factor.Empleado = reorder(Factor.Empleado, Puntuaciones, FUN = median)) %>%
 ggplot( mapping = aes(Factor.Empleado, Puntuaciones, fill = Valoradores)) + geom_boxplot() +
 xlab("")+ylab("")+ggtitle("Puntuaciones por empleado: \nCuartiles, máximo, mínimo y rango intercuartílico") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0)) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="bottom", 
 legend.text = element_text(size = 8)) 

colnames(programadores)[6] <- "Aprobador1"

programadores %>% mutate(Nombre.Valorador = reorder(Nombre.Valorador, FINAL, FUN = median)) %>%
 ggplot(aes(Nombre.Valorador, FINAL, fill = Aprobador1))+ geom_boxplot() +
 xlab("")+ylab("")+ggtitle("Puntuaciones Finales por valorador: \nCuartiles, máximo, mínimo y rango intercuartílico") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0)) +
 theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

### ### Diagrama de cajas de variables variables en el mismo grafico
# Para FINAL, Modulo 1, Modulo 2 y Modulo 3

Etiquetas <- c(rep("Final",55), rep("Competencias",55), rep("Conocimientos",55), rep("Funciones",55))
Puntuaciones <- c(programadores$FINAL, programadores$M1, programadores$M2, programadores$M3)

datos <- data.frame(Etiquetas, Puntuaciones)

opuesto <-  function(x){return(-median(x))}

datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = opuesto)) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(fill = c("darkviolet","palegreen","sandybrown", "paleturquoise1")) +
 labs(title="Puntuaciones Finales, Competencias, Conocimientos y Funciones", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=12, hjust=0)) 




















