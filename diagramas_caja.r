### Diagramas de cajas

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de cajas de una variable 
#Para FINAL

programadores %>% mutate(Etiquetas = rep("FINAL",55)) %>% ggplot(aes(Etiquetas, FINAL)) + geom_boxplot(fill = "darkviolet", alpha = 0.8) +
 labs(title="Puntuaciones Finales", subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico", x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0)) 


### ### Diagrama de cajas de una variable por factores
# Para UN, UT

programadores %>% ggplot(aes(UT, FINAL)) + geom_boxplot(aes(fill = UT)) +
 xlab("")+ylab("")+ggtitle("Puntuaciones Finales: \nCuartiles, máximo, mínimo y rango intercuartílico") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0)) 


### ### Diagrama de cajas de una variable por factores + facet_grid
# UT

Etiquetas <- c(rep("Puntuaciones finales",55), rep("Competencias", 55), rep("Conocimientos",55), rep("Desempeño",55))
Puntuaciones<- c(programadores$FINAL, programadores$M1, programadores$M2, programadores$M3)
Grupo <- rep(programadores$UT, 4)

datos <- data.frame(Etiquetas, Grupo, Puntuaciones)

datos %>% mutate(Grupo = reorder(Grupo, Puntuaciones, FUN = median))%>%
 ggplot() + geom_boxplot(aes(Grupo, Puntuaciones, fill = Etiquetas)) +
 xlab("")+ylab("")+ labs(title = "Cuartiles, máximo, mínimo y rango intercuartílico") +
 facet_grid(Etiquetas~.) +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=15), legend.position="none")

### ### Diagrama de cajas de una variable por factores + facet_grid
# UN

Etiquetas <- c(rep("Puntuaciones finales",55), rep("Competencias", 55), rep("Conocimientos",55), rep("Desempeño",55))
Puntuaciones<- c(programadores$FINAL, programadores$M1, programadores$M2, programadores$M3)
Grupo <- rep(programadores$UN, 4)

datos <- data.frame(Etiquetas, Grupo, Puntuaciones)

datos %>% mutate(Grupo = reorder(Grupo, Puntuaciones, FUN = median))%>%
 ggplot() + geom_boxplot(aes(Grupo, Puntuaciones, fill = Etiquetas)) +
 xlab("")+ylab("")+ labs(title = "Cuartiles, máximo, mínimo y rango intercuartílico") +
 facet_grid(Etiquetas~.) +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=15), legend.position="none")



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
# Modulo 1, Modulo 2 y Modulo 3

Etiquetas <- c(rep("Competencias",55), rep("Conocimientos",55), rep("Funciones",55))
Puntuaciones <- c(programadores$M1, programadores$M2, programadores$M3)

datos <- data.frame(Etiquetas, Puntuaciones)

opuesto <-  function(x){return(-median(x))}

datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = opuesto)) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(fill = c("palegreen","sandybrown", "paleturquoise1")) +
 labs(title="Puntuaciones de los módulos de competencias, conocimientos y funciones", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0)) 

# Competencias

Etiquetas <- c(rep("Calidad y mejora continua",55), rep("Pensamiento analítico",55), rep("Capacidad de aprendizaje",55),
 rep("Solución de problemas ",55), rep("Competencia digital",55))
Puntuaciones <- c(programadores$C1II, programadores$C2II, programadores$C3II, programadores$C4II, programadores$C5II)

datos <- data.frame(Etiquetas, Puntuaciones)


datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = median)) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(aes(fill = Etiquetas)) +
 labs(title="Puntuaciones por competencias", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), legend.position = "none") 


# Conocimientos

Etiquetas <- c(rep("Lenguajes de programación",55), rep("Herramientas de desarrollo (id)",55))
Puntuaciones <- c(programadores$CN1, programadores$CN2)

datos <- data.frame(Etiquetas, Puntuaciones)


datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = median)) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(aes(fill = Etiquetas)) +
 labs(title="Puntuaciones por conocimientos", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), legend.position = "none") 


# Funciones

Etiquetas <- c(rep("Programar aplicaciones",55), rep("Manejar bases de datos",55), rep("Asesorar a los usuarios",55), rep("Documentar los módulos",55),
 rep("Aplicar las normas",55))
Puntuaciones <- c(programadores$F1, programadores$F2, programadores$F3, programadores$F4, programadores$F5)

datos <- data.frame(Etiquetas, Puntuaciones)


datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = median)) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(aes(fill = Etiquetas)) +
 labs(title="Puntuaciones por funciones", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), legend.position = "none") 

















