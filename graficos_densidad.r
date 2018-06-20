### Gráficos de densidad

library(ggplot2)
library(dplyr)
library(ggthemes)

### ### Densidades con facet_grid 

#Competencias

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento analítico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Solución de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1II,programadores$C2II,programadores$C3II,programadores$C4II,programadores$C5II)

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias)

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + scale_y_continuous(limits=c(0,25)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.9) +  
 facet_grid(Nombre.Competencias~.) +
 labs(title="Competencias", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 360), legend.position="none", 
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))

#Conocimientos

Nombre.Conocimientos <- c(rep("Lenguajes de programación", 55), rep("Herramientas de desarrollo (id)", 55))
Puntuaciones.Conocimientos <- c(programadores$CN1,programadores$CN2)

datos <- data.frame(Nombre.Conocimientos, Puntuaciones.Conocimientos)

datos %>% ggplot(aes(Puntuaciones.Conocimientos, ..count.., fill = Nombre.Conocimientos)) + scale_y_continuous(limits = c(0,35)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.3) +  
 facet_grid(Nombre.Conocimientos~.) +
 labs(title="Conocimientos", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.position="none",
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))

#Funciones

Nombre.Funciones <- c(rep("Programar aplicaciones", 55), rep("Manejar bases de datos", 55), rep("Asesorar a los usuarios", 55),
 rep("Documentar los módulos", 55), rep("Aplicar las normas", 55))
Puntuaciones.Funciones <- c(programadores$F1,programadores$F2,programadores$F3,programadores$F4,programadores$F5)

datos <- data.frame(Nombre.Funciones, Puntuaciones.Funciones)

datos %>% ggplot(aes(Puntuaciones.Funciones, ..count.., fill = Nombre.Funciones)) + scale_y_continuous(limits = c(0,40)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") +
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.5) +  
 facet_grid(Nombre.Funciones~.) +
 labs(title="Funciones", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 0), legend.position="none",
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))



### ### Densidades con facet_grid y nivel requerido
#Competencias

Nombre.Competencias <- c(rep("Calidad y mejora continua", 55), rep("Pensamiento analítico", 55), rep("Capacidad de aprendizaje", 55),
 rep("Solución de problemas", 55), rep("Competencia digital", 55))
Puntuaciones.Competencias <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5)
Nivel.Requerido <- c(rep(2,55), rep(2,55),rep(2,55),rep(3,55),rep(2,55))

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + scale_y_continuous(limits=c(0,40)) + 
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.9, hjust = -0.3) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), alpha = 0.5, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Competencias~.) +
 scale_colour_manual("", values = "dodgerblue4")+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Competencias", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 360), legend.text = element_text(size=13, angle = 270),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13)) + guides(fill = FALSE)

#Conocimientos

Nombre.Conocimientos <- c(rep("Lenguajes de programación", 55), rep("Herramientas de desarrollo (id)", 55))
Puntuaciones.Conocimientos <- c(programadores$CN1,programadores$CN2)
Nivel.Requerido <- c(rep(3,55), rep(2,55))

datos <- data.frame(Nombre.Conocimientos, Puntuaciones.Conocimientos, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Conocimientos, ..count.., fill = Nombre.Conocimientos)) +  scale_y_continuous(limits = c(0,35)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.9, hjust = -0.3) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), alpha = 0.7, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Conocimientos~.) +
 scale_colour_manual("", values = "dodgerblue4")+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Conocimientos", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.text = element_text(size=13, angle = 270),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13)) + guides(fill = FALSE)


# Funciones

Nombre.Funciones <- c(rep("Programar aplicaciones", 55), rep("Manejar bases de datos", 55), rep("Asesorar a los usuarios", 55), 
 rep("Documetar los módulos", 55), rep("Aplicar las normas", 55))
Puntuaciones.Funciones <- c(programadores$F1,programadores$F2,programadores$F3,programadores$F4,programadores$F5)
Nivel.Requerido <- rep(4,55*5)

datos <- data.frame(Nombre.Funciones, Puntuaciones.Funciones, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Funciones, ..count.., fill = Nombre.Funciones)) + scale_y_continuous(limits = c(0,40)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.5, hjust = -0.3) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), alpha = 0.5, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Funciones~.) +
 scale_colour_manual("", values = "dodgerblue4")+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Funciones", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 0), legend.text = element_text(size=13, angle = 270),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13)) + guides(fill = FALSE)


### ### Densidad de variables apiladas 
#Modulos

Nombre.Modulos <- c(rep("Competencias", 55), rep("Conocimientos", 55), rep("Funciones", 55))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)

opuesto <-  function(x){return(-median(x))}

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos)
 
datos %>% mutate(Nombre.Modulos = reorder(Nombre.Modulos, Puntuaciones.Modulos, FUN = opuesto)) %>% 
  ggplot(aes(Puntuaciones.Modulos, ..count.., fill = Nombre.Modulos)) + 
 geom_density( alpha = 0.6, bw = 0.7, color = "white", position="stack")+
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= "Número apilado de empleados por puntuación") +
 scale_fill_discrete(name = "Módulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13) )




