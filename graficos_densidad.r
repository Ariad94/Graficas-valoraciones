### Gráficos de densidad

library(ggplot2)
library(dplyr)
library(ggthemes)

### ### Densidades con facet_grid 

#Competencias

n <- 16

Nombre.Competencias <- c(rep("Calidad y mejora continua", n), rep("Pensamiento analítico", n), rep("Capacidad de aprendizaje", n),
 rep("Solución de problemas", n), rep("Competencia digital", n))
Puntuaciones.Competencias <- c(programadores$C1II,programadores$C2II,programadores$C3II,programadores$C4II,programadores$C5II)

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias)

y_max <- max(table(Puntuaciones.Competencias, Nombre.Competencias))+1

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + scale_y_continuous(limits=c(0,y_max)) + 
 scale_x_continuous(limits = c(-0.01,6.01), breaks = seq(0,6, by = 1)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.9) +  
 facet_grid(Nombre.Competencias~.) +
 labs(title="Competencias", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 360), legend.position="none", 
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))


#Conocimientos

n <- 55

Nombre.Conocimientos <- c(rep("Lenguajes de programación", n), rep("Herramientas de desarrollo (id)", n))
Puntuaciones.Conocimientos <- c(programadores$CN1,programadores$CN2)

datos <- data.frame(Nombre.Conocimientos, Puntuaciones.Conocimientos)

y_max <- max(table(Puntuaciones.Conocimientos, Nombre.Conocimientos))+1

datos %>% ggplot(aes(Puntuaciones.Conocimientos, ..count.., fill = Nombre.Conocimientos)) + scale_y_continuous(limits = c(0,y_max)) +
 scale_x_continuous(limits = c(-0.01,4.01), breaks = seq(0,4,by = 1)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.3) +  
 facet_grid(Nombre.Conocimientos~.) +
 labs(title="Conocimientos", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.position="none",
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))

#Funciones
n <- 55

Nombre.Funciones <- c(rep("Programar aplicaciones", n), rep("Manejar bases de datos", n), rep("Asesorar a los usuarios", n),
 rep("Documentar los módulos", n), rep("Aplicar las normas", n))
#Nombre.Funciones <- c(rep("Aplicaciones informáticas",n), rep("Pruebas del código",n), rep("Documentar actividad",n), rep("Elaborar manual técnico",n),
 #rep("Interpretar aplicaciones",n))
Puntuaciones.Funciones <- c(programadores$F1,programadores$F2,programadores$F3,programadores$F4,programadores$F5)

datos <- data.frame(Nombre.Funciones, Puntuaciones.Funciones)

y_max <- max(table(Puntuaciones.Funciones, Nombre.Funciones))+1

datos %>% ggplot(aes(Puntuaciones.Funciones, ..count.., fill = Nombre.Funciones)) + scale_y_continuous(limits = c(0,y_max)) +
 scale_x_continuous(limits = c(-0.01,4.01), breaks = seq(0,4,by = 1)) +
 geom_density(alpha = 0.25, bw = 0.5, color = "white") +
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.5) +  
 facet_grid(Nombre.Funciones~.) +
 labs(title="Funciones", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 0), legend.position="none",
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))



### ### Densidades con facet_grid y nivel requerido
#Competencias

n <- 55
nr <- c(2,2,2,3,2)
#nr <- c(2,2,2,2,3)

Nombre.Competencias <- c(rep("Calidad y mejora continua", n), rep("Pensamiento analítico", n), rep("Capacidad de aprendizaje", n),
 rep("Solución de problemas", n), rep("Competencia digital", n))
Puntuaciones.Competencias <- c(programadores$C1,programadores$C2,programadores$C3,programadores$C4,programadores$C5)
Nivel.Requerido <- c(rep(nr[1],n), rep(nr[2],n),rep(nr[3],n),rep(nr[4],n),rep(nr[5],n))

datos <- data.frame(Nombre.Competencias, Puntuaciones.Competencias, Nivel.Requerido)

y_max <- max(table(Puntuaciones.Competencias, Nombre.Competencias))+2

datos %>% ggplot(aes(Puntuaciones.Competencias, ..count.., fill = Nombre.Competencias)) + scale_y_continuous(limits=c(0,y_max)) + 
 scale_x_continuous(limits = c(-0.01,4.01), breaks = seq(0,4,by = 1)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.9, hjust = -0.3) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), alpha = 0.5, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Competencias~.) +
 scale_colour_manual("", values = "dodgerblue4")+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Competencias", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 360), legend.text = element_text(size=13, angle = 270),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13)) + guides(fill = FALSE)

#Conocimientos


n <- 55
nr <- c(3,2)

Nombre.Conocimientos <- c(rep("Lenguajes de programación", n), rep("Herramientas de desarrollo (id)", n))
Puntuaciones.Conocimientos <- c(programadores$CN1,programadores$CN2)
Nivel.Requerido <- c(rep(nr[1],n),rep(nr[2],n))

datos <- data.frame(Nombre.Conocimientos, Puntuaciones.Conocimientos, Nivel.Requerido)

y_max <- max(table(Puntuaciones.Conocimientos, Nombre.Conocimientos))+2

datos %>% ggplot(aes(Puntuaciones.Conocimientos, ..count.., fill = Nombre.Conocimientos)) +  scale_y_continuous(limits=c(0,y_max)) + 
 scale_x_continuous(limits = c(-0.01,4.01), breaks = seq(0,4,by = 1)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.9, hjust = -0.3) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), alpha = 0.7, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Conocimientos~.) +
 scale_colour_manual("", values = "dodgerblue4")+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Conocimientos", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.text = element_text(size=13, angle = 270),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13)) + guides(fill = FALSE)


# Funciones

n <- 55

#Nombre.Funciones <- c(rep("Programar aplicaciones", n), rep("Manejar bases de datos", n), rep("Asesorar a los usuarios", n), 
 #rep("Documetar los módulos", n), rep("Aplicar las normas", n))

Nombre.Funciones <- c(rep("Aplicaciones informáticas", n), rep("Pruebas del código", n), rep("Documentar actividad", n), 
 rep("Elaborar manual técnico", n), rep("Interpretar aplicaciones", n))

Puntuaciones.Funciones <- c(programadores$F1,programadores$F2,programadores$F3,programadores$F4,programadores$F5)
Nivel.Requerido <- rep(2,n*5)

datos <- data.frame(Nombre.Funciones, Puntuaciones.Funciones, Nivel.Requerido)

y_max <- max(table(Puntuaciones.Funciones, Nombre.Funciones))+2

datos %>% ggplot(aes(Puntuaciones.Funciones, ..count.., fill = Nombre.Funciones)) + scale_y_continuous(limits=c(0,y_max)) + 
 scale_x_continuous(limits = c(-0.01,4.01), breaks = seq(0,4,by = 1)) +
 geom_density( alpha = 0.25, bw = 0.5, color = "white") + 
 geom_histogram(binwidth = 0.005) + stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust = -0.5, hjust = -0.3) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), alpha = 0.5, size = 1, linetype = "longdash") +  
 facet_grid(Nombre.Funciones~.) +
 scale_colour_manual("", values = "dodgerblue4")+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Funciones", subtitle="Número de empleados por puntuación", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold", angle = 0), legend.text = element_text(size=13, angle = 270),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13)) + guides(fill = FALSE)


### ### Densidad de variables apiladas 
#Modulos

n <- 55

Nombre.Modulos <- c(rep("Competencias", n), rep("Conocimientos", n), rep("Funciones", n))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)

opuesto <-  function(x){return(-median(x))}

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos)
 
datos %>% mutate(Nombre.Modulos = reorder(Nombre.Modulos, Puntuaciones.Modulos, FUN = opuesto)) %>% 
  ggplot(aes(Puntuaciones.Modulos, ..count.., fill = Nombre.Modulos)) + 
 geom_density( alpha = 0.6, bw = 0.7, color = "white", position="stack")+
 scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= "Número apilado de empleados por puntuación") +
 scale_fill_discrete(name = "Módulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13) )




