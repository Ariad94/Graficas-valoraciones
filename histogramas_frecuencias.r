### Histogramas de frecuencias

library(ggplot2)
library(dplyr)
library(ggthemes)



### ### Histograma de frecuencias apiladas
#Modulos

Etiquetas <- c(rep("Competencias",55), rep("Conocimientos",55),rep("Funciones",55))
Puntuaciones <- c(programadores$M1, programadores$M2, programadores$M3)

opuesto <-  function(x){return(-median(x))}

datos <- data.frame(Etiquetas, Puntuaciones)

datos  %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = opuesto)) %>% 
 ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limit=c(0,100),breaks = seq(0,100, by = 10)) + 
 geom_histogram(binwidth = 10, color = "black", position = "stack", boundary = 0) + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= 
 "Número de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Módulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13) )

#Competencias

n <- 55

Etiquetas <- c(rep("Calidad y mejora continua",n), rep("Pensamiento analítico",n), rep("Capacidad de aprendizaje",n),
 rep("Solución de problemas ",n), rep("Competencia digital",n))
Puntuaciones <- c(programadores$C1II, programadores$C2II, programadores$C3II, programadores$C4II, programadores$C5II)

datos <- data.frame(Etiquetas, Puntuaciones)

datos %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(breaks = seq(0,6, by = 1)) + 
 geom_bar(width = 0.3, color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias", subtitle= 
 "Número apilado de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Competencias") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13))

#Conocimientos

n <- 55

Etiquetas <- c(rep("Lenguajes de programación",n), rep("Herramientas de desarrollo (id)",n))
Puntuaciones <- c(programadores$CN1, programadores$CN2)

datos <- data.frame(Etiquetas, Puntuaciones)

datos %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limits = c(-0.5,4.5), breaks = seq(0,4, by = 1)) +
 geom_bar(width = 0.3, color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Conocimientos", subtitle= 
 "Número apilado de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Conocimientos") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13))


#Funciones

n <- 55

Etiquetas <- c(rep("Programación de aplicaciones",55), rep("Manejar bases de datos",55), rep("Asesorar a los usuarios",55),
 rep("Documentar los módulos",55), rep("Aplicar las normas",55))
#Etiquetas <- c(rep("Aplicaciones informáticas",n), rep("Pruebas del código",n), rep("Documentar actividad",n), rep("Elaborar manual técnico",n),
 #rep("Interpretar aplicaciones",n))

Puntuaciones <- c(programadores$F1, programadores$F2, programadores$F3, programadores$F4, programadores$F5)

datos <- data.frame(Etiquetas, Puntuaciones)

datos %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(breaks = seq(0,4, by = 1)) +
 geom_bar(width = 0.3, color = "black") +  
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Funciones", subtitle= 
 "Número apilado de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Funciones") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13))


### ### Histograma de frecuencias con curva normal

mean <- mean(programadores$FINAL)
sd <- sd(programadores$FINAL)

programadores  %>% ggplot(aes(FINAL)) + scale_x_continuous(limit=c(0,100), breaks = seq(0,100, by = 5)) + 
 geom_histogram(aes(y = ..count..), binwidth = 5, boundary = 0, color = "black", fill = "darkviolet", alpha=0.7) + 
 stat_bin(binwidth = 5, boundary = 0, geom="text", aes(label=..count..), vjust = -0.4) +
 stat_function(aes(color = "Curva normal teórica"), fun = function(x) dnorm(x, mean = mean, sd = sd)*350,size = 1) +
 scale_colour_manual("", values="darkred") +
 xlab("Puntuaciones finales")+ylab("Número de empleados")+ labs(title ="Puntuaciones finales", subtitle= 
 "Número de empleados por intervalo de puntuaciones") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13),legend.text = element_text(size=13)) 



### ### Histograma de frecuencias con linea en el valor requerido

#Puntuaciones finales

programadores  %>% ggplot(aes(FINAL)) + scale_x_continuous(limit=c(0,100), breaks = seq(0,100, by = 5)) + 
 geom_histogram(binwidth = 5, boundary = 0, color = "black", fill = "darkviolet", alpha=0.7) + 
 stat_bin(binwidth = 5, boundary = 0, geom="text", aes(label=..count..), vjust = -0.4, hjust = -0.2) +
 geom_vline(aes(xintercept=57.5, col = "Nivel requerido"), size = 1, linetype = "longdash") +
 scale_colour_manual("", values="dodgerblue4") +
 xlab("Puntuaciones finales")+ylab("Número de empleados")+ labs(title ="Puntuaciones finales", subtitle= 
 "Número de empleados por intervalo de puntuaciones") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13),legend.text = element_text(size=13)) 

#Modulos - facet_grid

n <- 55
nr <- c(56.25, 65, 50)
#nr <- c(57.5, 65, 50)

Nombre.Modulos <- c(rep("Competencias", n), rep("Conocimientos", n), rep("Funciones", n))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)
Nivel.Requerido <- c(rep(nr[1],n), rep(nr[2],n), rep(nr[3],n))

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos, Nivel.Requerido)

y_max <- max(table(Puntuaciones.Modulos, Nombre.Modulos))+2

datos %>% ggplot(aes(Puntuaciones.Modulos, ..count.., fill = Nombre.Modulos)) + 
 scale_x_continuous(limit=c(0,100), breaks = seq(0,100, by = 10)) + scale_y_continuous(limit=c(0,y_max))+
 geom_histogram(binwidth = 10, boundary = 0,color = "black", alpha=0.5) + 
 stat_bin(binwidth = 10, boundary = 0, geom="text", aes(label=..count..), vjust = -0.4, hjust = -0.2) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido"), size = 1, linetype = "longdash") +  
 scale_colour_manual("", values="dodgerblue4") +
 facet_grid(Nombre.Modulos~.) +
 labs(title="Módulos", subtitle="Número de empleados por intervalo de puntuaciones", x = "Puntuaciones", y = "Número de empleados") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), 
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), 
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), 
 axis.text = element_text(size=13),legend.text = element_text(size=13)) + guides(fill = FALSE)

# Modulos - facet_grid con mas nivel requerido

Nombre.Modulos <- c(rep("Competencias", 55), rep("Conocimientos", 55), rep("Funciones", 55))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)
Nivel.Requerido <- c(rep(55,55), rep(65,55), rep(50,55))
Nivel.Requerido.prueba <- c(rep(75,55), rep(75,55), rep(65,55))

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos, Nivel.Requerido)

datos %>% ggplot(aes(Puntuaciones.Modulos, ..count.., fill = Nombre.Modulos)) + 
 scale_x_continuous(limit=c(0,100), breaks = seq(0,100, by = 10)) + scale_y_continuous(limit=c(0,25))+
 geom_histogram(binwidth = 10, boundary = 0,color = "black", alpha=0.5) + 
 stat_bin(binwidth = 10, boundary = 0, geom="text", aes(label=..count..), vjust = -0.4, hjust = -0.2) +
 geom_vline(aes(xintercept=Nivel.Requerido, col = "Nivel requerido real"), size = 1, linetype = "longdash") + 
 geom_vline(aes(xintercept=Nivel.Requerido.prueba, col = "Nivel requerido modificado"), size = 1, linetype = "longdash") + 
 scale_colour_manual("", values=c("darkred","dodgerblue4")) +
 facet_grid(Nombre.Modulos~.) +
 labs(title="Módulos", subtitle="Número de empleados por intervalo de puntuaciones", x = "Puntuaciones", y = "Número de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), 
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), 
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), 
 axis.text = element_text(size=13),legend.text = element_text(size=13)) + guides(fill = FALSE)





### ### Histograma de frecuencias facet_grid y curva normal

Nombre.Modulos <- c(rep("Competencias", 55), rep("Conocimientos", 55), rep("Funciones", 55))
Puntuaciones.Modulos <- c(programadores$M1,programadores$M2,programadores$M3)

datos <- data.frame(Nombre.Modulos, Puntuaciones.Modulos)

stats <- aggregate(Puntuaciones.Modulos~Nombre.Modulos, datos, function(x) c(mean=mean(x), sd=sd(x)))
stats <- data.frame(Nombre.Modulos=stats[,1],stats[,2])

x <- seq(0,110, by = 0.1)
datos_normal <- do.call(rbind,lapply(1:nrow(stats), 
                            function(i) with(stats[i,],data.frame(Nombre.Modulos, x, y=dnorm(x,mean=mean,sd=sd)))))


datos %>% ggplot(aes(Puntuaciones.Modulos)) + 
 scale_x_continuous(limit=c(0,100), breaks = seq(0,100, by = 10)) + scale_y_continuous(limit=c(0,0.05)) +
 geom_histogram(aes(y = ..density.., fill = Nombre.Modulos),binwidth = 10, boundary = 0, color = "black", alpha=0.8, na.rm = TRUE) + 
 stat_bin(binwidth = 10, boundary = 0, geom="text", aes(y = ..density..,label=..count..), vjust = -3.4) +
 stat_density(aes(col = "Función de densidad de los datos"), geom="line", size = 0.75, alpha = 0.7, show.legend = TRUE)+
 geom_line(data=datos_normal, aes(x, y, col = "Función de densidad normal teórica"), size = 0.75, alpha = 0.7, na.rm = TRUE)+
 facet_grid(Nombre.Modulos~.) +
 scale_colour_manual("", values = c("darkblue", "darkred"))+
 guides(colour = guide_legend(override.aes = list(alpha = 1))) +
 labs(title="Competencias, conocimientos y funciones ", subtitle="Número de empleados por intervalos de puntuaciones y funciones de densidad",
 x = "Puntuaciones", y = "Densidad") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), 
 axis.text = element_text(size=13),legend.text = element_text(size=13)) + guides(fill = FALSE)

