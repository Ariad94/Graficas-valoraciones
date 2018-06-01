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
 ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limit=c(0,100),breaks = seq(0,100, by = 10)) + geom_histogram(bins=10, color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias, conocimientos y funciones", subtitle= 
 "Número de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Módulo") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0))

#Competencias

Etiquetas <- c(rep("Calidad y mejora continua",55), rep("Pensamiento analítico",55), rep("Capacidad de aprendizaje",55),
 rep("Solución de problemas ",55), rep("Competencia digital",55))
Puntuaciones <- c(programadores$C1II, programadores$C2II, programadores$C3II, programadores$C4II, programadores$C5II)

datos <- data.frame(Etiquetas, Puntuaciones)

datos %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limit=c(-0.5,6.5),breaks = seq(0,6, by = 1)) +
 geom_histogram(aes(bins=7), color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Competencias", subtitle= 
 "Número de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Competencias") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0))

#Conocimientos

Etiquetas <- c(rep("Lenguajes de programación",55), rep("Herramientas de desarrollo (id)",55))
Puntuaciones <- c(programadores$CN1, programadores$CN2)

datos <- data.frame(Etiquetas, Puntuaciones)

datos %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limit=c(-0.5,4.5),breaks = seq(0,4, by = 1)) +
 geom_histogram(aes(bins=5), color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Conocimientos", subtitle= 
 "Número de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Conocimientos") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0))


#Funciones

Etiquetas <- c(rep("Programación de aplicaciones",55), rep("Manejar bases de datos",55), rep("Asesorar a los usuarios",55),
 rep("Documentar los módulos",55), rep("Aplicar las normas",55))
Puntuaciones <- c(programadores$F1, programadores$F2, programadores$F3, programadores$F4, programadores$F5)

datos <- data.frame(Etiquetas, Puntuaciones)

datos %>% ggplot(aes(Puntuaciones, fill = Etiquetas)) + scale_x_continuous(limit=c(-0.5,4.5),breaks = seq(0,4, by = 1)) +
 geom_histogram(aes(bins=5), color = "black") + 
 xlab("Puntuaciones")+ylab("Número de empleados")+ labs(title ="Funciones", subtitle= 
 "Número de empleados por intervalo de puntuaciones") +
 scale_fill_discrete(name = "Funciones") + theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0))


### ### Histograma de frecuencias con curva normal

mean <- mean(programadores$FINAL)
sd <- sd(programadores$FINAL)

programadores  %>% ggplot(aes(FINAL)) + scale_x_continuous(limit=c(0,100), breaks = seq(0,100, by = 5)) + 
 geom_histogram(aes(y = ..count..), bins=20, color = "black", fill = "darkviolet", alpha=0.7) + stat_bin(bins = 20, geom="text", aes(label=..count..), vjust = -0.6) +
 stat_function(fun = function(x) dnorm(x, mean = mean, sd = sd)*400,
 color = "darkred", size = 1)+ 
 xlab("Puntuaciones finales")+ylab("Número de empleados")+ labs(title ="Puntuaciones finales", subtitle= 
 "Número de empleados por intervalo de puntuaciones") + theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0)) 

### ### Histograma de frecuencias con linea en el valor requerido

programadores  %>% ggplot(aes(FINAL)) + scale_x_continuous(limit=c(0,100), breaks = c(0,10,20,30,40,50,60,70,80,90,100)) + 
 geom_histogram(bins=10, color = "black", fill = "darkviolet", alpha=0.7) + stat_bin(bins = 10, geom="text", aes(label=..count..), vjust = -0.6) +
 geom_vline(aes(xintercept=78), col = "dodgerblue4", size = 1, linetype = "longdash") +
 xlab("Puntuaciones finales")+ylab("Número de empleados")+ labs(title ="Puntuaciones finales", subtitle= 
 "Número de empleados por intervalo de puntuaciones") + theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=15, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=10, hjust=0)) 

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
 scale_x_continuous(limit=c(0,110), breaks = seq(0,100, by = 10)) + 
 geom_histogram(aes(y = ..density.., fill = Nombre.Modulos), bins=10, color = "black", alpha=0.7) + 
 stat_bin(bins = 10, geom="text", aes(y = ..density..,label=..count..), vjust = -0.1) +
 stat_density(geom="line", color = "darkblue", size = 0.7)+
 geom_line(data=datos_normal, aes(x, y),color = "darkred", size = 0.7)+
 facet_grid(Nombre.Modulos~.) +
 labs(title="Competencias, conocimientos y funciones ", subtitle="Número de empleados por intervalos de puntuaciones",
 x = "Puntuaciones", y = "Densidad de empleados") +
 theme_pander() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=15), legend.position = "none")


























