### Diagramas de cajas

library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggrepel)

### ### Diagrama de cajas de una variable 
#Para FINAL

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

programadores %>% mutate(Etiquetas = rep("FINAL",55)) %>% mutate(outlier1=ifelse(is_outlier(FINAL),as.character(Nombre.Empleado), as.numeric(NA))) %>% 
 mutate(outlier2=ifelse(is_outlier(FINAL),FINAL, as.numeric(NA))) %>% 
 ggplot(aes(Etiquetas, FINAL)) + geom_boxplot(fill = "darkviolet", alpha = 0.8) + scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 20)) + 
 geom_text(aes(label= outlier1),na.rm=TRUE, nudge_y = 2) + geom_text(aes(label= round(outlier2,2)),na.rm=TRUE, nudge_y = -2) +
 labs(title="Puntuaciones Finales", subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico", x = " ", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.text = element_text(size=13) ,
 axis.text.x=element_blank()) 

#Para valoradores

valoradores <- programadores$Nombre.Valorador
C1 <- programadores$C1II*4/6
C2 <- programadores$C2II*4/6
C3 <- programadores$C3II*4/6
C4 <- programadores$C4II*4/6
C5 <- programadores$C5II*4/6
CN1 <- programadores$CN1
CN2 <- programadores$CN2
F1 <- programadores$F1
F2 <- programadores$F2
F3 <- programadores$F3
F4 <- programadores$F4
F5 <- programadores$F5

datos_valoradores <- data.frame(nombre = rep(valoradores,12), puntuaciones = c(C1,C2,C3,C4,C5,CN1,CN2,F1,F2,F3,F4,F5))

datos_medias <- data.frame(Grupo = rep("Valoradores",23),(aggregate(datos_valoradores[, 2], list(datos_valoradores$nombre), mean)))
colnames(datos_medias)[2] <- "Nombre.Valorador"
colnames(datos_medias)[3] <- "medias"

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

datos_medias %>% mutate(outlier1=ifelse(is_outlier(medias), as.character(Nombre.Valorador), as.numeric(NA))) %>% 
 mutate(outlier2=ifelse(is_outlier(medias),medias, as.numeric(NA))) %>% 
 ggplot(aes(Grupo, medias)) + geom_boxplot(fill = "darkviolet", alpha = 0.8) + scale_y_continuous(limits = c(0,4), breaks = seq(0,4, by = 1)) + 
 geom_text(aes(label= outlier1),na.rm=TRUE,nudge_y= 0.1) + geom_text(aes(label= round(outlier2,2)),na.rm=TRUE, nudge_y = -0.1) +
 labs(title="Puntuaciones medias dadas por los valoradores", subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico", x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.text = element_text(size=13) ,
 axis.text.x=element_blank())


### ### Diagrama de cajas de una variable por factores + facet_grid
# UT

Etiquetas <- c(rep("Puntuaciones finales",55), rep("Competencias", 55), rep("Conocimientos",55), rep("Desempeño",55))
Puntuaciones<- c(programadores$FINAL, programadores$M1, programadores$M2, programadores$M3)
Grupo <- rep(programadores$UT, 4)

datos <- data.frame(Etiquetas, Grupo, Puntuaciones)

datos %>% mutate(Grupo = reorder(Grupo, Puntuaciones, FUN = median))%>% 
 ggplot() + geom_boxplot(aes(Grupo, Puntuaciones, fill = Etiquetas)) + scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 50)) + 
 xlab("")+ylab("")+ labs(title = "Puntuaciones finales y de los módulos por unidad territorial",
 subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico") +
 facet_grid(Etiquetas~.) +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.position="none",
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.text = element_text(size=13))

### ### Diagrama de cajas de una variable por factores + facet_grid
# UN

Etiquetas <- c(rep("Puntuaciones finales",55), rep("Competencias", 55), rep("Conocimientos",55), rep("Desempeño",55))
Puntuaciones<- c(programadores$FINAL, programadores$M1, programadores$M2, programadores$M3)
Grupo <- rep(programadores$UN, 4)

datos <- data.frame(Etiquetas, Grupo, Puntuaciones)

datos %>% mutate(Grupo = reorder(Grupo, Puntuaciones, FUN = median))%>%
 ggplot() + geom_boxplot(aes(Grupo, Puntuaciones, fill = Etiquetas)) +
 xlab("")+ylab("")+ labs(title = "Puntuaciones finales y de los módulos por empresa", 
 subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico") +
 facet_grid(Etiquetas~.) +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 strip.text.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.position="none",
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), axis.text = element_text(size=13))



### ### Diagrama de cajas de una variable por factores y por colores
# Para empleado-valorador, valorador-aprobador

C1 <- programadores$C1II*4/6
C2 <- programadores$C2II*4/6
C3 <- programadores$C3II*4/6
C4 <- programadores$C4II*4/6
C5 <- programadores$C5II*4/6
CN1 <- programadores$CN1
CN2 <- programadores$CN2
F1 <- programadores$F1
F2 <- programadores$F2
F3 <- programadores$F3
F4 <- programadores$F4
F5 <- programadores$F5


Factor.Empleado <- rep(as.character(programadores$Nombre.Empleado), 12)
Puntuaciones <- c(C1,C2,C3,C4,C5,CN1,CN2,F1,F2,F3,F4,F5)
Valoradores <- rep(as.character(programadores$Nombre.Valorador),12)

datos <- data.frame(Factor.Empleado, Valoradores, Puntuaciones, SiglasVal = rep(programadores$Siglas.Valorador,12))

datos %>% mutate(Factor.Empleado = reorder(Factor.Empleado, Puntuaciones, FUN = median)) %>%
 ggplot( mapping = aes(Factor.Empleado, Puntuaciones, color = Valoradores)) + geom_boxplot() + 
 scale_y_continuous(limits = c(0,5.5), breaks = seq(0,4, by = 1)) +
 geom_text(aes(x = Factor.Empleado, y = rep(5,660), label = SiglasVal, color = Valoradores), angle = 90) +
 xlab("")+ylab("")+labs(title = "Puntuaciones obtenidas por cada empleado", 
 subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico")  +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 axis.text.x = element_text(angle = 90, hjust = 1), legend.position="bottom", 
 legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size = 8)) 

# valorador - aprobador

valoradores <- programadores$Nombre.Valorador
C1 <- programadores$C1II*4/6
C2 <- programadores$C2II*4/6
C3 <- programadores$C3II*4/6
C4 <- programadores$C4II*4/6
C5 <- programadores$C5II*4/6
CN1 <- programadores$CN1
CN2 <- programadores$CN2
F1 <- programadores$F1
F2 <- programadores$F2
F3 <- programadores$F3
F4 <- programadores$F4
F5 <- programadores$F5

datos_valoradores <- data.frame(Aprobador = rep(programadores$Nombre.Aprobador1, 12), SiglasApr = rep(programadores$Siglas.Aprobador1,12), 
 nombre = rep(valoradores,12), puntuaciones = c(C1,C2,C3,C4,C5,CN1,CN2,F1,F2,F3,F4,F5))

datos_siglas <- data.frame(Aprobador = programadores$Nombre.Aprobador1, Siglas = programadores$Siglas.Aprobador, Valorador = programadores$Nombre.Valorador)

datos_siglas1 <- datos_siglas %>% group_by(Valorador) %>% filter(!duplicated(Aprobador))

datos_valoradores %>% mutate(nombre = reorder(nombre, puntuaciones, FUN = median)) %>%
 ggplot(aes(nombre, puntuaciones, color = Aprobador))+ geom_boxplot() +  scale_y_continuous(limits = c(0,5.5), breaks = seq(0,4, by = 1)) +
 geom_text_repel(data = datos_siglas1, aes(x = Valorador, y =5, label = Siglas, color = Aprobador), angle = 90, point.padding = unit(0, "lines"),
 segment.color = "transparent", direction= "y") + 
 xlab("")+ylab("")+ labs(title = "Puntuaciones dadas por cada valorador", 
 subtitle = "Cuartiles, máximo, mínimo y rango intercuartílico")  +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),
 axis.text.x = element_text(angle = 90, hjust = 1),  
 legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size = 10), legend.key.size = unit(0.9, "cm"), axis.text = element_text(size=10)) 



### ### Diagrama de cajas de variables variables en el mismo grafico
# Modulo 1, Modulo 2 y Modulo 3

Etiquetas <- c(rep("Competencias",55), rep("Conocimientos",55), rep("Funciones",55))
Puntuaciones <- c(programadores$M1, programadores$M2, programadores$M3)

datos <- data.frame(Etiquetas, Puntuaciones, Nombre.Empleado = rep(programadores$Nombre.Empleado,3))

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

opuesto <-  function(x){return(-median(x))}

datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = opuesto)) %>% group_by(Etiquetas) %>%
 mutate(outlier1=ifelse(is_outlier(Puntuaciones), as.character(Nombre.Empleado), as.numeric(NA))) %>% 
 mutate(outlier2=ifelse(is_outlier(Puntuaciones),Puntuaciones, as.numeric(NA))) %>% 
 ggplot(aes(Etiquetas, Puntuaciones)) + scale_y_continuous(limits=c(0,100), breaks = seq(0,100, by = 10)) +
 geom_boxplot(fill = c("palegreen","sandybrown", "paleturquoise1")) +
 geom_text_repel(aes(label= outlier1, col = outlier1),na.rm=TRUE,nudge_y= 1.5, segment.color = "transparent", direction= "x", size = 3) + 
 geom_text(aes(label= round(outlier2,2)),na.rm=TRUE, nudge_x = -0.06) +
 labs(title="Puntuaciones de los módulos de competencias, conocimientos y funciones", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0),  axis.text = element_text(size=13), legend.position = "none") 

# Competencias

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

Etiquetas <- c(rep("Calidad y mejora continua",55), rep("Pensamiento analítico",55), rep("Capacidad de aprendizaje",55),
 rep("Solución de problemas ",55), rep("Competencia digital",55))
Puntuaciones <- c(programadores$C1II, programadores$C2II, programadores$C3II, programadores$C4II, programadores$C5II)

datos <- data.frame(Etiquetas, Puntuaciones, Nombre.Empleado = rep(programadores$Nombre.Empleado, 5))


datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = median)) %>% group_by(Etiquetas) %>%
 mutate(outlier1=ifelse(is_outlier(Puntuaciones), as.character(Nombre.Empleado), as.numeric(NA))) %>% 
 mutate(outlier2=ifelse(is_outlier(Puntuaciones),Puntuaciones, as.numeric(NA))) %>% 
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(aes(fill = Etiquetas)) + scale_y_continuous(limits = c(0,6), breaks = seq(0,6, by = 1)) +
 geom_text_repel(aes(label= outlier1, col = outlier2),na.rm=TRUE,nudge_y= 0.04, segment.color = "transparent", direction= "y", size =3) + 
 geom_text(aes(label= round(outlier2,2)),na.rm=TRUE, nudge_x = -0.06) +
 labs(title="Puntuaciones por competencias", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), legend.position = "none", axis.text = element_text(size=13),
 axis.text.x = element_text(angle = 90, hjust = 1)) 


# Conocimientos

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

Etiquetas <- c(rep("Lenguajes de programación",55), rep("Herramientas de desarrollo (id)",55))
Puntuaciones <- c(programadores$CN1, programadores$CN2)

datos <- data.frame(Etiquetas, Puntuaciones, Nombre.Empleado = rep(programadores$Nombre.Empleado, 2))


datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = median)) %>% group_by(Etiquetas) %>%
 mutate(outlier1=ifelse(is_outlier(Puntuaciones), as.character(Nombre.Empleado), as.numeric(NA))) %>% 
 mutate(outlier2=ifelse(is_outlier(Puntuaciones),Puntuaciones, as.numeric(NA))) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(aes(fill = Etiquetas)) + scale_y_continuous(limits = c(0,4), breaks = seq(0,4, by = 1)) +
 geom_text_repel(aes(label= outlier1, col = outlier2),na.rm=TRUE,nudge_y= 0.04, segment.color = "transparent", direction= "y", size =3) + 
 geom_text(aes(label= round(outlier2,2)),na.rm=TRUE, nudge_x = -0.03) +
 labs(title="Puntuaciones por conocimientos", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), legend.position = "none", axis.text = element_text(size=13),
 axis.text.x = element_text(angle = 90, hjust = 1)) 



# Funciones

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

Etiquetas <- c(rep("Programar aplicaciones",55), rep("Manejar bases de datos",55), rep("Asesorar a los usuarios",55), rep("Documentar los módulos",55),
 rep("Aplicar las normas",55))
Puntuaciones <- c(programadores$F1, programadores$F2, programadores$F3, programadores$F4, programadores$F5)

datos <- data.frame(Etiquetas, Puntuaciones, Nombre.Empleado = rep(programadores$Nombre.Empleado, 5))


datos %>% mutate(Etiquetas = reorder(Etiquetas, Puntuaciones, FUN = median)) %>% group_by(Etiquetas) %>%
 mutate(outlier1=ifelse(is_outlier(Puntuaciones), as.character(Nombre.Empleado), as.numeric(NA))) %>% 
 mutate(outlier2=ifelse(is_outlier(Puntuaciones),Puntuaciones, as.numeric(NA))) %>%
 ggplot(aes(Etiquetas, Puntuaciones)) + geom_boxplot(aes(fill = Etiquetas)) + scale_y_continuous(limits = c(0,4), breaks = seq(0,4, by = 1)) +
 geom_text_repel(aes(label= outlier1, col = outlier2),na.rm=TRUE,nudge_y= 0.04, segment.color = "transparent", direction= "y", size =3) + 
 geom_text(aes(label= round(outlier2,2)),na.rm=TRUE, nudge_x = -0.03) +
 labs(title="Puntuaciones por funciones", subtitle="Cuartiles, máximo, mínimo y rango intercuartílico", 
 x = "", y = "") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0), 
 plot.subtitle = element_text( color = "dodgerblue4", size=20, hjust=0), legend.position = "none", axis.text = element_text(size=13),
 axis.text.x = element_text(angle = 90, hjust = 1)) 

















