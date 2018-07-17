### Diagramas de dispersi�n

library(ggplot2)
library(ggthemes)
library(dplyr)

### ### Diagrama de dispersion de variables continuas y colores por factores
# Para M1 vs M3

n <- 55

Competencias <- c(programadores$M1)
M�dulo <- c( rep("Funciones",n))
Puntuaciones <- c(programadores$M3)

datos <- data.frame(Competencias, Puntuaciones, M�dulo)

datos %>% ggplot(aes(Competencias, Puntuaciones)) + scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 geom_point(aes(colour = M�dulo), show.legend = FALSE, size = 3) + 
 geom_smooth(aes(colour = M�dulo), size = 1, method = "lm", se = FALSE, show.legend = FALSE) + 
 labs(title="Competencias vs. Desempe�o ", x = "Puntuaciones del m�dulo de competencias", y = "Puntuaciones del m�dulo de funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13))



# Para M2 vs M1-M3

n <- 55

Conocimientos <- c(programadores$M2, programadores$M2)
M�dulo <- c(rep("Competencias",n), rep("Funciones",n))
Puntuaciones <- c(programadores$M1, programadores$M3)

datos <- data.frame(Conocimientos, Puntuaciones, M�dulo)

datos %>% ggplot(aes(Conocimientos, Puntuaciones)) + scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 geom_point(aes(colour = M�dulo), show.legend = TRUE, size = 3) + 
 geom_smooth(aes(colour = M�dulo), size = 1, method = "lm", se = FALSE, show.legend = TRUE) + 
 labs(title="Conocimientos vs. Competencias y Desempe�o", x = "Puntuaciones del m�dulo de conocimientos", y = "Puntuaciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), legend.title = element_text(color = "dodgerblue4", size=13, face = "bold"),
 legend.text = element_text(size=13), axis.text = element_text(size=13))



# Para M1 vs M3 por nivel requerido de competencias

n <- 55

nr <- 56.25
#nr <- 57.5

Competencias <- c(programadores$M1)
M�dulo <- c( rep("Funciones",n))
Puntuaciones <- c(programadores$M3)
Pordebajo_nivel_requerido <- ifelse(Competencias < nr, "Competencias por debajo del nivel requerido", "Competencias por encima o en el nivel requerido")

datos <- data.frame(Competencias, Puntuaciones, M�dulo, Pordebajo_nivel_requerido)

datos %>% ggplot(aes(Competencias, Puntuaciones, colour = Pordebajo_nivel_requerido)) + 
 scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 geom_point( show.legend = FALSE, size = 3) + 
 geom_smooth( size = 1, method = "lm", se = FALSE, show.legend = FALSE) + 
 facet_grid(.~Pordebajo_nivel_requerido) + 
 labs(title="Competencias vs. Desempe�o ", x = "Puntuaciones del m�dulo de competencias", y = "Puntuaciones del m�dulo de funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13),
 strip.text.x = element_text(color = "dodgerblue4", size=13, face = "bold"))

# Para M2 vs M3 por nivel requerido de conocimientos

n <- 55

nr <- 65

Conocimientos <- c(programadores$M2)
M�dulo <- c( rep("Funciones",n))
Puntuaciones <- c(programadores$M3)
Pordebajo_nivel_requerido <- ifelse(Conocimientos < nr, "Conocimientos por debajo del nivel requerido", "Conocimientos por encima o en el nivel requerido")

datos <- data.frame(Conocimientos, Puntuaciones, M�dulo, Pordebajo_nivel_requerido)

datos %>% ggplot(aes(Conocimientos, Puntuaciones, colour = Pordebajo_nivel_requerido)) + 
 scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 geom_point( show.legend = FALSE, size = 3) + 
 geom_smooth( size = 1, method = "lm", se = FALSE, show.legend = FALSE) + 
 facet_grid(.~Pordebajo_nivel_requerido) + 
 labs(title="Conocimietos vs. Desempe�o ", x = "Puntuaciones del m�dulo de conocimientos", y = "Puntuaciones del m�dulo de funciones") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13),
 strip.text.x = element_text(color = "dodgerblue4", size=13, face = "bold"))



# Para M3 vs M2 por nivel requerido de funciones

Funciones <- c(programadores$M3)
M�dulo <- c( rep("Conocimientos",55))
Puntuaciones <- c(programadores$M2)
Pordebajo_nivel_requerido <- ifelse(Funciones < 50, "Desempe�o por debajo del nivel requerido", "Desempe�o por encima o en el nivel requerido")

datos1 <- data.frame(Funciones, Puntuaciones, M�dulos, Pordebajo_nivel_requerido)

datos1 %>% ggplot(aes(Funciones, Puntuaciones, colour = Pordebajo_nivel_requerido)) + 
 scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 geom_point( show.legend = FALSE, size = 3) + 
 geom_smooth( size = 1, method = "lm", se = FALSE, show.legend = FALSE) + 
 facet_grid(.~Pordebajo_nivel_requerido) + 
 labs(title="Desempe�o vs. Conocimientos ", x = "Puntuaciones del m�dulo de funciones", y = "Puntuaciones del m�dulo de conocimientos") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13),
 strip.text.x = element_text(color = "dodgerblue4", size=13, face = "bold"))

# Para M3 vs M1  por nivel requerido de funciones

Funciones <- c(programadores$M3)
M�dulo <- c( rep("Competencias",55))
Puntuaciones <- c(programadores$M1)
Pordebajo_nivel_requerido <- ifelse(Funciones < 50, "Desempe�o por debajo del nivel requerido", "Desempe�o por encima o en el nivel requerido")

datos2 <- data.frame(Funciones, Puntuaciones, M�dulos, Pordebajo_nivel_requerido)

datos2 %>% ggplot(aes(Funciones, Puntuaciones, colour = Pordebajo_nivel_requerido)) + 
 scale_x_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) + 
 scale_y_continuous(limits = c(0,100), breaks = seq(0,100, by = 10)) +
 geom_point( show.legend = FALSE, size = 3) + 
 geom_smooth( size = 1, method = "lm", se = FALSE, show.legend = FALSE) + 
 facet_grid(.~Pordebajo_nivel_requerido) + 
 labs(title="Desempe�o vs. Competencias ", x = "Puntuaciones del m�dulo de funciones", y = "Puntuaciones del m�dulo de competencias") +
 theme_minimal() + theme(plot.title = element_text( color = "dodgerblue4", size=25, hjust=0),
 axis.title.x = element_text(color = "dodgerblue4", size=13, face = "bold"),
 axis.title.y = element_text(color = "dodgerblue4", size=13, face = "bold"), axis.text = element_text(size=13),
 strip.text.x = element_text(color = "dodgerblue4", size=13, face = "bold"))
 

 

