##Cargar paquetes necesarios

library(tidyverse)
library(stevemisc)
library(kableExtra)

parab <- function(x) {-x^2/2}  ##funcion de parabola
expparab <- function(x) {exp(-x^2/2)} ## funcion de parabola exponencial

##calculo de funcion de distribucion binomial. x = valores en eje de Xs, size = numero de intentos, prob = probabilidad

dbinom (x = 5, size = 10, prob = .5)  

##creamos tabla para grafico de ejemplo sobre numero de guerras 

tibble(num_wars = seq(0:7)-1,  ## creamos secuencia para numero de guerras 
       base = dbinom(num_wars, 76, .042), ## estimamos probabilidaad de guerra base 
       rbw = dbinom(num_wars, 76, .012)) ## estimamos probabilidad de guerras realmente grandes

tibble(num_wars = rep(c(0, 1, 2), 100)) %>%  
  arrange(num_wars) %>%
  mutate(period = rep(seq(1:100), 3),
         p = dbinom(num_wars, period, 0.012))  ## genera un tibble con tres columnas: # de guerras, period y probabilidad 



## grafica de probabilidad de guerra base y probabilidad de guerra realmente grande

tibble(num_wars = seq(0:7)-1,
       base = dbinom(num_wars, 76, .042),
       rbw = dbinom(num_wars, 76, .012)) %>%
  gather(var, val, -num_wars) %>%
  mutate(var = ifelse(var == "base", "Tasa Base de Guerra", "Tasa Base de Guerra 'Realmente Grande'")) %>%
  mutate(lab = round(val, 3)) %>%
  ggplot(.,aes(as.factor(num_wars), val, fill=var)) +
  geom_bar(stat = "identity", position = "dodge", color="black") +
  geom_text(aes(label=lab), vjust=-.5, colour="black",
            position=position_dodge(.9), size=3.5, family="Open Sans") +
  theme_minimal() +
  labs(title = "La Probabilidad del # de Guerras (Observadas) en 76 Años, Dadas las Tasas Asumidas de Guerra",
       subtitle = "Sabiendo que la probabilidad de observar un guerra 'realmente grande'es muy pequeña, es altamente probably (p = .404) qur no hayamos observado ninugna en 76 años",
       fill = "",
       x = "Número Observado de Guerras", y = "Probabilidad de Este # de Guerras en un Periodo de 76 Años")


## plot para probabilidad de observar un determinado numero (i.e. 0, 1, 2) de grandes guerras en 100 años

tibble(num_wars = rep(c(0, 1, 2), 100)) %>%
  arrange(num_wars) %>%
  mutate(period = rep(seq(1:100), 3),
         p = dbinom(num_wars, period, 0.012)) %>%
  mutate(cat = case_when(
    num_wars == 0 ~ "Cero Guerras",
    num_wars == 1 ~ "Una Guerra",
    num_wars == 2 ~ "Dos Guerras"),
  cat = fct_relevel(cat, "Cero Guerras", "Una Guerra", "Dos Guerras")) %>%
  ggplot(.,aes(period, p, color=cat, linetype=cat)) +
  geom_line(size=1.1) +
  theme_minimal() +
  labs(y = "Probabilidad de Observar este # de Guerras en 100 años",
       x = "",
       title = "La Probabilidad de Observar un Número Fijo de 'Grandes Guerras' en un Periodo de 100 años",
       color = "", linetype = "",
       subtitle = "Después de 76 años, todavía es más probable que todavía no hayamos observado una 'Gran Guerra' que haber observado solo una.")

## plot de una funcion normal de -4 a 4 en eje de Xs

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = dnorm, color="#003f5c", size=1.5) 

## un pot mas de una funcion normal simple

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  theme_minimal() +
  stat_function(fun = dnorm, color="#003f5c", size=1.5) +
  theme(plot.title = element_text(size=15)) +
  labs(title = "Una Distribución Normal Simple",
       subtitle = "El parámetro mu determina la tendencia central y sigma^2 lo ancho.",
       x = "", y="")

## plot de una parabola normal, usa funcion "parab" definida arriba 

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = parab, color="#003f5c", size=1.5) +
  theme_minimal() 

## plot de una parabola exponencial usando funcion "expparab" definida arriba

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = expparab, color="#003f5c", size=1.5) +
  theme_minimal() 

## un plot mas de una parabola basica

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = parab, color="#003f5c", size=1.5) +
  theme_minimal()  +
  theme(plot.title = element_text(size=20)) +
  labs(title="Una Parábola Básica",
       subtitle = "Hay que notar que la altura está en el 0 porque la parte negativa voltea la parábola hacia abajo.",
       x = "", y="") 

## otro plot de parabola exponencial basica

ggplot(data.frame(x = c(-4, 4)), aes(x)) +
  stat_function(fun = expparab, color="#003f5c", size=1.5) +
  theme_minimal() +
  theme(plot.title = element_text(size=20)) +
  labs(title="Una Parábola Exponencial Negativa",
       subtitle = "Al exponenciar, comprimímos la parábola, ajustamos la altura , y hacemos las colas asintóticas a 0.",
       x = "", y="") 

## calculo para saber altura de curva 

1/sqrt(2*pi)

## calculo de funcion de densidad normal con media = 0 y d.e. = 1

dnorm(0,mean=0,sd=1)

## pnorm nos permite calcular la funcion de distribucion acumulada (CDF) de la funcion normal. Esto es, la probabilidad de que la variable X tome valores menores o iguaes a x. 

pnorm(0, mean=0, sd=1)

## plot funcion de densidad normal, valores a la izquierda de la media

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  theme_minimal() +
  theme(plot.title = element_text(size=30)) +
  stat_function(fun = dnorm, 
                xlim = c(-4,0),
                size=0,
                geom = "area", fill="#F66733", alpha=.5) + 
  stat_function(fun = dnorm, color="#003f5c", size=1.5) 

## see above

ggplot(data.frame(x = c(-4, 4)), aes(x)) + 
  theme_minimal() +
  theme(plot.title = element_text(size=20)) +
  labs(caption="-Infinito a 0 tiene el 50% del área debajo de la curva") + 
  stat_function(fun = dnorm, 
                xlim = c(-4,0),
                size=0,
                geom = "area", fill="#F66733", alpha=.5) + 
  stat_function(fun = dnorm, color="#003f5c", size=1.5) +
  labs(title = "Una Distribución Normal Stándard",
       subtitle = "Hay que notar que la mitad de la distribución está entre -Infinito y 0.")

## la regla 68-90-95-99

pnorm(1,mean=0,sd=1)-pnorm(-1,mean=0,sd=1) ## probabilidad de que datos caigan a 1 d.e. de la media
pnorm(1.645,mean=0,sd=1)-pnorm(-1.645,mean=0,sd=1) ## probabilidad de que datos caigan a 1.645 d.e. de la media
pnorm(1.96,mean=0,sd=1)-pnorm(-1.96,mean=0,sd=1) ## probabilidad de que datos caigan a 1.96 d.e. de la media
pnorm(2.58,mean=0,sd=1)-pnorm(-2.58,mean=0,sd=1) ## probabilidad de que datos caigan a 2.58 d.e. de la media


## plot de regla 68-90-95-99

normal_dist("#003f5c","#F66733", "Open Sans") + 
  theme_minimal() + 
  theme(plot.title = element_text(size=20)) +
  labs(title = "El Área por Debajo de la Distribución Normal",
       subtitle = "Las colas se extienden hacia el infinito y son asintóticas a 0, pero el área completa es igual a 1. 95% de todos los posibles valores están aproximadamente 1.96 unidades standard de la media.",
       y = "Densidad",
       x = "")
