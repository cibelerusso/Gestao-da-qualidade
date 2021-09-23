# SME0824 Gestão da Qualidade
# Por Cibele Russo - ICMC USP
# Gráficos de controle Xbarra e R
# Considere que 25 amostras serão consideradas para construir os limites de controle

####### Graficos de controle para nao conformidades
setwd("/hdd/Disciplinas/!2021 Gestão da Qualidade/Dados/")
dados <- read.csv("Cozimento.csv", header=TRUE, dec=',')


amostras <- dados[,2:6]
row.names(amostras) <- dados[,1]

# Grafico X barra e R
library(qcc)

# Usando as amostras 'piloto'
qcc(amostras[1:25,], type='xbar')
qcc(amostras[1:25,], type='R')

Limites_Xbarra  = qcc(amostras[1:25,], type='xbar')$limits
Limites_R = qcc(amostras[1:25,], type='R')$limits

# Todas as observações
qcc(amostras, type='xbar', limits = Limites_Xbarra)
qcc(amostras, type='R', limits = Limites_R)


# Amostras piloto
qcc(amostras[1:25,], type='S')

Limites_S = qcc(amostras[1:25,], type='S')$limits

# Todas as observações

qcc(amostras, type='S', limits = Limites_S)




# Gráficos à mão com ggplot
library(ggplot2)

amostra_piloto <- dados[1:25,]

LC.R <- mean(amostra_piloto$R) # R barra

(LC.Xbarra <- mean(amostra_piloto$Xbarra)) # (X barra) barra


# Limites de controle para X barra

(LIC.Xbarra <- LC.Xbarra - 0.577 * LC.R)
(LSC.Xbarra <- LC.Xbarra + 0.577 * LC.R)

#Gráfico Xbarra para a amostra toda

(xbar <- ggplot(dados, aes(x=n_amostra, y=Xbarra)) +
    geom_point() + geom_line() +
    geom_hline(aes(yintercept = LIC.Xbarra), linetype = "solid") +
    geom_hline(aes(yintercept = LC.Xbarra), linetype = "dotdash") +
    geom_hline(aes(yintercept = LSC.Xbarra), linetype = "solid") +
    labs(x= "Amostras", y = "Médias das amostras", title="Gráfico X barra"))



# Gráfico de controle para R


LIC.R <- 0 * LC.R

LSC.R <- 2.114 * LC.R

(r <- ggplot(dados, aes(x=n_amostra, y=R)) +
    geom_point() + geom_line() +
    geom_hline(aes(yintercept = LIC.R), linetype = "solid") +
    geom_hline(aes(yintercept = LC.R), linetype = "dotdash") +
    geom_hline(aes(yintercept = LSC.R), linetype = "solid")+
    labs(x= "Amostras", y = "Amplitude das amostras", title="Gráfico R"))




# Os boxplots são informativos nesse caso?

attach(dados)
X <- c(X1, X2, X3, X4, X5)
id <- rep(n_amostra, 5)

id <- as.factor(id)
dados2 <- data.frame(id = id, X = X)

boxplot(X ~ id, data = dados2, col=id)

ggplot(dados2,aes(x=id, y=X)) +
  geom_boxplot(fill=unique(id)) 

