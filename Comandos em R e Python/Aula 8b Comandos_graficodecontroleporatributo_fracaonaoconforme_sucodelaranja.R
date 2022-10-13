# SME0824 Gestão da Qualidade
# Por Cibele Russo - ICMC USP
# Gráficos de controle Xbarra e R
# Considere que 25 amostras serão consideradas para construir os limites de controle


####### Graficos de controle para fracao nao conforme

amostras <- read.csv("https://raw.githubusercontent.com/cibelerusso/Gestao-da-qualidade/main/Dados/Sucodelaranja.csv",header=TRUE,sep=",")
View(amostras)
attach(amostras)

n<-50

D <- amostras$Embalagens_naoconformes
D.inicial <- (amostras$Embalagens_naoconformes[1:30])


### Grafico de controle inicial com 30 amostras
pbarra = sum(D.inicial)/(length(D.inicial)*n)
LSC.inicial <- pbarra + 3*sqrt(pbarra *(1-pbarra)/n)
LC.inicial <- pbarra
LIC.inicial <- max(pbarra - 3*sqrt(pbarra *(1-pbarra)/n),0)


plot(D.inicial/n, type="b", ylim=range(D.inicial/n, LSC.inicial, LC.inicial, LIC.inicial), pch=16, xlab="Amostras")
abline(LSC.inicial,0)
abline(LC.inicial,0)
abline(LIC.inicial,0)
identify(D.inicial/n)

### Grafico de controle com observacoes iniciais, excluindo as amostras 15 e 23
D.revisto <- (amostras$Embalagens_naoconformes[(1:30)[-c(15,23)]])
pbarra = sum(D.revisto)/(length(D.revisto)*n)

LSC.revisto <- pbarra + 3*sqrt(pbarra *(1-pbarra)/n)
LC.revisto <- pbarra
LIC.revisto <- max(pbarra - 3*sqrt(pbarra *(1-pbarra)/n),0)

plot(D.inicial /n, type="b", ylim=range(D.inicial/n, LSC.revisto, LC.revisto, LIC.revisto), pch=16, xlab="Amostras")

abline(LSC.revisto , 0)
abline(LC.revisto , 0)
abline(LIC.revisto , 0)
identify(D.inicial/n)


### Grafico de controle apos ajuste na maquina, com limites de controle revistos
D.final <- (amostras$Embalagens_naoconformes)
pbarra = sum(D.revisto )/(length(D.revisto )*n)

LSC.revisto <- pbarra + 3*sqrt(pbarra *(1-pbarra)/n)
LC.revisto <- pbarra
LIC.revisto <- max(pbarra - 3*sqrt(pbarra *(1-pbarra)/n),0)

plot(D.final/n, type="b", ylim=range(D.final/n, LSC.revisto, LC.revisto, LIC.revisto), pch=16, xlab="Amostras")
abline(LSC.revisto ,0)
abline(LC.revisto ,0)
abline(LIC.revisto ,0)
identify(D.revisto/n)

### Grafico de controle somente apos ajuste na maquina, com limites de controle revistos
D.aposajuste <- (amostras$Embalagens_naoconformes[31:54])
pbarra = sum(D.aposajuste )/(length(D.aposajuste )*n)
LSC.aposajuste <- pbarra + 3*sqrt(pbarra *(1-pbarra)/n)
LC.aposajuste<- pbarra
LIC.aposajuste <- max(pbarra - 3*sqrt(pbarra *(1-pbarra)/n),0)

plot(31:54,D.aposajuste/n, type="b", ylim=range(D.aposajuste/n, LSC.aposajuste, LC.aposajuste, LIC.aposajuste), pch=16, xlab="Amostras")
abline(LSC.aposajuste ,0)
abline(LC.aposajuste,0)
abline(LIC.aposajuste,0)
identify(D.final/n)


### Grafico de controle apos ajuste na maquina, com limites de controle revistos
D.final <- (amostras$Embalagens_naoconformes)
pbarra = sum(D.revisto )/(length(D.revisto )*n)

LSC.revisto <- pbarra + 3*sqrt(pbarra *(1-pbarra)/n)
LC.revisto <- pbarra
LIC.revisto <- max(pbarra - 3*sqrt(pbarra *(1-pbarra)/n),0)

plot(D.final/n, type="b", ylim=range(D.final/n, LSC.revisto, LC.revisto, LIC.revisto, 0, 0.6), pch=16, xlab="Amostras")

abline(LSC.revisto ,0, lty=2)
abline(LC.revisto ,0, lty=2)
abline(LIC.revisto ,0, lty=2)

abline(LSC.aposajuste ,0, col=2)
abline(LC.aposajuste,0, col=2)
abline(LIC.aposajuste,0, col=2)
identify(D.revisto/n)
legend(30,.6, legend=c("Antes do ajuste", "Após ajuste"),
       col=c("black", "red"), lty=c(2,1), cex=1)


# Outra forma de fazer
par(new=F)
D.final <- (amostras$Embalagens_naoconformes)
pbarra = sum(D.revisto )/(length(D.revisto )*n)
LSC.revisto <- pbarra + 3*sqrt(pbarra *(1-pbarra)/n)
LC.revisto <- pbarra
LIC.revisto <- max(pbarra - 3*sqrt(pbarra *(1-pbarra)/n),0)
plot(D.final/n, type="b", ylim=range(D.final/n, LSC.revisto, LC.revisto, LIC.revisto, 0, 0.6), pch=16, xlab="Amostras")


x<-c(0,30)
y<-c(LSC.revisto,LSC.revisto)
polygon(x, y, lty=2)

x<-c(0,30)
y<-c(LC.revisto,LC.revisto)
polygon(x,y, lty=2)

x<-c(0,30)
y<-c(LIC.revisto,LIC.revisto)
polygon(x,y, lty=2)


x<-c(31,54)
y<-c(LSC.aposajuste,LSC.aposajuste)
polygon(x,y, lty=1, col=2)

x<-c(31,54)
y<-c(LC.aposajuste,LC.aposajuste)
polygon(x,y, lty=1, col=2)

x<-c(31,54)
y<-c(LIC.aposajuste,LIC.aposajuste)
polygon(x,y, lty=1, col=2)

legend(32,.6, legend=c("Antes do ajuste", "Após ajuste"),
       lty=c(2,1), cex=1)

### Grafico de controle com limites de controle revistos e apos ajuste
plot(D.final/n, type="b", xlim=range(1,54), ylim=range(D.final/n, LSC.revisto, LC.revisto, LIC.revisto, LSC.aposajuste, LC.aposajuste, LIC.aposajuste), pch=16, xlab="Amostras", ylab="")
abline(LSC.revisto ,0, lty=2)
abline(LC.revisto ,0, lty=2)
abline(LIC.revisto ,0, lty=2)

par(new=T)
plot(31:54, D.aposajuste/n, type="b", xlim=range(1,54), col=2, ylim=range(D.final/n, LSC.revisto, LC.revisto, LIC.revisto, LSC.aposajuste, LC.aposajuste, LIC.aposajuste), pch=16, xlab="Amostras", ylab=expression(bar(p)), main="Grafico p")

abline(LSC.aposajuste ,0, col=2)
abline(LC.aposajuste,0, col=2)
abline(LIC.aposajuste,0, col=2)

legend(30,.49, legend=c("Antes do ajuste", "Após ajuste"),
       col=c("black", "red"), lty=c(2,1), cex=1)

# Teste de hipóteses para a comparação de proporções

prop.test(c(sum(D.revisto),sum(D.aposajuste)),c(28*50,24*50), alternative = "greater")


# Exercício: refaça os gráficos usando o ggplot2 ou Python
