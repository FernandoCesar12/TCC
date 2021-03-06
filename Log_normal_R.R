# Entrando com os pacotes necessários

library(survival)
library(plyr)

# Entrando com o banco de dados

dados <- read.csv("C:\\Users\\ferna\\OneDrive\\Área de Trabalho\\Sistema\\Análise Exploratória do banco\\Banco2.csv",sep=';')

dados <- dados[dados$Tempo > 50,]


dados$Objeto[which(dados$Objeto == 'FUNDEF/Fundo de Manutenção e Desenvolvimento do Ensino Fundamental e de Valorização do Magistério')] <- 'FUNDEF'

# Arrumando erros na coluna Tipo_Movimentação

dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Ato ordinatorio')] <- 'Ato ordinatório'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Antecipação de tutela')] <- 'Antecipação de tutela'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Ato ordinatorio')] <- 'Conclusão-Ato ordinatório'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Ato ordinatório')] <- 'Ato ordinatório'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Audiência')] <- 'Audiência'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Conclusão')] <- 'Conclusão'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Decurso de Prazo')] <- 'Decurso de Prazo'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Distribuição')] <- 'Distribuição'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Documento')] <- 'Documento'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Expedição de documento')] <- 'Expedição de documento'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Incompetência')] <- 'Incompetência'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Juntada')] <- 'Juntada'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Mandado')] <- 'Mandado'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Mero expediente')] <- 'Mero expediente'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Mudança de Classe Processual')] <- 'Mudança de Classe Processual'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Não-Acolhimento de Embargos de Declaração')] <- 'Não-Acolhimento de Embargos de Declaração'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Outros')] <- 'Outros'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Perempção, litispendência ou coisa julgada')] <- 'Perempção, litispendência ou coisa julgada'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Petição')] <- 'Petição'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Recebimento')] <- 'Recebimento'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Redistribuição')] <- 'Redistribuição'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Remessa')] <- 'Remessa'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Improcedência')] <- 'Improcedência'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Desistência')] <- 'Desistência'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Reativação')] <- 'Reativação'
dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Conclusão-Requisição de Informações')] <- 'Requisição de Informações'


# Removendo valores com contagens inferiores a 2

dados <- dados[dados$Tipo_Movimentação != 'Ausência de pressupostos processuais',]
dados <- dados[dados$Tipo_Movimentação != 'Baixa Definitiva',]
dados <- dados[dados$Tipo_Movimentação != 'Cancelamento de Distribuição',]
dados <- dados[dados$Tipo_Movimentação != 'Desistência',]
dados <- dados[dados$Tipo_Movimentação != 'Improcedência',]
dados <- dados[dados$Tipo_Movimentação != 'Julgamento em Diligência',]

##################################################################################################
#                          Calculando o Modelo sem a presença de Covariáveis
##################################################################################################

weibull <- survreg(Surv(Tempo, Censura)~1, data = dados, dist='weibull')

exponential <- survreg(Surv(Tempo, Censura)~1,data = dados, dist='exponential')

loglogistic <- survreg(Surv(Tempo, Censura)~1, data = dados, dist='loglogistic')

lognormal <- survreg(Surv(Tempo, Censura)~1, data = dados, dist='lognormal')

##################################################################################################
#                                          Utilizando o AIC
##################################################################################################

AIC(weibull)

AIC(exponential)

AIC(loglogistic)

AIC(lognormal) # Esse ganhou como melhor modelo

##################################################################################################
#                          Gráfico do modelo sem a presença das covariáveis
##################################################################################################

fit <- survfit(Surv(Tempo, Censura) ~ 1,type="kaplan-meier",data = dados)

plot(fit,conf.int=FALSE, main="Ajuste dos modelos sem a presença de covariáveis",
     xlab = "Tempo", ylab = "Probabilidade de Sobrevivência")
grid(4, 5, lty = "dotted")

lines(x = predict(weibull, type = "quantile", na.action = na.omit,p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "red", lty=1)

lines(x = predict(exponential, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "blue",lty=2)

lines(x = predict(loglogistic, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "green",lty=3)

lines(x = predict(lognormal, type = "quantile", p = seq(0.01, 0.99, by=.01))[1,],
      y = rev(seq(0.01, 0.99, by = 0.01)),
      col = "orange",lty=4)

legend(650,0.8, legend=c("Weibull", "Exponencial", "Log-Logistica", "Log-Normal","Kaplan Meier"),
       col=c("red", "blue", "green","orange",'Black'), lty=1:4, cex=0.8,box.lty=0, y.intersp = 0.3, x.intersp = 0.2,
       xpd = TRUE, horiz=FALSE, bty = "n",
       xjust=0, yjust=0)

##################################################################################################
#               Calculando os modelos para Log-Normal com a presença das covariáveis
##################################################################################################

dados$Tipo_Movimentação[which(dados$Tipo_Movimentação == 'Redistribuição')] <- 'AARedistribuição'

Modelo1_Lognormal <- survreg(Surv(Tempo, Censura)~Tipo_Movimentação, data = dados, dist='lognormal')

Modelo2_Lognormal <- survreg(Surv(Tempo, Censura)~Objeto, data = dados, dist='lognormal')

Modelo3_Lognormal <- survreg(Surv(Tempo, Censura)~Tipo_Movimentação + Objeto, data = dados, dist='lognormal')

Modelo4_Lognormal <- survreg(Surv(Tempo, Censura)~Tipo_Movimentação * Objeto, data = dados, dist='lognormal')

##################################################################################################
#                                          Utilizando o AIC
##################################################################################################

AIC(Modelo1_Lognormal)

AIC(Modelo2_Lognormal)

AIC(Modelo3_Lognormal) # Esse ganhou como melhor modelo

AIC(Modelo4_Lognormal) 

summary(Modelo3_Lognormal)

########################################################################################################
#                                     Gráfico de residuos Cox-snell
########################################################################################################

library(ggplot2)

fit1 <- coxph(formula = Surv(Tempo, Censura) ~ Objeto,
              data    = dados)

dados$resid_mart <- residuals(fit1, type = "martingale")

## Cox-Snell residuals
dados$resid_coxsnell <- -(dados$resid_mart - dados$Censura)

## Fit model on Cox-Snell residuals (Approximately Expo(1) distributed under correct model)
fit_coxsnell <- coxph(formula = Surv(resid_coxsnell, Censura) ~ 1,
                      data    = dados,
                      ties    = c("efron","breslow","exact")[1])

## Nelson-Aalen estimator for baseline hazard (all covariates zero)
df_base_haz <- basehaz(fit_coxsnell, centered = FALSE)

## Plot
ggplot(data = df_base_haz, mapping = aes(x = time, y = hazard)) +
  geom_point() +
  scale_x_continuous(limit = c(0,3.5)) +
  scale_y_continuous(limit = c(0,3.5)) +
  labs(subtitle = "5ª Vara da Justiça Federal",
       x = "Cox-Snell residuals as pseudo observed times",
       y = "Estimated cumulative hazard at pseudo observed times") +
  theme_bw() + theme(legend.key = element_blank())


########################################################################################################
#                           Algumas medidas em análise de Sobrevivência
########################################################################################################

like_lognormal<-function(parametro,tempo,delta){
  L1<-dlnorm(tempo,parametro[1],parametro[2],log=TRUE)
  L2<-plnorm(tempo,parametro[1],parametro[2],log.p=TRUE,lower.tail=FALSE)
  -sum(L1*delta + L2*(1-delta))}

chute.inicial<-c(2,2)
tempo<-dados$Tempo
delta<-dados$Censura

#tempo <- c(0.1,0.3,0.3,0.4,0.4,0.5,0.5,0.6,0.6,0.7,0.8,1,1,1.1,1.2,1.3,1.8,2.6,2.9,4.3,5.6,6.6,10.7,11.9,12.5,26.1,65,86.9,90.5,120.6)
#delta<-c(1,0,1,1,1,0,1,1,1,1,1,0,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1)

emv<-nlm(like_lognormal,chute.inicial,hessian=TRUE,tempo=tempo,delta=delta)
emv

var_cov = solve(emv$hessian) # fornece a matriz de variância-covariância de μ e σ
var_cov

############# Intervalo de confiança para μ 

Limite_inferior = emv$estimate[1]-2.238*sqrt(var_cov[1])

Limite_superior = emv$estimate[1]+2.238*sqrt(var_cov[1])

matriz <- matrix(c(emv$estimate[1],Limite_superior,Limite_inferior),ncol = 3,nrow = 1)
colnames(matriz) <- c("μ","Limite Superior", "Limite Inferior")

matriz

############# Intervalo de confiança para σ

Limite_inferior = emv$estimate[2]-2.238*sqrt(var_cov[4])

Limite_superior = emv$estimate[2]+2.238*sqrt(var_cov[4])

matriz <- matrix(c(emv$estimate[2],Limite_superior,Limite_inferior),ncol = 3,nrow = 1)
colnames(matriz) <- c("σ","Limite Superior", "Limite Inferior")

matriz

############# Correlação entre μ e σ

correlacao <- var_cov[2]/(sqrt(var_cov[1])*sqrt(var_cov[4]))
correlacao

# Como a correlação entre μ e σ não é tão grande, pode-se considerar os intervalos de confiança apresentados

############# Intervalo de confiança para o tempo médio de vida

E_T = exp(emv$estimate[1]+((emv$estimate[2]^2)/2))
VAR_T = exp((2*emv$estimate[1])+(emv$estimate[2]^2))*(exp(emv$estimate[2]^2)-1)

Limite_inferior = E_T-1.96*sqrt(VAR_T/sum(delta))

Limite_superior = E_T+1.96*sqrt(VAR_T/sum(delta))

matriz <- matrix(c(E_T,Limite_superior,Limite_inferior),ncol = 3,nrow = 1)
colnames(matriz) <- c("E[T]","Limite Superior", "Limite Inferior")

matriz

library(dplyr)
dados %>%
  group_by(Censura) %>%
  dplyr::summarize(Mean = mean(Tempo, na.rm=TRUE)) # Conferindo se o valor estimado da média esta coerente


#### Obtenção das estimativas de S(t), via Kaplan-Meier e modelo Log-Normal.

dados_2<-Surv(dados$Tempo,dados$Censura)

km<-survfit(dados_2~1)

plot(km,conf.int=F,main="Função de Sobrevivência Estimada",xlab="Tempo",ylab="S(t)")

t <- (1:140000)/100

estimativa<-emv$estimate

s.lognormal<-plnorm(t,estimativa[1],estimativa[2],lower.tail=F)

points(t,s.lognormal,type="l",lty=2)


legend(650,0.95,c("___ Kaplan-Meier","------ Log-Normal"), cex=1,box.lty=0, y.intersp = 0.45, x.intersp = 0.2,xpd = TRUE, horiz=FALSE, bty = "n")

estimativa2<-survreg(Surv(tempo,delta)~1,dist="lognormal")
mu<-estimativa2$coefficients
sigma<-estimativa2$scale

require(emdbook)

#### calculo da variância de S(t)

var.sob<-numeric(length(t))
for (i in 1:length(t)) {
  x<-t[i]
  var.sob[i]<-deltavar(fun = 1-pnorm((log(x)-mu)/sigma),Sigma=Sigma,meanval=c(mu=estimativa[1],sigma=estimativa[2]))
}

#### Construindo IC 95%
LI.sob<-s.lognormal - qnorm(0.975)*var.sob^.5
LS.sob<-s.lognormal + qnorm(0.975)*var.sob^.5

### Intervalo simétrico (truncando em 0 e 1)
LI.sob[which(LI.sob<0)]<-0
LS.sob[which(LS.sob>1)]<-1


points(t,LI.sob,type="l",lty=3,col=4)
points(t,LS.sob,type="l",lty=3,col=4)

# Estou considerando que o valor do intercepto é referente ao μ e o scale referente ao σ
# Eu utilizo esses valores para estimação da variância da função de sobrevivência e posteriormente seus intervalos de confiança

# A coluna Value no summary retorna a diferença do valor médio da variável selecionada no intercepto e as outras
