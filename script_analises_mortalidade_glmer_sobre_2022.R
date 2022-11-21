### Analises de mortalidade resumo conferencia sobre 2022 
#require(tidyverse)
require(dplyr)
require(tidyr)
require(lme4)
require(bbmle)
require(car)
require(MuMIn)
require(MASS)
require(effects)
require(parallel)

#getwd()
#setwd("~/Documentos/SIMPOSIOS/2022_SOBRE/Analises Mortalidade")

mort.tudo<-read.csv("dados_mortalidade_t1_t7_completo.csv", as.is=FALSE)
summary(mort.tudo)
str(mort.tudo)

##Transformando variaveis em fator
cols<-c("bloco", "plot","cod_sp")
mort.tudo[cols]<-lapply(mort.tudo[cols], as.factor)
str(mort.tudo)


## Excluindo dados que não serão usados nessas análises
## Removendo espécies que não são dos núcleos A e B , removendo núcleos que não são A e B, e 
## mantendo apenas dados de plantas sem danos antrópicos gerais (excluir_total=0):
mort.sobre22<-mort.tudo
mort.sobre22<-filter(mort.tudo, !nome_especie %in% c("Annona glabra", 
                                                  "Myrcia bicarinata",
                                                  "Myrsine venosa",
                                                  "Schizolobium parahyba",
                                                  "Tabebuia cassinoides",
                                                  "Eugenia neoglomerata"), 
                     !tipo_nu %in% c("C","D","F","G","H"),
                     excluir_total==0)
                     
summary(mort.sobre22)


##Criando os dataframes de cada intervalo de tempo e a variável resposta para o intervalo específico

##T1 a T2 (6 meses)
mort.t1.t2<-filter(mort.sobre22, excl_mort_t1==0, excl_mort_t2==0)
summary(mort.t1.t2)


mort.t1.t2$survt1t2 <- (ifelse(mort.t1.t2$survt1==1 & mort.t1.t2$survt2==1,1,
                    ifelse(mort.t1.t2$survt1==1 & mort.t1.t2$survt2==0,0,
                    ifelse(mort.t1.t2$survt1==0 & mort.t1.t2$survt2==1,NA,
                    ifelse(mort.t1.t2$survt1==0 & mort.t1.t2$survt2==0,NA,
                    NA)))))
                                   
write.csv(mort.t1.t2,"mort_t1_t2.csv", row.names=FALSE)           

##T1 a T4 (2 anos)
mort.t1.t4<-filter(mort.sobre22, excl_mort_t1==0, excl_mort_t4==0)
summary(mort.t1.t4)

mort.t1.t4$survt1t4 <- (ifelse(mort.t1.t4$survt1==1 & mort.t1.t4$survt4==1,1,
                        ifelse(mort.t1.t4$survt1==1 & mort.t1.t4$survt4==0,0,
                        ifelse(mort.t1.t4$survt1==0 & mort.t1.t4$survt4==1,NA,
                        ifelse(mort.t1.t4$survt1==0 & mort.t1.t4$survt4==0,NA,
                        NA)))))

write.csv(mort.t1.t4,"mort_t1_t4.csv", row.names=FALSE)           


##T4 a T6 (4 anos)
mort.t4.t6<-filter(mort.sobre22, excl_mort_t4==0, excl_mort_t6==0)
summary(mort.t4.t6)

mort.t4.t6$survt4t6 <- (ifelse(mort.t4.t6$survt4==1 & mort.t4.t6$survt6==1,1,
                               ifelse(mort.t4.t6$survt4==1 & mort.t4.t6$survt6==0,0,
                               ifelse(mort.t4.t6$survt4==0 & mort.t4.t6$survt6==1,NA,
                               ifelse(mort.t4.t6$survt4==0 & mort.t4.t6$survt6==0,NA,
                               NA)))))

write.csv(mort.t4.t6,"mort_t4_t6.csv", row.names=FALSE)           



## T6 a T7 (4 anos)
mort.t6.t7<-filter(mort.sobre22, excl_mort_t6==0, excl_mort_t7==0)
summary(mort.t6.t7)

mort.t6.t7$survt6t7 <- (ifelse(mort.t6.t7$survt6==1 & mort.t6.t7$survt7==1,1,
                        ifelse(mort.t6.t7$survt6==1 & mort.t6.t7$survt7==0,0,
                        ifelse(mort.t6.t7$survt6==0 & mort.t6.t7$survt7==1,NA,
                        ifelse(mort.t6.t7$survt6==0 & mort.t6.t7$survt7==0,NA,
                        NA)))))

write.csv(mort.t6.t7,"mort_t6_t6.csv", row.names=FALSE)           


##T1 a T7 (10 anos)
mort.t1.t7<-filter(mort.sobre22, excl_mort_t1==0, excl_mort_t7==0)
summary(mort.t1.t7)


mort.t1.t7$survt1t7 <- (ifelse(mort.t1.t7$survt1==1 & mort.t1.t7$survt7==1,1,
                        ifelse(mort.t1.t7$survt1==1 & mort.t1.t7$survt7==0,0,
                        ifelse(mort.t1.t7$survt1==0 & mort.t1.t7$survt7==1,NA,
                        ifelse(mort.t1.t7$survt1==0 & mort.t1.t7$survt7==0,NA,
                        NA)))))

write.csv(mort.t1.t7,"mort_t1_t7.csv", row.names=FALSE)           



###Análises de mortalidade
##options(na.action = "na.omit")

##T1->T2

mfull.t1.t2<- glmer(survt1t2~grupo*tipo+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t1.t2)
summary(mfull.t1.t2)


m01.t1.t2<- glmer(survt1t2~grupo+tipo+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t1.t2)
summary(m01.t1.t2)


m02.t1.t2<- glmer(survt1t2~grupo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t2)
summary(m02.t1.t2)


m03.t1.t2<- glmer(survt1t2~tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t2)
summary(m03.t1.t2)


mnull.t1.t2<- glmer(survt1t2~1+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t2)
summary(mnull.t1.t2)


AICtab(mfull.t1.t2,m01.t1.t2, m02.t1.t2, m03.t1.t2, mnull.t1.t2)
summary(mfull.t1.t2)

##criando newdata para colocar preditos
newdata<-expand.grid(grupo=unique(mort.sobre22$grupo),tipo=unique(mort.sobre22$tipo), 
                     bloco=c("2",NA), nome_especie=c("Eugenia astringens",NA))
##str(newdata)

newdata.t1t2<-newdata
newdata.t1t2$pred<-predict(mfull.t1.t2, newdata=newdata, type="response", allow.new.levels=TRUE)


#criando newdata para calulcar ICs, com niveis novos para os fatores aleatorios
newdata.boot= expand.grid(grupo = c("P","NP"), tipo = c("NU", "LI"), bloco ="indet", nome_especie ="indet")

## funcao que recebe o modelo, o newdata, e devolve previstos com efeitos aleatorios
f1 =function(.) predict(., newdata=newdata.boot, allow.new.levels=TRUE, type="response")
## funcao que recebe o modelo, o newdata, e devolve previstos sem efeitos aleatorios
f2 =function(.) predict(., newdata=newdata.boot, allow.new.levels=TRUE, type="response", re.form=NA)
## Testando para o modelot1t2
f1(mfull.t1.t2)

## Bootstrap incluindo efeitos aleatorios
#boot.t1t2.rand = bootMer(mfull.t1.t2, FUN=f1, nsim=1000, parallel = "multicore", ncpus =10)
## Bootstrap do previsto apenas pelos fixos
#boot.t1t2.fix<- bootMer(mfull.t1.t2, FUN=f2, nsim=1000,
#                                    parallel = "multicore", ncpus =10)

f3 <-  function(x)
  c(media=mean(x),quantile(x,0.025),quantile(x,0.975),desvio=sd(x))
apply(boot.t1t2.fix$t,2,f3)
apply(boot.t1t2.rand$t,2,f3)

## Receita do IC dos previstos (FAQ lmer Bolker)
newdata.ic= expand.grid(grupo = c("P","NP"), tipo = c("NU", "LI"))
ilogist = function(x) exp(x)/(1+exp(x))
newdata.ic[,"survt1t2"] = predict(mfull.t1.t2, newdata.ic, re.form = NA)
mm = model.matrix(terms(mfull.t1.t2),newdata.ic)
pvar1 = diag(mm %*% tcrossprod(vcov(mfull.t1.t2),mm))
newdata.ic = mutate(newdata.ic, low = survt1t2 - 2*sqrt(pvar1), 
                    upp = survt1t2 + 2*sqrt(pvar1),
                    ppred = ilogist(survt1t2), plow = ilogist(low), pupp = ilogist(upp))


##T1->T4
##proporcoes observadas
(prop.t1.t4 = 
  group_by(mort.t1.t4, tipo, grupo) %>%
  summarise(N = sum(!is.na(survt1t4)), nsobr=sum(survt1t4, na.rm=TRUE), prop = nsobr/N
))
prop.t1.t4.ic =  cbind(prop.t1.t4, binconf(prop.t1.t4$nsobr, prop.t1.t4$N)[,2:3])
mort.t1.t4=filter(mort.t1.t4,!is.na(survt1t4))
mfull.t1.t4<- glmer(survt1t4~grupo*tipo+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t1.t4)
mfull3.t1.t4<- glmer(survt1t4~grupo*tipo+(1|bloco), 
                    family=binomial, data=mort.t1.t4)
summary(mfull.t1.t4)
coef(mfull.t1.t4)

mfull2.t1.t4<- glmer(survt1t4~grupo*tipo+(1|bloco)+(1|nome_especie)+(1|bloco:nome_especie), 
                    family=binomial, data=mort.t1.t4)
summary(mfull2.t1.t4)
coef(mfull.t1.t4)

m01.t1.t4<- glmer(survt1t4~grupo+tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t4)
summary(m01.t1.t4)


m02.t1.t4<- glmer(survt1t4~grupo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t4)
summary(m02.t1.t4)


m03.t1.t4<- glmer(survt1t4~tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t4)
summary(m03.t1.t4)


mnull.t1.t4<- glmer(survt1t4~1+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t1.t4)
summary(mnull.t1.t4)

AICtab(mfull2.t1.t4, mfull3.t1.t4, mfull.t1.t4,m01.t1.t4, m02.t1.t4, m03.t1.t4, mnull.t1.t4)

summary(mfull.t1.t4)

newdata.t1t4<-newdata
newdata.t1t4$pred<-predict(mfull.t1.t4, newdata=newdata, type="response", allow.new.levels=TRUE)

## Bootstrap incluindo efeitos aleatorios
##boot.t1t4.rand = bootMer(mfull.t1.t4, FUN=f1, nsim=1000, parallel = "multicore", ncpus =10)
## Bootstrap do previsto apenas pelos fixos
# boot.t1t4.fix<- bootMer(mfull.t1.t4, FUN=f2, nsim=1000,
#                         parallel = "multicore", ncpus =10)
# 
# f3 <-  function(x)
#   c(media=mean(x),quantile(x,0.025),quantile(x,0.975),desvio=sd(x))
# 
# apply(boot.t1t4.fix$t,2,f3)
# 
# apply(boot.t1t4.rand$t,2,f3)


## IC fix
newdata.ic= expand.grid(grupo = c("P","NP"), tipo = c("NU", "LI"), bloco="2", nome_especie="Euterpe edulis")
newdata.ic[,"survt1t4"] = predict(mfull.t1.t4, newdata.ic, re.form = NA)
mm = model.matrix(terms(mfull.t1.t4),newdata.ic)
pvar1 = diag(mm %*% tcrossprod(vcov(mfull.t1.t4),mm))
(newdata.ic = mutate(newdata.ic, low = survt1t4 - 2*sqrt(pvar1), 
                    upp = survt1t4 + 2*sqrt(pvar1),
                    ppred = ilogist(survt1t4), plow = ilogist(low), pupp = ilogist(upp))
)

##T4->T6
mfull.t4.t6<- glmer(survt4t6~grupo*tipo+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t4.t6)
summary(mfull.t4.t6)
coef(mfull.t4.t6)


m01.t4.t6<- glmer(survt4t6~grupo+tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t4.t6)
summary(m01.t4.t6)


m02.t4.t6<- glmer(survt4t6~grupo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t4.t6)
summary(m02.t4.t6)


m03.t4.t6<- glmer(survt4t6~tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t4.t6)
summary(m03.t4.t6)


mnull.t4.t6<- glmer(survt4t6~1+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t4.t6)
summary(mnull.t4.t6)

AIC(mfull.t4.t6,m01.t4.t6, m02.t4.t6, m03.t4.t6, mnull.t4.t6)
summary(mfull.t4.t6)
summary(m02.t4.t6)
summary(mnull.t4.t6)

newdata.t4t6<-newdata
newdata.t4t6$pred<-predict(mfull.t4.t6, newdata=newdata, type="response", allow.new.levels=TRUE)


##T6->T7
mfull.t6.t7<- glmer(survt6t7~grupo*tipo+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t6.t7)
summary(mfull.t6.t7)
coef(mfull.t6.t7)


m01.t6.t7<- glmer(survt6t7~grupo+tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t6.t7)
summary(m01.t6.t7)


m02.t6.t7<- glmer(survt6t7~grupo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t6.t7)
summary(m02.t6.t7)


m03.t6.t7<- glmer(survt6t7~tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t6.t7)
summary(m03.t6.t7)


mnull.t6.t7<- glmer(survt6t7~1+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t6.t7)
summary(mnull.t6.t7)

AIC(mfull.t6.t7,m01.t6.t7, m02.t6.t7, m03.t6.t7, mnull.t6.t7)

summary(mfull.t6.t7)
summary(mnull.t6.t7)

newdata.t6t7<-newdata
newdata.t6t7$pred<-predict(mfull.t6.t7, newdata=newdata, type="response", allow.new.levels=TRUE)



##T1->T7

mfull.t1.t7<- glmer(survt1t7~grupo*tipo+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t1.t7)
summary(mfull.t1.t7)


m01.t1.t7<- glmer(survt1t7~grupo+tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t7)
summary(m01.t1.t7)


m02.t1.t7<- glmer(survt1t7~grupo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t7)
summary(m02.t1.t7)


m03.t1.t7<- glmer(survt1t7~tipo+(1|bloco)+(1|nome_especie), 
                  family=binomial, data=mort.t1.t7)
summary(m03.t1.t7)


mnull.t1.t7<- glmer(survt1t7~1+(1|bloco)+(1|nome_especie), 
                    family=binomial, data=mort.t1.t7)
summary(mnull.t1.t7)

AIC(mfull.t1.t7,m01.t1.t7, m02.t1.t7, m03.t1.t7, mnull.t1.t7)
summary(mfull.t1.t7)

newdata.t1t7<-newdata
newdata.t1t7$pred<-predict(mfull.t1.t7, newdata=newdata, type="response", allow.new.levels=TRUE)




