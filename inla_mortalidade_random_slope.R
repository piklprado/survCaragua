library(tidyr)
source("funcoes.R")


################################################################################
## T1 - T7
################################################################################
## 1. Leitura dos dados
dados.t1t7 <- prep.dados("dados/mort_t1_t7.csv", "survt1t7")
dados.t1t7$sp.tipo <- paste(dados.t1t7$nome_especie,dados.t1t7$tipo, sep="_")

## 2. ajuste do modelo
rs.form <- surv ~ tipo*grupo +
                     f(bloco, model = "iid") +
                     f(sp.tipo, model = "iid")
rs.t1t7 <- inla( rs.form,
                data = dados.t1t7,
                family = "binomial",
                control.family = list(link="logit"),
                control.compute = list(dic=TRUE, waic=TRUE, config=TRUE)
                ) %>%
    inla.rerun()

## Resumo dos coeficientes do modelo selecionado
rs.t1t7$summary.fixed
rs.t1t7$summary.random

## 3. Posterior das odds de P/NP em LI e NU,
## e de NU/LI em P e NP, apenas pelos efeitos fixos
odds.tipo.grupo(rs.t1t7)


## 4. Probs previstas para cada especie, incluindo variacao entre blocos
## Dados para previsao: dataframe com nomes das especies e seus grupos, com NA para os demais valores
newdata <- newd.spp(dados.t1t7)
newdata$sp.tipo <- paste(newdata$nome_especie,newdata$tipo, sep="_")
## Previstos para os valores em newdata
pspp.t1t7.rs <- pred.new(newdata = newdata, dados = dados.t1t7, formula = rs.form)
## Junta com as probabilidade observadas
pspp.t1t7.clean.rs <- probs.spp(dados.t1t7, pspp.t1t7.rs)
## Salva numa planilha
write.csv(pspp.t1t7.rs, file = "outputs/post_prob_surv_spp_t1t7.rs.csv")

## 5. Graficos
## Nao pioneiras
pspp.t1t7.clean.rs %>%
    filter(grupo == "NP") %>%
    p1() + xlab("")
ggsave(filename="outputs/NP.sp.t1t7.rs.pdf", width = 9, height = 7.5, device = cairo_pdf)
## Pioneiras
pspp.t1t7.clean.rs %>%
    filter(grupo == "P") %>%
    p1() + xlab("")
ggsave("outputs/P.sp.t1t7.rs.pdf", width = 9, height = 7.5, device = cairo_pdf)
