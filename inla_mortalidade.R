library(tidyr)
source("funcoes.R")

################################################################################
## T1 - T4
################################################################################
## 1. Leitura dos dados
dados.t1t4 <- prep.dados("dados/mort_t1_t4.csv", "survt1t4")

## 2. Selecao de modelos
sel.t1t4 <- mod.sel(dados.t1t4)
## Valores do waics ordenados: primeiro é o modelo selecionado
sel.t1t4$waics
## Formula do modelo selecionado
sel.t1t4$formula
## Resumo dos coeficientes do modelo selecionado
sel.t1t4$sel.mod$summary.fixed

## 3. Posterior das odds de P/NP em LI e NU,
## e de NU/LI em P e NP, apenas pelos efeitos fixos
(odfix.t1t4 <- odds.tipo.grupo(sel.t1t4$sel.mod))
## Salva numa planilha
write.csv(odfix.t1t4, file="outputs/estimated_effects_on_odds_t1t4.csv")

## 4. Probs previstas para cada especie, incluindo variacao entre blocos
## Dados para previsao: dataframe com nomes das especies e seus grupos, com NA para os demais valores
newdata <- newd.spp(dados.t1t4)
## Previstos para os valores em newdata
pspp.t1t4 <- pred.new(newdata = newdata, dados = dados.t1t4, formula = sel.t1t4$formula)
## Junta com as probabilidade observadas
pspp.t1t4.clean <- probs.spp(dados.t1t4, pspp.t1t4)
## Salva numa planilha
write.csv(pspp.t1t4, file = "outputs/post_prob_surv_spp_t1t4.csv")

## 5. Graficos
## Nao pioneiras
pspp.t1t4.clean %>%
    filter(grupo == "NP") %>%
    p1() + xlab("")
ggsave(filename="outputs/NP.sp.t1t4.pdf", width = 9, height = 7.5, device = cairo_pdf)
## Pioneiras
pspp.t1t4.clean %>%
    filter(grupo == "P") %>%
    p1() + xlab("")
ggsave("outputs/P.sp.t1t4.pdf", width = 9, height = 7.5, device = cairo_pdf)


################################################################################
## T1 - T7
################################################################################
## 1. Leitura dos dados
dados.t1t7 <- prep.dados("dados/mort_t1_t7.csv", "survt1t7")

## 2. Selecao de modelos
sel.t1t7 <- mod.sel(dados.t1t7)
## Valores do waics ordenados: primeiro é o modelo selecionado
sel.t1t7$waics
## Formula do modelo selecionado
sel.t1t7$formula
## Resumo dos coeficientes do modelo selecionado
sel.t1t7$sel.mod$summary.fixed

## 3. Posterior das odds de P/NP em LI e NU,
## e de NU/LI em P e NP, apenas pelos efeitos fixos
(odfix.t1t7 <- odds.tipo.grupo(sel.t1t7$sel.mod))
## Salva numa planilha
write.csv(odfix.t1t7, file="outputs/estimated_effects_on_odds_t1t7.csv")

## 4. Probs previstas para cada especie, incluindo variacao entre blocos
## Dados para previsao: dataframe com nomes das especies e seus grupos, com NA para os demais valores
newdata <- newd.spp(dados.t1t7)
## Previstos para os valores em newdata
pspp.t1t7 <- pred.new(newdata = newdata, dados = dados.t1t7, formula = sel.t1t7$formula)
## Junta com as probabilidade observadas
pspp.t1t7.clean <- probs.spp(dados.t1t7, pspp.t1t7)
## Salva numa planilha
write.csv(pspp.t1t7, file = "outputs/post_prob_surv_spp_t1t7.csv")

## 5. Graficos
## Nao pioneiras
pspp.t1t7.clean %>%
    filter(grupo == "NP") %>%
    p1() + xlab("")
ggsave(filename="outputs/NP.sp.t1t7.pdf", width = 9, height = 7.5, device = cairo_pdf)
## Pioneiras
pspp.t1t7.clean %>%
    filter(grupo == "P") %>%
    p1() + xlab("")
ggsave("outputs/P.sp.t1t7.pdf", width = 9, height = 7.5, device = cairo_pdf)
