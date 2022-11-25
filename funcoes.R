library(dplyr)
library(ggplot2)
library(ggthemes)
library(forcats)
library(INLA)

## Leitura e prep dados csv enviados pela Dri
prep.dados <- function(nome, surv.var){
    x <- read.csv(nome)[,c("bloco", "nome_especie", "grupo", "tipo", surv.var)]
    names(x)[5] <- "surv"
    x$bloco <- factor(x$bloco)
    x$nome_especie <- factor(x$nome_especie)
    x$grupo <- factor(x$grupo, levels=c("P","NP"))
    x$tipo <- factor(x$tipo, levels =c("LI","NU"))
    na.omit(x)
}

## Ajuste e selecao dos modelos
mod.sel <- function(dados, ...){
    ##dots <- as.list(...)
    ## List of competing models formulas
    formula.list <- list(
        full = surv ~ tipo*grupo +
            f(bloco, model = "iid") +
            f(nome_especie, model = "iid"),
        full.ad = surv ~ tipo + grupo +
            f(bloco, model = "iid") +
            f(nome_especie, model = "iid"),
        tipo = surv ~ tipo +
            f(bloco, model = "iid") +
            f(nome_especie, model = "iid"),
        grupo = surv ~ grupo +
            f(bloco, model = "iid") +
            f(nome_especie, model = "iid")
    )
    ## Fits models and stores in a list
    mod.list <- lapply(formula.list, function(x, ...) inla(formula =x, ...),
                       data = dados,
                       family = "binomial",
                       control.family = list(link="logit"),
                       control.compute = list(dic=TRUE, waic=TRUE)
                       ) %>%
        lapply(inla.rerun)
    ## WAICs
    waics <- sort(sapply(mod.list, function(x) x$waic$waic))
    ## DICs
    dics <- sort(sapply(mod.list, function(x) x$dic$dic))
    ## Refit the selected model with com marginais etc
    formula <- formula.list[[names(waics[1])]]
    sel.model <- inla(formula,
                      data = dados,
                      family = "binomial",
                      control.family = list(link="logit"),
                      control.predictor = list(link=1, compute=TRUE),
                      control.compute = list(return.marginals.predictor=TRUE, cpo=TRUE, config=TRUE)
                      ) %>%
        inla.rerun()
    list(sel.mod = sel.model, formula = formula, waics = waics, dic = dics)
}
    
## Odds ratio dos efeitos fixos
## Funcao auxiliar
## Posterior das diferencas P - NP em LI e NU, e das diferencas NU-LI em P e NP, apenas pelos efeitos fixos
ff <- function(x){
    Intercept <- x$latent["(Intercept):1",1]
    grupoNP <- x$latent["grupoNP:1",1]
    tipoNU <- x$latent["tipoNU:1",1]
    tipoNUgrupoNP <- x$latent["tipoNU:grupoNP:1",1]
    LI_P <- Intercept 
    LI_NP<- Intercept + grupoNP
    NU_P <- Intercept + tipoNU
    NU_NP<- Intercept + grupoNP + tipoNU + tipoNUgrupoNP 
    c("P-NP (LI)"= exp(LI_P-LI_NP), "P-NP (NU)"= exp(NU_P-NU_NP),
      "NU-LI (P)"=exp(NU_P - LI_P), "NU-LI (NP)"= exp(NU_NP - LI_NP))
}
## Funcao principal: calcula os odds e seus intervalos
odds.tipo.grupo <- function(modelo){
    samp.list <- inla.posterior.sample(n=1000, modelo)
    ## Calcula os odds para cada elemento da amostra
    fix.dif <- sapply(samp.list, ff)
    ## odds ratio entre LI e NU para NP
    ## e diferença entre P e NP em LI
    cbind(mean = apply(fix.dif, 1, mean),
          low = apply(fix.dif,1,quantile, 0.025),
          upp = apply(fix.dif,1,quantile, 0.975))
}

## Probs previstos para novos dados
pred.new <- function(newdata, dados, formula){
    dados.pred <- rbind(dados,newdata)
    ## %>% mutate(nome_especie = factor(nome_especie))
    ## A data.frame with only the independent variables values for prediction
    indice <- (nrow(dados.pred)-nrow(newdata)+1):(nrow(dados.pred))
    ## Refit the model
    model <- inla(formula,
                  data = dados.pred,
                  family = "binomial",
                  control.family = list(link="logit"),
                  control.predictor = list(link=1, compute=TRUE),
                  control.compute = list(return.marginals.predictor=TRUE, cpo=TRUE, config=TRUE)
                  ) %>%
        inla.rerun()
    ## Valores previstos de probabilidade de sobrevivência para as
    ## combinações de grupo e tipo, e seus intervalos de credibilidade,
    ## considerando a variação devido a todos os outros fatores.
    cbind(dados.pred[indice, ], model$summary.fitted.values[indice,])
}

## Prepara dados para previsao de probs por especies
newd.spp <- function(dados){
    unique(dados[, c("nome_especie","grupo", "tipo")]) %>%
    arrange(grupo, nome_especie, tipo) %>%
    mutate(bloco = NA, surv = NA) %>%
        select(bloco, nome_especie, tipo, grupo, surv)
}

## Organiza probs previstas para as especies por tipo com probs observadas
probs.spp <- function(dados, pred){
    preds <-
        dplyr::select(pred, nome_especie, grupo, tipo, mean, sd, "0.025quant", "0.5quant", "0.975quant") %>%
        dplyr::rename(low = "0.025quant", upp = "0.975quant", median = "0.5quant")
    obs <-
        dplyr::group_by(dados, nome_especie, grupo, tipo) %>%
        dplyr::summarise(N = n(), p.surv = mean(surv))
    ##merge(obs,preds, by=c("dados","nome_especie","grupo","tipo"))
    merge(obs,preds)
}

################################################################################
## Graficos
################################################################################
theme_Publication <- function(base_size=14, base_family="helvetica") {
    (theme_foundation(base_size=base_size, base_family=base_family)
        + theme(plot.title = element_text(face = "bold",
                                          size = rel(1.2), hjust = 0.5),
                text = element_text(),
                panel.background = element_rect(colour = NA),
                plot.background = element_rect(colour = NA),
                panel.border = element_rect(colour = NA),
                axis.title = element_text(face = "bold",size = rel(1)),
                axis.title.y = element_text(angle=90,vjust =2),
                axis.title.x = element_text(vjust = -0.2),
                axis.text = element_text(), 
                axis.line = element_line(colour="black"),
                axis.ticks = element_line(),
                panel.grid.major = element_line(colour="#f0f0f0"),
                panel.grid.minor = element_blank(),
                legend.key = element_rect(colour = NA),
                legend.position = "bottom",
                legend.direction = "horizontal",
                legend.key.size= unit(0.2, "cm"),
                ##legend.margin = unit(0, "cm"),
                legend.spacing = unit(0.2, "cm"),
                legend.title = element_text(face="italic"),
                plot.margin=unit(c(10,5,5,5),"mm"),
                strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                strip.text = element_text(face="bold")
                ))
    
}


## Grafico das proporcoes de sobrevivencia e intervalos previstos pelos modelos
p1 <- function(dados){
    dados %>%
        mutate(nome_especie = fct_reorder(nome_especie, p.surv, .fun="max")) %>%
        ggplot(aes(x = nome_especie, colour = tipo)) +
        geom_point(aes(y = p.surv), position=position_dodge(width =0.25), size=3) +
        geom_linerange(aes(ymin=low, ymax=upp), position=position_dodge(width =0.25), size=1.5) +
        ##coord_flip() +
        theme_Publication() +
        theme(axis.text.x = element_text(angle = 45, hjust=1))
}
