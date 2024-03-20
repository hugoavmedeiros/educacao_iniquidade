########## ETL IDEPE ESCOLAS ##########
## PACOTES ##
pacman::p_load(car, caret, corrplot, cowplot, cluster, data.table, dplyr, factoextra, fastDummies, feisr, foreign, ggplot2, gplots, gridExtra,jtools, kableExtra, knitr, lmtest, MASS, multipanelfigure, plm, readxl, REEMtree, SmartEDA, stargazer, texreg, tidyverse) # carregar pacotes necessários para a análise

## ETL PRÉ-AED ##
# LEITURA #
idepe_escolas <- fread('idepe_escolas_final.csv', dec = ",", encoding = 'Latin-1', stringsAsFactors = T) # carregar base de dados
municipos_pe <- read_excel("../microdados/municipios_pe/municipos_pe_indicadores_basicos.xlsx")
municipos_pe <- municipos_pe %>% mutate(across(c(ano), factor))
# TRATAMENTO PRÉ-AED #
glimpse(idepe_escolas) # olhada nos dados
idepe_escolas <- idepe_escolas %>% mutate(across(c(ano, cod_municipio, cod_escola), factor)) # acertar classes dos dados

# AED
ExpData(data=idepe_escolas, type=2)

# MATRIZ SOMBRA  - TESTE DE ALEATORIDADE NOS NA #
idepe_escolas <- fastDummies::dummy_cols(idepe_escolas, select_columns = c("tp_escola", 'TP_LOCALIZACAO'))

x <- as.data.frame(abs(is.na(idepe_escolas)))
y <- x[which(sapply(x, sd) > 0)]
cor(y) # observa a correlação entre variáveis
cor(idepe_escolas[ , c(9:13, 15, 17:28)], y, use="pairwise.complete.obs") # busca padrões entre os valores específicos das variáveis e os NA
corrplot(cor(idepe_escolas[ , c(9:13, 15, 17:28)], y, use="pairwise.complete.obs"))

## ETL PÓS-AED ##
# TRATAMENTO PÓS-AED #
idepe_escolas <- idepe_escolas %>% filter_at(vars(
  idepe, 
  tdi_3em, 
  IN_INTERNET, 
  IN_NOTURNO), all_vars(!is.na(.)))
ExpData(data=idepe_escolas, type=2)

# conversão da base original em formato pdataframe
idepe_escolas_plm <- pdata.frame(idepe_escolas, index = c('cod_escola', 'ano')) 

# análise do tipo de painel
pdim(idepe_escolas_plm) # n>T

# SUMARIZAÇÃO #
# idepe_escolas <- fastDummies::dummy_cols(idepe_escolas, select_columns = c("tp_escola", 'TP_LOCALIZACAO'))
municipos_pe_rds = municipos_pe %>% group_by(ano, rd) %>% dplyr::summarize(
  pib_total = sum(pib), 
  populacao_total = sum(populacao), 
  pib_capita = pib_total / populacao_total)

idepe_rds = idepe_escolas %>% group_by(ano, rd) %>% dplyr::summarize(
  idepe = mean(idepe), 
  tdi_3em = mean(tdi_3em), 
  integral = sum(tp_escola_INTEGRAL), 
  regular = sum(tp_escola_REGULAR), 
  semi = sum(tp_escola_SEMI_INTEGRAL), 
  tecnica = sum(tp_escola_TÉCNICA), 
  totalIntegral = integral+tecnica+semi, 
  urbana = sum(TP_LOCALIZACAO_Urbana), 
  rural = sum(TP_LOCALIZACAO_Rural), 
  total = urbana + rural, 
  internet = sum(IN_INTERNET), 
  noturno = sum(IN_NOTURNO), 
  eja = sum(IN_EJA), 
  matriculas = sum(QT_MAT_BAS), 
  matInt = sum(QT_MAT_MED_INT), 
  internetTx = internet / total, 
  noturnoTx = noturno / total, 
  ejaTx = eja / total, 
  matIntTx = matInt / matriculas, 
  integralTx = integral / total, 
  regularTx = regular / total, 
  semiTx = semi / total, 
  tecnicaTx = tecnica / total, 
  urbanaTx = urbana / total, 
  idepe2 = idepe*idepe, 
  branco = sum(QT_MAT_BAS_BRANCA), 
  masculino = sum(QT_MAT_BAS_MASC), 
  feminino = sum(QT_MAT_BAS_FEM), 
  brancoTx = branco / matriculas, 
  femininoTx = feminino / matriculas) 

idepe_rds_aux <- aggregate(x = idepe_escolas$idepe, by = list(idepe_escolas$rd, idepe_escolas$ano), FUN = "sd")

# idepe_rds <- cbind(idepe_rds, idepe_rds_aux) 
idepe_rds <- left_join(idepe_rds, idepe_rds_aux, by=c('ano'='Group.2', 'rd'='Group.1'))
idepe_rds <- left_join(idepe_rds, municipos_pe_rds, by=c('ano', 'rd'))
# idepe_rds <- idepe_rds %>% dplyr::select(-c('Group.1', 'Group.2')) %>% rename(sd_idepe = x)
idepe_rds <- idepe_rds %>% rename(sd_idepe = x)

idepe_rds <- idepe_rds %>% filter_at(vars(sd_idepe), all_vars(!is.na(.)))

idepe_rds$integralD <- ifelse(idepe_rds$totalIntegral > 0, 1, 0)
idepe_rds$tecnicaD = ifelse(idepe_rds$tecnica > 0, 1, 0)

idepe_rds = idepe_rds %>% dplyr::mutate(
  integral_capita = 100000*integral / populacao_total, 
  regular_capita = 100000*regular / populacao_total, 
  semi_capita = 100000*semi / populacao_total, 
  tecnica_capita = 100000*tecnica / populacao_total, 
  urbana_capita = 100000*urbana / populacao_total, 
  internet_capita = 100000*internet / populacao_total, 
  noturno_capita = 100000*noturno / populacao_total, 
  eja_capita = 100000*eja / populacao_total, 
  matriculas_capita = 100000*matriculas / populacao_total) 

# conversão em formato pdataframe
idepe_rds_plm <- pdata.frame(idepe_rds, index = c('rd', 'ano')) 

corrplot(cor(idepe_rds_plm[ , c(3:22)]))

# análise do tipo de painel
pdim(idepe_rds_plm) # n>T

## DADOS EM PAINEL ##
# EQUAÇÕES BASE #
equacao <- as.numeric(sd_idepe) ~ idepe + tdi_3em + integral + regular + semi + tecnica + urbana + rural + idepe2
# equacao_ano <- as.numeric(sd_idepe) ~ ano + idepe + tdi_3em + idepe2 + internetTx + noturnoTx + ejaTx + matIntTx + integralTx + regularTx + semiTx + tecnicaTx + urbanaTx
equacao_ano <- as.numeric(sd_idepe) ~ ano + idepe + idepe2 + pib_capita + integral_capita + regular_capita + semi_capita + tecnica_capita + urbana_capita + internet_capita + noturno_capita + eja_capita + brancoTx + femininoTx + tdi_3em

# OLS #
ols_idepe_gre <- lm(equacao, data = idepe_rds_plm)
summary(ols_idepe_gre)

# OLS COM DUMMY #
fixed_dum_idepe_gre <- lm(equacao_ano, data = idepe_rds_plm)
summary(fixed_dum_idepe_gre)

# POOLED #
pooled_idepe_gre <- plm(equacao_ano, data = idepe_rds_plm, model="pooling")
summary(pooled_idepe_gre)

# FIXED #
fixed_idepe_gre <- plm(equacao_ano, data = idepe_rds_plm, model="within", effect = 'twoways')
summary(fixed_idepe_gre)
summary(fixef(fixed_idepe_gre))
r.squared(fixed_idepe_gre, dfcor = TRUE)
shapiro.test(fixed_idepe_gre$residuals)

# RANDOM #
random_idepe_gre <- plm(equacao_ano, data = idepe_rds_plm, model="random", effect = 'twoways', random.method = 'walhus')
summary(random_idepe_gre)
r.squared(random_idepe_gre, dfcor = TRUE)
shapiro.test(random_idepe_gre$residuals)

# FIRST DIFFERENCES #
fd_idepe_gre <- plm(equacao_ano, data = idepe_rds_plm, model = 'fd')
summary(fd_idepe_gre)

# SELEÇÃO POR ANÁLISE GRÁFICA #
mode_eff <- plm(sd_idepe ~ idepe, data = idepe_rds_plm, index = c('rd', 'ano', group = 'rd'), model = 'within')
plot(mode_eff) # gráfico de comparação de interceptos

## TESTES DE MODELO ##
idepeFeTest <- pFtest(fixed_idepe_gre, pooled_idepe_gre)
idepeFeTest

idepeReTest <- plmtest(pooled_idepe_gre, type='bp', effect="individual")
idepeReTest

idepeReTestTw <- plmtest(pooled_idepe_gre, type='bp', effect="twoways")
idepeReTestTw

idepeFReTest <- phtest(fixed_idepe_gre, random_idepe_gre) # If the p-value is < 0.05 then use fixed effects
idepeFReTest

idepeFDeTest <- pwfdtest(fd_idepe_gre, h0 = 'fd') # If the p-value is < 0.05 then use fixed effects
idepeFDeTest

data.frame(
  Method = c('F Test', 'Breusch-Pagan', 'Hausman Test', 'Wooldridges first-difference test'), 
  Statistics = c(idepeFeTest$statistic, idepeReTestTw$statistic, idepeFReTest$statistic, idepeFDeTest$statistic),
  p_value = c(idepeFeTest$p.value, idepeReTestTw$p.value, idepeFReTest$p.value, idepeFDeTest$p.value),
  Alternative = c(idepeFeTest$alternative, idepeReTestTw$alternative, idepeFReTest$alternative, idepeFDeTest$alternative)
           ) %>% kbl(
  caption = "Hypothesis Test for Panel Data Model Selection",
  type = 'html'
) %>%
  kable_paper()

## MODELO FINAL ##
summary(fixed_idepe_gre)
equacao_final <- as.numeric(sd_idepe) ~ ano + idepe + idepe2 + regular_capita + tecnica_capita + urbana_capita + internet_capita
fixed_idepe_gre_final <- plm(equacao_final, data = idepe_rds_plm, model="within", effect = 'twoways')
summary(fixed_idepe_gre_final)

## TESTES DO MODELO FINAL ##
# TESTE GRÁFICO DE TENDÊNCIA #
idepe_rds_plm_est <- idepe_rds_plm %>%
  group_by(ano) %>%
  mutate(mediaSdIdepe = mean(sd_idepe, na.rm=T)) #banco com a media dos votos

ggplot(idepe_rds_plm_est, aes(x=ano, y=sd_idepe, group=rd, color=rd)) +
  geom_line() + 
  geom_line(aes(x=ano, y=mediaSdIdepe, group=1), linetype='dashed', size=3, color='black') + 
  ylab('Standard Deviation - IDEPE (sd_idepe)') + 
  xlab('Year') + 
  ggtitle("Schooling Inequality - overall mean (2008-2019)") +
  guides(color='none')

ggplot(idepe_rds_plm_est, aes(x=ano, y=sd_idepe, group=rd, color=rd)) +
  geom_line() + 
  geom_line(aes(x=ano, y=mediaSdIdepe, group=1), linetype='dashed', size=3, color='black') + 
  ylab('Standard Deviation - IDEPE (sd_idepe)') + 
  xlab('Year') + 
  ggtitle("Schooling Inequality - overall mean (2008-2019)") +
  guides(color='none')

# TESTE DE CORRELAÇÃO SERIAL #
fixed_serial_test <- pwartest(fixed_idepe_gre_final)
fixed_serial_test2 <- pbgtest(fixed_idepe_gre_final)
# TESTE DE DEPENDÊNCIA CONTEMPORÂNEA (CROSS-SECTIONAL DEPENDENCE) #
fixed_cross_test <- pcdtest(fixed_idepe_gre_final)
pcdtest(random_idepe_gre)
# TESTES de REQUISITOS #
r.squared(fixed_idepe_gre_final, dfcor = TRUE)
fixed_residuals_test <- shapiro.test(fixed_idepe_gre_final$residuals)
# COEFICIENTES ROBUSTOS #
fixed_idepe_gre_sc <- coeftest(fixed_idepe_gre_final, vcov = vcovHC(fixed_idepe_gre_final, type="HC3", cluster = "group")) #serial correlation (SC)
fixed_idepe_gre_sc
fixed_idepe_gre_csd <- coeftest(fixed_idepe_gre_final, vcov = vcovBK(fixed_idepe_gre_final, type="HC3", cluster = "group")) #cross-sectional dependence (CSD)
fixed_idepe_gre_csd
fixed_idepe_gre_scc <- coeftest(fixed_idepe_gre_final, vcov = vcovSCC(fixed_idepe_gre_final, type="HC3", cluster = "group")) #cross-sectional and serial correlation (SCC)
fixed_idepe_gre_scc

data.frame(
  Method = c("Shapiro-Wilk", "Wooldridge's Test", "Pesaran CD Test"), 
  Statistics = c(fixed_residuals_test$statistic, fixed_serial_test$statistic, fixed_cross_test$statistic),
  p_value = c(fixed_residuals_test$p.value, fixed_serial_test$p.value, fixed_cross_test$p.value),
  Alternative = c("Non-normally distributed", fixed_serial_test $alternative, fixed_cross_test$alternative)
) %>% kbl(
  caption = "Hypothesis Test for Panel Data Model Assumption",
  type = 'html'
) %>%
  kable_paper()

stargazer(fixed_idepe_gre_final, fixed_idepe_gre_csd,
          title = "Schooling Inequality - Fixed Effetcs",
          dep.var.labels = 'Standard Deviation - IDEPE',
          type = 'html',
          column.labels = c("Fixed Effects", "PCSE"),
          covariate.labels = c("Idepe", "Idepe²", "Regular", "Professional", "Urban", "Internet"),
          out = "table3.html")

#####
kable(tidy(fixed_idepe_gre), format = "simple", digits=3, caption="Pooled model")

kable(tidy(idepeReTest), format = "simple", digits=3, caption=
        "A random effects test for the wage equation")

kable(tidy(pFtest(fixed_idepe_gre, pooled_idepe_gre)), format = "simple", digits=3,caption=
        "Fixed effects test: Ho:'No fixed effects'")

pFtest(fixed_idepe_gre, ols_idepe_gre) # If the p-value is < 0.05 then the fixed effects model is a better choice

summary(random_idepe_gre)
kable(tidy(random_idepe_gre), format = "simple", digits=3, caption="Modelo com Efeitos Aleatórios")

tidy(random_idepe_gre) %>%
  kbl(caption = paste("Modelo com Efeitos Aleatórios - R²", round(r.squared(random_idepe_gre), 4))) %>%
  kable_classic(full_width = F, html_font = "Cambria")

r.squared(random_idepe_gre, dfcor = TRUE)

shapiro.test(random_idepe_gre$residuals)

phtest(fixed_idepe_gre, random_idepe_gre) # If the p-value is < 0.05 then use fixed effects

# GRÁFICOS #
idepe_rds <- idepe_rds %>% filter(gre == "Agreste Centro Norte", "Agreste Meridional", "Mata Centro", "Mata Norte", "Mata Sul", "Metropolitana Norte", "Metropolitana Sul", "Recife Norte", "Recife Sul")

par(mfrow=c(1,1))

plotmeans(sd_idepe ~ rd, main="Heterogeneidade entre RDs", data=idepe_rds)

plotmeans(idepe ~ ano, main="Schooling Effectiveness - heterogeneity across years (2008-2021)", data=idepe_rds, xlab = 'Year', ylab = 'IDEPE')

plotmeans(sd_idepe ~ ano, main="Schooling Inequality - heterogeneity across years (2008-2021)", data=idepe_rds, xlab = 'Year', ylab = 'Standard Deviation - IDEPE')

par(mfrow=c(2,1))

plot1 <- plotmeans(regular_capita ~ ano, main="Typology - heterogeneity across years (2008-2021)", data=idepe_rds, xlab = 'Year', ylab = 'Regular Schools per capita')

plot2 <- plotmeans(tecnica_capita ~ ano, main="Typology - heterogeneity across years (2008-2021)", data=idepe_rds, xlab = 'Year', ylab = 'Professional Schools per capita')

par(mfrow=c(2,1))

plot3 <- plotmeans(internet_capita ~ ano, main="Region Development - heterogeneity across years (2008-2021)", data=idepe_rds, xlab = 'Year', ylab = 'Schools with Internet per capita')

plot4 <- plotmeans(urbana_capita ~ ano, main="Localization - heterogeneity across years (2008-2021)", data=idepe_rds, xlab = 'Year', ylab = 'urban Schools per capita')

figure1 <- multi_panel_figure(columns = 2, rows = 2, panel_label_type = "none")
# show the layout
figure1

figure1 %<>%
  fill_panel(plot1, column = 1, row = 1) %<>%
  fill_panel(plot2, column = 2, row = 1) %<>%
  fill_panel(plot3, column = 1, row = 2) %<>%
  fill_panel(plot4, column = 2, row = 2)
figure1

scatterplot(sd_idepe ~ as.integer(ano) | rd, boxplots=FALSE, smooth=TRUE, legend=list(coords="topleft"), data=idepe_rds)

ggplot(data = idepe_rds, aes(x = ano, y = sd_idepe, color = as.factor(rd), shape = as.factor(rd))) +
  geom_point() + # plots the scatter plot
  geom_smooth(method = "lm", se = F) + # plots the linear model
  geom_smooth(se = F) + # plots the loess model
  theme_minimal() # changes some of the formatting

ggplot(idepe_rds, aes(x=ano, y=idepe, group = 1)) + geom_line(color="steelblue") + 
  geom_point(color="blue") + 
  geom_smooth() + 
  facet_wrap(~ rd) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Year", y = "IDEPE", title = "Schooling Effectiveness - heterogeneity across development regions (2008-2021)")

ggplot(idepe_rds, aes(x=ano, y=sd_idepe, group = 1)) + geom_line(color="steelblue") + 
  geom_point(color="blue") + 
  geom_smooth() + 
  facet_wrap(~ rd) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Year", y = "Standard Deviation - IDEPE", title = "Schooling Inequality - heterogeneity across development regions (2008-2021)")

ggplot(idepe_rds, aes(x=ano, y=urbana_capita, group = 1)) + geom_line(color="steelblue") + 
  geom_point(color="blue") + 
  geom_smooth() + 
  facet_wrap(~ rd) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  labs(x = "Year", y = "Schools with internet per capita", title = "Region Development - heterogeneity across development regions (2008-2019)")

ggplot(idepe_rds, aes(x=ano, y=tdi_3em, group = 1)) + geom_line(color="steelblue") + geom_point(color="blue") + facet_wrap(~ rd)

ggplot(idepe_rds, aes(x=sd_idepe, y=idepe2, size = idepe)) +
  geom_point(alpha=0.7)

plot(idepe_rds$sd_idepe, idepe_rds$idepe, main = 'Associação entre IDEPE e DESVIO-PADRÃO DO IDEPE', xlab = 'DESVIO-PADRÃO IDEPE', ylab = 'IDEPE')

plot(idepe_rds$sd_idepe, idepe_rds$idepe2, main = 'Associação entre IDEPE e DESVIO-PADRÃO DO IDEPE', xlab = 'DESVIO-PADRÃO IDEPE', ylab = 'IDEPE')

ggplot(idepe_escolas, aes(x = idepe, colour = tp_escola, fill = tp_escola)) + geom_freqpoly() + facet_grid(~tp_escola)

baseEscolas <- idepe_escolas %>% group_by(ano, TP_LOCALIZACAO) %>% summarise(counts = n())

ggplot(baseEscolas, aes(x = ano, y = counts)) + geom_bar(fill = "#0073C2FF", stat = "identity") + geom_text(aes(label = counts), vjust = -0.3) + facet_wrap(~ TP_LOCALIZACAO) + labs(title = "TOTAL DE ESCOLAS por TIPO DE ESCOLA de 2008-2019", x = "ANO", y = 'TOTAL DE ESCOLAS') + theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5), axis.text.y = element_text(colour = "grey20", size = 12), strip.text = element_text(face = "italic"), text = element_text(size = 16))

ggplot(idepe_escolas, aes(x = factor(1), y = idepe)) + geom_boxplot(width = 0.4, fill = "white") + geom_jitter(aes(color = tp_escola), width = 0.1, size = 1) + scale_color_manual(values = c("#00AFBB", "#E7B800", 'red', 'blue'), name = "Tipo de Escola") + labs(title = "IDEPE por TIPO DE ESCOLA de 2008-2019", x = NULL, y = 'IDEPE') + facet_wrap(~ ano)

ggplot() +
  geom_point(data = idepe_rds_cluster, 
             mapping = aes(x = sd_idepe, 
                           y = idepe, 
                           colour = tp_escola)) +
  geom_point(mapping = aes_string(x = cls$centers[ , "sd_idepe"], 
                                  y = cls$centers[ , "idepe"]),
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = cls$centers[ , "sd_idepe"], 
                                 y = cls$centers[ , "idepe"],
                                 label = 1:3),
            color = "white", size = 2) +
  theme_light()

ggplot() +
  geom_point(data = idepe_escolas, 
             mapping = aes(x = tdi_3em, 
                           y = idepe, 
                           colour = tp_escola)) +
  geom_point(mapping = aes_string(x = cls$centers[ , "tdi_3em"], 
                                  y = cls$centers[ , "idepe"]),
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = cls$centers[ , "tdi_3em"], 
                                 y = cls$centers[ , "idepe"],
                                 label = 1:3),
            color = "white", size = 2) +
  theme_light()

ggplot() +
  geom_point(data = idepe_escolas_cluster, 
             mapping = aes(x = tdi_3em, 
                           y = idepe, 
                           colour = cluster)) +
  geom_point(mapping = aes_string(x = cls$centers[ , "tdi_3em"], 
                                  y = cls$centers[ , "idepe"]),
             color = "red", size = 4) +
  geom_text(mapping = aes_string(x = cls$centers[ , "tdi_3em"], 
                                 y = cls$centers[ , "idepe"],
                                 label = 1:3),
            color = "white", size = 2) +
  theme_light()

y1 <- fixef(fixed_idepe_gre)
x1 <- names(y1)
plot(y1)

############# RELATÓRIO DOS MODELOS ##############
knitreg(list(pooled_idepe_gre, fd_idepe_gre, fixed_idepe_gre, random_idepe_gre),
        caption="Linear models predicting a movie's tomato meter rating",
        digits = 3,
        caption.above=TRUE, 
        include.rsquared=TRUE,
        include.adjrs=FALSE,
        include.nobs=TRUE,
        include.rmse=FALSE)

stargazer(pooled_idepe_gre, fd_idepe_gre, fixed_idepe_gre, random_idepe_gre, type = "html",
          title = "Schooling Inequality - Four Methods for Panel Data Models",
          dep.var.labels = 'Standard Deviation - IDEPE',
          covariate.labels = c('2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', '2018', '2019', '2021', 'idepe', 'idepe²', 'GDP per Capita', '5 days', 'Regular', '3 days', 'Professional', 'Urban', 'Internet', 'Night Classes', 'Adult Classes', 'White Students', 'Female Students', 'Age-grade Distortion'),
          column.labels = c("Pooled", "First Difference", "Fixed", "Random"),
          # add.lines = list(c("Corrected AIC", round(AICc(pooled_idepe_gre), 1), round(AICc(fd_idepe_gre), 1), round(AICc(fixed_idepe_gre), 1), round(AICc(random_idepe_gre), 1))),
          out = "table1.html"
)

###################### SLOPES ###############
alpha <- slopes(fixed_idepe_gre_final)
head(alpha) 
