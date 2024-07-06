
##### Carregando os pacotes necessários 

library(tidyverse)
library(PNADcIBGE)
library(deflateBR)
library(cowplot)
library(purrr)
library(readr)
theme_set(theme_minimal())
tema <- theme_minimal()

##### Carregando os dados e separando as variáveis

dados_2012 <- get_pnadc(year=2012,interview  = 1)
dados_2019 <- get_pnadc(year=2019,interview  = 1)

X2012 <- dados_2012$variables
X2019 <- dados_2019$variables

##### Carregando os dados e separando as variáveis

dados_separados_2012 = dplyr::select(X2012, Ano, Trimestre, UF, V20082,
                                     V2009, V2010, V3009, V3014, V2007,
                                     VD4020)

dados_separados_2012 <- dados_separados_2012%>%
  rename(V3009A = V3009)

dados_separados_2019 = dplyr::select(X2019,Ano, Trimestre, UF, V20082,
                                     V2009, V2010, V3009A, V3014, V2007,
                                     VD4020)

dados_totais_2012_2019 = bind_rows(dados_separados_2012, dados_separados_2019)

#### Criando a função de cálculo da NAGIC

percentis = seq(.01,.99,.01)

gt_np = function(p,y1,y2){
  g2 = quantile(y2,p,na.rm=T)
  g1 = quantile(y1,p,na.rm=T)
  g = log(g2)-log(g1)
  
}


#### Separando a base de dados por cohorts

## Por cor/raça

dados_analise_pretos_2012 = dados_totais_2012_2019 %>%
  filter(V2010 == "Preta", Ano == 2012)

dados_analise_pretos_2019 = dados_totais_2012_2019 %>%
  filter(V2010 == "Preta", Ano == 2019)

dados_analise_branca_2012 = dados_totais_2012_2019 %>%
  filter(V2010 == "Branca", Ano == 2012)

dados_analise_branca_2019 = dados_totais_2012_2019 %>%
  filter(V2010 == "Branca", Ano == 2019)

dados_analise_parda_2012 = dados_totais_2012_2019 %>%
  filter(V2010 == "Parda", Ano == 2012)

dados_analise_parda_2019 = dados_totais_2012_2019 %>%
  filter(V2010 == "Parda", Ano == 2019)

dados_analise_indigena_2012 = dados_totais_2012_2019 %>%
  filter(V2010 == "Indígena", Ano == 2012)

dados_analise_indigena_2019 = dados_totais_2012_2019 %>%
  filter(V2010 == "Indígena", Ano == 2019)




# Cálculo da NAGIC para essa cohort 

nagic_pretos = gt_np(percentis, dados_analise_pretos_2012$VD4020, dados_analise_pretos_2019$VD4020)

plot_pretos <- ggplot(as.data.frame(nagic_pretos), aes(x = percentis, y = nagic_pretos)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC PRETOS - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

nagic_brancos = gt_np(percentis, dados_analise_branca_2012$VD4020, dados_analise_branca_2019$VD4020)

plot_brancos <- ggplot(as.data.frame(nagic_pretos), aes(x = percentis, y = nagic_brancos)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC BRANCOS - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

nagic_pardos = gt_np(percentis, dados_analise_parda_2012$VD4020, dados_analise_parda_2019$VD4020)

plot_pardos <- ggplot(as.data.frame(nagic_pardos), aes(x = percentis, y = nagic_pardos)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC PARDOS - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária


nagic_indigena = gt_np(percentis, dados_analise_indigena_2012$VD4020, dados_analise_indigena_2019$VD4020)

plot_indigena <- ggplot(as.data.frame(nagic_indigena), aes(x = percentis, y = nagic_indigena)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC INDÍGENAS - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária


plot_grid(plot_pretos,plot_brancos,plot_indigena,plot_pardos)

## Por Escolaridade

dados_analise_superior_2012 = dados_totais_2012_2019 %>%
  filter(V3009A == "Superior - graduação", V3014 == "Sim", Ano == "2012")

dados_analise_superior_2019 = dados_totais_2012_2019 %>%
  filter(V3009A == "Superior - graduação", V3014 == "Sim", Ano == "2019")

dados_analise_medio_2012 = dados_totais_2012_2019 %>%
  filter(V3009A == "Regular do ensino médio ou do 2º grau", V3014 == "Sim", Ano == "2012")

dados_analise_medio_2019 = dados_totais_2012_2019 %>%
  filter(V3009A == "Regular do ensino médio ou do 2º grau", V3014 == "Sim", Ano == "2019")

dados_analise_mestrado_2012 = dados_totais_2012_2019 %>%
  filter(V3009A == "Mestrado", V3014 == "Sim", Ano == "2012")

dados_analise_mestrado_2019 = dados_totais_2012_2019 %>%
  filter(V3009A == "Mestrado", V3014 == "Sim", Ano == "2019")

dados_analise_doutorado_2012 = dados_totais_2012_2019 %>%
  filter(V3009A == "Doutorado", V3014 == "Sim", Ano == "2012")

dados_analise_doutorado_2019 = dados_totais_2012_2019 %>%
  filter(V3009A == "Doutorado", V3014 == "Sim", Ano == "2019")

# Cálculo da NAGIC para essa cohort 
nagic_superior = gt_np(percentis, dados_analise_superior_2012$VD4020, dados_analise_superior_2019$VD4020)

plot_superior <- ggplot(as.data.frame(nagic_superior), aes(x = percentis, y = nagic_superior)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC SUPERIOR - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária


nagic_medio = gt_np(percentis, dados_analise_medio_2012$VD4020, dados_analise_medio_2019$VD4020)

plot_medio <- ggplot(as.data.frame(nagic_medio), aes(x = percentis, y = nagic_medio)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC MÉDIO - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

nagic_mestrado = gt_np(percentis, dados_analise_mestrado_2012$VD4020, dados_analise_mestrado_2019$VD4020)

plot_mestrado <- ggplot(as.data.frame(nagic_mestrado), aes(x = percentis, y = nagic_mestrado)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC MESTRADO - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

nagic_doutorado = gt_np(percentis, dados_analise_doutorado_2012$VD4020, dados_analise_doutorado_2019$VD4020)

plot_doutorado <- ggplot(as.data.frame(nagic_doutorado), aes(x = percentis, y = nagic_doutorado)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 3), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC DOUTORADO - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária


plot_grid(plot_medio,plot_superior,plot_mestrado, plot_doutorado)

## Por cohorts de idade

dados_idade1824_2012 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 18 & V2009 <= 24)

dados_idade1824_2019 = dados_totais_2012_2019 %>%
  filter(Ano == 2019 & V2009 >= 25 & V2009 <= 31)

nagic_idade1824 = gt_np(percentis, dados_idade1824_2012$VD4020, dados_idade1824_2019$VD4020)

plot_idade1824 <- ggplot(as.data.frame(nagic_idade1824), aes(x = percentis, y = nagic_idade1824)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC 18 a 24 - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária


dados_idade2534_2012 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 25 & V2009 <= 34)

dados_idade2534_2019 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 32 & V2009 <= 41)

nagic_idade2534 = gt_np(percentis, dados_idade2534_2012$VD4020, dados_idade2534_2019$VD4020)

plot_idade2534 <- ggplot(as.data.frame(nagic_idade2534), aes(x = percentis, y = nagic_idade2534)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC 25 a 34 - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

dados_idade3544_2012 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 35 & V2009 <= 44)

dados_idade3544_2019 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 42 & V2009 <= 51)

nagic_idade3544 = gt_np(percentis, dados_idade3544_2012$VD4020, dados_idade3544_2019$VD4020)

plot_idade3544 <- ggplot(as.data.frame(nagic_idade3544), aes(x = percentis, y = nagic_idade3544)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC 35 a 44 - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

dados_idade4554_2012 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 45 & V2009 <= 54)

dados_idade4554_2019 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 52 & V2009 <= 61)

nagic_idade4554 = gt_np(percentis, dados_idade4554_2012$VD4020, dados_idade4554_2019$VD4020)

plot_idade4554 <- ggplot(as.data.frame(nagic_idade4554), aes(x = percentis, y = nagic_idade4554)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC 45 a 54 - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária


dados_idade5564_2012 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 55 & V2009 <= 64)

dados_idade5564_2019 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 62 & V2009 <= 71)

nagic_idade5564 = gt_np(percentis, dados_idade5564_2012$VD4020, dados_idade5564_2019$VD4020)

plot_idade5564 <- ggplot(as.data.frame(nagic_idade5564), aes(x = percentis, y = nagic_idade5564)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC 55 a 64 - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

dados_idade65_2012 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 65)

dados_idade65_2019 = dados_totais_2012_2019 %>%
  filter(Ano == 2012 & V2009 >= 72)

nagic_idade65 = gt_np(percentis, dados_idade65_2012$VD4020, dados_idade65_2019$VD4020)

plot_idade65 <- ggplot(as.data.frame(nagic_idade65), aes(x = percentis, y = nagic_idade65)) +
  geom_smooth(method = lm, formula = y ~ poly(x, 2), se = TRUE, color = "black", fill = "lightblue") +  # Adiciona a linha de regressão com intervalo de confiança
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # Adiciona a linha y=0
  labs(title = "NAGIC 65 + - BRASIL | 2012 - 2019",
       x = "Percentis",
       y = "Variação na renda média") +
  theme(axis.text = element_text(size = 10),  # Ajusta o tamanho do texto dos eixos
        panel.grid.major = element_line(color = "lightgray"),  # Adiciona uma grade de fundo
        panel.grid.minor = element_blank())  # Remove as linhas da grade secundária

plot_grid(plot_idade1824, plot_idade2534, plot_idade3544, plot_idade4554, plot_idade5564, plot_idade65)



