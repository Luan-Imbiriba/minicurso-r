lista_pacotes <- c("tidyverse", "ggtext", "ggthemes",
                   "broom", "patchwork", "rstatix",
                   "car", "devtools")
install.packages(lista_pacotes)

# Operadores básicos
10 + 50 # Adição
10 - 5 # subtração
5 * 5 # multiplicação
50 / 2 # Divisão
2 ^ 8 # Exponenciação
2 ** 8 # Exponenciação

# Operadores de comparação
2 == 3 # igualdade
2 != 3 # diferente
2  > 1 # maior que
2 < 3 # menor que
2 >= 1 # maior ou igual a
2 <= 3 # menor ou igual a 

# Operador de atribuição 
# vem pra ca <- tudo o que fizer aqui

x <- 10
x

# Funções
nome_funcao(argumento1, argumento2....)

raiz <- sqrt(81)

raiz + x # Soma de objetos

help(sqrt)
?sqrt

# Função de concatenação c()

x <- c(10, 15, 150, 50, 80)
x

# Estruras de dados

vetor <- data.frame(c(10, 15, 150, 50, 80)) # Vetores
dim(vetor) # Dimensão

dataframe <- data.frame(
  riqueza = c(50, 10, 80, 70),
  nome_esp = c("pirarucu", "bodó",
               "jaraqui", "pacu")
)
dataframe
edit(dataframe)

# Indexação 
# acessando vetor
vetor[5] <- 60
vetor     
# acessar dataframe
dataframe[1,1] 

# Acessar dados
# Como os antigos

setwd("C:/Users/luanc/Documents/Luan/minicurso-r")
getwd()

# Versão prática


# Leitura dos dados
dados_ictio <- read.delim2(file = "clipboard", header = TRUE)
dados_ictio

dados_csv <-  read.csv2("dados/dados_anova.csv")
dados_csv

# Pacotes
library(tidyverse)
library(readxl)
library(magrittr)

dados_ictio <- read_excel("dados/dados_esp.xlsx", sheet = 1)
dados_ictio

# Pipe %>%
biologo <-  tcc(graduaçao(vestibular(vitor)))

biologo %>% # E então 
  vitor %>% 
  vetibular %>% 
  graduação %>%  
  tcc

meus_dados <- c(25, 52, 15, 10)
mean(cumsum(meus_dados))

meus_dados %>% 
  cumsum() %>% 
  mean()


library(dplyr)

# Explorar dados
View(dados_ictio)

dados_ictio %>% 
  filter(order == "Characiformes")

# Ordens
dados_ictio %>% 
  select(order) %>% 
  distinct()

# Espécies mais frequentes
esp <- dados_ictio %>% 
  select(species) %>% 
  count(species) %>%
  filter( n > 512) %>%
  arrange(desc(n))

# ggplot2

# Gramatica de gráficots

  # Gráfico de barra
ggplot(data = esp ,mapping = aes(x = reorder(species,n), y = n))+
  # Gráfico de barra
  geom_bar(stat = "identity", fill = "darkorange")+
  # Coordenada
  coord_flip()+
  # Texto
  geom_text(aes(label = n), vjust = 0.5, hjust = 1.5)+
  # Tema
  labs(x = "Principais espécies", y = "Número de indivíduos")+
  theme_classic(base_size = 14)+
  # deixar em itálico
  theme(axis.title = element_text(face = "bold",
                                    family = "Comic Sans MS",
                                  size = 20))

gráfico_barra
library(extrafont)
extrafont::font_install()

# Boxplot e facetas
abundancia <- dados_ictio %>% 
  group_by(river_system, habitat, date) %>% 
  count()

# Gráfico
grafico_boxplot <- ggplot(abundancia, aes( x = river_system, y = n, fill = habitat))+
  geom_boxplot()+
  facet_wrap(~habitat)+
  labs(x = "Sistema de rio", 
       y = "Abundância de especies", 
       fill = "Habitat")+
  theme_bw(base_size = 14)+
  # "right", "left", "bottom", "top"
  theme(legend.position = "bottom",
        strip.text = element_blank())
 # theme(axis.title.y = element_text(margin = margin(0,-200)))
grafico_boxplot

library(patchwork)
grafico_barras + grafico_boxplot

grafico_barras / grafico_boxplot +
  plot_annotation(tag_levels = "a", tag_suffix = ')')

ggsave("resultados/grafico.tiff", width = 21, height = 29.7, units = "cm", dpi = 300)

library(esquisse)
esquisser(viewer = "browser")

dados_anova <- read_csv2("dados/dados_anova.csv")



ggplot(dados_anova) +
  aes(x = temp, y = riqueza, colour = lago) +
  geom_point(shape = "circle", size = 4L) +
  geom_smooth(method = "lm", aes(fill = lago), alpha = 0.2)+
  scale_color_manual(
    values = c(catalao = "#8E0152",
               janauaca = "#04B0A4",
               `lago do rei` = "#276419")
  ) +
  labs(
    x = "Temperatura (Cº)",
    y = "Abundância de espécies",
    color = "Lagos", fill = "Lagos"
  ) +
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")


# Estatística descritiva

glimpse(dados_ictio) # estruturação de dados

dados_ictio <- read_excel("dados/dados_esp.xlsx", 
                          sheet = 1, 
                          na = "NA")
glimpse(dados_ictio)

dados_limno <- dados_ictio[,c(5,7:12)]

dados_limno <- dados_limno %>%
  drop_na()

glimpse(dados_limno)

dados_limno %>%  
  group_by(river_system, habitat) %>% 
  summarise(media = mean(ph),
            desvio = sd(ph))

library(rstatix)

sumario_do <-  dados_limno %>%  
  group_by(river_system) %>% 
  select(do) %>% 
  get_summary_stats(type = "common")

ggplot(sumario_do, aes(x = river_system, y = mean))+
  geom_point(size = 3)+
  geom_errorbar(aes(ymin = mean - sd, 
                    ymax = mean + sd), 
                width = 0.3)+
  theme_bw()+
  labs(x = "Sistema de rio", 
       y = bquote( "DO mgL"^-1))

# Salvando o gráfico
ggsave("resultados/grafico_do.tiff", width = 10, 
       height = 8, units = "cm", dpi = 300)

# Salvando a tabela estatística
write.csv2(sumario_do, file = "resultados/sumario_do.csv")

# Teste T

# Dados
dados_peixes <- read_csv2("dados/teste_t.csv")

# Pergunta
# Será se o comprimento padrão da espécie é diferente por período?
# modelo linear
residuos <- lm(cp ~ periodo, data = dados_peixes)

# Normalidade
residuos_modelo <- residuals(residuos)
plot(residuos_modelo)
shapiro.test(residuos_modelo)
hist(residuos_modelo)

# Homocedasticidade
library(car)

wilcox.test() # teste equivalente do teste T
friedman.test() # Equivalente do teste T pareado

leveneTest(cp ~ as.factor(periodo), 
           data = dados_peixes, center = mean)

# resultado
options(scipen = 100)

t.test(cp ~ periodo, data  = dados_peixes)

ggplot(dados_peixes, aes(x = periodo, y = cp, 
                        fill = periodo))+
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(ymax),
                   ymax = after_stat(ymin)), 
               width =0.2)+
  geom_boxplot()+
  theme_bw(base_size = 14)
  
# Anova
dados_anova <- read_csv2("dados/dados_anova.csv")

# Modelo
# Existe diferença na riqueza de espécies entre os períodos?

modelo <- aov(riqueza ~ periodo, data = dados_anova)

# Normalidade
# Deu ok
# Homocedasticidade
# Deu ok

# resultado
oneway.test(riqueza ~ periodo, data = dados_anova)

summary(modelo)
anova(modelo)

welch_anova_test(dados_anova, riqueza ~ periodo)

# Tukey - Quem foi diferente de quem?
TukeyHSD(modelo)

ggplot(dados_anova, aes(x = periodo, y = riqueza, 
                         fill = periodo))+
  stat_boxplot(geom = "errorbar",
               aes(ymin = after_stat(ymax),
                   ymax = after_stat(ymin)), 
               width =0.2)+
  geom_boxplot(show.legend = FALSE)+
  theme_bw(base_size = 14)


# Regressão

# A temperatura afeta no tamanho da espécie?

regressao <- read_csv2('dados/regressao.csv')

# modelo
modelo_regressao <- lm(cp ~ temp, data =  regressao)

# Normalidade
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_regressao)
graphics.off()
# Deu ok
# Homocedasticidade
# Deu ok

# Resultado
summary(modelo_regressao)

# Visualizando o resultado
ggplot(regressao, aes(x = temp, y = cp))+
  geom_point(size = 4, shape = 21, 
             alpha = 0.7, fill = "forestgreen")+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  theme_bw()+
  labs(x = "Temperatura (C°)", 
       y = "Comprimento padrão (mm)")
  







