#--------------------------------------------------------------#
#                                                              #
# Noções básicas de linguagem R aplicadas à biologia aquática  #
#                                                              #
#--------------------------------------------------------------#

# Ministrantes:
# Luan Campos Imbiriba
# MSc. Ândrocles Oliveira Borges

# R básico ------------

## Operadores matemáticos -----

1993 + 30 # + Adição.
2023 - 1993 # - Subtração
88 * 200 # * Multiplicação.
303 / 2 # / Divisão.
2 ^ 8 # ^ Exponenciação.
2 ** 8 # ** Exponenciação.

## Operadores de Comparação: ------

2 == 2 # == Igual a.
2 != 2 # != Diferente de.
5 > 2 # > Maior que.
5 < 2 # < Menor que
5 >= 2 # >= Maior ou igual a.
2 <= 5# <= Menor ou igual a.

## Operadores Lógicos: ------

5 > 2 & 2 < 5 # &  ou &&  E lógico (elemento a elemento e escalar).
5 > 2 | 2 > 5 # | ou || OU lógico (elemento a elemento e escalar).
!5 < 2 # ! NÃO lógico (negação).

# vem pra ca <- tudo o que eu fizer aqui


# Funções -------------------------------------------------------

nome_da_funcao(argumento1, argumento2, argumento3..)

sqrt(81)  # Calcula a raiz quadrada.

help(sqrt)

# Funções estatísticas

mean() # Calcula a média de um vetor numérico.
median() # Calcula a mediana de um vetor numérico.
sum() # Calcula a soma dos elementos de um vetor numérico.
min() # Encontra o valor mínimo em um vetor.
max() # Encontra o valor máximo em um vetor.
sd() # Calcula o desvio padrão de um vetor.
var() # Calcula a variância de um vetor.


# Operador de atribuição ----------

# colocar aqui <- oque tiver aqui

uma_espécie_coletada <- 1
uma_espécie_coletada

# Para atribuir vários dados usa-se o operador de concatenação `c()`
especies_coletadas <- c(5, 20, 12, 29)
especies_coletadas

# Comentários -------------
# Tudo que se escreve após o símbolo #, o interpretador não lê
especies_coletadas <- c(5, 20, 12, 29) # Espécies coletadas no rio
especies_coletadas <- c(2, 10, 5, 52) # Espécies coletadas no lago

# Tipos de Dados ----------------

## Números racionais (numeric/double).-------------
c(1, 2, 5, 1.2, 6, 8.10)

## Texto (character). -----------------
c("bodó","pirarucu","sardinha","jaraqui")
# Para escrever textos usa-se aspas `" "`

## Lógicos (logical). -----------------
c(TRUE, FALSE)

## Inteiros (integer). -----------------
c(1L, 2L)

# Estrutura de Dados -------------

## Vetores ----------------

riqueza_esp <- c(25, 30, 35, 40)
riqueza_esp

nome_esp <- c("pirarucu","bodó","jaraqui","pacu")
nome_esp

## Matriz -----------------

matriz_dados <- matrix(1:12, nrow = 3, ncol = 4)
matriz_dados

## Listas -----------------

lista_esp <- list(esp = "pirarucu", riqueza = 30, fase = "adulto")
lista_esp

## Data Frame -------------

dados <- data.frame(riqueza_esp = c(25, 30, 35, 40),
                    nome_esp = c("pirarucu","bodó",
                                 "jaraqui","pacu"))
dados

# Depois de criar objetos, você pode realizar diversas operações com eles, como realizar cálculos, criar gráficos, realizar análises estatísticas e muito mais. Os objetos no R facilitam o armazenamento e a manipulação de dados, tornando-o uma linguagem poderosa para análise de dados e estatísticas.


# Manipulando vetores e dataframe -------------------------------

# Indexação de vetores
## Vetores--------------

dados_vetor <- c(1:20)
dados_vetor[2] <- 3

dados_vetor2 <- c("Rio Amazonas", "Rio Negro")
dados_vetor2[1] <- "Rio Solimões"


### Exercícios ----------------------

# Exercício de Indexação de Vetores em R:

# Considere o vetor a seguir:

# Vetor original
meu_vetor <- c(10, 25, 35, 42, 50, 67, 73, 88, 95, 100)

# 1. Acesse o terceiro elemento do vetor.
meu_vetor[3]

# 2. Extraia os elementos do quarto ao sétimo do vetor.
meu_vetor[4:7]

# 3. Substitua o valor do segundo elemento por 30.
meu_vetor[2] <- 30
meu_vetor

# 4. Crie um novo vetor contendo apenas os elementos maiores que 50.
novo_vetor <- meu_vetor[meu_vetor > 50]
novo_vetor

# 5. Calcule a média dos elementos do vetor.
mean(meu_vetor)

# 6. Calcule a soma cumulativa dos elementos do vetor original.
cumsum(meu_vetor)


## Dataframe------------
# Indexação de dataframe
dados <- data.frame(
  Espécies = paste0("esp", 1:12),
  Periodo = as.factor(rep(c("Cheia", "Seca"), each = 6)),
  Abundancia = c(24,25,22,30,55,45,38,75,58,62,64,48)
)

dados[1] # Acessar a primeira coluna
dados[-3] # Acessar as colunas menos a 3
dados[1,] # Acessar a primeira linha
dados[1,1] # Acessar a primeira linha e a primeira coluna
dados[1:5,] # Acessar as 5 primeiras linhas
# glimpse(dados)

# Cheia
sum(dados[1:6,3])
# usando um operador lógico
sum(dados[dados$Periodo == "Cheia", 3])
# 201

# Seca
sum(dados[7:12,3])
# usando um operador lógico
sum(dados[dados$Periodo == "Seca", 3])
# 345

soma_periodo <- c(Cheia = 201, Seca = 345)
# names(soma_periodo)

grafico_barra <- barplot(soma_periodo,
                         # Cores dos gráficos
                         col = c("#08bb00",	"#b300bb"),
                         # nome dos eixos
                         ylab = "Abundância de espécies",
                         xlab = "Período",
                         # Largura das bordas
                         lwd = 2,
                         # Limite do eixo Y
                         ylim = c(0,380),
                         # Tamanho da fonte dos eixos
                         cex.lab = 1.5)
# Adicionando uma linha em formato L
box(bty = "L", lwd = 2)
# Adicionando os valores nas barras
text(x= grafico_barra, y = soma_periodo+20,
     labels = soma_periodo, cex = 1.5)

# Boxplot
boxplot(Abundancia ~ Periodo, data = dados,
        # Cores das caixas
        col = c("#08bb00",	"#b300bb"),
        # Nomes dos eixos
        ylab = "Abundância de espécies",
        xlab = "Período", lwd = 2, cex.lab = 1.2,
        cex.axis = 1.2)

### Exercícios ----------------------

# Considere o seguinte dataframe:

# Criar um dataframe
meu_dataframe <- data.frame(
  peixes = c("Aracu", "Piranha", "Sardinha", "Jaraqui"),
  local = as.factor(rep(c("Rio", "Lago"), each = 4)),
  contagem = c(80, 92, 75, 88, 95, 80, 92, 75)
)

# 1. Extraia a contagem de todos os peixes no dataframe.
meu_dataframe$contagem

# 2. Substitua a contagem de Aracu no Rio por 85.
meu_dataframe[1,3] <- 88
meu_dataframe

# 3. Crie um novo dataframe contendo apenas dados do Rio
dados_rio <- meu_dataframe[meu_dataframe$local == "Rio", ]
dados_rio

# 4. Crie um novo dataframe contendo apenas dados do Lago
dados_lago <- meu_dataframe[meu_dataframe$local == "Lago", ]
dados_lago

# 5. Acesse a coluna rio e mude para "Rio Amazonas".
levels(meu_dataframe$local)[2] <- "Rio Amazonas"

# Operador Pipe -----------------------------
# `and then ou e então`

# Exercicio
meus_dados <- c(1,5,10,20,15,25)
mean(c(1,76))

mean(range(cumsum(meus_dados)))

# Sem o Pipe
biologo <- diploma(tcc(estagio(pibic(enem(Androclis)))))

# Com o Pipe
biologo <- Androclis |>
  enem() |>
  piblic() |>
  estagio() |>
  tcc() |>
  diploma()


meus_dados |>
  cumsum() |>
  range() |>
  mean()

meus_dados <- c(25, 52.5, 15.2, 45, 22, 100)

round(mean(cumsum(meus_dados)), digits = 2)

meus_dados |>
  cumsum() |>
  mean() |>
  round(digits = 2)


# Pacotes------------------------------------------------

library(tidyverse)
library(readxl)
library(ggtext)
library(ggthemes)
library(broom)
library(patchwork)
library(rstatix)
library(car)
library(devtools)


# Leitura dos dados ------------------------------------

# Função read_xlsx do pacote readxl

# Lendo dados pelo Ctrl C + Ctrl V
dados_ictio <- read.delim2(file = "clipboard", header = TRUE)

# Lendo dados de arquivos .csv
dados_ictio <- read.csv2("dados/dados_esp.csv")
dados_ictio

# Lendo dados de arquivos do excel .xls e .xlsx
dados_ictio <- read_excel("dados/dados_esp.xlsx", sheet = 1, na = "NA")
dados_ictio

# Manipulação de dados ------------------------------------------

# Antes de começar a manipular os dados é bom verificar se os dados foram carregados corretamente. Existem várias formas, serão abordadas algumas.

# Verificando se os dados foram carregados corretamente
# View(dados_ictio) # Visualiza os dados em uma planilha
glimpse(dados_ictio) # Verifica a estrutura dos dados e os tipos de dados
class(dados_ictio) # Verifica a classe do objeto
dim(dados_ictio) # Verifica as dimensões do objeto
head(dados_ictio) # Carrega os primeiros dados
tail(dados_ictio) # Carrega os últimos dados


# Transformando algumas colunas em fator
dados_ictio$river_system <- as.factor(dados_ictio$river_system)
dados_ictio$habitat <- as.factor(dados_ictio$habitat)
glimpse(dados_ictio)

levels(dados_ictio$habitat)


# Função filter()

dados_ictio |>
  filter(river_system == "Amazonas") |>
  View()


# Função group_by() + summarise() +6 n_distinct()

dados_ictio |>
  group_by(river_system) |>
  # n_distinct conta valores únicos
  summarise(n = n_distinct(species))

# Função select() + count()

dados_ictio |>
  group_by(river_system) |>
  select(species) |>
  count()

# Função arrange

# Ordem crescente - Padrão
 dados_ictio |>
  group_by(river_system) |>
  count() |>
  arrange(n)

 # Ordem decrescente
 dados_ictio |>
   group_by(river_system) |>
   count() |>
   arrange(desc(n))

# Função mutate
dados_ictio |>
   group_by(river_system, order) |>
   count() |>
   # desagrupa
   #ungroup() |>
   mutate(n_prop = (n/sum(n))*100)|>
   arrange(desc(n))

# Exploração de dados -------------------------------------------

# A exploração de dados pode ser feita por:
# análise numérica: computar estatísticas descritivas
# análise gráfica: explorar o comportamento e a relação entre as variáveis através de gráficos.

# São quantas ordem, familias e espécies?

# Ordens
dados_ictio |>
   select(order) |>
   distinct()


# Contribuição de cada ordem

 dados_ictio |>
   select(order) |>
   distinct()

# Familias
 dados_ictio |>
   select(family) |>
   distinct()

# Contribuição de cada familia

 dados_ictio |>
   select(family) |>
   count(family) |>
   arrange(desc(n))

# Espécies

 dados_ictio |>
  select(species) |>
  distinct()

 # Contribuição de especie

 # Espécies por sistema
 dados_ictio |>
   group_by(river_system) |>
   select(species) |>
   distinct() |>
   count()

# Espécies mais frequentes
 esp <- dados_ictio |>
   select(species) |>
   count(species) |>
   filter(n > 357) |>
   arrange(desc(n))



# Gráfico

ggplot(esp, aes(x = reorder(species, n), y = n)) +
  # gráfico de barra
 geom_bar(stat = "identity", fill = "darkgreen") +
  # Inverte os eixos
 coord_flip() +
  # adciona rótulos nas barras
 geom_text(
   aes(label = n),
   color = "white",
   vjust = 0.5,
   hjust = 1.5
 ) +
  # Renomeia os eixos
 labs(x = "Principais espécies", y = "Número de indivíduos") +
  # tema do gráfico
 theme_bw(base_size = 14)

# Grafico 2 - Lolipop
ggplot(esp, aes(x = reorder(species, n), y = n)) +
  # gráfico de barra
  geom_point() +
  geom_segment(aes(y = 0,
                   x = species,
                   yend = n,
                   xend = species),
               color = "black")+
  # Inverte os eixos
  coord_flip() +
  # adciona rótulos nas barras
  geom_text(
    aes(label = n),
    color = "black",
    vjust = 0.4,
    hjust = -.2
  ) +
  # Renomeia os eixos
  labs(x = "Principais espécies", y = "Número de indivíduos") +
  # tema do gráfico
  theme_bw(base_size = 14)


 #------- Para deixar itálico------------#
 # Primeiro adiciona <i> para todas
 esp$esp2 <- paste0("<i>", esp$species, "</i>")

 # Separa sp
 esp$esp2<- sub(" sp</i>", "</i> sp",  esp$esp2)

 # Separação manual
 esp$esp2[3] <-"<i>Bryconops</i> cf <i>transitoria</i>"
 esp$esp2[8] <- "<i>Eigenmannia</i> sp (A)"
 esp$esp2[16] <- "<i>Pimelodella</i> sp (A)"

 # Gráfico
 ggplot(esp, aes(x = reorder(esp2, n), y = n)) +
   geom_bar(stat = "identity", fill = "darkgreen") +
   coord_flip() +
   geom_text(
     aes(label = n),
     color = "white",
     vjust = 0.5,
     hjust = 1.5
   ) +
   labs(x = "Principais espécies", y = "Número de indivíduos") +
   theme_bw(base_size = 14) +
   theme(axis.text.y = element_markdown())

# Exercicio

 # ggplot(esp, aes(x = reorder(species, n), y = n)) +
 #   geom_bar(stat = "identity", fill = "_____________") +
 #   coord_flip() +
 #   geom_text(aes(label = n),
 #     color = "________",
 #     vjust = 0.5,
 #     hjust = 1.5) +
 #   labs(title = "____________", x = "Principais espécies",
 #     y = "Número de indivíduos") +
 #   theme___________()


# Abundancia-------------------------------
abundancia <-  dados_ictio |>
  group_by(river_system,habitat ,date) |>
  count()



# Boxplot

# Traduzir os fatores
abundancia$habitat <- as.factor(abundancia$habitat )
levels(abundancia$habitat) <- c("Lago de várzea", "Margem do rio")

plot_abundancia <- ggplot(abundancia, aes(x= river_system, y = n))+
  # Geometria boxplot
  geom_boxplot(width = .7, fill = "darkseagreen1",
    show.legend = FALSE, size = 1)+
  # geometria de ponto
  geom_point(show.legend = FALSE)+
  # tema do gráfico
  theme_classic(base_size = 14)+
  # Divisão do dataset
  facet_wrap( ~ habitat)+
  # tema do gráfico
  theme(strip.text = element_text(face = "bold"))+
  labs(x = "", y = "Abundancia")+
  theme(strip.background = element_blank())

plot_abundancia

# Riqueza de espécies -----------------------------------

riqueza <-  dados_ictio |>
  group_by(river_system,habitat ,date) |>
  summarise(riq = n_distinct(species))

# Boxplot

riqueza$habitat <- as.factor(riqueza$habitat)
levels(riqueza$habitat) <- c("Lago de várzea", "Margem do rio")

plot_riqueza <- ggplot(riqueza, aes(x= river_system, y = riq))+
  geom_boxplot(width = .7, fill = "darkseagreen1",
    show.legend = FALSE, size = 1)+
  geom_point(show.legend = FALSE)+
  theme_classic(base_size = 14)+
  facet_wrap( ~ habitat)+
  theme(strip.text = element_text(face = "bold"))+
  labs(x = "Sistema fluvial", y = "Riqueza")+
  theme(strip.background = element_blank())

plot_riqueza

# juntando os gráficos
plot_abundancia / plot_riqueza

# Riqueza de espécies por sistema

riqueza <- dados_ictio |>
  group_by(river_system) |>
  summarise(riq = n_distinct(species)) |>
  ungroup()

# Porcentagem
riqueza_prop <- riqueza %>%
  dplyr::mutate(prop = round(riq/sum(riq), 4)*100)


# Gráfico de pizza
ggplot(data = riqueza_prop, aes(x = "", y = prop,
  fill = river_system)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
    position = position_stack(vjust = .5), size = 6) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_polar(theta = "y", start = 0) +
  theme_void() +
  labs(fill = "Sistema fluvial")+
  theme(legend.text = element_markdown(color = "black",
    size = 10))

# Gráfico de donut
ggplot(data = riqueza_prop, aes(x = 2, y = prop, fill = river_system)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(prop, "%")), color = "white",
    position = position_stack(vjust = .5), size = 4) +
  scale_fill_manual(values = c("darkorange", "purple", "cyan4")) +
  coord_polar(theta = "y", start = 0) +
  xlim(0, 2.5) +
  theme_void() +
  theme(legend.position = c(.5, .5)) +
  labs( fill = "Espécies")


# Variáveis limnológicas ----------------------------------------

glimpse(dados_ictio[,c(5, 7:13)])

dados_limno <-  dados_ictio[,c(4,5, 7:13)]

# Temperatura
dados_temp <- dados_limno |>
  group_by(river_system, habitat, temp) |>
  select(species) |>
  count()

ggplot(dados_temp, aes(x = temp, y = n))+
  geom_point(aes(col = river_system, shape = habitat), size = 3)+
  theme_bw(base_size = 14)+
  labs(x = "Temperatura", y = "Abundância", col = "Sistema fluvial ", shape = "Habitat")+
  # "right", "left", "bottom", "top"
  theme(legend.position = "right",
    legend.key.height = unit(.3, 'cm'),
    legend.text = element_text(size=12),
    legend.background  = element_blank())

# Da para ver o comportamento dos dados por um histograma
dados_ictio |>
  group_by(river_system, species, temp) |>
  count(species) |>
  ggplot(aes(x = temp,  fill = river_system))+
  geom_histogram(alpha = .4, position = "dodge",bins = 15)+
  scale_fill_manual(values = c("darkorchid","darkorange", "cyan4"))+
  theme_base()

# Ou por um gráfico de densidade
dados_ictio |>
  group_by(river_system, species, temp) |>
  count(species) |>
  ggplot(aes(x = temp,  fill = river_system))+
  geom_density(alpha = .4)+
  scale_fill_manual(values = c("darkorchid","darkorange", "cyan4"))+
  theme_base()


# Sumário estatístico de temperatura
sumario_temp <-  dados_limno |>
  group_by(river_system, habitat) |>
  select(temp) |>
  get_summary_stats(type = "common")

sumario_temp

# Visualizando
plot_temp <-  ggplot(sumario_temp,
  aes(x = river_system,y = mean,col = habitat,group = habitat)) +
  geom_line(linewidth = 1, position = position_dodge(.5)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
    linewidth = 1,width = .2, position = position_dodge(0.5))+
  theme_bw()+
  scale_color_manual(values = c("#08bb00",	"#b300bb"))
plot_temp

# Sumário estatístico de pH
sumario_ph <-  dados_limno |>
  group_by(river_system, habitat) |>
  select(ph) |>
  get_summary_stats(type = "common")

# Visualizando
plot_ph <-  ggplot(sumario_ph,
  aes(
    x = river_system,
    y = mean,
    col = habitat,
    group = habitat
  )) +
  geom_line(linewidth = 1, position = position_dodge(.5)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    linewidth = 1,
    width = .2,
    position = position_dodge(0.5)
  ) +
  theme_bw() +
  scale_color_manual(values = c("#08bb00",	"#b300bb"))

plot_ph


# Sumário estatístico de Do
sumario_do <-  dados_limno |>
  group_by(river_system, habitat) |>
  select(do) |>
  get_summary_stats(type = "common")

# Visualizando
plot_do <-  ggplot(sumario_do, aes( x = river_system,
                                    y = mean, col = habitat,
                                    group = habitat)) +
  geom_line(linewidth = 1, position = position_dodge(.5)) +
  geom_point(position = position_dodge(0.5)) +
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    linewidth = 1,
    width = .2,
    position = position_dodge(0.5)
  ) +
  theme_bw() +
  labs(y = bquote("Média de DO mgL"^-1)) +
  scale_color_manual(values = c("#08bb00",	"#b300bb"))

plot_do

# Gráfico de barras com erros
ggplot(sumario_do, aes( x = river_system,
                        y = mean, fill = habitat,
                        group = habitat)) +
#  geom_line(linewidth = 1, position = position_dodge(.5)) +
  geom_bar(stat="identity", position=position_dodge())+
  geom_errorbar(
    aes(ymin = mean - sd, ymax = mean + sd),
    linewidth = 1,
    width = .2,
    position = position_dodge(0.9)
  )+
  geom_point(position = position_dodge(0.9)) +
  theme_bw() +
  scale_fill_manual(values = c("#08bb00",	"#b300bb"))

# Fazendo sumario de todas
dados_limno |>
  pivot_longer(cols = c(temp:tb),
    names_to = "variaveis", values_to = "count") |>
  group_by(river_system, habitat, variaveis) |>
  get_summary_stats(type = "common") |>
  arrange(variaveis)|>
  print(n=30) |>
  flextable::flextable()


plot_ph/ plot_temp / plot_do + plot_layout(guides = "collect")+
  plot_annotation(tag_levels = "A")

# Salvando o gráfico
ggsave('plot.jpeg', width = 15, height = 10, units = "cm", dpi = 300)


# Testes estatisticos -------------------------------------------


# Teste t -------------------------------------------------------

dados_peixes <- read_csv2('dados/teste_t.csv')

# Pergunta
# Será se o comprimento padrão da espécie é diferente por período?

# Normalidade

# Teste de normalidade
residuos <- lm(cp ~ as.factor(periodo), data = dados_peixes)

# visualização dos resíduos
qqPlot(residuos)

# Teste T
t.test(cp ~ periodo, data = dados_peixes, var.equal = TRUE)

## Teste de Shapiro-Wilk
residuos_modelo <- residuals(residuos)
shapiro.test(residuos_modelo)
# Homocedasticidade

## Teste de homogeneidade de variância
leveneTest(cp ~ as.factor(periodo), data = dados_peixes)

dados_peixes |> t_test(cp ~ periodo,  var.equal = TRUE)

ggplot(dados_peixes, aes(x = periodo, y = cp))+
  geom_boxplot(fill = c("#08bb00",	"#b300bb"),
               color = "black",
               linewidth = 1) +
  scale_color_manual(values = c("black", "black")) +
  theme_bw() +
  theme(legend.position = "none")

# Regressão -----------------------------------------------------

## regressão simples

# A temperatura afeta no tamanho da espécie?

regressao <- read_csv2('dados/regressao.csv')

# modelo
modelo_regressao <- lm(cp ~ temp, data = regressao)
## Verificar as premissas do teste
par(mfrow = c(2, 2), oma = c(0, 0, 2, 0))
plot(modelo_regressao)

## Teste de Shapiro-Wilk
residuos_modelo <- residuals(modelo_regressao)
shapiro.test(residuos_modelo)

## Resultados usando a função anova
anova(modelo_regressao)

## Resultados usando a função summary
summary(modelo_regressao)


ggplot(regressao, aes(x = temp, y = cp))+
  geom_point(size = 4, shape = 19, alpha = 0.7)+
  geom_smooth(method = "lm", se = TRUE, color = "black")+
  theme_bw()


# Sim existe uma influência da temperatura

# Anova ---------------------------------------------------------


dados_anova <- read_csv2("dados/dados_anova.csv")

modelo <- aov(riqueza ~ periodo, data = dados_anova)

## Normalidade
shapiro.test(residuals(modelo))

## Homogeneidade da variância
bartlett.test(riqueza ~ periodo, data = dados_anova)
# ou
leveneTest(riqueza ~ as.factor(periodo), data = dados_anova)

# Anova de Welch
oneway.test(riqueza ~ periodo, data = dados_anova)

# Resultado bonitinho
welch_anova_test(dados_anova, riqueza ~ periodo)

## Diferenças entre os períodos
TukeyHSD(modelo)
# Resultado bonitinho
tukey_hsd(modelo)

# Não paramétrico
kruskal_test(dados_anova, riqueza ~ periodo)
dunn_test(dados_anova, riqueza ~ periodo)

ggplot(dados_anova, aes(x = periodo, y = riqueza))+
  geom_boxplot()
