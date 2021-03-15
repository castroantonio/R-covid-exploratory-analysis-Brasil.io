
## Carga dos dados
arquivo <- "caso.csv"


setwd("/home/castro/Downloads/brasil IO - covid")

dados <- read.csv(arquivo, header = TRUE,stringsAsFactors = FALSE)


# Vejamos algumas linhas das informações que ele traz.
head(dados)
tail(dados)

# Informações básicas sobre os atributos do conjunto de dados:
str(dados)

# Sumarizando os dados temos informações mais detalhadas sobre cada um dos atributos:
summary(dados)


# Vamos manter apenas os dados dos municípios do estado do Rio de Janeiro:
dados <- dados[dados$state == "RJ" & dados$place_type == "city", ]

# Existem NAs?
any(is.na(dados))

# Em que coluna?
colSums(is.na(dados))

# Vamos ver:
dados[is.na(dados$estimated_population), ]

# Ah, esqueci de retirar os os Importados/Indefinidos:
dados <- dados[dados$city != "Importados/Indefinidos", ]

# Ainda existem NAs?
any(is.na(dados))

# Que chato! Em que coluna?
colSums(is.na(dados))

# Vamos dar uma olhada:
dados[is.na(dados$confirmed_per_100k_inhabitants), ]

# Não parece que isso vai atrapalhar em nada. Ao que parece o atributo confirmed_per_100k_inhabitants ficou com valor NA sempre que confirmed tem o valor 0 (zero). Vamos confirmar:
nrow(dados[is.na(dados$confirmed_per_100k_inhabitants), ]) == nrow(dados[dados$confirmed == 0, ])

# Qual o máximo de mortes e os dados de onde ocorreu?
dados[dados$deaths == max(dados$deaths), ]

# Vamos plotar alguns gráficos.

# Primeiro carregando a biblioteca necessária:

library("ggplot2")

# Agora sim, o grafico.
ggplot(dados, aes(x=date, y=deaths, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  scale_x_discrete(name="data", breaks, labels, limits) +
  labs(x = "data", y = "mortes") +
  theme_minimal()


# Vamos plotar menos dados:
temp <- dados[dados$city %in% c("Japeri", "Resende", "Itatiaia", "Paraty"), ]

ggplot(temp, aes(x=date, y=deaths, group=city)) +
  geom_point(aes(color=city)) +
  geom_line(aes(color=city)) +
  labs(x = "data", y = "mortes") +
  theme_minimal()

# É importante observar que o gráfico só cresce! O dado de mortes é cumulativo!!! Eu não havia percebido isso até agora!

# Vamos criar um novo atributo com o numero de mortes por dia.
dados$deaths_day <- 0

## define o formato do conjunto de dados
municipios <- sort(unique(dados$city))

for (municipio in municipios) {
  indices <- sort(dados[dados$city == municipio, "order_for_place"])
  acumulado <- 0
  for (indice in indices) {
    mortes_no_dia <- dados[dados$city == municipio & dados$order_for_place == indice, "deaths"] - acumulado
    dados[dados$city == municipio & dados$order_for_place == indice, "deaths_day"] <- mortes_no_dia
    acumulado <- acumulado + mortes_no_dia
  }
}

# Ficou tudo certo?
any(dados$deaths_day < 0)
dados[dados$deaths_day < 0,]

# Eita! Será que eu errei no código? Vejamos os dados:
dados[dados$deaths_day < 0,]

# Vamos olhar um exemplo e ver os dados originais para um exemplo, o próprio registro e uns antes:
dados[dados$city == "Araruama" & dados$order_for_place %in% c(64, 65, 66, 67),]

# Ficou difícil de ver, vamos melhorar isso, vendo só algumas colunas:
dados[dados$city == "Araruama" & dados$order_for_place %in% c(64, 65, 66, 67), c(1,3,6,7,14)]

# Do dia 2020-06-06 para o dia 2020-06-07 alguém ressucitou!!! Aleluia!
  
# Ou os dados não são confiáveis...


# Sem muito o que fazer, vou zerar todos os negativos.
dados[dados$deaths_day < 0, "deaths_day"] <- 0

# Agora como ficaram os dados?
min(dados$deaths_day)

max(dados$deaths_day)



# Vamos plotar os dados:

ggplot(dados, aes(x=date, y=deaths_day, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "data", y = "mortes") +
  theme_minimal()


# Agora transformar em dados temporais:

# seleciona apenas colunas de interesse
dados <- dados[ , c(1,3,14)]

# atributos e registros
municipios <- sort(unique(dados$city))
datas_ordenadas <- as.character(sort(unique(dados$date)))

# define o formato do conjunto de dados
linhas <- length(datas_ordenadas)
colunas <- length(municipios)

D <- array(0, dim=c(linhas, colunas))

# nomes para as linhas e colunas
rownames(D) <- datas_ordenadas
colnames(D) <- municipios

# dados do numero de mortes de cada municipio para determinada data
for (municipio in municipios) {
  for (data in datas_ordenadas) {
    dado <- 0
    registro <- dados[dados$city==municipio & dados$date == data, ]
    if (nrow(registro) > 0) {
      dado <- dados[dados$city==municipio & dados$date == data, 3]
    }
    D[data, municipio] <- dado
  }
}


