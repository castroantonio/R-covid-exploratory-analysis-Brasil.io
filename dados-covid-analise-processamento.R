
setwd("/home/castro/Downloads/brasil IO - covid")

# Carga dos dados
arquivo <- "caso.csv"

dados <- read.csv(arquivo, header = TRUE, stringsAsFactors = FALSE)


#### Analise Exploratoria

# Vejamos algumas linhas das informações que ele traz.
head(dados)
tail(dados)

# Informações básicas sobre os atributos do conjunto de dados:
str(dados)

# Sumarizando os dados temos informações mais detalhadas sobre cada um dos atributos:
summary(dados)

# Vamos manter apenas os dados dos municípios do estado do Rio de Janeiro e vamos remover colunas que não serão utilizadas:
dados <- dados[dados$state == "RJ" & dados$place_type == "city", ]
dados <- dados[ , - c(2, 4, 9)] 

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



#### Pre-processamento

## Numero de mortes por dia

# Vamos criar um novo atributo com o numero de mortes por dia.
dados$deaths_day <- 0

municipios <- sort(unique(dados$city))

for (municipio in municipios) {
  indices <- sort(dados[dados$city == municipio, "order_for_place"])
  acumulado <- 0
  for (indice in indices) {
    mortes_no_dia <- dados[dados$city == municipio & dados$order_for_place == indice, "deaths"] - acumulado
    dados[dados$city == municipio & dados$order_for_place == indice, "deaths_day"] <- mortes_no_dia
    acumulado <- dados[dados$city == municipio & dados$order_for_place == indice, "deaths"]
  }
}

# Ficou tudo certo?
any(dados$deaths_day < 0)

# Eita! Será que eu errei no código? Vejamos os dados:
dados[dados$deaths_day < 0,]

# Vamos olhar um exemplo e ver os dados originais para um exemplo, o próprio registro e uns antes:
dados[dados$city == "Araruama" & dados$order_for_place %in% c(64, 65, 66, 67),]

# Ficou difícil de ver, vamos melhorar isso, vendo só algumas colunas:
dados[dados$city == "Araruama" & dados$order_for_place %in% c(64, 65, 66, 67), c(1,2,4,11)]

# Do dia 2020-06-06 para o dia 2020-06-07 alguém ressucitou!!! Aleluia!
  
# Ou os dados não são confiáveis...


# Sem muito o que fazer.
# dados[dados$deaths_day < 0, "deaths_day"] <- 0

# Agora como ficaram os dados?
min(dados$deaths_day)

max(dados$deaths_day)


# Vamos plotar os dados:
ggplot(dados, aes(x=date, y=deaths_day, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "data", y = "mortes/dia") +
  theme_minimal()


## Número de mortes/dia 100k habitantes

# Seria melhor se todos fossem medidos com uma mesma escala.
# Os dados já trazem uma coluna que usa isso confirmed_per_100k_inhabitants, vamos fazer algo parecido para o número de mortes por dia:</p>
dados$deaths_day_per_100k_inhabitants <- 100000 * dados$deaths_day / dados$estimated_population


# Vejamos como ficou:
ggplot(dados, aes(x=date, y=deaths_day_per_100k_inhabitants, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "data", y = "mortes/dia 100k habitantes") +
  theme_minimal()


## Padronização por z-score - número de mortes/dia 100k habitantes

# Padronizacao por z-score.
dados$deaths_day_100k_zscore <- 0
for (municipio in municipios) {
  media <- mean(dados[dados$city == municipio, "deaths_day_per_100k_inhabitants"], na.rm = TRUE)
  desvio_padrao <- sd(dados[dados$city == municipio, "deaths_day_per_100k_inhabitants"], na.rm = TRUE)
  dados[dados$city == municipio, "deaths_day_100k_zscore"] <- ((dados[dados$city == municipio, "deaths_day_per_100k_inhabitants"] - media) / desvio_padrao)
}

# Vejamos como ficou:
ggplot(dados, aes(x=date, y=deaths_day_100k_zscore, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "data", y = "mortes/dia 100k habitantes (z-score)") +
  theme_minimal()




## Formato de entrada

# atributos e registros
municipios <- sort(unique(dados$city))
datas <- as.character(sort(unique(dados$date)))

# define o formato do conjunto de dados
linhas <- length(datas)
colunas <- length(municipios)

D <- array(0, dim=c(linhas, colunas))

# nomes para as linhas e colunas
rownames(D) <- datas
colnames(D) <- municipios

# dados do numero de mortes de cada municipio para determinada data
for (municipio in municipios) {
  for (data in datas) {
    dado <- 0
    registro <- dados[dados$city==municipio & dados$date == data, ]
    if (nrow(registro) > 0) {
      dado <- dados[dados$city==municipio & dados$date == data, "deaths_day_100k_zscore"]
    }
    D[data, municipio] <- dado
  }
}


## Dividindo em intervalos e discretizando
minimo <- min(D)
maximo <- max(D)
numero_intervalos <- 10
quebras <- seq(from = minimo, to = maximo, length = numero_intervalos + 1)

t <- cut(D, breaks = quebras, labels = letters[1:numero_intervalos], include.lowest=TRUE)
D <- matrix(t, nrow = nrow(D), ncol = ncol(D))

D <- as.data.frame(D, stringsAsFactors=FALSE)
rownames(D) <- datas
colnames(D) <- municipios






## Fazendo a media de mortes semanal.

# seleciona apenas colunas de interesse
temp <- dados[ , c(1,2,11)] # date, city, deaths_day

datas <- sort(unique(temp$date))

# Primeiro todos tem de ter o mesmo numero de datas
linha <- nrow(temp)
for (municipio in municipios) {
  for (data in datas) {
    if (nrow(temp[temp$city == municipio & temp$date == data,]) == 0) {
      linha <- linha + 1
      temp[(linha), "date"] <- data
      temp[(linha), "city"] <- municipio
      temp[(linha), "deaths_day"] <- 0 
    }
  }
}

# Funcionou?
nrow(temp) / length(datas) == length(municipios)


# Numero de cada semana
temp$week <- 0

for (municipio in municipios) {
  i <- 0
  for (data in datas) {
    temp[temp$city == municipio & temp$date == data, "week"] <- as.integer(i / 7) + 1
    i <- i + 1
  }
}


# Semanal por municipio
semanal <- data.frame(week = 0, city = "", mean_deaths_week = 0, stringsAsFactors = FALSE)

semanas <- sort(unique(temp$week))
i <- 1
for (municipio in municipios) {
  for (semana in semanas) {
    total <- sum(temp[temp$city == municipio & temp$week == semana, "deaths_day"])
    linhas <- nrow(temp[temp$city == municipio & temp$week == semana, ])
    semanal[i, "week"] <- semana
    semanal[i, "city"] <- municipio
    semanal[i, "mean_deaths_week"] <- total / linhas
    i<- i + 1
  }
}


# O gráfico:
ggplot(semanal, aes(x=week, y=mean_deaths_week, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "semana", y = "média mortes/semana") +
  theme_minimal()




# Média semanal por 100k habitantes
semanal$mean_deaths_week_100k_inhabitants <- 0
for (municipio in municipios) {
  populacao <- unique(dados[dados$city == municipio, "estimated_population"])
  semanal[semanal$city == municipio, "mean_deaths_week_100k_inhabitants"] <- 100000 * semanal[semanal$city == municipio, "mean_deaths_week"] / populacao
}

# O gráfico:
ggplot(semanal, aes(x=week, y=mean_deaths_week_100k_inhabitants, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "semana", y = "média  mortes/semana 100k habitantes") +
  theme_minimal()



# Padronizacao por z-score para média de mortes por semana.
semanal$mean_deaths_week_100k_zscore <- 0
for (municipio in municipios) {
  media <- mean(semanal[semanal$city == municipio, "mean_deaths_week_100k_inhabitants"], na.rm = TRUE)
  desvio_padrao <- sd(semanal[semanal$city == municipio, "mean_deaths_week_100k_inhabitants"], na.rm = TRUE)
  semanal[semanal$city == municipio, "mean_deaths_week_100k_zscore"] <- ((semanal[semanal$city == municipio, "mean_deaths_week_100k_inhabitants"] - media) / desvio_padrao)
}

# Vejamos como ficou:
ggplot(semanal, aes(x=week, y=mean_deaths_week_100k_zscore, group=city)) +
  geom_point(aes(color=city), show.legend = FALSE) +
  geom_line(aes(color=city), show.legend = FALSE) +
  labs(x = "data", y = "mortes/dia 100k habitantes (z-score)") +
  theme_minimal()


## Agora transformar no formato desejado:


# atributos e registros
municipios <- sort(unique(semanal$city))
semanas <- sort(unique(semanal$week))

# define o formato do conjunto de dados
linhas <- length(semanas)
colunas <- length(municipios)

D <- array(0, dim=c(linhas, colunas))

# nomes para as linhas e colunas
rownames(D) <- semanas
colnames(D) <- municipios

# dados do numero de mortes de cada municipio para determinada data
for (municipio in municipios) {
  for (semana in semanas) {
    D[semana, municipio] <- semanal[semanal$city==municipio & semanal$week == semana, "mean_deaths_week_100k_zscore"]
  }
}



## Dividindo em intervalos e discretizando

minimo <- min(D)
maximo <- max(D)
numero_intervalos <- 10
quebras <- seq(from = minimo, to = maximo, length = numero_intervalos + 1)

t <- cut(D, breaks = quebras, labels = letters[1:numero_intervalos], include.lowest=TRUE)
D <- matrix(t, nrow = nrow(D), ncol = ncol(D))

D <- as.data.frame(D, stringsAsFactors=FALSE)
rownames(D) <- semanas
colnames(D) <- municipios


