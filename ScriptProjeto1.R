
## Ao avaliar os dados, decidi não incluir a variável attributed_time pois ela só consta
## nos casos de realização de downloads. Tentei alguns algoritimos e o Random Forest se 
## mostrou melhor.

library(lubridate)
library(randomForest)
library(caret)
library(ROSE)

dt = read.csv(file='train_sample.csv')

# Ajuste nas variáveis

dt$click_time = ymd_hms(dt$click_time)
dt$attributed_time = ymd_hms(dt$attributed_time)
dt$is_attributed = as.factor(dt$is_attributed)

# divisao de treino e teste

amostra = sample(2,100000,replace=T, prob=c(0.7,0.3))
treino = dt[amostra==1,]
teste = dt[amostra==2,]

# Construindo o modelo 

modelo = randomForest(is_attributed ~ ip + app + device + os + channel + click_time, treino)

# Fazendo as previsões

previsoes = predict(modelo, teste)

# Avaliando o modelo

confusionMatrix(teste$is_attributed, previsoes)
roc.curve(teste$is_attributed, previsoes, plotit = T, col = "red")
