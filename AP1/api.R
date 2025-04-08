library(plumber)

#* @apiTitle API de Modelos de Regressão

#* Regressão Linear
#* @param leitura Nota de leitura
#* @get /predicao
function(leitura){
  leitura <- as.numeric(leitura)
  modelo <- readRDS("modelo_linear.rds")
  pred <- predict(modelo, newdata=data.frame(reading.score=leitura))
  list(leitura=leitura, escrita_prevista=pred)
}


#* Regressão Logística
#* @param math Nota de matemática
#* @param reading Nota de leitura
#* @param writing Nota de escrita
#* @get /preparacao
function(math, reading, writing){
  math <- as.numeric(math)
  reading <- as.numeric(reading)
  writing <- as.numeric(writing)
  
  modelo <- readRDS("modelo_logistico_preparacao.rds")
  prob <- predict(modelo, newdata = data.frame(
    math.score = math,
    reading.score = reading,
    writing.score = writing
  ), type = "prob")[,2]
  
  pred <- ifelse(prob > 0.5, 1, 0)
  
  list(
    math = math,
    reading = reading,
    writing = writing,
    probabilidade_de_ter_feito_preparatorio = prob,
    fez_preparatorio = as.character(pred)
  )
}