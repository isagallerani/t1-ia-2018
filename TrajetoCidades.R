source("Estado.R")

## Classe e métodos para o problema do Trajeto Entre Cidades
TrajetoCidades <- function(desc = NULL, pai = NULL, cidades = NULL){
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("cidades", cidades, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("TrajetoCidades", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre trajeto de cidades
Ops.TrajetoCidades = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica print do R
print.TrajetoCidades <- function(obj){
	cat("Cidade: ", obj$desc, "\n")
	cat("G(n): ", obj$g, "\n")
	cat("H(n): ", obj$h, "\n")
	cat("F(n): ", obj$f, "\n")
}


## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.TrajetoCidades <- function(atual){
  #Continuar...
}

## Criação do método de geração de filhos
geraFilhos <- function(obj) {
    filhosDesc <- list()
	  filhos <- list()
	  cidades <- obj$cidades
	  vizinhos <- t(cidades[obj$desc,])
    ## Continuar...
}
