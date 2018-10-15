source("Estado.R")

## Classe e métodos para o problema do Trajeto Entre Cidades
TrajetoCidades <- function(desc = NULL, pai = NULL) {
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("cidades", grafoCidades(), envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("TrajetoCidades", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre trajeto de cidades
Ops.TrajetoCidades = function(obj1,obj2) {
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica print do R
print.TrajetoCidades <- function(obj) {
	cat("Cidade: ", obj$desc, "\n")
	cat("G(n): ", obj$g, "\n")
	cat("H(n): ", obj$h, "\n")
	cat("F(n): ", obj$f, "\n")
}

## Função para retornar o nome das cidades do grafo
nomeCidades <- function() {
  return(c("A","B","C","D","F","G","L","M","O","P","R","S","T","U","Z"))
}

## Função para construir o grafo de custo de passo para todas as cidades
grafoCidades <- function() {
  nomeCidades <- nomeCidades()
  grafo <- matrix(c(
    Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,140,118,Inf,75,
    Inf,Inf,Inf,Inf,211,90,Inf,Inf,Inf,101,Inf,Inf,Inf,85,Inf,
    Inf,Inf,Inf,120,Inf,Inf,Inf,Inf,Inf,138,146,Inf,Inf,Inf,Inf,
    Inf,Inf,120,Inf,Inf,Inf,Inf,75,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
    Inf,211,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,99,Inf,Inf,Inf,
    Inf,90,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
    Inf,Inf,Inf,Inf,Inf,Inf,Inf,70,Inf,Inf,Inf,Inf,111,Inf,Inf,
    Inf,Inf,Inf,75,Inf,Inf,70,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
    Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,151,Inf,Inf,71,
    Inf,101,138,Inf,Inf,Inf,Inf,Inf,Inf,Inf,97,Inf,Inf,Inf,Inf,
    Inf,Inf,146,Inf,Inf,Inf,Inf,Inf,Inf,97,Inf,80,Inf,Inf,Inf,
    140,Inf,Inf,Inf,99,Inf,Inf,Inf,151,Inf,80,Inf,Inf,Inf,Inf,
    118,Inf,Inf,Inf,Inf,Inf,111,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
    Inf,Inf,85,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,Inf,
    75,Inf,Inf,Inf,Inf,Inf,Inf,Inf,71,Inf,Inf,Inf,Inf,Inf,Inf),
    nrow=15, ncol=15)
  rownames(grafo) <- nomeCidades
  colnames(grafo) <- nomeCidades
  return(grafo)
}

## Função para calcular as distâncias em linha reta de todas as cidades para o objetivo
geraDistancias <- function(obj) {
  ## Na verdade só temos as distâncias para a cidade B, então obj = "B"
  distancias <- matrix(c(366,0,160,242,178,77,244,241,380,98,193,253,329,80,374))
  rownames(distancias) <- nomeCidades()
  colnames(distancias) <- "B"
  
  return(distancias[obj,])
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.TrajetoCidades <- function(atual) {
  return(geraDistancias(atual$desc))
}

## Função de geração de estados filhos
geraFilhos <- function(obj) {
    filhosDesc <- list()
	  filhos <- list()
	  
	  # gera uma lista de vizinhos do nó atual a partir do grafo global
	  vizinhos <- obj$cidades[obj$desc,]
	  # remove os nós que não tem conexão (custo infinito)
    vizinhos <- vizinhos[vizinhos != Inf]
    # gera lista de nomes dos nós filhos
    filhosDesc <- names(vizinhos)
    
    ## gera os objetos TrajetoCidades para os filhos
    for(filhoDesc in filhosDesc) {
      filho <- TrajetoCidades(desc = filhoDesc, pai = obj)
      filho$h <- heuristica(filho)
      filho$g <- vizinhos[filho$desc]
      filho$f <- filho$h + filho$g
      filho$cidades <- obj$cidades
      filhos <- c(filhos, list(filho))
    }
    
    return(filhos)
}
