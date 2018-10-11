source("Estado.R")

## Classe e métodos para o problema do quebra cabeça de 8 peças
QuebraCabecas <- function(desc = NULL, pai = NULL) {
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("QuebraCabecas", "Estado")
  
  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.QuebraCabecas = function(obj1,obj2){
  if(.Generic == "=="){
    return(all(obj1$desc == obj2$desc))
  }
}

## Sobrecarga da função genérica "print" do R
print.QuebraCabecas <- function(obj) {
  cat("Tabuleiro: ", t(matrix(obj$desc,nrow=3,ncol=3)), "\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.QuebraCabecas <- function(atual){
  
  if(is.null(atual$desc))
    return(Inf)
  ## h(obj) = soma((posX[i] - objX[i]) + (posY[i] - objY[i]))
  ## objetivo = [1 2 3
  ##             4 5 6
  ##             7 8 -]
  dist <- 0
  pos <- t(matrix(atual,nrow=3,ncol=3))
  for(x in 1:3) {
    for(y in 1:3) {
      ## Gerando posição objetivo
      if(pos[x,y] == 1) obj <- c(1,1)
      else if(pos[x,y] == 2) obj <- c(1,2)
      else if(pos[x,y] == 3) obj <- c(1,3)
      else if(pos[x,y] == 4) obj <- c(2,1)
      else if(pos[x,y] == 5) obj <- c(2,2)
      else if(pos[x,y] == 6) obj <- c(2,3)
      else if(pos[x,y] == 7) obj <- c(3,1)
      else if(pos[x,y] == 8) obj <- c(3,2)
      else if(pos[x,y] == "vazio") obj <- c(3,3)
      ## Cálculo da distância de Manhattan para a posição pos(x,y)
      dist <- dist + abs(x - obj[[1]]) + abs(y - obj[[2]])
    }
  }
  return(dist)
}

## Função de geração de estados filhos
geraFilhos.QuebraCabecas <- function(atual){}

