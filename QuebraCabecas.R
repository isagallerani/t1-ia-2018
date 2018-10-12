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
  mat <- obj$desc
  mat[which(mat == -1)] <- "_"
  mat <- t(matrix(mat, nrow=3, ncol=3))
  cat("Tabuleiro: [", mat[1,], "]\n")
  cat("\t   [", mat[2,], "]\n")
  cat("\t   [", mat[3,], "]\n")
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
  ##             7 8 -1]
  dist <- 0
  pos <- t(matrix(atual$desc,nrow=3,ncol=3))
  
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
      else if(pos[x,y] == -1) obj <- c(3,3)
      
      ## Cálculo da distância de Manhattan para a posição pos(x,y)
      dist <- dist + abs(x - obj[[1]]) + abs(y - obj[[2]])
    }
  }
  
  return(dist)
}

## Função de geração de estados filhos
geraFilhos.QuebraCabecas <- function(obj){
  filhos <- list()
  filhosDesc <- list()
  desc <- obj$desc
  matDesc <- t(matrix(desc, nrow=3, ncol=3))
  
  ## Vetor com as coordenadas x e y da posição vazia
  posVazia <- which(matDesc == -1, arr.ind=TRUE)
  ## Vetor de possíveis posições para cada nó filho
  novosPares <- list()
  ## gera pares de posição-valor com todos os operadores possíveis a partir da posição vazia
  operadores <- list(c(-1,0),c(1,0),c(0,-1),c(0,1))
  novosPares <- lapply(operadores, function(op) c(posVazia[1]+op[1], posVazia[2]+op[2]))
  ## verifica posições incompatíveis com o problema  
  incompativeis <- lapply(1:length(novosPares),
                          function(i){
                            nPar <- novosPares[[i]]
                            ## Se a posição resultante é inválida, o conteúdo dela é nulo
                            if((nPar[1] < 1  || nPar[1] > 3) ||
                               (nPar[2] < 1 || nPar[2] > 3)) i ## é incompatível, retorna índice
                            else 0 ## senão é compatível
                          })
  ## mantém no vetor apenas as que são incompatíveis
  incompativeis <- unlist(incompativeis[incompativeis != 0])
  ## remove posições incompatíveis
  if(!is.null(incompativeis)) novosPares <- novosPares[-incompativeis]
  # gera descrições dos estados filhos
  filhosDesc <- lapply(novosPares, 
                       function(nPar){
                         fDesc <- matDesc
                         val <- fDesc[nPar[1],nPar[2]]
                         fDesc[nPar[1],nPar[2]] <- -1
                         fDesc[posVazia] <- val
                         unlist(t(fDesc))
                       })
  ## gera os objetos QuebraCabecas para os filhos
  for(filhoDesc in filhosDesc){
    filho <- QuebraCabecas(desc = filhoDesc, pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  print(filhos)
  return(filhos)
}

