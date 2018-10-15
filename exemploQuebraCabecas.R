debugSource("QuebraCabecas.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

inicial <- QuebraCabecas(desc = c(1,2,3,0,4,5,7,8,6))

objetivo <- QuebraCabecas()
objetivo$desc <- c(1,2,3,4,5,6,7,8,0)

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))
