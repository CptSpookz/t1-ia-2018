source("Estado.R")

## Classe e métodos para o problema do quebra cabeça de 8 peças
QuebraCabecas <- function(desc = NULL, pai = NULL, objetivo = NULL) {
  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("objetivo", objetivo, envir = e)
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
  mat[which(mat == 0)] <- "_"
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
  
  if(is.null(atual$desc) || is.null(atual$objetivo))
    return(Inf)
  ## h(obj) = soma((atual.X[i] - objetivo.X[i]) + (atual.Y[i] - objetivo.Y[i]))
  dist <- 0
  posAtual <- t(matrix(atual$desc,nrow=3,ncol=3))
  posObj <- t(matrix(atual$objetivo,nrow=3,ncol=3))
  
  for(x in 1:3) {
    for(y in 1:3) {
      ## Gerando posição objetivo
      obj <- which(posObj == posAtual[x,y], arr.ind = TRUE) 
      
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
  posVazia <- which(matDesc == 0, arr.ind=TRUE)
  ## Vetor de possíveis posições para cada nó filho
  novosPares <- list()
  ## gera pares de posição-valor com todos os operadores possíveis a partir da posição vazia
  operadores <- list(c(-1,0),c(1,0),c(0,-1),c(0,1))
  novosPares <- lapply(operadores, function(op) c(posVazia[1]+op[1], posVazia[2]+op[2]))
  ## verifica posições incompatíveis com o problema  
  incompativeis <- sapply(1:length(novosPares), 
                          function(i) {
                            nPar <- novosPares[[i]]
                            ## checa se qualquer uma das posições resultantes está fora do alcance do tabuleiro
                            if(any(nPar[1:2] < 1)  || any(nPar[1:2] > 3))
                              i ## incompatível, adiciona indice
                            else 0
                          })
  incompativeis <- incompativeis[incompativeis != 0]
  ## remove posições incompatíveis
  if(length(incompativeis) >= 1) novosPares <- novosPares[-incompativeis]
  # gera descrições dos estados filhos
  for(nPar in novosPares) {
    fDesc <- matDesc
    val <- fDesc[nPar[1],nPar[2]]
    fDesc[nPar[1], nPar[2]] <- 0
    fDesc[posVazia[1], posVazia[2]] <- val
    filhosDesc <- c(filhosDesc, list(c(t(fDesc))))
  }
  ## gera os objetos QuebraCabecas para os filhos
  for(filhoDesc in filhosDesc) {
    filho <- QuebraCabecas(desc = filhoDesc, pai = obj)
    filho$objetivo <- obj$objetivo
    filho$h <- heuristica(filho)
    filho$g <- obj$g + 1
    filhos <- c(filhos, list(filho))
  }
  return(filhos)
}
