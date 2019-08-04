#' Cantor Set
#'
#' This function plots a Cantor Set fractal.
#' @param n Level number (default = 5)
#' @param n Line width (default = 2)
#' @return No return
#' @export
cantorset <- function(n=5, largura=2) {
  m <- n
  k <- 1
  xTodos <- c(0,1)

  xAbaixo <- function(x1, x2) {
    return(c(x1, x1+(x2-x1)/3, x1+(x2-x1)*(2/3), x2))
  }

  plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0, 1), ylim=c(0, n))

  while(m > 0) {
    novoxTodos <- c()

    for(i in seq(1, length(xTodos), by=2)) {
      lines(c(xTodos[i], xTodos[i+1]), c(m,m), lwd=largura)
      novoxTodos <- c(novoxTodos, xAbaixo(xTodos[i],xTodos[i+1]))
    }
    xTodos <- novoxTodos

    m <- m - 1
  }
}

#' Koch Star
#'
#' This function plots the Koch Star or Snowflake fractal.
#' @param n Level number (default = 5)
#' @return No return
#' @export
kochstar <- function(n=5) {

  obterPontos <- function(p1x, p1y, p2x, p2y) {
    p5x <- p2x
    p5y <- p2y

    distP1P5 <- sqrt((p1x-p5x)^2+(p1y-p5y)^2)
    cosTeta <- (p5x-p1x)/distP1P5
    senTeta <- (p5y-p1y)/distP1P5

    ## ENCONTRANDO COORDENADAS DE P2
    ## Em relacao ao eixo com origem em P1 e inclinacao da reta P1P5
    p2xdesl <- distP1P5/3
    p2ydesl <- 0
    ## Rotando para eixo original
    p2xrot <- p2xdesl*cosTeta## - p2ydesl*senTeta
    p2yrot <- p2xdesl*senTeta## + p2ydesl*cosTeta
    ## Transladando para eixo original
    p2x <- p2xrot + p1x
    p2y <- p2yrot + p1y

    ## ENCONTRANDO COORDENADAS DE P4
    ## Em relacao ao eixo com origem em P1 e inclinacao da reta P1P5
    p4xdesl <- distP1P5*(2/3)
    p4ydesl <- 0
    ## Rotando para eixo original
    p4xrot <- p4xdesl*cosTeta## - p4ydesl*senTeta
    p4yrot <- p4xdesl*senTeta## + p4ydesl*cosTeta
    ## Transladando para eixo original
    p4x <- p4xrot + p1x
    p4y <- p4yrot + p1y

    ##ENCONTRANDO COORDENADAS DE P3
    ## Em relacao ao eixo com origem em P2 e inclinacao da reta P2P4
    distP2P4 <- sqrt((p2x-p4x)^2+(p2y-p4y)^2)
    p3xdesl <- distP2P4/2
    p3ydesl <- (-1)*(distP2P4)*(sqrt(3)/2) ##Altura do triangulo equilatero
    ## Rotando para eixo original
    p3xrot <- p3xdesl*cosTeta - p3ydesl*senTeta
    p3yrot <- p3xdesl*senTeta + p3ydesl*cosTeta
    ## Transladando para eixo original
    p3x <- p3xrot + p2x
    p3y <- p3yrot + p2y

    x <- c(p1x, p2x, p3x, p4x)
    y <- c(p1y, p2y, p3y, p4y)
    pontos <- data.frame(x,y)

    return(pontos)
  }

  ## First triangle vertices
  x <- c(0, 1, 0.5)
  y <- c(0, 0, sqrt(3)/2)
  vertices <- data.frame(x,y)

  verticesTemp <- data.frame(x=numeric(), y=numeric())

  if(n != 0) {
    for(j in 1:n) {
      verticesTemp <- data.frame(x=numeric(), y=numeric())
      for(i in 1:length(vertices[,1])) {
        k <- i+1
        if(k>length(vertices[,1])) {k=1}
        verticesTemp <- rbind(verticesTemp, obterPontos(vertices$x[i], vertices$y[i], vertices$x[k], vertices$y[k]))
      }
      vertices <- verticesTemp
    }
  }

  ## Draw
  plot(0,0,type="n",xlim=c(-0.3, 1.3),ylim=c(-0.3, 1.3), frame=FALSE, xlab="", ylab="", xaxt="n", yaxt="n")
  polygon(vertices$x, vertices$y, density = 0)
}

#' Chaos Game
#'
#' This function plots a fractal graphics through the Chaos Game.
#' @param max Level number (default = 10.000)
#' @param n Vertices number (default = 3)
#' @param r Distance fraction (0 < r < 1) (default = 0.5)
#' @param draw.pol Draw polygon (default = TRUE)
#' @return No return
#' @export
chaosgame <- function(max=10000, n=3, r=0.5, draw.pol=TRUE) {

  ##n <- 3    4     5    6    7    12    14
  ##r <- 1/2  0.4   3/8  1/3  0.3  0.4   0.2

  ## Divide circunferencia unitaria em "n" partes iguais
  vertices <- data.frame(x=0, y=1)
  angulo <- 2*pi/n
  for(i in 1:(n-1)) {
    vertices <- rbind(vertices, c(sin(i*angulo), cos(i*angulo)))
  }

  plot(0,0,type="n",xlim=c(-1,1),ylim=c(-1,1), frame=F, xlab="", ylab="", xaxt="n", yaxt="n")
  if(draw.pol) {
    polygon(vertices$x, vertices$y)
  }

  pontox <- 0
  pontoy <- 0

  ## Primeiro ponto guia (ponto semente)
  pontoGuiax <- runif(1, min=-1, max=1)
  pontoGuiay <- runif(1, min=-1, max=1)

  ## Associa uma cor aleatoriamente definida a cada vertice
  vertices$cor <- ""
  for(i in 1:n) {
    vertices[i,]$cor <- paste0("#", sample(9, 1), sample(9, 1), sample(9, 1), sample(9, 1), sample(9, 1), sample(9, 1))
  }

  for(i in 1:max) {

    sorteio <- sample(1:n, 1, replace=T)

    pontox <- (pontoGuiax + vertices[sorteio,]$x)*r
    pontoy <- (pontoGuiay + vertices[sorteio,]$y)*r

    points(pontox, pontoy, cex=0.1, pch=19, col=vertices[sorteio,]$cor)

    pontoGuiax <- pontox
    pontoGuiay <- pontoy
  }
}

#' Sierpinski Triangle
#'
#' This function plots a Sierpinski Triangle fractal.
#' @param n Level number (default = 5)
#' @return No return
#' @export
sierpinski <- function(n=5) {

  centros <- data.frame(x=0, y=0)
  lado <- 16

  ## Retorna os vertices de um triangulo equilatero dado seu centro e lado
  obterVertices <- function(xCentro, yCentro, lado) {
    x <- c(xCentro-lado/2, xCentro, xCentro+lado/2)
    y <- c(yCentro-lado*(sqrt(3)/6), yCentro+lado*(sqrt(3)/3), yCentro-lado*(sqrt(3)/6))
    vertices <- data.frame(x, y)
    return(vertices)
  }
  ## Retorna tres centros de triangulos equilateros menores, dado o centro e lado do original
  obterCentros <- function(xCentro, yCentro, lado) {
    h <- lado*sqrt(3)/2
    x <- c(xCentro-lado/4, xCentro, xCentro+lado/4)
    y <- c(yCentro-h/6, yCentro+h/3, yCentro-h/6)
    centros <- data.frame(x, y)
    return(centros)
  }

  ## Realiza as iteracoes o número "n" de vezes
  if(n != 0) {
    for(i in 1:n) {
      centrosTemp <- data.frame(x=numeric(), y=numeric())
      for(j in 1:length(centros[,1])) {
        centrosTemp <- rbind(centrosTemp, obterCentros(centros[j,]$x, centros[j,]$y, lado))
      }
      centros <- centrosTemp
      lado <- lado/2
    }
  }

  ## Desenha
  plot(0, 0, type="n", xlim=c(-10, 10), ylim=c(-10, 10), frame=FALSE, xlab="", ylab="", xaxt="n", yaxt="n")
  vertices <- data.frame(x=numeric(), y=numeric())
  for(i in 1:length(centros[,1])) {
    vertices <- obterVertices(centros[i,]$x, centros[i,]$y, lado)
    polygon(vertices$x, vertices$y, density = 200)
  }
}

#' Dragao Curve
#'
#' This function plots a Dragon Curve fractal.
#' @param n Level number (default = 7)
#' @return No return
#' @export
dragoncurve <- function(n = 7) {

  obterPontos <- function(p1x, p1y, p2x, p2y, k) {

    distP1P2 <- sqrt((p1x-p2x)^2+(p1y-p2y)^2)
    cosTeta <- (p2x-p1x)/distP1P2
    senTeta <- (p2y-p1y)/distP1P2

    ##ENCONTRANDO COORDENADAS DE P3
    ## Em relacao ao eixo com origem em P2 e inclinacao da reta P2P4
    p3xdesl <- distP1P2/2
    p3ydesl <- (k)*(distP1P2/2)
    ## Rotando para eixo original
    p3xrot <- p3xdesl*cosTeta - p3ydesl*senTeta
    p3yrot <- p3xdesl*senTeta + p3ydesl*cosTeta
    ## Transladando para eixo original
    p3x <- p3xrot + p1x
    p3y <- p3yrot + p1y

    x <- c(p1x, p3x)
    y <- c(p1y, p3y)
    pontos <- data.frame(x,y)

    return(pontos)
  }

  ## Pontos iniciais
  x <- c(-1, 0, 1)
  y <- c(1, 0, 1)

  pontos <- data.frame(x,y)

  if(n != 0) {
    for(j in 1:n) {
      k <- -1
      qtdPontos <- length(pontos[,1])
      pontosTemp <- data.frame(x=numeric(), y=numeric())
      for(i in 1:(qtdPontos-1)) {
        pontosTemp <- rbind(pontosTemp, obterPontos(pontos$x[i], pontos$y[i], pontos$x[i+1], pontos$y[i+1], k))
        ## Intercala ponto a direita ou esquerda da curva
        k <- k*(-1)
      }
      ## Adiciona ultimo ponto
      pontos <- rbind(pontosTemp, c(pontos[qtdPontos,]$x, pontos[qtdPontos,]$y))
    }
  }

  ## Desenha
  plot(0,0,type="n",xlim=c(min(pontos$x)*1.02, max(pontos$x)*1.02), ylim=c(min(pontos$y)*1.02, max(pontos$y)*1.02), frame=FALSE, xlab="", ylab="", xaxt="n", yaxt="n")
  for(i in 1:(length(pontos[,1])-1)) {
    lines(c(pontos[i,]$x, pontos[i+1,]$x), c(pontos[i,]$y, pontos[i+1,]$y))
  }
}

#' Barnsley Fern
#'
#' This function plots a Barnsley Fern fractal.
#' @param n Level number (default = 10.000)
#' @return No return
#' @export
barnsleyfern <- function(n = 10000) {

  ## Primeiro ponto (origem)
  pontos <- data.frame(x=0, y=0)

  for(i in 1:n) {
    sorteio <- runif(1, min=0, max=1)

    ## 1 porcento
    if(0 <= sorteio & sorteio <= 0.01) {
      x <-  0
      y <-  0.16 * pontos[i,]$y
    }
    ## 85 porcento
    else if(0.01 < sorteio & sorteio <= 0.86) {
      x <-  0.85 * pontos[i,]$x + 0.04 * pontos[i,]$y
      y <- -0.04 * pontos[i,]$x + 0.85 * pontos[i,]$y + 1.6
    }
    ## 7 porcento
    else if(0.86 < sorteio & sorteio <= 0.93) {
      x <-  0.02 * pontos[i,]$x - 0.26 * pontos[i,]$y
      y <-  0.23 * pontos[i,]$x + 0.22 * pontos[i,]$y + 1.6
    }
    ## 7 porcento
    else if(0.93 < sorteio & sorteio <= 1) {
      x <- -0.15 * pontos[i,]$x + 0.28 * pontos[i,]$y
      y <-  0.26 * pontos[i,]$x + 0.24 * pontos[i,]$y + 0.44
    }
    pontos <- rbind(pontos, c(x, y))
  }

  ## Desenha
  plot(0, 0, type="n", xlim=c(min(pontos$x)*1.02, max(pontos$x)*1.02), ylim=c(min(pontos$y)*1.02, max(pontos$y)*1.02), frame=FALSE, xlab="", ylab="", xaxt="n", yaxt="n")
  points(pontos$x, pontos$y, col="darkgreen", pch=19, cex=0.06)
}
