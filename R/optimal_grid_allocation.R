optimal_grid_allocation <- function(data, grid.cols=NULL, grid.rows=NULL, addGrid=0,plot=FALSE){
  if(is.null(grid.cols)&&is.null(grid.rows)){
    x <- apply(data,2,function(x)max(x)-min(x))
    grid.rows <- ceiling(sqrt(nrow(data)/(x[1]/x[2])))
    grid.cols <- ceiling(nrow(data)/grid.rows)
  }else if(is.null(grid.cols)){
    grid.cols <- ceiling(nrow(data)/grid.rows)
  }else if(is.null(grid.rows)){
    grid.rows <- ceiling(nrow(data)/grid.cols)
  }
  grid.cols <- grid.cols +addGrid
  grid.rows <- grid.rows +addGrid
  gridpoints <- grid.rows * grid.cols
  data2 = data
  data2[,1] = (data[,1] - min(data[,1]))/(max(data[,1] - min(data[,1]))) * (grid.cols-1)
  data2[,2] = (data[,2] - min(data[,2]))/(max(data[,2] - min(data[,2]))) * (grid.rows-1)
  data1 <- as.matrix(expand.grid(0:(grid.cols-1), 0:(grid.rows-1)))
  n1 <- nrow(data1)
  n2 <- nrow(data2)
  colnames(data2) <- colnames(data1)
  if(plot){
    plot(data1)
    text(x=data2[,1],y=data2[,2],lab=rownames(data2))
  }
  D <- as.matrix(dist(rbind(data1, data2)))[1:n1, (n1+1):((n1+n2))]

  f.obj <- as.vector(D)
  f.con <- matrix(0, nrow=n1+n2, ncol=n1*n2)
  for(i in 0:(n2-1)){
    f.con[i+1, i*n1+(1:n1)] <- 1
  }

  for(i in 0:(n1-1)){
    f.con[i+n2+1, seq(1, n1*n2-n1+1, by=n1)+i] <- 1
  }

  f.dir <- c(rep("==",n2), rep("<=", n1))
  f.rhs <- c(rep(1, n1+n2))
  rglpk <- simplex(f.obj,A1=f.con[f.dir=="<=",],A3=f.con[f.dir=="==",],b1=f.rhs[f.dir=="<="],b3=f.rhs[f.dir=="=="])
  #rglpk <- Rglpk_solve_LP(f.obj, f.con, f.dir, f.rhs, types=rep("B", n1*n2), max=FALSE,
  #                        bounds=NULL, verbose=FALSE)
  #cat("Minimal sum of squared distances:\n",round(rglpk$optimum,3),"\n")
  cat("Minimal sum of squared distances:\n",round(rglpk$value,3),"\n")
  #sol <- matrix(rglpk$solution, byrow=TRUE, ncol=n1)
  sol <- matrix(rglpk$soln, byrow=TRUE, ncol=n1)
  sol1 <- sol
  ind <- apply(sol, 1, which.max)
  dat1ord <- data1[ind,]
  if(plot){
    par(mfrow=c(1,2))
    plot(data2[,2]~data2[,1], type="n", ann=FALSE, axes=FALSE)
    box(lwd=1)
    text(x=data2[,1], y=data2[,2], labels=rownames(data2))
    plot(dat1ord[,2]~dat1ord[,1], type="n", ann=FALSE, axes=FALSE)
    box(lwd = 1)
    text(x=dat1ord[,1], y=dat1ord[,2],labels=rownames(data2))
  }
  return (ind)
}
