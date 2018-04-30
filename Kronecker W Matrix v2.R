left <- function(dims, shift){
  require(Matrix)
  B <- as(diag(1, dims-shift, dims-shift), "CsparseMatrix")
  D <- as(matrix(rep(0, dims), nrow = dims, ncol = shift), "CsparseMatrix")
  E <- cbind(rbind(t(D[1:(dims-shift),]),B), D)
  return(E)
}

up <- function(dims, shift){
  require(Matrix)
  B <- as(diag(1, dims-shift, dims-shift), "CsparseMatrix")
  D <- as(matrix(rep(0, dims), nrow = dims, ncol = shift), "CsparseMatrix")
  E <- cbind(D, rbind(B, t(D[1:(dims-shift),])))
  return(E)
}

# Block 1-Block 4 #

ic <- as(diag(1,ncol = c,nrow = c),"CsparseMatrix")
ir <- as(diag(1,ncol = r,nrow = r),"CsparseMatrix")
dim.c <- ncol(ic);dim.r <-ncol(ir) 

ac <- left(dim.c, 1) + up(dim.c, 1); ar <- left(dim.r, 1) + up(dim.r, 1)

## For nearest neighbors we defined shift = 1 ## 
# Block 5 #
W <- kronecker(ar,ic)+kronecker(ir,ac)+kronecker(ar,ac)
