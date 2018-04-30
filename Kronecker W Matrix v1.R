## Constructing the Function ##
left <- function(dims){
require(Matrix)
B <-as(diag(1, dims-1, dims-1), "CsparseMatrix")
D <-as(matrix(rep(0, dims), nrow = dims, ncol = 1), "CsparseMatrix")
E <-cbind(rbind(t(D[1: dims-1]), B), D)
return(E)
}

up <– function(dims){
require(Matrix)
B <-as(diag(1, dims-1, dims-1), "CsparseMatrix")
D <-as(matrix(rep(0, dims), nrow = dims, ncol = 1), "CsparseMatrix")
E <-cbind(D, rbind(B, t(D[1: dims-1])))
return(E)
}

ic <- as(diag(1, ncol = c, nrow = c), "CsparseMatrix")
ir <- as(diag(1, ncol = r, nrow = r), "CsparseMatrix")
dim. c <- ncol(ic);
dim. r <– ncol(ir)

# Block 1-Block 6 #
ac <- left(dim. c) + up(dim. c);
ar <- left(dim. r) + up(dim. r);
uc <-up(dim. c); lc <- left(dim. c);
ur <-up(dim. r); lr <- left(dim. r);

# Block 7 #
W <- kronecker((lr+ur), ic)+ kronecker(ir, (lc+uc))+kronecker(ur, (lc+uc)) + kronecker(lr, (lc+uc))