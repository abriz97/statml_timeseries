library(data.table)
library(here)

# paths
gitdir <- here()
gitdir.data <- file.path(gitdir, 'data')

filename <- file.path(gitdir.data, 'processed_data_walden.rda')
load(filename)

# helpers

get_frequency_grid <- function(B, fl)
{
    # tests indep iff f_{j + 1} - f_j > B

    # exlcude freqs close to the boundary.
    idx <- ( (fl <= (1/2 - B/2)) & (fl >= B/2))
    fl <- fl[idx]

    # now exclude if gaps are too small
    final_fl <- c()

    for ( f in fl )
    {
        stopifnot(f > final_fl)
        if( f - max(final_fl) > B ) 
            final_fl <- c(final_fl, f)
    }

    return( final_fl )

}

build_matrix_hatS <- function(DT, f)
{
    # extract upper triangular entries
    tmp <- S_dt[freq == f, .(j,k,complex)] |> unique()

    # compute lower triangular entries (using hermitian)
    tmp2 <- tmp[k > j, .(j=k, k=j, complex=Conj(complex) )]

    mat<- rbind(tmp, tmp2)
    setkey(mat, j,k)

    extract_matrix <- matrix(mat$complex,nrow=sqrt(nrow(mat)), byrow = TRUE)
    stopifnot(extract_matrix[1,2] == tmp[j==1 & k==2, complex])

    is.conjugate <- sum(extract_matrix - Conj(t(extract_matrix)))==0
    stopifnot( is.conjugate )

    return(extract_matrix)
}

build_partial_coherences <- function(f)
{
    # get inverse
    S_inverse <- build_matrix_hatS(S_dt, f=f) |> solve()

    N <- nrow(S_inverse)
    partial_coherence <- matrix(NA, nrow = N, ncol = N)

    # could be sped up but who cares
    for(row in 1:N)
    {
        for(col in 1:N)
        {
            partial_coherence[row, col] <- abs(S_inverse[row, col])^2/(S_inverse[row, row]*S_inverse[col,col])
        }
    }

    suppressWarnings(partial_coherence <- as.numeric(partial_coherence))
    partial_coherence
}

########
# MAIN #
########

# get frequency grid 
fl_final <- get_frequency_grid(B=B, fl=fl)

# subset hat_S_jk of interest
S_dt <- S_dt[freq %in% fl_final]
S_dt[, complex := real + 1i*imaginary]

# 
partial_coherences <- lapply(fl_final, build_partial_coherences)


carry_wilk_MHT <- function()
{
}



