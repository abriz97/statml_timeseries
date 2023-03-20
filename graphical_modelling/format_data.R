library(data.table)
library(here)
library(stringr)

# paths
gitdir <- here()
gitdir.data <- file.path(gitdir, 'data')

# load data
path.data <- file.path(gitdir.data, "SDM_2.dat")
lines <- readLines(path.data)

# helpers
read_between_lines <- function(from, to)
    lines[(from:to)] |> as.numeric()


filename <- file.path(gitdir.data, 'processed_data_walden.rda')

# get parameters
params <- strsplit(lines[1], split=' ') |> unlist() 
r <- params[1] |> as.integer()
B <- params[2] |> as.numeric()
K <- params[3] |> as.integer()
nf <- params[4] |> as.integer()


# extract f_ls (lines between first and second empty lines)
idx_space <- which(lines %like%  "^$")
idx_fl <- (idx_space[1] + 1):(idx_space[2])
fl <- read_between_lines(from=idx_space[1] + 1, to=idx_space[2] - 1)
stopifnot(length(fl) == nf)

# extract hat(S)_jk(f)
idx <- which(lines %like%  "^[0-9]+ [0-9]+$")

# output a data.table with columns j, k, f, Real(S), Im(S)

S_dt <- lapply(lines[idx], strsplit, split=' ') |>  
    lapply(as.data.table) |>
    lapply(t) |> 
    lapply(as.data.table) |>
    rbindlist()
names(S_dt) <- c('j','k')

S_dt[, start := idx]
S_dt[, end := shift(idx, -1)-1]
S_dt[is.na(end), end := length(lines)]
stopifnot(S_dt[, uniqueN(start - end)==1])

# read in also hat_S_jk and format 
S_dt <- S_dt[, .(hat_S_jk = read_between_lines(from=start, to=end)), by=c('j', 'k')]
S_dt <- subset(S_dt, !is.na(hat_S_jk))
S_dt[, `:=` (
    type=c(rep("real", nf), rep("imaginary", nf)), 
    freq=rep(fl, 2))
    ,by=c('j','k')]

S_dt <- dcast( S_dt, j+k+freq ~ type, value.var = 'hat_S_jk')
setkey(S_dt, freq, j, k)
setcolorder(S_dt, c('freq', 'j','k'))

save(r, B, K, nf, fl, S_dt, file=filename)
