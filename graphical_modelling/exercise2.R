library(data.table)
library(here)

# paths
gitdir <- here()
gitdir.data <- file.path(gitdir, 'data')
gitdir.plots <- file.path(gitdir, 'plots')

path.data <- file.path(gitdir.data, "walden_exercise2.csv")

# paramrams
A = 46
K = 40
r = 3
mu=(2*K)/(K-r+1)

# load data
W_samples <- fread(path.data)  |> unlist() |> unname()
stopifnot(length(W_samples) == K)

# sort
W_samples_ordered <- sort(W_samples)
# normalise
W_norm <- (W_samples_ordered - A*mu)/(sqrt(A)*mu)

# null distribution Gamma(A, (K - r + 1)/2K )
tmp <- rgamma(n=100000, shape=A, rate=(K-r+1)/(2*K))

filename=file.path(gitdir.plots, 'qqplots.png')
png(filename=filename, width=800, height=500)
par(mfrow=c(1,2))
# make first qqplot
qqplot( tmp, W_samples_ordered,
    main="Gamma Q-Q Plot",
    xlab="Theoretical Quantiles",
    ylab="Sample Quantiles")
abline(0,1)
qqnorm(W_norm)
abline(0,1)
dev.off()
