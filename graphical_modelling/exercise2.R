library(data.table)
library(here)

# paths
gitdir <- here()
gitdir.data <- file.path(gitdir, "data")
gitdir.plots <- file.path(gitdir, "plots")

path.data <- file.path(gitdir.data, "walden_exercise2.csv")

# parameters
A = 46
K = 10
r = 3
mu = (2*K)/(K-r+1)

# load data
wilk_samples <- fread(path.data)
names(wilk_samples) <- NULL
wilk_samples <- unlist(wilk_samples)

# sort
wilk_samples <- sort(wilk_samples)
# normalise
wilk_samples_normalised <- (wilk_samples - A*mu)/(sqrt(A)*mu)

# null distribution Gamma(A, (K - r + 1)/2K)
null_samples <- rgamma(n=100000, shape=A, scale=mu)

filename=file.path(gitdir.plots, "qqplots.png")
png(filename=filename, width=800, height=500)
par(mfrow=c(1,2))
# make first qqplot
qqplot(
  null_samples, 
  wilk_samples,
  main="Gamma Q-Q Plot",
  xlab="Theoretical Quantiles",
  ylab="Sample Quantiles",
)
abline(0,1)
qqnorm(wilk_samples_normalised)
abline(0,1)
dev.off()
