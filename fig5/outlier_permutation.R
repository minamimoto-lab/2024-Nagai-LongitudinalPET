library(tidyverse)


# import data
x=read.csv("titer.csv")

# variables
n_perm <- 1000000
T_perm <- numeric(n_perm)

#-#-#-#-#-#-#-#-#-#-#-#-#-#
# identifying 1st outlier #
#-#-#-#-#-#-#-#-#-#-#-#-#-#
obs_value <- max(x$titer)
print(obs_value)

T_obs <- abs(obs_value - mean(x$titer))

for (i in 1:n_perm) {
  x_perm <- sample(x$titer)  # shuffle
  T_perm[i] <- abs(x_perm[1] - mean(x_perm[-1]))
}

# p-value
p_val <- mean(T_perm >= T_obs)

cat("p =", p_val, "\n")


#-#-#-#-#-#-#-#-#-#-#-#-#-#
# identifying 2nd outlier #
#-#-#-#-#-#-#-#-#-#-#-#-#-#

# exclude 1st outlier
x=read.csv("titer.csv") %>%
  dplyr::filter(titer<obs_value)

obs_value <- max(x$titer)
print(obs_value)

T_obs <- abs(obs_value - mean(x$titer))


for (i in 1:n_perm) {
  x_perm <- sample(x$titer)  # shuffle
  T_perm[i] <- abs(x_perm[1] - mean(x_perm[-1]))
}

# p-value
p_val <- mean(T_perm >= T_obs)

cat("p =", p_val, "\n")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# confirm no other outlier  #
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#

# exclude 1st outlier
x=read.csv("titer.csv") %>%
  dplyr::filter(titer<obs_value)

obs_value <- max(x$titer)
print(obs_value)

T_obs <- abs(obs_value - mean(x$titer))

for (i in 1:n_perm) {
  x_perm <- sample(x$titer)  # shuffle
  T_perm[i] <- abs(x_perm[1] - mean(x_perm[-1]))
}

# p-value
p_val <- mean(T_perm >= T_obs)

cat("p =", p_val, "\n")

