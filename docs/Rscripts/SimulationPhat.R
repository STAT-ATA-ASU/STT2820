# Simulation of phat

phatfuncA <- function(m = 10000, n = 100, p = 0.5){
  require(ggplot2)
  X <- rbinom(m, n, p)
  phat <- X/n
  mph <- round(mean(phat), 4)
  sigp <- round(sd(phat), 4)
  # Sampled mean and sd of phats
  cat("Mean of phats =", mph)
  cat("\nStandard deviation of phats =", sd(phat)) 
  ggplot(data = data.frame(x = phat), aes(x = x)) + 
    geom_histogram(fill = "purple", color = "black", binwidth = 1/n) + 
    theme_bw() + 
    labs(x = expression(hat(p)), y = "", title = substitute(paste("The simulation yields " ,hat(mu)[hat(p)] == mph," and ", 
         hat(sigma)[hat(p)] == sigp), list(mph = mph, sigp = sigp)))
}

phatfuncA(m = 100000, n = 81, p = 0.6)
phatfuncA(m = 100000, n = 256, p = 0.8)
phatfuncA(m = 100000, n = 625, p = 0.3)
phatfuncA(m = 100000, n = 233, p = 0.07)
