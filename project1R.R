library(ggplot2)
library(tidyverse)

dat <- tibble(Hour = seq(1:168),
              Concentration_High = rep(15,168),
              Hour_Since_Dose = rep(c(0,1,2,3),42))

dat <- dat %>% mutate(Concentration = 
                        dat$Concentration_High * (0.5^(dat$Hour_Since_Dose / 4)))

doses <- 42
dose_strength <- 5
half_life <- 4
concentration <- function(doses, dose_strength, half_life) {
final_mat <- data.frame(matrix(0, nrow = 168, ncol = doses + 1))
final_mat[,1] <- rep(0:(168/doses - 1), doses)
track = 0
for (i in 1:168){
  for (j in 1:doses){
    if (final_mat[i,1] == 0) {
      final_mat[i,j+1+track] = dose_strength
      track = track + 1
      break
    }
  }
}
head(final_mat)

for (j in 1:doses) {
  hour = 0
  for (i in 1:167) {
    if (final_mat[i,j + 1] == dose_strength) {
      hour = 1
      final_mat[i+1, j+1] = final_mat[i, j+1] * 0.5^(hour / half_life)
    } else if (final_mat[i + 1, j + 1] == dose_strength) {
      
    } else {
      final_mat[i+1, j+1] = final_mat[i, j+1] * 0.5^(hour / half_life)
      hour = hour + 1
    }
  }
}
head(final_mat)

final_mat$Total <- apply(final_mat[,-1], MARGIN = 1, sum)
return(final_mat$Total)
}

strength_5 <- concentration(doses = 42,
                            dose_strength = 5,
                            half_life = 4)
strength_15 <- concentration(doses = 28,
                            dose_strength = 15,
                            half_life = 4)


dat$Total_5 <- strength_5
dat$Total_15 <- strength_15

adult <- ggplot(dat, aes(x = Hour)) +
  geom_line(aes(y = Total_5)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             color = 'red', linetype = 2) + 
  theme_bw() +
  xlim(0, 24) + # Single Day
  labs(title = 'Concentration Over Time', y = 'Concentration')

adult

adult + geom_line(aes(y = Total_15), 
                  color = 'blue')
