library(ggplot2)
library(tidyverse)
library(ggthemes)

dat <- tibble(Hour = seq(1:168),
              Concentration_High = rep(15,168),
              Hour_Since_Dose = rep(c(0,1,2,3),42))

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
#return(final_mat)
}


    ######## OPIOIDS ########
strength_5 <- concentration(doses = 28,
                            dose_strength = 5,
                            half_life = 4)
strength_10 <- concentration(doses = 28,
                             dose_strength = 10,
                             half_life = 4)
strength_15 <- concentration(doses = 28,
                            dose_strength = 15,
                            half_life = 4)


dat$Total_5 <- strength_5
dat$Total_10 <- strength_10
dat$Total_15 <- strength_15

colors = c('15mg every 6 hours' = 'red',
           '10mg every 6 hours' = 'dark green',
           '5mg every 4 hours' = 'blue')

adult <- ggplot(dat, aes(x = Hour,color = '5mg every 4 hours')) +
  geom_line(aes(y = Total_5)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  xlim(0, 48) + # Single Day
  ylim(0, 16) +
  labs(title = 'Quantity of Drug in Body Over Time',
       y = 'Quantity (mg)',
       color = 'Legend')

adult <- adult + geom_line(aes(y = Total_15, color = '15mg every 6 hours')) +
  scale_color_manual(values = colors)

adult <- adult + geom_line(aes(y = strength_10, color = '10mg every 6 hours'))
adult

      ######## IBUPROFEN ########
strength_4hrs <- concentration(doses = 42,
                            dose_strength = 400,
                            half_life = 2)
strength_6hrs <- concentration(doses = 28,
                             dose_strength = 400,
                             half_life = 2)


dat$Total_4hrs <- strength_4hrs
dat$Total_6hrs <- strength_6hrs

colors = c('400mg every 4 hours' = 'red',
           '400mg every 6 hours' = 'dark green')

adult <- ggplot(dat, aes(x = Hour, color = '400mg every 4 hours')) +
  geom_line(aes(y = Total_4hrs)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  xlim(0, 48) + # Number of hours
  labs(title = 'Quantity of Drug in Body Over Time',
       y = 'Quantity (mg)',
       color = 'Legend')
adult

adult <- adult + geom_line(aes(y = Total_6hrs,
                               color = '400mg every 6 hours')) +
  scale_color_manual(values = colors)
adult


1, 17
2, 22.5
3, 25
4, 17.5
5, 15
6, 12

adult <- ggplot(dat, aes(x = Hour, color = '400mg every 4 hours')) +
  geom_line(aes(y = Total_4hrs/5)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  xlim(0, 48) + # Number of hours
  labs(title = 'Concentration of Drug in Body Over Time',
       y = 'Concentration (mg/L)',
       color = 'Legend')
adult

adult <- adult + geom_line(aes(y = Total_6hrs / 5,
                               color = '400mg every 6 hours')) +
  scale_color_manual(values = colors)

adult + geom_hline(yintercept = c(10,50))



    ######COMBO######
oxy <- concentration(doses = 28, dose_strength = 5,
                               half_life = 4)
ibu <- concentration(doses = 28, dose_strength = 400,
                               half_life = 2)
dat$oxy <- oxy
dat$ibu <- ibu

colors = c('5mg Oxycodone every 6 hours' = 'red',
           '400mg Ibuprofen every 6 hours' = 'dark green')

adult <- ggplot(dat, aes(x = Hour, 
                  color = '400mg Ibuprofen every 6 hours')) +
  geom_line(aes(y = ibu)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  xlim(0, 48) + # Number of hours
  labs(title = 'Quantity of Drug in Body Over Time',
       y = 'Quantity (mg)',
       color = 'Legend')
adult

adult <- adult + geom_line(aes(y = oxy,
                      color = '5mg Oxycodone every 6 hours')) +
  scale_color_manual(values = colors)
adult
