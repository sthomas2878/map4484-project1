### Clean R Project 1
library(ggplot2)
library(tidyverse)
library(ggthemes)

dat <- tibble(Hour = seq(1:168),
              Concentration_High = rep(15,168),
              Hour_Since_Dose = rep(c(0,1,2,3),42))

### IBUPROFEN CONCENTRATION
ibu_concentration <- function(doses, c_max, half_life) {
  #TESTING
  #doses = 28
  #c_max = 25
  #half_life = 2
  
  final_mat <- data.frame(matrix(0, nrow = 168, ncol = doses + 1))
  final_mat[,1] <- rep(0:(168/doses - 1), doses)
  track = 0
  for (i in 1:168){
    for (j in 1:doses){
      if (final_mat[i,1] == 0) {
        final_mat[i+1,j+1+track] = c_max * 0.7
        final_mat[i+2,j+1+track] = c_max
        final_mat[i+3,j+1+track] = c_max
        track = track + 1
        break
      }
    }
  }
  head(final_mat, 50)
  
  for (j in 1:doses) {
    hour = 0
    for (i in 1:167) {
      if (final_mat[i+1, j + 1] == c_max * 0.7) {
        
      } else if (i > 165) {
        
      } else if (final_mat[i+1, j+1] == 0) {
        
      } else if (final_mat[i+2,j + 1] == c_max) {
        final_mat[i+2, j+1] = final_mat[i+2, j+1] * 0.5^(hour / half_life)
        hour = 1
      } else {
        final_mat[i+2, j+1] = final_mat[i+1, j+1] * 0.5^(hour / half_life)
        hour = hour + 1
      }
    }
  }
  head(final_mat, 20)
  
  
  
  final_mat$Total <- apply(final_mat[,-1], MARGIN = 1, sum)
  return(final_mat$Total)
  #return(final_mat)
}

ibu_concentration_5 <- function(doses, c_max, half_life) {
  #TESTING
  #doses = 30
  #c_max = 25
  #half_life = 2
  
  final_mat <- data.frame(matrix(0, nrow = 150, ncol = doses + 1))
  final_mat[,1] <- rep(0:4, 30)
  track = 0
  for (i in 1:150){
    for (j in 1:doses){
      if (final_mat[i,1] == 0) {
        final_mat[i+1,j+1+track] = c_max * 0.7
        final_mat[i+2,j+1+track] = c_max
        final_mat[i+3,j+1+track] = c_max
        track = track + 1
        break
      }
    }
  }
  head(final_mat, 50)
  
  for (j in 1:doses) {
    hour = 0
    for (i in 1:150) {
      if (i > 147) {
        
      } else if (final_mat[i+1, j + 1] == c_max * 0.7) {
        
      } else if (final_mat[i+1, j+1] == 0) {
        
      } else if (final_mat[i+2,j + 1] == c_max) {
        final_mat[i+2, j+1] = final_mat[i+2, j+1] * 0.5^(hour / half_life)
        hour = 1
      } else {
        final_mat[i+2, j+1] = final_mat[i+1, j+1] * 0.5^(hour / half_life)
        hour = hour + 1
      }
    }
  }
  head(final_mat, 20)
  
  
  
  final_mat$Total <- apply(final_mat[,-1], MARGIN = 1, sum)
  return(c(final_mat$Total,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1))
  #return(final_mat)
}


######## Opioid Concentration #####
op_concentration <- function(doses, c_max, half_life) {
  #TESTING
  #doses = 28
  #c_max = 11.5
  #half_life = 4
  
  final_mat <- data.frame(matrix(0, nrow = 168, ncol = doses + 1))
  final_mat[,1] <- rep(0:(168/doses - 1), doses)
  track = 0
  for (i in 1:168){
    for (j in 1:doses){
      if (final_mat[i,1] == 0) {
        final_mat[i+1,j+1+track] = c_max * 0.2
        final_mat[i+2,j+1+track] = c_max * 0.5
        final_mat[i+3,j+1+track] = c_max * 0.8
        final_mat[i+4,j+1+track] = c_max * 0.9
        final_mat[i+5,j+1+track] = c_max
        track = track + 1
        break
      }
    }
  }
  head(final_mat, 50)
  tail(final_mat, 50)
  
  for (j in 1:doses) {
    hour = 0
    for (i in 1:167) {
      if (i > 163) {
        
      } else if (final_mat[i+5,j+1] == c_max*0.2 | final_mat[i+5,j+1] == c_max*0.5 |
                 final_mat[i+5,j+1] == c_max*0.8 | final_mat[i+5,j+1] == c_max*0.9) {
        
      } else if (final_mat[i+5,j+1] == c_max) {
        final_mat[i+5, j+1] = final_mat[i+5, j+1] * 0.5^(hour / half_life)
        hour = 1
      } else if (final_mat[i+4,j+1] != 0) {
        final_mat[i+5, j+1] = final_mat[i+4, j+1] * 0.5^(hour / half_life)
        hour = hour + 1
      } 
    }
  }
  head(final_mat, 20)
  
  final_mat$Total <- apply(final_mat[,-1], MARGIN = 1, sum)
  return(final_mat$Total[1:168])
}

op_concentration_5 <- function(doses, c_max, half_life) {
  #TESTING
  #doses = 30
  #c_max = 11.5
  #half_life = 4
  
  final_mat <- data.frame(matrix(0, nrow = 150, ncol = doses + 1))
  final_mat[,1] <- rep(0:4, 30)
  track = 0
  for (i in 1:150){
    for (j in 1:doses){
      if (final_mat[i,1] == 0) {
        final_mat[i+1,j+1+track] = c_max * 0.2
        final_mat[i+2,j+1+track] = c_max * 0.5
        final_mat[i+3,j+1+track] = c_max * 0.8
        final_mat[i+4,j+1+track] = c_max * 0.9
        final_mat[i+5,j+1+track] = c_max
        track = track + 1
        break
      }
    }
  }
  head(final_mat, 50)
  tail(final_mat, 50)
  
  for (j in 1:doses) {
    hour = 0
    for (i in 1:150) {
      if (i > 145) {
        
      } else if (final_mat[i+5,j+1] == c_max*0.2 | final_mat[i+5,j+1] == c_max*0.5 |
                 final_mat[i+5,j+1] == c_max*0.8 | final_mat[i+5,j+1] == c_max*0.9) {
        
      } else if (final_mat[i+5,j+1] == c_max) {
        final_mat[i+5, j+1] = final_mat[i+5, j+1] * 0.5^(hour / half_life)
        hour = 1
      } else if (final_mat[i+4,j+1] != 0) {
        final_mat[i+5, j+1] = final_mat[i+4, j+1] * 0.5^(hour / half_life)
        hour = hour + 1
      } 
    }
  }
  head(final_mat, 20)
  
  final_mat$Total <- apply(final_mat[,-1], MARGIN = 1, sum)
  last_thing <- c(final_mat$Total,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
  #return(final_mat$Total[1:150])
}

######## OPIOIDS ########
strength_5 <- op_concentration(doses = 42,
                            c_max = 8,
                            half_life = 4)
strength_10 <- op_concentration_5(doses = 30,
                             c_max = 11.5,
                             half_life = 4)
strength_15 <- op_concentration(doses = 28,
                             c_max = 17.25,
                             half_life = 4)


dat$Total_5 <- strength_5
dat$Total_10 <- strength_10
dat$Total_15 <- strength_15

colors = c('15mg every 6 hours' = 'red',
           '10mg every 5 hours' = 'dark green',
           '5mg every 4 hours' = 'blue')

adult <- ggplot(dat, aes(x = Hour,color = '5mg every 4 hours')) +
  geom_line(aes(y = Total_5)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  xlim(0, 48) + # Number of Hours
  labs(title = 'Concentration of Oxycodone in Body Over Time',
       y = 'Concentration (ng/mL)',
       color = 'Legend')

adult <- adult + geom_line(aes(y = Total_15, color = '15mg every 6 hours'))

adult <- adult + geom_line(aes(y = Total_10, color = '10mg every 5 hours')) +
  scale_color_manual(values = colors) + geom_hline(yintercept = 10) +
  annotate('text', y = 10.5, x = 1, label = 'MEC')

adult

######## IBUPROFEN ########
strength_4hrs <- ibu_concentration(doses = 42,
                               c_max = 25,
                               half_life = 2)
strength_5hrs <- ibu_concentration_5(doses = 30,
                                   c_max = 25,
                                   half_life = 2)
strength_6hrs <- ibu_concentration(doses = 28,
                               c_max = 25,
                               half_life = 2)


dat$Total_4hrs <- strength_4hrs
dat$Total_5hrs <- strength_5hrs
dat$Total_6hrs <- strength_6hrs

colors = c('400mg every 4 hours' = 'red',
           '400mg every 5 hours' = 'dark green',
           '400mg every 6 hours' = 'blue')

adult <- ggplot(dat, aes(x = Hour, color = '400mg every 4 hours')) +
  geom_line(aes(y = Total_4hrs)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  geom_hline(yintercept = c(10, 50)) +
  annotate('text', y = 10.7, x = 1, label = 'MEC') +
  annotate('text', y = 49.3, x = 1, label = 'MTC') +
  theme_solarized() +
  xlim(0, 48) + # Number of hours
  labs(title = 'Concentration of Ibuprofen in Body Over Time',
       y = 'Concentration (microgram / mL)',
       color = 'Legend')

adult <- adult + geom_line(aes(y = Total_5hrs,
                               color = '400mg every 5 hours'))

adult <- adult + geom_line(aes(y = Total_6hrs,
                               color = '400mg every 6 hours')) +
  scale_color_manual(values = colors)

adult


######COMBO######
oxy <- op_concentration(doses = 28, c_max = 8,
                     half_life = 4)
ibu <- ibu_concentration(doses = 28, c_max = 25,
                     half_life = 2)
dat$oxy <- oxy / 1000
dat$ibu <- ibu

colors = c('5mg Oxycodone every 6 hours' = 'red',
           '400mg Ibuprofen every 6 hours' = 'dark green')

adult <- ggplot(dat, aes(x = Hour, 
                         color = '400mg Ibuprofen every 6 hours')) +
  geom_line(aes(y = ibu)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  geom_hline(yintercept = 10) +
  annotate('text', y = 10.7, x = 1, label = 'MEC (Ibuprofen)') +
  xlim(0, 48) + # Number of hours
  labs(title = 'Concentration of Drug in Body Over Time',
       y = 'Concentration (microgram/mL)',
       color = 'Legend')
adult

adult <- adult + geom_line(aes(y = oxy,
                               color = '5mg Oxycodone every 6 hours')) +
  scale_color_manual(values = colors)

adult

adult <- ggplot(dat, aes(x = Hour, 
                         color = '5mg Oxycodone every 6 hours')) +
  geom_line(aes(y = oxy)) + 
  geom_vline(xintercept = c(0, 24, 48, 72, 96, 120, 144, 168),
             linetype = 2) + 
  theme_solarized() +
  geom_hline(yintercept = 10 / 1000) +
  annotate('text', y = 10.7/1000, x = 1, label = 'MEC (Oxycodone)') +
  xlim(0, 48) + # Number of hours
  labs(title = 'Concentration of Drug in Body Over Time',
       y = 'Concentration (microgram/mL)',
       color = 'Legend')
adult
