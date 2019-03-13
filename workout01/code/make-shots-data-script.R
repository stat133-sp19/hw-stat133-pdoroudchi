# Making Shots Global Table
# description: using individual raw data files of five Golden State Warriors NBA players to create one global table named shots-data.csv 
# input(s): andre-iguodala.csv, draymond-green.csv, klay-thompson.csv, kevin-durant.csv, stephen-curry.csv 
# output: shots-data.csv

# importing data files

iguodala <- read.csv('../data/andre-iguodala.csv', stringsAsFactors = FALSE)
draymond <- read.csv('../data/draymond-green.csv', stringsAsFactors = FALSE)
thompson <- read.csv('../data/klay-thompson.csv', stringsAsFactors = FALSE)
durant <- read.csv('../data/kevin-durant.csv', stringsAsFactors = FALSE)
curry <- read.csv('../data/stephen-curry.csv', stringsAsFactors = FALSE)

# adding name column

iguodala$name <- 'Andre Iguodala'
draymond$name <- 'Draymond Green'
thompson$name <- 'Klay Thompson'
durant$name <- 'Kevin Durant'
curry$name <- 'Stephen Curry'

# changing 'n' and 'y' values in shot_made_flag column

iguodala$shot_made_flag[iguodala$shot_made_flag == 'n'] <- 'shot_no'
iguodala$shot_made_flag[iguodala$shot_made_flag == 'y'] <- 'shot_yes'

draymond$shot_made_flag[draymond$shot_made_flag == 'n'] <- 'shot_no'
draymond$shot_made_flag[draymond$shot_made_flag == 'y'] <- 'shot_yes'

thompson$shot_made_flag[thompson$shot_made_flag == 'n'] <- 'shot_no'
thompson$shot_made_flag[thompson$shot_made_flag == 'y'] <- 'shot_yes'

durant$shot_made_flag[durant$shot_made_flag == 'n'] <- 'shot_no'
durant$shot_made_flag[durant$shot_made_flag == 'y'] <- 'shot_yes'

curry$shot_made_flag[curry$shot_made_flag == 'n'] <- 'shot_no'
curry$shot_made_flag[curry$shot_made_flag == 'y'] <- 'shot_yes'

# adding a column minute

iguodala$minute <- 12 * iguodala$period - iguodala$minutes_remaining
draymond$minute <- 12 * draymond$period - draymond$minutes_remaining
thompson$minute <- 12 * thompson$period - thompson$minutes_remaining
durant$minute <- 12 * durant$period - durant$minutes_remaining
curry$minute <- 12 * curry$period - curry$minutes_remaining

# using sink() to send summary output of each imported data frame into individual text files

sink(file = '../output/andre-iguodala-summary.txt')
summary(iguodala)
sink()

sink(file = '../output/draymond-green-summary.txt')
summary(draymond)
sink()

sink(file = '../output/klay-thompson-summary.txt')
summary(thompson)
sink()

sink(file = '../output/kevin-durant-summary.txt')
summary(durant)
sink()

sink(file = '../output/stephen-curry-summary.txt')
summary(curry)
sink()

# using rbind() to stack tables into one data frame

shots_data <- rbind(iguodala, draymond, thompson, durant, curry)

# exporting shots_data to a csv in data/ folder

write.csv(shots_data, file = '../data/shots-data.csv')

# using sink() to send summary output of shots_data to output/ folder

sink(file = '../output/shots-data-summary.txt')
summary(shots_data)
sink()
