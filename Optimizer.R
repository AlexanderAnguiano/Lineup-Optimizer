

library(gtools)
library(dplyr)


### Set Working Environment ###
setwd("C:/Users/Your/Desktop")

### Read in prepped data for chosen starters. ### 

id <- read.csv("wk8starts.csv", stringsAsFactors = FALSE)

### Get Combinations, n = Number of players being used, r = number of players in each lineup. ###
### Use player ID in combination list. ###

### WARNING!!!  CPU INTENSIVE, REDUCE n = # IF NECESSARY ###
Combo <- combinations(n = 32, r = 9, v = c("11503310", "11503568", "11503360", "11503481", "11502979", "11502992", "11503173", "11503386", "11503430", "11503267", "11503400", "11503526", "11503234", "11503119", "11503384", "11503102", "11503002", "11502903", "11503530", "11502865", "11503402", "11502866", "11502926", "11502883", "11503213", "11503085", "11503304", "11502837", "11502841", "11502877", "11503392", "11502846"), repeats.allowed = FALSE)


### Format Data and Convert to A Tibble ###

matrix_DF <- as_data_frame(Combo)
DataFrame <- data.frame(matrix_DF)

DataFrame$V1 <- as.integer(DataFrame$V1)
DataFrame$V2 <- as.integer(DataFrame$V2)
DataFrame$V3 <- as.integer(DataFrame$V3)
DataFrame$V4 <- as.integer(DataFrame$V4)
DataFrame$V5 <- as.integer(DataFrame$V5)
DataFrame$V6 <- as.integer(DataFrame$V6)
DataFrame$V7 <- as.integer(DataFrame$V7)
DataFrame$V8 <- as.integer(DataFrame$V8)
DataFrame$V9 <- as.integer(DataFrame$V9)

str(DataFrame)
head(DataFrame)

combo_tib <- as_tibble(DataFrame)
head(combo_tib)


tib_id <- as_tibble(id)


### Join in each Players Salary ###

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V1" = "ID"))
combo_tib <- mutate(combo_tib, Salary1 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:9, 17)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V2" = "ID"))
combo_tib <- mutate(combo_tib, Salary2 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:10, 18)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V3" = "ID"))
combo_tib <- mutate(combo_tib, Salary3 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:11, 19)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V4" = "ID"))
combo_tib <- mutate(combo_tib, Salary4 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:12, 20)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V5" = "ID"))
combo_tib <- mutate(combo_tib, Salary5 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:13, 21)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V6" = "ID"))
combo_tib <- mutate(combo_tib, Salary6 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:14, 22)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V7" = "ID"))
combo_tib <- mutate(combo_tib, Salary7 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:15, 23)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V8" = "ID"))
combo_tib <- mutate(combo_tib, Salary8 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:16, 24)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V9" = "ID"))
combo_tib <- mutate(combo_tib, Salary9 = combo_tib$Salary)
combo_tib <- select(combo_tib, 1:17, 25)

### Add all salaries and filter Total Salaries under 50000 requirement. ###
combo_tib <- mutate(combo_tib, 
                    Total_Salary = Salary1 + Salary2 + Salary3 + Salary4 + Salary5 + Salary6 + Salary7 + Salary8 + Salary9)


combo_tib <- filter(combo_tib, Total_Salary <= 50000, Total_Salary >= 49500)


### Join in each Players Position ###

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V1" = "ID"))
combo_tib <- mutate(combo_tib, Pos1 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:19, 27)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V2" = "ID"))
combo_tib <- mutate(combo_tib, Pos2 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:20, 28)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V3" = "ID"))
combo_tib <- mutate(combo_tib, Pos3 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:21, 29)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V4" = "ID"))
combo_tib <- mutate(combo_tib, Pos4 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:22, 30)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V5" = "ID"))
combo_tib <- mutate(combo_tib, Pos5 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:23, 31)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V6" = "ID"))
combo_tib <- mutate(combo_tib, Pos6 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:24, 32)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V7" = "ID"))
combo_tib <- mutate(combo_tib, Pos7 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:25, 33)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V8" = "ID"))
combo_tib <- mutate(combo_tib, Pos8 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:26, 34)

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V9" = "ID"))
combo_tib <- mutate(combo_tib, Pos9 = combo_tib$Roster.Position)
combo_tib <- select(combo_tib, 1:27, 35)


### Joine in each Players Projected Points ###

combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V1" = "ID"))
combo_tib <- mutate(combo_tib, Points1 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:28, 36)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V2" = "ID"))
combo_tib <- mutate(combo_tib, Points2 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:29, 37)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V3" = "ID"))
combo_tib <- mutate(combo_tib, Points3 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:30, 38)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V4" = "ID"))
combo_tib <- mutate(combo_tib, Points4 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:31, 39)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V5" = "ID"))
combo_tib <- mutate(combo_tib, Points5 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:32, 40)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V6" = "ID"))
combo_tib <- mutate(combo_tib, Points6 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:33, 41)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V7" = "ID"))
combo_tib <- mutate(combo_tib, Points7 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:34, 42)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V8" = "ID"))
combo_tib <- mutate(combo_tib, Points8 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:35, 43)


combo_tib <- combo_tib %>%
  left_join(tib_id, by = c("V9" = "ID"))
combo_tib <- mutate(combo_tib, Points9 = combo_tib$Projected)
combo_tib <- select(combo_tib, 1:36, 44)


combo_tib <- mutate(combo_tib, 
                    Total_Projected = Points1 + Points2 + Points3 + Points4 + Points5 + Points6 + Points7 + Points8 + Points9)


### Filter Positions to required 1QB, 2RB, 3WR, 1TE, 1Flex(1RB, or 1WR, or 1TE), 1DST for each lineup. ###


vlookall <- data.frame(combo_tib)



vlookall$DSTreduced1 <- ifelse(vlookall$Pos1 == "DST", 1, 0)
vlookall$DSTreduced2 <- ifelse(vlookall$Pos2 == "DST", 1, 0)
vlookall$DSTreduced3 <- ifelse(vlookall$Pos3 == "DST", 1, 0)
vlookall$DSTreduced4 <- ifelse(vlookall$Pos4 == "DST", 1, 0)
vlookall$DSTreduced5 <- ifelse(vlookall$Pos5 == "DST", 1, 0)
vlookall$DSTreduced6 <- ifelse(vlookall$Pos6 == "DST", 1, 0)
vlookall$DSTreduced7 <- ifelse(vlookall$Pos7 == "DST", 1, 0)
vlookall$DSTreduced8 <- ifelse(vlookall$Pos8 == "DST", 1, 0)
vlookall$DSTreduced9 <- ifelse(vlookall$Pos9 == "DST", 1, 0)

vlookall$DSTreducedTotal <- vlookall$DSTreduced1 + vlookall$DSTreduced2 + vlookall$DSTreduced3 + vlookall$DSTreduced4 + vlookall$DSTreduced5 + vlookall$DSTreduced6 + vlookall$DSTreduced7 + vlookall$DSTreduced8 + vlookall$DSTreduced9


vlookall <- vlookall[vlookall$DSTreducedTotal == 1,]


vlookall$QBreduced1 <- ifelse(vlookall$Pos1 == "QB", 1, 0)
vlookall$QBreduced2 <- ifelse(vlookall$Pos2 == "QB", 1, 0)
vlookall$QBreduced3 <- ifelse(vlookall$Pos3 == "QB", 1, 0)
vlookall$QBreduced4 <- ifelse(vlookall$Pos4 == "QB", 1, 0)
vlookall$QBreduced5 <- ifelse(vlookall$Pos5 == "QB", 1, 0)
vlookall$QBreduced6 <- ifelse(vlookall$Pos6 == "QB", 1, 0)
vlookall$QBreduced7 <- ifelse(vlookall$Pos7 == "QB", 1, 0)
vlookall$QBreduced8 <- ifelse(vlookall$Pos8 == "QB", 1, 0)
vlookall$QBreduced9 <- ifelse(vlookall$Pos9 == "QB", 1, 0)

vlookall$QBreducedTotal <- vlookall$QBreduced1 + vlookall$QBreduced2 + vlookall$QBreduced3 + vlookall$QBreduced4 + vlookall$QBreduced5 + vlookall$QBreduced6 + vlookall$QBreduced7 + vlookall$QBreduced8 + vlookall$QBreduced9

vlookall <- vlookall[vlookall$QBreducedTotal == 1,]


vlookall$TEreduced1 <- ifelse(vlookall$Pos1 == "TE/FLEX", 1, 0)
vlookall$TEreduced2 <- ifelse(vlookall$Pos2 == "TE/FLEX", 1, 0)
vlookall$TEreduced3 <- ifelse(vlookall$Pos3 == "TE/FLEX", 1, 0)
vlookall$TEreduced4 <- ifelse(vlookall$Pos4 == "TE/FLEX", 1, 0)
vlookall$TEreduced5 <- ifelse(vlookall$Pos5 == "TE/FLEX", 1, 0)
vlookall$TEreduced6 <- ifelse(vlookall$Pos6 == "TE/FLEX", 1, 0)
vlookall$TEreduced7 <- ifelse(vlookall$Pos7 == "TE/FLEX", 1, 0)
vlookall$TEreduced8 <- ifelse(vlookall$Pos8 == "TE/FLEX", 1, 0)
vlookall$TEreduced9 <- ifelse(vlookall$Pos9 == "TE/FLEX", 1, 0)


vlookall$TEreducedTotal <- vlookall$TEreduced1 + vlookall$TEreduced2 + vlookall$TEreduced3 + vlookall$TEreduced4 + vlookall$TEreduced5 + vlookall$TEreduced6 + vlookall$TEreduced7 + vlookall$TEreduced8 + vlookall$TEreduced9


vlookall <- vlookall[vlookall$TEreducedTotal <= 2,]
vlookall <- vlookall[vlookall$TEreducedTotal >= 1,]


vlookall$RBreduced1 <- ifelse(vlookall$Pos1 == "RB/FLEX", 1, 0)
vlookall$RBreduced2 <- ifelse(vlookall$Pos2 == "RB/FLEX", 1, 0)
vlookall$RBreduced3 <- ifelse(vlookall$Pos3 == "RB/FLEX", 1, 0)
vlookall$RBreduced4 <- ifelse(vlookall$Pos4 == "RB/FLEX", 1, 0)
vlookall$RBreduced5 <- ifelse(vlookall$Pos5 == "RB/FLEX", 1, 0)
vlookall$RBreduced6 <- ifelse(vlookall$Pos6 == "RB/FLEX", 1, 0)
vlookall$RBreduced7 <- ifelse(vlookall$Pos7 == "RB/FLEX", 1, 0)
vlookall$RBreduced8 <- ifelse(vlookall$Pos8 == "RB/FLEX", 1, 0)
vlookall$RBreduced9 <- ifelse(vlookall$Pos9 == "RB/FLEX", 1, 0)

vlookall$RBreducedTotal <- vlookall$RBreduced1 + vlookall$RBreduced2 + vlookall$RBreduced3 + vlookall$RBreduced4 + vlookall$RBreduced5 + vlookall$RBreduced6 + vlookall$RBreduced7 + vlookall$RBreduced8 + vlookall$RBreduced9


vlookall <- vlookall[vlookall$RBreducedTotal <= 3,]
vlookall <- vlookall[vlookall$RBreducedTotal >= 2,]


vlookall$WRreduced1 <- ifelse(vlookall$Pos1 == "WR/FLEX", 1, 0)
vlookall$WRreduced2 <- ifelse(vlookall$Pos2 == "WR/FLEX", 1, 0)
vlookall$WRreduced3 <- ifelse(vlookall$Pos3 == "WR/FLEX", 1, 0)
vlookall$WRreduced4 <- ifelse(vlookall$Pos4 == "WR/FLEX", 1, 0)
vlookall$WRreduced5 <- ifelse(vlookall$Pos5 == "WR/FLEX", 1, 0)
vlookall$WRreduced6 <- ifelse(vlookall$Pos6 == "WR/FLEX", 1, 0)
vlookall$WRreduced7 <- ifelse(vlookall$Pos7 == "WR/FLEX", 1, 0)
vlookall$WRreduced8 <- ifelse(vlookall$Pos8 == "WR/FLEX", 1, 0)
vlookall$WRreduced9 <- ifelse(vlookall$Pos9 == "WR/FLEX", 1, 0)

vlookall$WRreducedTotal <- vlookall$WRreduced1 + vlookall$WRreduced2 + vlookall$WRreduced3 + vlookall$WRreduced4 + vlookall$WRreduced5 + vlookall$WRreduced6 + vlookall$WRreduced7 + vlookall$WRreduced8 + vlookall$WRreduced9


vlookall <- vlookall[vlookall$WRreducedTotal <= 4,]
vlookall <- vlookall[vlookall$WRreducedTotal >= 3,]


### Keep desired columns and export as csv file ###

vlookfinal <- vlookall[c(1:9, 19:28, 38)]

write.csv(vlookfinal, file = "Wk7FinalLineUpsv3.csv")









