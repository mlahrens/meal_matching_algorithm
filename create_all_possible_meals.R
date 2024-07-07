library(data.table)
library(tidyverse)

min_foods <- 3
max_foods <- 6
total_energy <- 300
tolerance <- 0

start <- Sys.time()
##### information about the foods PER CALORIE
###### This file must have columns called group, name, energy_lower, energy_high, and step_size
food_information <- read.csv("food_and_group_information.csv", row.names = NULL) 
character_vec <- c("fat", "cho", "protein", "sfa", "dfib", "glyc_index", "glyc_load")

group0_foods <- sort(food_information$name[food_information$group == 0])
all_options_group0 <- lapply(group0_foods, 
                             function(x) {
     c(0, seq(food_information$energy_low[food_information$name == x], 
              food_information$energy_high[food_information$name == x], 
              by = food_information$step_size[food_information$name == x]))
                             })
names(all_options_group0) <- group0_foods

all_options_group0 <- all_options_group0[order(sapply(all_options_group0, length))]
all_combindations_group0 <- expand.grid(all_options_group0[-c((length(all_options_group0) - 2):length(all_options_group0))])

### start filtering out unnecessary groupings -- we want no more than 5 (max_foods) foods in a meal
all_combindations_group0 <- all_combindations_group0[rowSums(all_combindations_group0 != 0) <= max_foods - 1,] 

all_combindations_group0[,rev(group0_foods)[3]] <- 0
all_combindations_group0[,rev(group0_foods)[2]] <- 0
all_combindations_group0[,rev(group0_foods)[1]] <- 0
all_combindations_group0 <- all_combindations_group0[,group0_foods]

food_information_group0 <- subset(food_information, group == 0)
food_information_group0 <- food_information_group0[order(food_information_group0$name),]

oa <- NULL
for (x1 in all_options_group0[[length(all_options_group0) - 2]]){
  print(x1)
  for (x2 in all_options_group0[[length(all_options_group0) - 1]]){
    print(x2)
    for (x3 in all_options_group0[[length(all_options_group0)]]){
      
      all_combindations_group0[,rev(names(all_options_group0))[3]] <- x1
      all_combindations_group0[,rev(names(all_options_group0))[2]] <- x2
      all_combindations_group0[,rev(names(all_options_group0))[1]] <- x3
      
      all_combindations_current <- all_combindations_group0[min_foods <= rowSums(all_combindations_group0 != 0) &
                                                              rowSums(all_combindations_group0 != 0) <= max_foods,]
      
      energy <- rowSums(all_combindations_current)
      all_combindations_current     <- subset(all_combindations_current, abs(energy - total_energy) <= tolerance)
      all_combindations_current_mat <- as.matrix(all_combindations_current)
      
      for(characteristic in character_vec){
        all_combindations_current[,characteristic] <- all_combindations_current_mat %*% food_information_group0[,characteristic]
      }
      
      oa <- rbind(oa, all_combindations_current)
    }
  }
}
fwrite(oa, file = paste0("./out_group0.csv"), append = FALSE)



##################################################################################
group1_foods <- sort(food_information$name[food_information$group == 1])
all_options_group1 <- lapply(group1_foods, 
                             function(x) {
                               unique(c(0, seq(food_information$energy_low[food_information$name == x], 
                                               food_information$energy_high[food_information$name == x], 
                                               by = food_information$step_size[food_information$name == x])))
                             })
names(all_options_group1) <- group1_foods

all_options_group1 <- all_options_group1[order(sapply(all_options_group1, length))]
all_combindations_group1 <- expand.grid(all_options_group1[-c((length(all_options_group1) - 2):length(all_options_group1))])

### start filtering out unnecessary groupings -- we want no more than 5 (max_foods) foods in a meal
all_combindations_group1 <- all_combindations_group1[rowSums(all_combindations_group1 != 0) <= max_foods,] 

all_combindations_group1[,rev(names(all_options_group1))[3]] <- 0
all_combindations_group1[,rev(names(all_options_group1))[2]] <- 0
all_combindations_group1[,rev(names(all_options_group1))[1]] <- 0
all_combindations_group1 <- all_combindations_group1[,group1_foods]

food_information_group1 <- subset(food_information, group == 1)
food_information_group1 <- food_information_group1[order(food_information_group1$name),]

oa <- NULL
for (x1 in all_options_group1[[length(all_options_group1) - 2]]){
  print(x1)
  for (x2 in all_options_group1[[length(all_options_group1) - 1]]){
    print(x2)
    for (x3 in all_options_group1[[length(all_options_group1)]]){
      
      all_combindations_group1[,rev(names(all_options_group1))[3]] <- x1
      all_combindations_group1[,rev(names(all_options_group1))[2]] <- x2
      all_combindations_group1[,rev(names(all_options_group1))[1]] <- x3
      
      nfoods <- rowSums(all_combindations_group1 != 0)
      all_combindations_current <- all_combindations_group1[min_foods <= nfoods & nfoods <= max_foods, ]
      
      energy <- rowSums(all_combindations_current)
      all_combindations_current     <- subset(all_combindations_current, abs(energy - total_energy) <= tolerance)
      all_combindations_current_mat <- as.matrix(all_combindations_current)
      
      for(characteristic in character_vec){
        all_combindations_current[,characteristic] <- all_combindations_current_mat %*% food_information_group1[,characteristic]
      }
      
      oa <- rbind(oa, all_combindations_current)
    }
  }
}
fwrite(oa, file = paste0("./out_group1.csv"), append = FALSE)
end <- Sys.time()
end - start
