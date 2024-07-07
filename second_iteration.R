clean <- function(all_options, details, characteristics){
  all_combindations <- expand.grid(all_options)
  details <- subset(details, name %in% names(all_options))
  all_combindations <- all_combindations[,details$name]
  
  energy <- rowSums(all_combindations)
  all_combindations1 <- all_combindations[275 <= energy & energy <= 325,]
  
  all_combindations1_mat <- as.matrix(all_combindations1)
  for (char in characteristics){
    all_combindations1[, char] <- all_combindations1_mat %*% details[,char]
  }
  return(all_combindations1)
}


combine <- function(group0_combos, group1_combos, characteristics){
  
  if (nrow(group0_combos) == 0 | nrow(group1_combos) == 0) return(NULL)
  
  group1_info_mat <- as.matrix(group1_combos[,paste0(characteristics, "_group1")])
  group0_info_mat     <- as.matrix(group0_combos[, paste0(characteristics, "_group0")])
  
  save_dat <- NULL
  for (i in 1:nrow(group0_combos)){
    per_diff <- rowSums(abs(group0_info_mat[rep(i, nrow(group1_combos)),] - group1_info_mat) / group1_info_mat)
    save_dat <- rbind(save_dat, 
                      cbind(as.matrix(group0_combos[rep(i, nrow(group1_combos)),]), 
                            as.matrix(group1_combos), per_diff))
  }
  as.data.frame(save_dat)
}


library(data.table)
library(dplyr)
options(echo=TRUE) # if you want see commands in output file
args <- commandArgs(trailingOnly = TRUE)
ij   <- 1#args[1]
tmp  <- read.csv("./food_and_group_information.csv")
matches <- read.csv("./matches_outfiles.csv")

character_vec <- c("fat",	"cho", "protein",	"sfa",	"dfib",	"glyc_load", "glyc_index")

nbatch <- 100
nstart <- (ij - 1) * nbatch + 1

start2 <- Sys.time()

colstokeep <- names(matches)[!grepl("_group\\d", names(matches)) &
                                !grepl("per_diff|Energy", names(matches))]

save_dat <- NULL
for (j in nstart:nbatch){
  print(j)
  cd <- as.list(zapsmall(matches[j, colstokeep]))
  names(cd) <- gsub("\\.", " ", names(cd))
  cd <- cd[cd != 0]
  
  group0 <- names(cd)[names(cd) %in% tmp$name[tmp$group == 0]]
  coptions_group0 <- lapply(group0, function(x){
    seq(cd[[x]] - tmp$step_size[tmp$name == x] / 2, cd[[x]] + tmp$step_size[tmp$name == x] / 2, tmp$step_size[tmp$name == x] / 10)
  })
  names(coptions_group0) <- group0

  group1 <- names(cd)[names(cd) %in% tmp$name[tmp$group == 1]]
  coptions_group1 <- lapply(group1, function(x){
    seq(cd[[x]] - tmp$step_size[tmp$name == x] / 2, cd[[x]] + tmp$step_size[tmp$name == x] / 2, tmp$step_size[tmp$name == x] / 10)
  })
  names(coptions_group1) <- group1

  group0_combos <- clean(coptions_group0, tmp, characteristics = character_vec)
  group1_combos <- clean(coptions_group1, tmp, characteristics = character_vec)

  names(group0_combos)[names(group0_combos) %in% character_vec] <- 
    paste0(names(group0_combos)[names(group0_combos) %in% character_vec], "_group0")
  names(group1_combos)[names(group1_combos) %in% character_vec] <- 
    paste0(names(group1_combos)[names(group1_combos) %in% character_vec], "_group1")
  
  out <- combine(group0_combos, group1_combos, characteristics = character_vec)
  
  if (is.null(out)) next
  
  names(out) <- c(names(group0_combos), 
                  names(group1_combos), "per_diff")
  out <- out[order(out$per_diff),]
  
  save_dat <- bind_rows(save_dat, out[1:min(100, nrow(out)),])
}


name_order <- c(tmp$name[tmp$group == 0],
                names(save_dat)[grep("_upf", names(save_dat))],
                tmp$name[tmp$group == 1],
                names(save_dat)[grep("_mpf", names(save_dat))],
                "per_diff") [ c(tmp$name[tmp$group == 0],
                                names(save_dat)[grep("_upf", names(save_dat))],
                                tmp$name[tmp$group == 1],
                                names(save_dat)[grep("_mpf", names(save_dat))],
                                "per_diff") %in% names(save_dat)]
save_dat <- save_dat[,name_order]
if (!is.null(save_dat)){
  write.table(save_dat[order(save_dat$per_diff),],
              paste0("./matches_second_iter_outfiles_", ij,".csv"),
              append = FALSE, sep = ",", row.names = FALSE)
}
end2 <- Sys.time()

print(end2 - start2)