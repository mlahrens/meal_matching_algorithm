library(data.table)
library(tidyverse)

start1 <- Sys.time()

character_vec <- c("fat",	"cho", "protein",	"sfa",	"dfib",	"glyc_load", "glyc_index")

combos_group0 <- fread("../out_group0.csv")
combos_group1 <- fread("../out_group1.csv")

group1_info_mat <- as.matrix(combos_group1 %>% select(all_of(character_vec)))
group0_info_mat <- as.matrix(combos_group0 %>% select(all_of(character_vec)))

names(combos_group0)[names(combos_group0) %in% character_vec] <- 
  paste0(names(combos_group0)[names(combos_group0) %in% character_vec], "_group0")

names(combos_group1)[names(combos_group1) %in% character_vec] <- 
  paste0(names(combos_group1)[names(combos_group1) %in% character_vec], "_group1")

save_dat <- NULL
### loop through every group 0 combination and compare each one to all the group 1 combinations
for (i in 1:nrow(combos_group0)){
  if (i %% 1000 == 1) print(i)
  per_diff <- rowSums(abs(group0_info_mat[rep(i, nrow(combos_group1)),] - group1_info_mat) / group1_info_mat)
  # keep top 100 matches
  rows_to_keep  <- order(per_diff)[1:100]
  save_dat <- rbind(save_dat, 
                    cbind(combos_group0[rep(i, length(rows_to_keep)),], 
                          combos_group1[rows_to_keep,], 
                          per_diff = per_diff[rows_to_keep]))
}

save_dat <- data.frame(save_dat)
save_dat <- save_dat[order(save_dat$per_diff),]

if (!is.null(save_dat)){
  write.table(save_dat[1:100,], ## only save top 100
            paste0("./matches_outfiles.csv"), 
            append = FALSE, sep = ",")
}
end1 <- Sys.time()
end1 - start1
