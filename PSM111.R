# This code take as input the datasets generated in SAS, performs the perimeters computation, and exports the triplets matched

library(readxl)
library(dplyr)
library(openxlsx)

for(d in 1:1500){
  
  pz_centerA_cs <- read_excel(paste0("<enter your path here>/pz_centerA_cs_simul",d,".xlsx"))
  pz_centerB_cs <- read_excel(paste0("<enter your path here>/pz_centerB_cs_simul",d,".xlsx"))
  pz_centerC_cs <- read_excel(paste0("<enter your path here>/pz_centerC_cs_simul",d,".xlsx"))
  
  pz_centerA_cs_copy <- data.frame(pz_centerA_cs)
  n_a <- nrow(pz_centerA_cs_copy)
  pz_centerB_cs_copy <- data.frame(pz_centerB_cs)
  pz_centerC_cs_copy <- data.frame(pz_centerC_cs)
  
  # all (available) perimeters computation
  perim <- data.frame()
  # caliper width
  caliper <- 0.09
  # perimeters of the triplets chosen  
  simul_perim <- data.frame()
  # controls B selected
  id_b_select <- data.frame()
  # controls C selected
  id_c_select <- data.frame()
  for(i in 1:n_a){
    a <- pz_centerA_cs_copy[pz_centerA_cs_copy$tempID_a==i,]
    n_b <- nrow(pz_centerB_cs_copy)
    for(j in 1:n_b){
      b <- pz_centerB_cs_copy[pz_centerB_cs_copy$tempID_B==j,]
      n_c <- nrow(pz_centerC_cs_copy)
      for(k in 1:n_c){
        c <- pz_centerC_cs_copy[pz_centerC_cs_copy$tempID_c==k,]
        abc <- cbind(a,b,c)
        # perimeter computation
        abc$l1 <- sqrt((abc$x_b-abc$x_a)^2+(abc$y_b-abc$y_a)^2)
        abc$l2 <- sqrt((abc$x_c-abc$x_b)^2+(abc$y_c-abc$y_b)^2)
        abc$l3 <- sqrt((abc$x_a-abc$x_c)^2+(abc$y_a-abc$y_c)^2)
        abc$perim <- abc$l1+abc$l2+abc$l3
        abc <- abc[abc$perim <= caliper, ]
        abc2 <- abc[, c("id_a", "id_b","id_c","perim")]
        perim <- rbind(perim,abc2)
      }
    }
    perim_order <- perim[order(perim[, "perim"]), ]
    perim_small <- perim_order[1, ]
    simul_perim <- rbind(simul_perim,perim_small)
    perim <- data.frame()
    perim_small_b <- perim_small[,"id_b"]
    id_b_select <- rbind(id_b_select,perim_small_b)
    colnames(id_b_select) <- "id_b"
    pz_centerB_cs_copy <- anti_join(pz_centerB_cs_copy, id_b_select, by = "id_b")
    pz_centerB_cs_copy <- mutate(pz_centerB_cs_copy,tempID_B=row_number())
    perim_small_c <- perim_small[,"id_c"]
    id_c_select <- rbind(id_c_select,perim_small_c)
    colnames(id_c_select) <- "id_c"
    pz_centerC_cs_copy <- anti_join(pz_centerC_cs_copy, id_c_select, by = "id_c")
    pz_centerC_cs_copy <- mutate(pz_centerC_cs_copy,tempID_c=row_number())
  }
  
  write.xlsx(na.omit(simul_perim), file = paste0("<enter your path here>/match111_simul",d,".xlsx"), rowNames = F)
  
}