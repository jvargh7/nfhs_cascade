nfhs5_iair <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iair_women.RDS"))
nfhs5_iamr <- readRDS(paste0(path_cascade_folder,"/working/nfhs5 iamr_men.RDS"))

with(nfhs5_iair,table(current_dm,toldhigh_dm))
with(nfhs5_iamr,table(current_dm,toldhigh_dm))
