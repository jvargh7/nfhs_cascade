op = readRDS(paste0("C:/code/external/nfhs_cascade/abstract/op.RDS"))
ip = readRDS(paste0("C:/code/external/nfhs_cascade/abstract/ip.RDS"))

op$prop_medicine - 1.96*sqrt(op$prop_medicine*(1-op$prop_medicine)/op$n)
op$prop_medicine + 1.96*sqrt(op$prop_medicine*(1-op$prop_medicine)/op$n)

ip$prop_medicine - 1.96*sqrt(ip$prop_medicine*(1-ip$prop_medicine)/ip$n)
ip$prop_medicine + 1.96*sqrt(ip$prop_medicine*(1-ip$prop_medicine)/ip$n)


p1 = (op$total_all)/115224
p2 = ip$total_all_rs/115224

p_tot = p1+p2

se_p_tot = sqrt((p1*(1-p1)/op$n) + (p2*(1-p2)/ip$n))

p_tot - 1.96*se_p_tot
p_tot + 1.96*se_p_tot
