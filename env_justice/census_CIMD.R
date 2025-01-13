require(openxlsx)


can_cimd = read.xlsx("./data/can_scores_quintiles_EN.xlsx")
colnames(can_cimd)[1:3] = c("DAUID","Province","DA_pop_density")

at_cimd = read.xlsx("./data/atl_scores_quintiles_EN.xlsx")
on_cimd = read.xlsx("./data/on_scores_quintiles_EN.xlsx")
qc_cimd = read.xlsx("./data/qc_scores_quintiles_EN.xlsx")
colnames(at_cimd)[1:3] = c("DAUID","Province","DA_pop_density")
colnames(on_cimd)[1:3] = c("DAUID","Province","DA_pop_density")
colnames(qc_cimd)[1:3] = c("DAUID","Province","DA_pop_density")


prov_cimd = rbind(at_cimd, on_cimd, qc_cimd)

colnames(can_cimd)[4:length(can_cimd)] = paste0("can_", colnames(can_cimd)[4:length(can_cimd)])
colnames(prov_cimd)[4:length(prov_cimd)] = paste0("prov_", colnames(prov_cimd)[4:length(prov_cimd)])

