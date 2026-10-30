# So sanh voi Bang 12: OSP best, cpm-Thetaf, cpm-ETS
# - cpm: lay tu results/bang12/cpm_tong_hop.csv (script 12)
# - OSP best: nhu bai bao, lay gia tri nho nhat theo tung cot trong cac bien the OSP-TSP
#   cua Bang 3, Bang 5 da tai hien (khong tinh dong Total series)
#   Luu y: o Daily, bai bao lay MAPE 4,81 trung dong Total series ETS, o day khong lay dong Total
# Bai bao cat so (khong lam tron) den 2 chu so thap phan, nen dong cpm cung cat de so cho dung.

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
kq <- function(...) file.path(THU_MUC_KHO, "results", ...)

cat2 <- function(x) floor(x * 100) / 100

# so lieu Bang 12, moi tan suat: OSP best, cpm-Thetaf, cpm-ETS (MAPE, MASE)
bang12 <- data.frame(
  tan_suat = rep(c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly"), each = 3),
  dong     = rep(c("OSP best", "cpm-Thetaf", "cpm-ETS"), 6),
  MAPE_bai_bao = c(16.39, 16.90, 17.68,  11.75, 12.04, 12.03,  15.49, 15.91, 17.11,
                   9.11, 9.19, 9.34,     4.81, 5.62, 5.28,     21.65, 23.81, 29.44),
  MASE_bai_bao = c(2.42, 2.97, 2.98,     0.94, 1.18, 1.12,     0.83, 0.97, 0.95,
                   1.98, 2.67, 9.34,     2.69, 3.31, 3.31,     1.61, 2.48, 1.75)
)

# dong cpm
cpm <- read.csv(kq("bang12", "cpm_tong_hop.csv"))
cpm_dong <- rbind(
  data.frame(tan_suat = cpm$tan_suat, dong = "cpm-Thetaf", MAPE = cat2(cpm$mape_theta), MASE = cat2(cpm$mase_theta)),
  data.frame(tan_suat = cpm$tan_suat, dong = "cpm-ETS",    MAPE = cat2(cpm$mape_ets),   MASE = cat2(cpm$mase_ets)))

# dong OSP best
b35 <- read.csv(kq("so_sanh_bang3_bang5.csv"))
b35 <- b35[b35$phuong_phap != "Total series", ]
# Bang 5 cua bai bao chi co nhan trung binh (Average), nen voi Daily, Hourly chi lay nhan nay
b35 <- b35[!(b35$bo %in% c("Daily", "Hourly") & b35$nhom != "Average"), ]
osp <- aggregate(cbind(MAPE = MAPE_tai_hien, MASE = MASE_tai_hien) ~ bo, data = b35, FUN = min)
names(osp)[1] <- "tan_suat"
w <- read.csv(kq("tai_hien_weekly", "so_sanh_bang5.csv"))
w <- w[w$phuong_phap != "Total series", ]
osp <- rbind(osp, data.frame(tan_suat = "Weekly", MAPE = min(w$MAPE_tai_hien), MASE = min(w$MASE_tai_hien)))
osp$dong <- "OSP best"

tai_hien <- rbind(cpm_dong, osp[, c("tan_suat", "dong", "MAPE", "MASE")])
names(tai_hien)[3:4] <- c("MAPE_tai_hien", "MASE_tai_hien")

so_sanh <- merge(bang12, tai_hien, by = c("tan_suat", "dong"), sort = FALSE)
so_sanh$chenh_MASE <- round(so_sanh$MASE_tai_hien - so_sanh$MASE_bai_bao, 2)
so_sanh <- so_sanh[order(match(so_sanh$tan_suat, unique(bang12$tan_suat)),
                         match(so_sanh$dong, unique(bang12$dong))),
                   c("tan_suat", "dong", "MAPE_bai_bao", "MAPE_tai_hien", "MASE_bai_bao", "MASE_tai_hien", "chenh_MASE")]

# OSP best co tot hon ca hai dong cpm khong (theo MASE)
for (t in unique(so_sanh$tan_suat)) {
  x <- so_sanh[so_sanh$tan_suat == t, ]
  so_sanh$osp_tot_hon_cpm[so_sanh$tan_suat == t] <- x$MASE_tai_hien[x$dong == "OSP best"] < min(x$MASE_tai_hien[x$dong != "OSP best"])
}

print(so_sanh, row.names = FALSE)
write.csv(so_sanh, kq("so_sanh_bang12.csv"), row.names = FALSE)
