# So sanh voi Bang 10
# dong 10-16 trong file final_res: 10 allmin, 11 allmean, 12 clsmin, 13 regmin, 14 clsmean, 15 regmean, 16 random

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

# lay theo nhan mean, them cot theo min de doi chieu
phuong_phap <- c("Full model", "classification", "Regression", "Random", "Total series")
dong_mean   <- c(11, 14, 15, 16, 1)
dong_min    <- c(10, 12, 13, 16, 1)

# so lieu Bang 10
bai_bao <- list(
  Yearly = list(
    ETS    = list(MAPE = c(16.19, 16.27, 16.53, 17.11, 18.06), MASE = c(2.62, 2.51, 2.70, 3.04, 3.44)),
    thetaf = list(MAPE = c(16.82, 16.98, 16.84, 17.07, 17.06), MASE = c(2.51, 2.41, 2.53, 3.01, 3.40))),
  Quarterly = list(
    ETS    = list(MAPE = c(11.67, 11.72, 11.82, 11.98, 12.20), MASE = c(0.97, 0.94, 0.98, 1.10, 1.15)),
    thetaf = list(MAPE = c(11.79, 11.86, 11.83, 12.01, 11.82), MASE = c(0.99, 0.97, 1.01, 1.14, 1.22))),
  Monthly = list(
    ETS    = list(MAPE = c(16.46, 16.45, 16.66, 16.94, 16.83), MASE = c(0.85, 0.83, 0.87, 1.00, 0.95)),
    thetaf = list(MAPE = c(15.44, 15.46, 15.54, 16.72, 15.68), MASE = c(0.86, 0.84, 0.88, 1.04, 0.97)))
)

tat_ca <- list()
for (bo in names(bai_bao)) {
  thu_muc <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo)))
  for (mh in c("ETS", "thetaf")) {
    tep <- file.path(thu_muc, paste0(tolower(mh), "_", bo, "_final_res.csv"))
    if (!file.exists(tep)) { cat("Chua co ket qua:", tep, "\n"); next }
    x <- read.csv(tep)
    b <- data.frame(
      bo = bo, mo_hinh = mh, phuong_phap = phuong_phap,
      MAPE_bai_bao     = bai_bao[[bo]][[mh]]$MAPE,
      MAPE_tai_hien    = round(x$V5[dong_mean], 2),
      MASE_bai_bao     = bai_bao[[bo]][[mh]]$MASE,
      MASE_tai_hien    = round(x$V6[dong_mean], 2),
      MASE_theo_min    = round(x$V6[dong_min], 2)
    )
    b$chenh_MASE <- round(b$MASE_tai_hien - b$MASE_bai_bao, 2)
    b$tot_hon_total <- b$MASE_tai_hien < b$MASE_tai_hien[5]
    b$tot_hon_total[5] <- NA
    tat_ca[[paste(bo, mh)]] <- b
  }
}

so_sanh <- do.call(rbind, tat_ca)
print(so_sanh, row.names = FALSE)
write.csv(so_sanh, file.path(THU_MUC_KHO, "results", "so_sanh_bang10.csv"), row.names = FALSE)
