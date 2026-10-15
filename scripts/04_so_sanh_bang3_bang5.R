# So sanh ket qua Daily (Bang 5) va Yearly, Quarterly, Monthly (Bang 3, m = 5)
# V5 = MAPE, V6 = MASE
# dong 1 = Total series, dong 2-5 = nhan min (Actual), dong 6-9 = nhan mean (Average)
# thu tu trong moi nhom: xgbcls, xgbreg, lgbcls, lgbreg

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

pp <- c("xgbcls", "xgbreg", "lgbcls", "lgbreg")

# so lieu bai bao: moi bo, moi mo hinh nen -> MAPE va MASE theo thu tu
# Actual (4 dong), Average (4 dong), Total series (1 dong)
bai_bao <- list(
  Yearly = list(
    ETS    = list(MAPE = c(17.70, 17.08, 17.62, 16.86,  16.49, 16.65, 16.39, 16.58,  18.06),
                  MASE = c( 2.67,  2.66,  2.61,  2.68,   2.52,  2.67,  2.51,  2.71,   3.44)),
    thetaf = list(MAPE = c(17.20, 16.95, 17.08, 17.01,  17.20, 16.87, 16.98, 16.93,  17.06),
                  MASE = c( 2.48,  2.51,  2.48,  2.52,   2.42,  2.52,  2.47,  2.52,   3.40))),
  Quarterly = list(
    ETS    = list(MAPE = c(11.95, 11.95, 12.06, 11.88,  11.75, 11.83, 11.83, 11.87,  12.20),
                  MASE = c( 0.99,  0.98,  0.99,  0.98,   0.95,  0.98,  0.94,  0.98,   1.15)),
    thetaf = list(MAPE = c(12.20, 11.94, 12.27, 11.98,  11.89, 11.85, 11.92, 11.87,  11.82),
                  MASE = c( 1.04,  1.00,  1.06,  1.00,   0.98,  1.00,  0.98,  1.00,   1.22))),
  Monthly = list(
    ETS    = list(MAPE = c(16.69, 16.66, 16.67, 16.72,  16.49, 16.72, 16.52, 16.69,  16.83),
                  MASE = c( 0.87,  0.87,  0.87,  0.87,   0.83,  0.87,  0.83,  0.87,   0.95)),
    thetaf = list(MAPE = c(15.76, 15.70, 15.81, 15.72,  15.49, 15.53, 15.50, 15.60,  15.68),
                  MASE = c( 0.89,  0.88,  0.89,  0.88,   0.85,  0.87,  0.84,  0.88,   0.97))),
  # Bang 5 chi co nhom Average, dien NA cho Actual
  Daily = list(
    ETS    = list(MAPE = c(NA, NA, NA, NA,  4.90, 5.11, 4.95, 5.08,  4.81),
                  MASE = c(NA, NA, NA, NA,  2.71, 2.77, 2.69, 2.77,  3.28)),
    thetaf = list(MAPE = c(NA, NA, NA, NA,  4.97, 5.35, 5.12, 5.42,  5.58),
                  MASE = c(NA, NA, NA, NA,  2.70, 2.77, 2.72, 2.79,  3.25))),

  Hourly = list(
    ETS    = list(MAPE = c(NA, NA, NA, NA,  22.15, 21.65, 21.68, 22.09,  23.15),
                  MASE = c(NA, NA, NA, NA,   1.79,  1.61,  1.72,  1.71,   1.99)),
    thetaf = list(MAPE = c(NA, NA, NA, NA,  25.04, 24.52, 24.34, 24.50,  23.57),
                  MASE = c(NA, NA, NA, NA,   1.63,  1.62,  1.61,  1.64,   2.60)))
)

dong   <- c(2:5, 6:9, 1)
nhom   <- c(rep("Actual", 4), rep("Average", 4), "Total series")
ten_pp <- c(pp, pp, "Total series")

tat_ca <- list()
for (bo in names(bai_bao)) {
  thu_muc <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo)))
  for (mh in c("ETS", "thetaf")) {
    tep <- file.path(thu_muc, paste0(tolower(mh), "_", bo, "_final_res.csv"))
    if (!file.exists(tep)) { cat("Chua co ket qua:", tep, "\n"); next }
    x <- read.csv(tep)
    so_chuoi <- length(readLines(file.path(thu_muc, "danh_sach_chuoi.txt")))
    b <- data.frame(
      bo = bo, so_chuoi = so_chuoi, mo_hinh = mh, nhom = nhom, phuong_phap = ten_pp,
      MAPE_bai_bao  = bai_bao[[bo]][[mh]]$MAPE,
      MAPE_tai_hien = round(x$V5[dong], 2),
      MASE_bai_bao  = bai_bao[[bo]][[mh]]$MASE,
      MASE_tai_hien = round(x$V6[dong], 2)
    )
    b$chenh_MASE <- round(b$MASE_tai_hien - b$MASE_bai_bao, 2)
    # OSP co tot hon Total series tren chinh ket qua cua minh khong
    b$tot_hon_total <- b$MASE_tai_hien < b$MASE_tai_hien[9]
    b$tot_hon_total[9] <- NA
    tat_ca[[paste(bo, mh)]] <- b
  }
}

so_sanh <- do.call(rbind, tat_ca)
print(so_sanh, row.names = FALSE)
write.csv(so_sanh, file.path(THU_MUC_KHO, "results", "so_sanh_bang3_bang5.csv"),
          row.names = FALSE)
