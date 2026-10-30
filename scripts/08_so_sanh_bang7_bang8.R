# So sanh ket qua 5 bo du lieu thuc voi Bang 7, Bang 8 va Bang 9
# file ket qua cua tac gia: moi mo hinh 5 dong (Total series, xgbcls, xgbreg, lgbcls, lgbreg), V5 = MAPE, V6 = MASE
# Bang 8, Bang 9: 10 dong (ETS, thetaf). Bang 7: 20 dong (ETS, thetaf, ARIMA, nnetar)

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

phuong_phap <- c("Total series", "xgbcls", "xgbreg", "lgbcls", "lgbreg")

so_sanh <- function(bai_bao, tep, thu_muc, cac_mo_hinh) {
  tat_ca <- list()
  for (bo in names(tep)) {
    duong_dan <- file.path(THU_MUC_KHO, "results", thu_muc, tep[[bo]])
    if (!file.exists(duong_dan)) { cat("Chua co ket qua:", duong_dan, "\n"); next }
    x <- read.csv(duong_dan)
    for (k in seq_along(cac_mo_hinh)) {
      mh <- cac_mo_hinh[k]
      dong <- (k - 1) * 5 + 1:5
      b <- data.frame(
        bo = bo, mo_hinh = mh, phuong_phap = phuong_phap,
        MAPE_bai_bao  = bai_bao[[bo]][[mh]]$MAPE,
        MAPE_tai_hien = round(x$V5[dong], 2),
        MASE_bai_bao  = bai_bao[[bo]][[mh]]$MASE,
        MASE_tai_hien = round(x$V6[dong], 2)
      )
      b$chenh_MASE <- round(b$MASE_tai_hien - b$MASE_bai_bao, 2)
      b$tot_hon_total <- b$MASE_tai_hien < b$MASE_tai_hien[1]
      b$tot_hon_total[1] <- NA
      tat_ca[[paste(bo, mh)]] <- b
    }
  }
  do.call(rbind, tat_ca)
}

# ---------------- Bang 8 (mo hinh hoc tren M4) ----------------

tep_bang8 <- c(GDP = "GDP_M4.csv", Industry = "china_industry_M4.csv",
               ExchangeRate = "exchange_rate_M4_12.csv",
               Confidence = "Confidence_index_M4_12.csv", Imports = "Imports_M4_12.csv")

# thu tu Total series, xgbcls, xgbreg, lgbcls, lgbreg
bang8 <- list(
  GDP = list(
    ETS    = list(MAPE = c(16.74, 14.85, 15.09, 15.41, 15.11), MASE = c(5.13, 2.61, 2.32, 2.72, 2.36)),
    thetaf = list(MAPE = c(14.20, 12.40, 12.54, 13.29, 12.88), MASE = c(4.99, 2.23, 2.06, 2.36, 2.13))),
  Industry = list(
    ETS    = list(MAPE = c(11.21, 9.45, 9.00, 9.22, 9.16),     MASE = c(1.42, 0.94, 0.91, 0.91, 0.94)),
    thetaf = list(MAPE = c(14.89, 11.83, 11.92, 12.24, 11.99), MASE = c(1.70, 1.14, 1.16, 1.16, 1.17))),
  ExchangeRate = list(
    ETS    = list(MAPE = c(11.92, 11.14, 10.71, 12.47, 16.31), MASE = c(1.74, 1.68, 1.55, 1.93, 2.23)),
    thetaf = list(MAPE = c(8.99, 9.34, 9.35, 9.30, 9.29),      MASE = c(1.35, 1.46, 1.26, 1.46, 1.25))),
  Confidence = list(
    ETS    = list(MAPE = c(2.00, 2.05, 2.06, 2.11, 2.10),      MASE = c(1.57, 1.52, 1.50, 1.54, 1.58)),
    thetaf = list(MAPE = c(1.56, 1.53, 1.54, 1.65, 1.58),      MASE = c(1.19, 1.07, 1.17, 1.19, 1.20))),
  Imports = list(
    ETS    = list(MAPE = c(27.13, 28.27, 28.60, 27.50, 27.58), MASE = c(1.60, 1.10, 1.02, 1.13, 1.04)),
    thetaf = list(MAPE = c(26.89, 28.24, 28.26, 30.22, 28.92), MASE = c(1.63, 1.13, 1.08, 1.19, 1.12)))
)

kq8 <- so_sanh(bang8, tep_bang8, "bang8", c("ETS", "thetaf"))
if (!is.null(kq8)) {
  print(kq8, row.names = FALSE)
  write.csv(kq8, file.path(THU_MUC_KHO, "results", "so_sanh_bang8.csv"), row.names = FALSE)
}

# ---------------- Bang 7 (tu huan luyen tren 70% chuoi) ----------------

# ten file do code tac gia dat (Imports, exchange_rate dung chu "gratis" nhung la file Train_by_oneself)
tep_bang7 <- c(GDP = "GDP_by_oneself_12.csv", Industry = "industry_by_oneself.csv",
               ExchangeRate = "exchange_rate_gratis_12.csv",
               Confidence = "Confidence_index_by_oneself_12.csv", Imports = "Imports_gratis_12.csv")

# thu tu Total series, xgbcls, xgbreg, lgbcls, lgbreg
# Bang 7 in nhom thetaf theo thu tu xgbreg, lgbcls, lgbreg, xgbcls; o day da sap lai theo ten
bang7 <- list(
  GDP = list(
    ETS    = list(MAPE = c(13.97, 13.89, 13.66, 13.72, 13.55), MASE = c(4.57, 3.48, 4.06, 3.53, 3.96)),
    thetaf = list(MAPE = c(12.99, 12.23, 12.58, 13.01, 12.48), MASE = c(4.73, 3.34, 3.64, 3.46, 3.67)),
    ARIMA  = list(MAPE = c(12.09, 13.19, 14.22, 13.51, 16.49), MASE = c(3.93, 3.28, 3.68, 3.30, 4.55)),
    nnetar = list(MAPE = c(15.22, 14.92, 14.10, 14.34, 14.73), MASE = c(5.11, 3.84, 3.99, 3.69, 4.17))),
  Industry = list(
    ETS    = list(MAPE = c(11.21, 9.00, 9.90, 10.18, 10.18),   MASE = c(1.42, 1.08, 1.12, 1.18, 1.18)),
    thetaf = list(MAPE = c(14.89, 12.17, 12.26, 12.12, 12.12), MASE = c(1.70, 1.41, 1.40, 1.40, 1.40)),
    ARIMA  = list(MAPE = c(18.96, 19.33, 19.25, 18.39, 19.22), MASE = c(1.73, 1.51, 1.40, 1.40, 1.34)),
    nnetar = list(MAPE = c(26.23, 22.18, 27.94, 28.31, 27.22), MASE = c(2.59, 1.82, 2.47, 2.22, 2.64))),
  ExchangeRate = list(
    ETS    = list(MAPE = c(10.93, 10.71, 9.36, 10.74, 10.74),  MASE = c(1.43, 1.57, 1.23, 1.55, 1.41)),
    thetaf = list(MAPE = c(7.29, 8.25, 8.25, 7.48, 7.98),      MASE = c(0.95, 1.19, 1.15, 1.08, 1.16)),
    ARIMA  = list(MAPE = c(9.09, 10.60, 10.66, 9.98, 9.98),    MASE = c(1.20, 1.55, 1.46, 1.50, 1.33)),
    nnetar = list(MAPE = c(9.57, 9.03, 8.40, 9.23, 8.89),      MASE = c(1.31, 1.44, 1.29, 1.51, 1.46))),
  Confidence = list(
    ETS    = list(MAPE = c(1.72, 2.08, 2.13, 2.24, 2.18),      MASE = c(1.48, 1.56, 1.29, 1.83, 1.18)),
    thetaf = list(MAPE = c(1.14, 1.14, 1.24, 1.42, 1.42),      MASE = c(1.01, 0.74, 0.68, 1.13, 0.76)),
    ARIMA  = list(MAPE = c(1.21, 1.91, 1.95, 1.78, 1.78),      MASE = c(1.08, 1.52, 1.07, 1.48, 0.98)),
    nnetar = list(MAPE = c(0.98, 1.89, 1.24, 1.03, 0.82),      MASE = c(0.84, 1.18, 0.65, 0.68, 0.42))),
  Imports = list(
    ETS    = list(MAPE = c(24.37, 22.39, 26.04, 26.06, 26.09), MASE = c(1.61, 1.59, 1.90, 1.91, 1.90)),
    thetaf = list(MAPE = c(20.65, 23.05, 23.10, 25.16, 25.16), MASE = c(1.52, 1.65, 1.66, 1.79, 1.79)),
    ARIMA  = list(MAPE = c(29.10, 23.58, 22.05, 23.53, 21.94), MASE = c(1.97, 0.95, 0.87, 1.08, 0.86)),
    nnetar = list(MAPE = c(42.29, 36.16, 30.21, 29.13, 36.88), MASE = c(3.03, 1.41, 1.24, 1.37, 1.24)))
)

kq7 <- so_sanh(bang7, tep_bang7, "bang7", c("ETS", "thetaf", "ARIMA", "nnetar"))
if (!is.null(kq7)) {
  print(kq7, row.names = FALSE)
  write.csv(kq7, file.path(THU_MUC_KHO, "results", "so_sanh_bang7.csv"), row.names = FALSE)
}

# ---------------- Bang 9 (hoc tren chuoi mo phong GRATIS) ----------------

tep_bang9 <- c(GDP = "GDP_gratis_12.csv", Industry = "industry_index_gratis_12.csv",
               ExchangeRate = "exchange_rate_gratis_12.csv",
               Confidence = "Confidence_index_gratis_12.csv", Imports = "Imports_gratis_12.csv")

# thu tu Total series, xgbcls, xgbreg, lgbcls, lgbreg
bang9 <- list(
  GDP = list(
    ETS    = list(MAPE = c(16.74, 15.14, 14.95, 14.96, 14.67), MASE = c(5.13, 3.03, 2.67, 2.96, 2.60)),
    thetaf = list(MAPE = c(14.20, 13.96, 13.17, 13.45, 12.77), MASE = c(4.99, 2.87, 2.48, 2.81, 2.33))),
  Industry = list(
    ETS    = list(MAPE = c(11.21, 9.65, 10.91, 10.49, 9.54),   MASE = c(1.42, 0.99, 1.02, 1.11, 1.05)),
    thetaf = list(MAPE = c(14.89, 12.03, 12.25, 12.15, 12.25), MASE = c(1.70, 1.12, 1.20, 1.17, 1.21))),
  ExchangeRate = list(
    ETS    = list(MAPE = c(11.92, 11.62, 10.93, 11.62, 11.77), MASE = c(1.74, 1.82, 1.61, 1.72, 1.72)),
    thetaf = list(MAPE = c(8.99, 10.09, 9.08, 9.28, 9.22),     MASE = c(1.35, 1.48, 1.38, 1.35, 1.39))),
  Confidence = list(
    ETS    = list(MAPE = c(2.00, 2.11, 2.04, 2.12, 2.08),      MASE = c(1.57, 1.58, 1.47, 1.54, 1.48)),
    thetaf = list(MAPE = c(1.56, 1.72, 1.54, 1.61, 1.58),      MASE = c(1.19, 1.21, 1.01, 1.14, 0.97))),
  Imports = list(
    ETS    = list(MAPE = c(27.13, 26.05, 27.70, 26.10, 26.39), MASE = c(1.60, 1.18, 1.19, 1.20, 1.16)),
    thetaf = list(MAPE = c(26.89, 27.62, 27.56, 27.86, 27.53), MASE = c(1.63, 1.16, 1.10, 1.16, 1.08)))
)

kq9 <- so_sanh(bang9, tep_bang9, "bang9", c("ETS", "thetaf"))
if (!is.null(kq9)) {
  print(kq9, row.names = FALSE)
  write.csv(kq9, file.path(THU_MUC_KHO, "results", "so_sanh_bang9.csv"), row.names = FALSE)
}
