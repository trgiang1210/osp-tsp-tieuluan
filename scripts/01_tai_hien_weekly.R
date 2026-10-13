# Chay lai thuc nghiem M4 Weekly bang code cua tac gia (Bang 5)

# duong dan
THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

MA_GOC  <- file.path(THU_MUC_TAC_GIA, "OSP-TSP on M4",
                     "Time series 5 equal parts (m=5)", "Weekly")
KET_QUA <- file.path(THU_MUC_KHO, "results", "tai_hien_weekly")
dir.create(KET_QUA, recursive = TRUE, showWarnings = FALSE)

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))

# code goc ghi file vao thu muc hien htai nen chuyen sang thu muc ket qua
setwd(KET_QUA)

# 6 file chay theo thu tu
cac_buoc <- c(
  "Weekly Feature extraction.r",      # trich dac trung
  "starting_point_ets.r",             # cat chuoi, du bao ETS
  "starting_point_thetaf.r",          # cat chuoi, du bao thetaf
  "ets_opt_forecast.r",               # xgboost/lightgbm cho ETS
  "thetaf_opt_forecast.r",            # xgboost/lightgbm cho thetaf
  "week_ETS_thetaf_final_result.r"    # tong hop ket qua
)

thoi_gian <- data.frame(buoc = cac_buoc, giay = NA_real_)
for (i in seq_along(cac_buoc)) {
  cat("\n========== Đang chạy:", cac_buoc[i], "==========\n")
  bat_dau <- Sys.time()
  source(file.path(MA_GOC, cac_buoc[i]), local = new.env())
  thoi_gian$giay[i] <- round(as.numeric(difftime(Sys.time(), bat_dau,
                                                 units = "secs")), 1)
  cat("Xong sau", thoi_gian$giay[i], "giây\n")
}

write.csv(thoi_gian, "thoi_gian_chay.csv", row.names = FALSE)
writeLines(capture.output(sessionInfo()), "phien_ban_thu_vien.txt")

cat("\nHoàn tất. Kết quả nằm trong:", KET_QUA, "\n")
