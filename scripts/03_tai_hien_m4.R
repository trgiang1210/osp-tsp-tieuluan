# Chay lai thuc nghiem M4 (Bang 3 va Bang 5) cho cac bo Daily, Yearly, Quarterly, Monthly
# Bo nao nang qua thi lay mau ngau nhien mot so chuoi

# duong dan, sua lai neu doi may
THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))
library(M4comp2018)
M4_DAY_DU <- M4

# chay 6 file cua tac gia cho mot bo du lieu
# bo: ten bo (Daily, Yearly...), so_mau: NULL = chay het
chay_bo <- function(bo, file_tong_hop, so_mau = NULL) {
  ma_goc  <- file.path(THU_MUC_TAC_GIA, "OSP-TSP on M4",
                       "Time series 5 equal parts (m=5)", bo)
  ket_qua <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo)))
  dir.create(ket_qua, recursive = TRUE, showWarnings = FALSE)

  # lay mau: thay bien M4 bang danh sach chi gom cac chuoi duoc chon
  # code goc loc bang Filter(... M4) nen se chi thay cac chuoi nay
  chuoi <- Filter(function(l) l$period == bo, M4_DAY_DU)
  if (!is.null(so_mau) && so_mau < length(chuoi)) {
    set.seed(2026)
    chuoi <- chuoi[sort(sample(length(chuoi), so_mau))]
  }
  assign("M4", chuoi, envir = globalenv())
  writeLines(sapply(chuoi, function(l) l$st), file.path(ket_qua, "danh_sach_chuoi.txt"))

  cac_file <- c(paste(bo, "Feature extraction.r"),
                "starting_point_ets.r", "starting_point_thetaf.r",
                "ets_opt_forecast.r", "thetaf_opt_forecast.r",
                file_tong_hop)

  thu_muc_cu <- setwd(ket_qua)
  thoi_gian <- data.frame(buoc = cac_file, giay = NA_real_, loi = "")
  for (i in seq_along(cac_file)) {
    cat("\n==========", bo, "- đang chạy:", cac_file[i], "==========\n")
    bat_dau <- Sys.time()
    # neu loi thi ghi lai va chay tiep file sau
    # (vd. phan tinh do quan trong dac trung o cuoi file *_opt_forecast.r
    #  bi loi khi cac mo hinh dung so dac trung khac nhau, ket qua chinh da luu truoc do)
    tryCatch(source(file.path(ma_goc, cac_file[i]), local = new.env()),
             error = function(e) {
               thoi_gian$loi[i] <<- conditionMessage(e)
               cat("LOI:", conditionMessage(e), "\n")
             })
    thoi_gian$giay[i] <- round(as.numeric(difftime(Sys.time(), bat_dau,
                                                   units = "secs")), 1)
    cat("Xong sau", thoi_gian$giay[i], "giây\n")
  }
  write.csv(thoi_gian, "thoi_gian_chay.csv", row.names = FALSE)
  writeLines(capture.output(sessionInfo()), "phien_ban_thu_vien.txt")
  setwd(thu_muc_cu)
  rm("M4", envir = globalenv())
  cat("\nXong", bo, "- so chuoi:", length(chuoi), "- ket qua trong:", ket_qua, "\n")
}

# chay tung bo, bo dau # o dong nao muon chay
# chay_bo("Daily",     "daily_ETS_thetaf_final_result.r")                  # ~2 gio
# chay_bo("Yearly",    "Yearly_ETS_thetaf_final_result.r")                 # ~2,5 gio
# chay_bo("Quarterly", "Quarterly_ETS_thetaf_final_result.r", so_mau = 1000) # ~45 phut
# chay_bo("Monthly",   "Monthly_ETS_thetaf_final_result.r",   so_mau = 1000) # ~3,5 gio
# chay_bo("Hourly", "Hourly_ETS_thetaf_final_result.r", so_mau = 200)
