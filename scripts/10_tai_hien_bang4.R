# Chay lai thuc nghiem Bang 4 cua bai bao: M4 Yearly, Quarterly, Monthly voi (m, n) = (10, 4)
# Code tac gia nam trong thu muc "Time series 10 equal parts (m=10)", giong het ban m=5
# ngoai tham so m. Dung cung cach lay mau voi Bang 3 (seed 2026) de hai bang so sanh duoc.
#
# Cach chay: mo R, source file nay, roi bo dau # o dong chay_bo_m10(...) muon chay.
# Co the mo 2 cua so R va chay song song 2 bo khac nhau (vd. Yearly o mot cua so,
# Quarterly roi Monthly o cua so kia) neu may co tu 4 nhan tro len.

THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))
library(M4comp2018)
M4_DAY_DU <- M4

chay_bo_m10 <- function(bo, file_tong_hop, so_mau = NULL) {
  ma_goc  <- file.path(THU_MUC_TAC_GIA, "OSP-TSP on M4",
                       "Time series 10 equal parts (m=10)", bo)
  ket_qua <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo), "_m10"))
  dir.create(ket_qua, recursive = TRUE, showWarnings = FALSE)

  # lay mau giong het Bang 3: cung seed, cung cach chon
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
    cat("\n==========", bo, "(m=10) - đang chạy:", cac_file[i], "==========\n")
    bat_dau <- Sys.time()
    tryCatch(source(file.path(ma_goc, cac_file[i]), local = new.env()),
             error = function(e) {
               thoi_gian$loi[i] <<- conditionMessage(e)
               cat("LOI:", conditionMessage(e), "\n")
             })
    thoi_gian$giay[i] <- round(as.numeric(difftime(Sys.time(), bat_dau,
                                                   units = "secs")), 1)
    cat("Xong sau", thoi_gian$giay[i], "giây\n")
    # ghi thoi gian sau moi buoc, neu may tat giua chung van biet da xong toi dau
    write.csv(thoi_gian, "thoi_gian_chay.csv", row.names = FALSE)
  }
  writeLines(capture.output(sessionInfo()), "phien_ban_thu_vien.txt")
  setwd(thu_muc_cu)
  rm("M4", envir = globalenv())
  cat("\nXong", bo, "(m=10) - so chuoi:", length(chuoi), "- ket qua trong:", ket_qua, "\n")
}

# Thoi gian uoc tinh (khoang gap doi Bang 3 vi moi chuoi du bao 40 chuoi con thay vi 20):
# chay_bo_m10("Yearly",    "Yearly_ETS_thetaf_final_result.r")                   # ~6 gio
# chay_bo_m10("Quarterly", "Quarterly_ETS_thetaf_final_result.r", so_mau = 1000) # ~2 gio
# chay_bo_m10("Monthly",   "Monthly_ETS_thetaf_final_result.r",   so_mau = 1000) # ~4,5 gio
