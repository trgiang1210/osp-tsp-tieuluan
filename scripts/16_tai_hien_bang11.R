# Bang 11: OSP-TSP voi mo hinh co so nnetar (mang no-ron tu hoi quy), 6 tan suat M4
# Chay lai 3 notebook cua tac gia cho moi tan suat, thu muc "Complementary experiments/nnetar/":
#   1. <bo>_nnetar.ipynb              : du bao nnetar tu 20 diem bat dau, tinh sai so -> <bo>_nnetar_datalist.RData
#   2. <bo>_nnetar_opt_forecast.ipynb : gan nhan, huan luyen XGBoost/LightGBM, doan khoang -> <bo>_nnetar_opt_pre_result.RData
#   3. <bo>_nnetar_final_result.ipynb : tinh MAPE/MASE -> nnetar_<bo>_final_res.csv
# Script doc tung o code trong file .ipynb va chay lan luot (khong chep code tac gia vao kho).
#
# Lay mau (giong cach script 03): thay bien M4 bang cac chuoi duoc chon (set.seed(2026)); file dac trung
# <bo>_features_matrix.csv cua tac gia (tinh tren toan bo chuoi) duoc cat dung cac dong tuong ung.
# Phuong an da chon: Yearly 1.000, Quarterly 1.000, Monthly 500, Daily 500; Weekly 359 va Hourly 414 chay du.
#
# Sua / bo qua trong notebook (khong doi logic tinh toan):
#   - opt_forecast: nhan as.matrix(as.factor(nhan)) -> as.numeric(nhan) (xgboost 3.x khong nhan nhan dang chu)
#   - opt_forecast (Daily, Hourly): bo qua o tinh do quan trong dac trung (Bang 6 da bo)
#   - final_result (Weekly): bo qua cac o thu nghiem dung bien ddm, dd khong ton tai (o 40-44, 49, 68, 69)
#   - xgboost(), predict() cua xgboost 3.x / lightgbm 4.x: qua compat.R
#   - o nao loi van chay tiep va ghi lai; neu thieu file ket qua cuoi thi bao loi

THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))
suppressMessages({
  library(M4comp2018); library(forecast); library(dplyr)
  library(xgboost); library(Matrix); library(Ckmeans.1d.dp); library(lightgbm); library(jsonlite)
})
M4_DAY_DU <- M4
THU_MUC_NB <- file.path(THU_MUC_TAC_GIA, "Complementary experiments", "nnetar")

# cac o bo qua (so khop bieu thuc chinh quy voi noi dung o)
BO_QUA <- c("importance",                       # do quan trong dac trung (Daily, Hourly)
            "ddm", "^dd\\s*$", "apply\\(dd,",   # o thu nghiem Weekly dung bien khong ton tai
            "^accuracy\\(fore_l,real\\)\\s*$", "^data\\[\\[index\\]\\]\\$xx\\s*$",
            "^data\\[\\[12\\]\\]\\$xx\\s*$", "^fore_l\\$mean\\s*$")

sua_o <- function(ma) gsub("as\\.matrix\\(as\\.factor\\(([A-Za-z_]+)\\)\\)", "as.numeric(\\1)", ma)

# chay tung o code cua mot notebook trong moi truong env
chay_notebook <- function(tep, env) {
  o <- fromJSON(tep, simplifyVector = FALSE)$cells
  loi <- character(0)
  for (k in seq_along(o)) {
    if (o[[k]]$cell_type != "code") next
    ma <- paste(unlist(o[[k]]$source), collapse = "")
    if (!nzchar(trimws(ma))) next
    if (any(sapply(BO_QUA, grepl, x = ma))) next
    ma <- sua_o(ma)
    tryCatch(eval(parse(text = ma), envir = env),
             error = function(e) {
               loi <<- c(loi, sprintf("o %d: %s", k - 1, conditionMessage(e)))
               cat("  [loi o", k - 1, "]", conditionMessage(e), "\n")
             })
  }
  loi
}

# bo: Yearly, Quarterly, Monthly, Weekly, Daily, Hourly; so_mau: NULL = chay du
chay_bang11 <- function(bo, so_mau = NULL) {
  ket_qua <- file.path(THU_MUC_KHO, "results", "bang11", tolower(bo))
  dir.create(ket_qua, recursive = TRUE, showWarnings = FALSE)

  chuoi <- Filter(function(l) l$period == bo, M4_DAY_DU)
  chon <- seq_along(chuoi)
  if (!is.null(so_mau) && so_mau < length(chuoi)) {
    set.seed(2026)
    chon <- sort(sample(length(chuoi), so_mau))
  }
  assign("M4", chuoi[chon], envir = globalenv())
  writeLines(sapply(chuoi[chon], function(l) l$st), file.path(ket_qua, "danh_sach_chuoi.txt"))

  # dac trung cua dung cac chuoi duoc chon (file tac gia tinh tren toan bo, cung thu tu M4)
  dac_trung <- read.csv(file.path(THU_MUC_NB, paste0(bo, "_features_matrix.csv")))
  stopifnot(nrow(dac_trung) == length(chuoi))
  write.csv(dac_trung[chon, ], file.path(ket_qua, paste0(bo, "_features_matrix.csv")), row.names = FALSE)

  buoc <- data.frame(
    notebook = paste0(bo, c("_nnetar.ipynb", "_nnetar_opt_forecast.ipynb", "_nnetar_final_result.ipynb")),
    ket_qua  = c(paste0(bo, "_nnetar_datalist.RData"), paste0(bo, "_nnetar_opt_pre_result.RData"),
                 paste0("nnetar_", bo, "_final_res.csv")),
    giay = NA_real_, loi = "")

  thu_muc_cu <- setwd(ket_qua)
  on.exit({ setwd(thu_muc_cu); if (exists("M4", envir = globalenv())) rm("M4", envir = globalenv()) })
  for (i in 1:3) {
    if (file.exists(buoc$ket_qua[i])) {        # chay lai thi bo qua buoc da xong
      cat("Da co", buoc$ket_qua[i], "-> bo qua\n"); next
    }
    cat("\n==========", bo, "(", length(chon), "chuoi ) -", buoc$notebook[i], "==========\n")
    bat_dau <- Sys.time()
    loi <- chay_notebook(file.path(THU_MUC_NB, buoc$notebook[i]), new.env(parent = globalenv()))
    buoc$giay[i] <- round(as.numeric(difftime(Sys.time(), bat_dau, units = "secs")), 1)
    buoc$loi[i] <- paste(loi, collapse = " | ")
    cat("Xong sau", buoc$giay[i], "giay\n")
    if (!file.exists(buoc$ket_qua[i])) stop("Khong tao duoc ", buoc$ket_qua[i], " - xem loi o tren")
    if (nzchar(buoc$loi[i]))
      warning(bo, " - ", buoc$notebook[i], " co o bi loi (ghi trong thoi_gian_chay.csv), kiem tra lai ket qua")
  }
  tep_tg <- "thoi_gian_chay.csv"
  write.csv(if (file.exists(tep_tg)) rbind(read.csv(tep_tg), buoc) else buoc, tep_tg, row.names = FALSE)
  invisible(buoc)
}

# ---- so sanh voi Bang 11 ----
# file final_res: dong 1 Total series, dong 10 trung binh 4 bien the nhan min (Actual mean),
# dong 11 trung binh 4 bien the nhan mean (Average mean); cot V5 = MAPE, V6 = MASE
so_sanh_bang11 <- function() {
  bai_bao <- data.frame(
    bo   = rep(c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly"), each = 3),
    dong = rep(c("Actual mean", "Average mean", "Total series"), 6),
    MAPE_bai_bao = c(20.82, 21.21, 21.61,  16.50, 15.38, 15.89,  19.01, 18.64, 19.80,
                     8.26, 8.50, 9.91,     4.82, 5.08, 5.82,     25.98, 23.82, 14.67),
    MASE_bai_bao = c(3.54, 3.57, 4.06,     1.35, 1.37, 1.57,     1.03, 1.03, 1.15,
                     2.40, 2.56, 3.86,     3.54, 3.42, 4.08,     1.03, 1.01, 1.08))
  so_dong <- c("Actual mean" = 10, "Average mean" = 11, "Total series" = 1)
  bai_bao$so_chuoi <- NA_integer_
  bai_bao$MAPE_tai_hien <- NA_real_; bai_bao$MASE_tai_hien <- NA_real_
  for (bo in unique(bai_bao$bo)) {
    thu_muc <- file.path(THU_MUC_KHO, "results", "bang11", tolower(bo))
    tep <- file.path(thu_muc, paste0("nnetar_", bo, "_final_res.csv"))
    if (!file.exists(tep)) { cat("Chua co ket qua", bo, "\n"); next }
    kq <- read.csv(tep)
    d <- bai_bao$bo == bo
    bai_bao$MAPE_tai_hien[d] <- round(kq$V5[so_dong[bai_bao$dong[d]]], 2)
    bai_bao$MASE_tai_hien[d] <- round(kq$V6[so_dong[bai_bao$dong[d]]], 2)
    bai_bao$so_chuoi[d] <- length(readLines(file.path(thu_muc, "danh_sach_chuoi.txt")))
  }
  bai_bao$chenh_MASE <- round(bai_bao$MASE_tai_hien - bai_bao$MASE_bai_bao, 2)
  tot <- function(x) ave(x, bai_bao$bo, FUN = function(v) v < v[3])
  bai_bao$tot_hon_total_bai_bao  <- as.logical(tot(bai_bao$MASE_bai_bao))
  bai_bao$tot_hon_total_tai_hien <- as.logical(tot(bai_bao$MASE_tai_hien))
  bai_bao[bai_bao$dong == "Total series", c("tot_hon_total_bai_bao", "tot_hon_total_tai_hien")] <- NA
  print(bai_bao, row.names = FALSE)
  write.csv(bai_bao, file.path(THU_MUC_KHO, "results", "so_sanh_bang11.csv"), row.names = FALSE)
  invisible(bai_bao)
}

# ---- cach chay: mo 3 terminal R, moi terminal source file nay roi chay mot nhom ----
# (thoi gian do tren may Claude: nnetar ~15 giay/chuoi voi Weekly, Hourly, Daily; <1 giay voi Y/Q/M)
# Terminal 1 (~2 gio):  chay_bang11("Hourly")
# Terminal 2 (~2 gio):  chay_bang11("Daily", so_mau = 500)
# Terminal 3 (~2 gio):
#   chay_bang11("Yearly", so_mau = 1000); chay_bang11("Quarterly", so_mau = 1000)
#   chay_bang11("Monthly", so_mau = 500); chay_bang11("Weekly")
# Xong ca 3 terminal: so_sanh_bang11()
