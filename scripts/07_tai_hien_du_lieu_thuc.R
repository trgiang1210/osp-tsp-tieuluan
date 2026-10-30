# Chay lai thuc nghiem tren 5 bo du lieu thuc (Bang 7, Bang 8, Bang 9)
# Bang 7: file *_Train_by_oneself.r, Bang 8: file *_M4.r, Bang 9: file *_gratis5.r

THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))

MA_GOC <- file.path(THU_MUC_TAC_GIA, "Empirical analysis")

cac_bo <- list(
  GDP          = list(thu_muc = "Yearly",    ten = "GDP",              du_lieu = "GDP_worldbank.csv",       m4 = "Yearly",    tien_to = "Y_"),
  Industry     = list(thu_muc = "Quarterly", ten = "industry",         du_lieu = "industry.csv",            m4 = "Quarterly", tien_to = "Q_"),
  ExchangeRate = list(thu_muc = "Monthly",   ten = "exchange_rate",    du_lieu = "exchange_rate.csv",       m4 = "Monthly",   tien_to = "M_"),
  Confidence   = list(thu_muc = "Monthly",   ten = "Confidence_index", du_lieu = "Confidence_index100.csv", m4 = "Monthly",   tien_to = "M_"),
  Imports      = list(thu_muc = "Monthly",   ten = "Imports",          du_lieu = "Imports.csv",             m4 = "Monthly",   tien_to = "M_")
)

ten_mo_hinh <- c(xgbreg = "xgb_mean_reg", xgbcls = "xgb_mean_cl",
                 lgbreg = "lgb_mean_reg", lgbcls = "lgb_mean_cl")

# 8 file mo hinh .rds hoc tren M4, can cho Bang 8
# file nay chi duoc luu khi *_opt_forecast.r chay het; neu thieu thi huan luyen lai
lay_mo_hinh <- function(bo_m4, tien_to) {
  thu_muc_kq <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo_m4)))
  can_co <- paste0(tien_to, rep(c("ets", "thetaf"), each = 4), names(ten_mo_hinh), ".rds")
  if (all(file.exists(file.path(thu_muc_kq, can_co)))) return(file.path(thu_muc_kq, can_co))

  cat("Thieu file .rds cua", bo_m4, "-> huan luyen lai\n")
  tam <- file.path(tempdir(), paste0("mohinh_", bo_m4))
  dir.create(tam, showWarnings = FALSE)
  file.copy(list.files(thu_muc_kq, full.names = TRUE), tam, overwrite = TRUE)
  ma_m4 <- file.path(THU_MUC_TAC_GIA, "OSP-TSP on M4", "Time series 5 equal parts (m=5)", bo_m4)
  thu_muc_cu <- setwd(tam)
  for (mh in c("ets", "thetaf")) {
    env <- new.env()
    tryCatch(source(file.path(ma_m4, paste0(mh, "_opt_forecast.r")), local = env),
             error = function(e) cat("Bo qua loi:", conditionMessage(e), "\n"))
    for (k in names(ten_mo_hinh))
      saveRDS(env[[ten_mo_hinh[k]]], file.path(thu_muc_kq, paste0(tien_to, mh, k, ".rds")))
  }
  setwd(thu_muc_cu)
  file.path(thu_muc_kq, can_co)
}

# loai = "M4" (Bang 8), "Train_by_oneself" (Bang 7) hoac "gratis5" (Bang 9)
so_bang <- c(M4 = 8, Train_by_oneself = 7, gratis5 = 9)

# Bang 9: cac cho sua code *_gratis5.r nam o file rieng
source(file.path(THU_MUC_KHO, "scripts", "09_sua_ma_gratis.R"))

chay_bo_thuc <- function(ten_bo, loai) {
  b <- cac_bo[[ten_bo]]
  ket_qua <- file.path(THU_MUC_KHO, "results", paste0("bang", so_bang[[loai]]))
  dir.create(ket_qua, recursive = TRUE, showWarnings = FALSE)

  tam <- file.path(tempdir(), paste(ten_bo, loai, sep = "_"))
  dir.create(tam, showWarnings = FALSE)
  file.copy(file.path(MA_GOC, b$thu_muc, b$du_lieu), tam, overwrite = TRUE)

  if (loai == "M4") {
    tep <- lay_mo_hinh(b$m4, b$tien_to)
    file.copy(tep, tam, overwrite = TRUE)
    # file cua bo thang doc ten khong co tien to M_
    file.copy(tep, file.path(tam, sub("^[YQM]_", "", basename(tep))), overwrite = TRUE)
  }

  tep_ma <- file.path(MA_GOC, b$thu_muc, paste0(b$ten, "_", loai, ".r"))
  if (loai == "gratis5") {
    ma <- sua_ma_gratis(readLines(tep_ma, warn = FALSE, encoding = "UTF-8"), b$ten)
    tep_ma <- file.path(tam, paste0(b$ten, "_gratis_da_sua.r"))
    writeLines(ma, tep_ma, useBytes = TRUE)
  }

  thu_muc_cu <- setwd(tam)
  truoc <- list.files()
  cat("\n==========", ten_bo, loai, "==========\n")
  bat_dau <- Sys.time()
  loi <- ""
  tryCatch(source(tep_ma, local = new.env()),
           error = function(e) { loi <<- conditionMessage(e); cat("LOI:", loi, "\n") })
  giay <- round(as.numeric(difftime(Sys.time(), bat_dau, units = "secs")), 1)
  cat("Xong sau", giay, "giay\n")

  moi <- setdiff(list.files(pattern = "\\.csv$"), truoc)
  file.copy(moi, ket_qua, overwrite = TRUE)
  setwd(thu_muc_cu)

  tg <- file.path(ket_qua, "thoi_gian_chay.csv")
  dong <- data.frame(bo = ten_bo, file = paste0(b$ten, "_", loai, ".r"), giay = giay, loi = loi)
  write.csv(if (file.exists(tg)) rbind(read.csv(tg), dong) else dong, tg, row.names = FALSE)
  writeLines(capture.output(sessionInfo()), file.path(ket_qua, "phien_ban_thu_vien.txt"))
}

# Bang 8 (~1 gio)
# for (bo in names(cac_bo)) chay_bo_thuc(bo, "M4")

# Bang 7 (~1,7 gio)
# for (bo in names(cac_bo)) chay_bo_thuc(bo, "Train_by_oneself")

# Bang 9 (can goi gratis, feasts)
# for (bo in names(cac_bo)) chay_bo_thuc(bo, "gratis5")
