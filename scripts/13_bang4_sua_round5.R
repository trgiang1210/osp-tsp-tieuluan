# Bang 4 (m=10): chay lai buoc tong hop sau khi sua loi round5
# Trong 3 file *_ETS_thetaf_final_result.r cua ban m=10, ham round5 ep du doan hoi quy
# (xgbreg, lgbreg) vao khoang 1-5, chep nguyen tu ban m=5. Voi m=10, mo hinh hoi quy
# khong bao gio chon duoc khoang 6-10. O day doi gioi han thanh 10.
# Chi chay lai buoc cuoi, dung lai ket qua cac buoc truoc trong results/tai_hien_<bo>_m10.
# Ket qua ghi vao results/tai_hien_<bo>_m10_sua, khong de len ket qua cu.

THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))
library(M4comp2018)
M4_DAY_DU <- M4

sua_round5 <- function(bo, so_mau = NULL) {
  cu  <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo), "_m10"))
  moi <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo), "_m10_sua"))
  dir.create(moi, recursive = TRUE, showWarnings = FALSE)

  # 4 file .RData cua buoc tao nhan va buoc hoc mo hinh
  tep <- list.files(cu, pattern = "(datalist|opt_pre_result)\\.RData$", full.names = TRUE)
  if (length(tep) < 4) stop("Thieu file .RData trong ", cu)
  file.copy(tep, moi, overwrite = TRUE)

  # cung mau chuoi voi script 10
  chuoi <- Filter(function(l) l$period == bo, M4_DAY_DU)
  if (!is.null(so_mau) && so_mau < length(chuoi)) {
    set.seed(2026)
    chuoi <- chuoi[sort(sample(length(chuoi), so_mau))]
  }
  assign("M4", chuoi, envir = globalenv())

  # sua round5: gioi han tren 5 -> 10
  goc <- file.path(THU_MUC_TAC_GIA, "OSP-TSP on M4", "Time series 10 equal parts (m=10)", bo,
                   paste0(bo, "_ETS_thetaf_final_result.r"))
  ma <- readLines(goc, warn = FALSE)
  ma <- sub("if\\(l\\[i\\]>5\\)", "if(l[i]>10)", ma)
  ma <- sub("l\\[i\\]=5$", "l[i]=10", ma)
  cat("Da sua", sum(grepl("l\\[i\\]>10|l\\[i\\]=10$", ma)), "dong trong round5\n")
  # ban da sua de o thu muc tam, khong dua code tac gia vao kho
  tep_ma <- file.path(tempdir(), paste0(bo, "_final_result_da_sua.r"))
  writeLines(ma, tep_ma)

  thu_muc_cu <- setwd(moi)
  bat_dau <- Sys.time()
  source(tep_ma, local = new.env())
  cat("Xong", bo, "sau", round(as.numeric(difftime(Sys.time(), bat_dau, units = "secs")), 1), "giay\n")
  setwd(thu_muc_cu)
  rm("M4", envir = globalenv())
}

# cung so mau voi Bang 4
# sua_round5("Yearly")
# sua_round5("Quarterly", so_mau = 1000)
# sua_round5("Monthly",   so_mau = 1000)
