

if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
source(file.path(THU_MUC_KHO, "scripts", "cai_tien", "cai_tien_chung.R"))
options(width = 200)

BE_RONG <- 5

# 3 ham cua tsfeatures; kiem tra co tham so width truoc khi chay
if (!exists("gay_ham")) {
  if (!requireNamespace("tsfeatures", quietly = TRUE))
    stop("Chua cai tsfeatures. Chay: install.packages(\"tsfeatures\")")
  gay_ham <- list(muc = tsfeatures::max_level_shift,
                  phuong_sai = tsfeatures::max_var_shift,
                  kl = tsfeatures::max_kl_shift)
}
for (ten in names(gay_ham))
  if (!"width" %in% names(formals(gay_ham[[ten]])))
    stop("Ham diem gay '", ten, "' cua tsfeatures khong co tham so width: kiem tra phien ban goi")

ten_cot_gay <- c("gay_muc", "vt_gay_muc", "gay_phuong_sai", "vt_gay_phuong_sai", "gay_kl", "vt_gay_kl")

gay_mot_chuoi <- function(x) {
  x <- as.numeric(x); n <- length(x)
  s <- sd(x)
  if (!is.finite(s) || s == 0) return(setNames(rep(NA_real_, 6), ten_cot_gay))
  z <- ts((x - mean(x)) / s, frequency = 1)
  mot <- function(f) {
    r <- tryCatch(suppressWarnings(as.numeric(f(z, width = BE_RONG))), error = function(e) c(NA, NA))
    if (length(r) != 2) r <- c(NA, NA)
    c(r[1], r[2] / n)              # do lon, vi tri tuong doi
  }
  v <- c(mot(gay_ham$muc), mot(gay_ham$phuong_sai), mot(gay_ham$kl))
  v[!is.finite(v)] <- NA           # xgboost va lightgbm xu ly duoc NA
  setNames(v, ten_cot_gay)
}

# ============================ dac trung diem gay ============================

ma_chuoi <- sapply(chuoi, function(l) l$st)
tep_gay <- file.path(thu_muc_ra, paste0("diem_gay_dac_trung_", tolower(BO), ".csv"))
giay_gay <- NA_real_
gay <- NULL
if (file.exists(tep_gay)) {
  tam <- read.csv(tep_gay, check.names = FALSE)
  if (nrow(tam) == so_chuoi && identical(as.character(tam$chuoi), as.character(ma_chuoi))) {
    gay <- tam[, -1, drop = FALSE]
    cat("Doc lai dac trung diem gay da luu:", ncol(gay), "cot\n")
  }
}
if (is.null(gay)) {
  cat("Tinh dac trung diem gay cho", so_chuoi, "chuoi (cua so", BE_RONG, ")...\n")
  t0 <- system.time(gay <- as.data.frame(t(sapply(chuoi, function(l) gay_mot_chuoi(l$x)))))
  giay_gay <- unname(t0["elapsed"])
  write.csv(cbind(chuoi = ma_chuoi, gay), tep_gay, row.names = FALSE)
  cat("Xong:", round(giay_gay, 1), "giay\n")
}
stopifnot(nrow(gay) == so_chuoi)
cat("Ty le NA tung cot:\n"); print(round(colMeans(is.na(gay)), 3))
gay <- gay[, colSums(!is.na(gay)) > 0, drop = FALSE]    # bo cot toan NA neu co

# catch22 (neu da chay buoc catch22 thi dung lai file da luu)
tep_c22 <- file.path(thu_muc_ra, paste0("catch22_dac_trung_", tolower(BO), ".csv"))
c22 <- NULL
if (file.exists(tep_c22)) {
  tam <- read.csv(tep_c22, check.names = FALSE)
  if (nrow(tam) == so_chuoi && identical(as.character(tam$chuoi), as.character(ma_chuoi))) {
    c22 <- tam[, -1, drop = FALSE]
    c22 <- c22[, colSums(!is.na(c22)) > 0, drop = FALSE]
  }
}
if (is.null(c22)) cat("Khong thay file catch22 hop le, bo qua bien the catch22 + diem gay.\n")

feat_goc <- feat
bo_dac_trung <- list("+ diem gay" = cbind(feat_goc, gay))
if (!is.null(c22)) bo_dac_trung[["+ catch22 + diem gay"]] <- cbind(feat_goc, c22, gay)
cat("Ma tran dac trung:", ncol(feat_goc), "cot (tsfeatures) ->",
    paste(sapply(bo_dac_trung, ncol), collapse = " / "), "cot\n")

# ============================ chay ============================

ket_qua <- list(); tung_chuoi <- list(); thoi_gian <- list()

for (ten_mh in c("ets", "thetaf")) {
  d <- chuan_bi(ten_mh)    # gom huan luyen 8 mo hinh bang tsfeatures goc

  # nhan: tinh lai tu MASE mau so co dinh (giong chuan_bi)
  MASE_moi <- array(NA_real_, dim = dim(d$predh)[1:3])
  for (k in 1:so_chuoi) for (i in 1:M) for (j in 1:N)
    MASE_moi[k, i, j] <- mean(abs(real_xx[k, ] - d$predh[k, i, j, ])) / scale_x[k]
  nhan <- gan_nhan(MASE_moi)

  b <- mo_bang()
  them_total_osp(b, d)
  them(b, d, "TB deu 5 khoang", "Moc", apply(d$fc_ts, c(1, 3), mean), so_voi = "Total series")

  tg <- data.frame(mo_hinh = ten_mh, so_chuoi = so_chuoi, so_cot_goc = ncol(feat_goc),
                   giay_tinh_diem_gay_ca_bo = giay_gay, be_rong_cua_so = BE_RONG)
  tg$giay_huan_luyen_goc <- unname(system.time(chay_osp(feat_goc, nhan, index, M))["elapsed"])
  for (ten_bo in names(bo_dac_trung)) {
    f_moi <- bo_dac_trung[[ten_bo]]
    t_hl <- system.time(osp_moi <- chay_osp(f_moi, nhan, index, M))["elapsed"]
    tg[[paste0("so_cot ", ten_bo)]] <- ncol(f_moi)
    tg[[paste0("giay_huan_luyen ", ten_bo)]] <- unname(t_hl)
    for (loai in c("min", "mean")) {
      dd <- osp_moi[[loai]]$du_doan[test_idx, , drop = FALSE]
      for (mh in colnames(dd))
        them(b, d, paste(mh, ten_bo), paste("Cai tien:", ten_bo),
             du_bao_chon(d, dd[, mh]), loai, dd[, mh], so_voi = paste0(mh, "_", loai))
    }
  }
  thoi_gian[[ten_mh]] <- tg
  them_tran(b, d)

  bang <- bang_ket_qua(b)
  bang$p_wilcoxon <- NULL                       # khong dung p-value
  # muc giam MASE (%) so voi dong moc trong cot so_voi
  moc <- setNames(bang$MASE_moi, ifelse(is.na(bang$nhan), bang$phuong_phap, paste0(bang$phuong_phap, "_", bang$nhan)))
  bang$giam_so_voi_moc_pct <- ifelse(is.na(bang$so_voi), NA,
                                     round(100 * (bang$MASE_moi / moc[bang$so_voi] - 1), 2))
  ket_qua[[ten_mh]] <- bang
  tung_chuoi[[ten_mh]] <- bang_tung_chuoi(b, d)
  cat("\nMASE trung binh (mau so co dinh, 30% chuoi kiem tra); giam_so_voi_moc_pct am = tot hon:\n")
  print(bang[, c("phuong_phap", "nhan", "MASE_moi", "so_voi", "giam_so_voi_moc_pct", "ty_le_dung_khoang_tran")],
        row.names = FALSE, digits = 4)
}

ghi(ket_qua, paste0("ket_qua_diem_gay_", tolower(BO), ".csv"))
ghi(tung_chuoi, paste0("mase_tung_chuoi_diem_gay_", tolower(BO), ".csv"))
ghi(thoi_gian, paste0("thoi_gian_diem_gay_", tolower(BO), ".csv"))
cat("\nThoi gian:\n"); print(do.call(rbind, thoi_gian), row.names = FALSE)
cat("\nDa ghi ket qua vao:", thu_muc_ra, "\n")
