
if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
source(file.path(THU_MUC_KHO, "scripts", "cai_tien_monthly", "cai_tien_chung_monthly.R"))
options(width = 200)

CHON_DIEM <- c(1, 3)
stopifnot(N == 4)
ma_chuoi <- sapply(chuoi, function(l) l$st)

doc_dac_trung <- function(ten_tep, buoc) {
  tep <- file.path(thu_muc_ra, ten_tep)
  if (!file.exists(tep)) stop("Khong thay ", tep, ". Chay ", buoc, " truoc.")
  tam <- read.csv(tep, check.names = FALSE)
  if (nrow(tam) != so_chuoi || !identical(as.character(tam$chuoi), as.character(ma_chuoi)))
    stop(ten_tep, " khong khop danh sach chuoi hien tai: xoa file va chay lai ", buoc)
  x <- tam[, -1, drop = FALSE]
  x[, colSums(!is.na(x)) > 0, drop = FALSE]
}
gay <- doc_dac_trung(paste0("diem_gay_dac_trung_", tolower(BO), ".csv"), "cai_tien_4b_diem_gay.R")
c22 <- doc_dac_trung(paste0("catch22_dac_trung_", tolower(BO), ".csv"), "cai_tien_4_catch22.R")

# ten bo dac trung khong bat dau bang dau "+" (Excel doc nhu cong thuc va bao #NAME?)
bo_dac_trung <- list("tsfeatures" = feat,
                     "tsfeatures + diem gay" = cbind(feat, gay),
                     "tsfeatures + catch22" = cbind(feat, c22),
                     "tsfeatures + catch22 + diem gay" = cbind(feat, c22, gay))
cat("So cot:", paste(names(bo_dac_trung), sapply(bo_dac_trung, ncol), sep = " = ", collapse = " | "), "\n")

hang_all <- list(); tom_tat_all <- list(); tung_chuoi <- list(); thoi_gian <- list()

for (ten_mh in c("ets", "thetaf")) {
  d <- chuan_bi(ten_mh)                       # n = 4 voi tsfeatures: osp goc

  MASE4 <- array(NA_real_, dim = dim(d$predh)[1:3])
  for (k in 1:so_chuoi) for (i in 1:M) for (j in 1:N)
    MASE4[k, i, j] <- mean(abs(real_xx[k, ] - d$predh[k, i, j, ])) / scale_x[k]
  nhan4 <- gan_nhan(MASE4)

  predh2 <- d$predh[, , CHON_DIEM, , drop = FALSE]
  MASE2 <- array(NA_real_, dim = dim(predh2)[1:3])
  for (k in 1:so_chuoi) for (i in 1:M) for (j in 1:2)
    MASE2[k, i, j] <- mean(abs(real_xx[k, ] - predh2[k, i, j, ])) / scale_x[k]
  nhan2 <- gan_nhan(MASE2)
  fc_ts2 <- du_bao_khoang(predh2)[test_idx, , , drop = FALSE]
  mase_kh2 <- sapply(1:M, function(i) mase_chuoi(fc_ts2[, i, ], d$real_t, d$sc_t))
  d2 <- d; d2$fc_ts <- fc_ts2; d2$mase_kh <- mase_kh2
  d2$khoang_tran <- apply(mase_kh2, 1, which.min); d2$predh <- predh2

  hang <- list(); ms <- list()
  ms_total <- mase_chuoi(d$predh[test_idx, 1, 1, , drop = TRUE], d$real_t, d$sc_t)
  ms[["Total series"]] <- ms_total
  mase_total <- tb_huu_han(ms_total)

  for (n_diem in c(4, 2)) {
    dd <- if (n_diem == 4) d else d2
    nhan <- if (n_diem == 4) nhan4 else nhan2
    for (ten_bo in names(bo_dac_trung)) {
      t_hl <- system.time({
        osp <- if (n_diem == 4 && ten_bo == "tsfeatures") d$osp else chay_osp(bo_dac_trung[[ten_bo]], nhan, index, M)
      })["elapsed"]
      thoi_gian[[length(thoi_gian) + 1]] <- data.frame(
        mo_hinh = ten_mh, n = n_diem, bo_dac_trung = ten_bo, so_cot = ncol(bo_dac_trung[[ten_bo]]),
        giay_huan_luyen_8_mo_hinh = if (n_diem == 4 && ten_bo == "tsfeatures") NA else unname(t_hl))
      for (loai in c("min", "mean")) {
        du_doan <- osp[[loai]]$du_doan[test_idx, , drop = FALSE]
        for (mh in colnames(du_doan)) {
          m_chuoi <- mase_chuoi(du_bao_chon(dd, du_doan[, mh]), dd$real_t, dd$sc_t)
          khoa <- paste(n_diem, ten_bo, mh, loai, sep = " | ")
          ms[[khoa]] <- m_chuoi
          hang[[length(hang) + 1]] <- data.frame(
            mo_hinh = ten_mh, n = n_diem, bo_dac_trung = ten_bo, bien_the = mh, nhan = loai,
            MASE = tb_huu_han(m_chuoi),
            ty_le_dung_khoang_tran = mean(du_doan[, mh] == dd$khoang_tran),
            stringsAsFactors = FALSE)
        }
      }
    }
    ms[[paste0("Tran n = ", n_diem)]] <- mase_chuoi(du_bao_chon(dd, dd$khoang_tran), dd$real_t, dd$sc_t)
  }

  df <- do.call(rbind, hang)
  k_hien_tai <- paste(df$n, df$bo_dac_trung, df$bien_the, df$nhan)
  mase_n4 <- df$MASE[match(paste(4, df$bo_dac_trung, df$bien_the, df$nhan), k_hien_tai)]
  mase_ts <- df$MASE[match(paste(df$n, "tsfeatures", df$bien_the, df$nhan), k_hien_tai)]
  df$giam_so_voi_total_pct <- round(100 * (df$MASE / mase_total - 1), 2)
  df$doi_so_voi_n4_cung_dt_pct <- round(100 * (df$MASE / mase_n4 - 1), 2)
  df$doi_so_voi_tsfeatures_cung_n_pct <- round(100 * (df$MASE / mase_ts - 1), 2)
  hang_all[[ten_mh]] <- df
  tung_chuoi[[ten_mh]] <- do.call(data.frame, c(
    list(mo_hinh = ten_mh, chuoi = sapply(chuoi[test_idx], function(l) l$st)), ms, list(check.names = FALSE)))

  # tom tat: bien the tot nhat cua tung o (n, bo dac trung), so voi bien the tot nhat cua n = 4 + tsfeatures
  goc_tot <- min(df$MASE[df$n == 4 & df$bo_dac_trung == "tsfeatures"])
  tt <- do.call(rbind, lapply(split(df, list(df$n, df$bo_dac_trung), drop = TRUE), function(g) {
    g <- g[which.min(g$MASE), ]
    data.frame(mo_hinh = ten_mh, n = g$n, bo_dac_trung = g$bo_dac_trung,
               bien_the_tot_nhat = paste(g$bien_the, g$nhan), MASE = g$MASE,
               giam_so_voi_total_pct = g$giam_so_voi_total_pct,
               doi_so_voi_osp_goc_tot_nhat_pct = round(100 * (g$MASE / goc_tot - 1), 2))
  }))
  tt <- tt[order(-tt$n, match(tt$bo_dac_trung, names(bo_dac_trung))), ]
  tom_tat_all[[ten_mh]] <- tt
  cat("\nTOM TAT", toupper(ten_mh), "- bien the tot nhat moi o (Total =", round(mase_total, 3),
      "; Tran n=4 =", round(tb_huu_han(ms[["Tran n = 4"]]), 3), "; Tran n=2 =", round(tb_huu_han(ms[["Tran n = 2"]]), 3), ")\n")
  print(tt, row.names = FALSE, digits = 4)
}

ghi(hang_all, paste0("ket_qua_giam_diem_dac_trung_", tolower(BO), ".csv"))
ghi(tom_tat_all, paste0("tom_tat_giam_diem_dac_trung_", tolower(BO), ".csv"))
ghi(tung_chuoi, paste0("mase_tung_chuoi_giam_diem_dac_trung_", tolower(BO), ".csv"))
write.csv(do.call(rbind, thoi_gian), file.path(thu_muc_ra, paste0("thoi_gian_giam_diem_dac_trung_", tolower(BO), ".csv")), row.names = FALSE)
cat("\nDa ghi ket qua vao:", thu_muc_ra, "\n")
