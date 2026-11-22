if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
source(file.path(THU_MUC_KHO, "scripts", "cai_tien_monthly", "cai_tien_chung_monthly.R"))
options(width = 200)

CHON_DIEM <- c(1, 3)     # diem giu lai trong 4 diem cua moi khoang (n = 2: do lech 0 va m_l / 2)
stopifnot(N == 4, length(CHON_DIEM) == 2)

ket_qua <- list(); tung_chuoi <- list(); thoi_gian <- list()

for (ten_mh in c("ets", "thetaf")) {
  t_n4 <- system.time(d <- chuan_bi(ten_mh))["elapsed"]    # n = 4: doc du lieu, gan nhan, huan luyen 8 mo hinh goc

  # --- n = 2: giu 2 diem trong moi khoang ---
  predh2 <- d$predh[, , CHON_DIEM, , drop = FALSE]
  MASE2 <- array(NA_real_, dim = dim(predh2)[1:3])
  for (k in 1:so_chuoi) for (i in 1:M) for (j in 1:2)
    MASE2[k, i, j] <- mean(abs(real_xx[k, ] - predh2[k, i, j, ])) / scale_x[k]
  nhan2 <- gan_nhan(MASE2)
  t_hl2 <- system.time(osp2 <- chay_osp(feat, nhan2, index, M))["elapsed"]

  # d2: cung cau truc voi d nhung dung du bao 2 diem (fc_ts, MASE moi khoang, khoang Tran)
  fc_ts2 <- du_bao_khoang(predh2)[test_idx, , , drop = FALSE]
  mase_kh2 <- sapply(1:M, function(i) mase_chuoi(fc_ts2[, i, ], d$real_t, d$sc_t))
  d2 <- d
  d2$fc_ts <- fc_ts2; d2$mase_kh <- mase_kh2; d2$khoang_tran <- apply(mase_kh2, 1, which.min)
  d2$predh <- predh2

  b <- mo_bang()
  them_total_osp(b, d)                       # Total + 8 bien the n = 4 (so voi Total)
  them(b, d, "TB deu 5 khoang (n = 4)", "Moc", apply(d$fc_ts, c(1, 3), mean), so_voi = "Total series")
  for (loai in c("min", "mean")) {           # 8 bien the n = 2, so voi chinh bien the n = 4 (cung nhan)
    dd <- osp2[[loai]]$du_doan[test_idx, , drop = FALSE]
    for (mh in colnames(dd))
      them(b, d2, paste0(mh, " (n = 2)"), "Giam diem: n = 2",
           du_bao_chon(d2, dd[, mh]), loai, dd[, mh], so_voi = paste0(mh, "_", loai))
  }
  them_tran(b, d)                            # Tran n = 4
  b$hang[[length(b$hang)]]$phuong_phap <- "Tran n = 4: khoang tot nhat"
  them(b, d2, "Tran n = 2: khoang tot nhat", "Tran (tham khao)", du_bao_chon(d2, d2$khoang_tran))

  bang <- bang_ket_qua(b)
  bang$p_wilcoxon <- NULL                    # khong dung p-value
  moc <- setNames(bang$MASE_moi, ifelse(is.na(bang$nhan), bang$phuong_phap, paste0(bang$phuong_phap, "_", bang$nhan)))
  bang$thay_doi_so_voi_moc_pct <- ifelse(is.na(bang$so_voi), NA,
                                         round(100 * (bang$MASE_moi / moc[bang$so_voi] - 1), 2))   # duong = MASE tang (kem hon)
  ket_qua[[ten_mh]] <- bang
  tung_chuoi[[ten_mh]] <- bang_tung_chuoi(b, d)

  thoi_gian[[ten_mh]] <- data.frame(
    mo_hinh = ten_mh, so_chuoi = so_chuoi,
    so_lan_du_bao_n4 = so_chuoi * M * 4, so_lan_du_bao_n2 = so_chuoi * M * 2,
    giay_huan_luyen_8_mo_hinh_n2 = unname(t_hl2))

  cat("\nMASE trung binh (mau so co dinh, 30% chuoi kiem tra); thay_doi duong = MASE tang so voi moc:\n")
  print(bang[, c("phuong_phap", "nhan", "MASE_moi", "so_voi", "thay_doi_so_voi_moc_pct", "ty_le_dung_khoang_tran")],
        row.names = FALSE, digits = 4)
}

ghi(ket_qua, paste0("ket_qua_giam_diem_", tolower(BO), ".csv"))
ghi(tung_chuoi, paste0("mase_tung_chuoi_giam_diem_", tolower(BO), ".csv"))
ghi(thoi_gian, paste0("thoi_gian_giam_diem_", tolower(BO), ".csv"))
cat("\nSo lan du bao:\n"); print(do.call(rbind, thoi_gian), row.names = FALSE)
cat("\nDa ghi ket qua vao:", thu_muc_ra, "\n")
