# Buoc 1 (M4 Monthly): co dinh mau so MASE (A), tran, kiem chung mau so cu, Wilcoxon
#
#   Bang so sanh: Total, Random (4 diem ngau nhien trong 20), 8 bien the OSP, Tran.
#   Tran: MASE neu moi chuoi kiem tra chon dung khoang tot nhat (biet truoc xx, chi de tham khao).
#   Wilcoxon: Random va 8 bien the OSP so voi Total, tren MASE tung chuoi kiem tra.
#   Kiem chung: tinh lai MASE bang mau so CU cua tac gia (chuoi cat tu dau khoang da chon, giong predres)
#     (1) tu du doan tac gia da luu (*_opt_pre_result.RData, neu co) -> phai trung file ket qua tai hien
#     (2) tu mo hinh huan luyen lai voi nhan cu -> xem du doan co trung tac gia khong
#
# Dau ra (results/cai_tien/monthly/):
#   ket_qua_buoc1_monthly.csv          : moi dong la mot phuong phap: MASE moi, p Wilcoxon, MASE cu
#   mase_theo_khoang_monthly.csv       : MASE trung binh khi luon chon khoang 1..5, Tran
#   kiem_chung_mau_so_cu_monthly.csv   : MASE tinh lai bang mau so cu so voi file ket qua tai hien
#   mase_tung_chuoi_buoc1_monthly.csv  : MASE cua tung chuoi kiem tra
# Chay:  source("D:/osp-tsp-tieuluan/scripts/cai_tien_monthly/cai_tien_1_mau_so_monthly.R")

if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
source(file.path(THU_MUC_KHO, "scripts", "cai_tien_monthly", "cai_tien_chung_monthly.R"))

ket_qua <- list(); theo_khoang <- list(); kiem_chung <- list(); tung_chuoi <- list()

for (ten_mh in c("ets", "thetaf")) {
  d <- chuan_bi(ten_mh)
  b <- mo_bang()

  # --- bang so sanh ---
  them_total_osp(b, d)
  set.seed(2026)   # random: moi chuoi chon ngau nhien 4 trong 20 diem, lay trung binh du bao
  fc_random <- t(sapply(test_idx, function(k) {
    chon <- sample(M * N, N)
    colMeans(t(sapply(chon, function(p) d$predh[k, (p - 1) %/% N + 1, (p - 1) %% N + 1, ])))
  }))
  them(b, d, "Random 4 diem", "Moc", fc_random, so_voi = "Total series")
  them_tran(b, d)
  bang <- bang_ket_qua(b)

  # tran theo diem don le (khong dat duoc vi OSP lay trung binh 4 diem)
  mase_diem <- sapply(1:(M * N), function(p)
    mase_chuoi(d$predh[test_idx, (p - 1) %/% N + 1, (p - 1) %% N + 1, ], d$real_t, d$sc_t))
  bang <- rbind(bang, data.frame(
    mo_hinh = ten_mh, nhom = "Tran (tham khao)", phuong_phap = "Tran: diem don le tot nhat trong 20",
    nhan = NA, MASE_moi = tb_huu_han(apply(mase_diem, 1, min)), so_voi = NA, p_wilcoxon = NA,
    ty_le_dung_khoang_tran = NA))

  # --- cot MASE cu (ket qua tai hien voi mau so cua tac gia) ---
  cu <- cach_doc_cu(ten_mh)
  bang$MASE_cu <- NA_real_
  if (!is.null(cu)) for (r in seq_len(nrow(bang))) {
    khoa <- switch(bang$phuong_phap[r], "Total series" = "Total", "Random 4 diem" = "Random",
                   paste0(bang$phuong_phap[r], "_", bang$nhan[r]))
    if (khoa %in% names(dong_cu)) bang$MASE_cu[r] <- cu$V6[dong_cu[[khoa]]]
  }
  ket_qua[[ten_mh]] <- bang
  tung_chuoi[[ten_mh]] <- bang_tung_chuoi(b, d)
  theo_khoang[[ten_mh]] <- data.frame(
    mo_hinh = ten_mh,
    chon = c(paste0("luon chon khoang ", 1:M), "Tran (chon dung khoang)"),
    MASE_tb = c(apply(d$mase_kh, 2, tb_huu_han), tb_huu_han(d$mase_kh[cbind(1:nt, d$khoang_tran)])))

  # --- kiem chung bang mau so cu ---
  if (!is.null(cu)) {
    osp_cu <- chay_osp(feat, gan_nhan(d$pred_cu[, , , 6]), index, M)
    tep_pr <- file.path(thu_muc_vao, paste0(tolower(BO), "_", ten_mh, "_opt_pre_result.RData"))
    pr_luu <- if (file.exists(tep_pr)) doc_mot_doi_tuong(tep_pr) else NULL
    for (loai in c("min", "mean")) {
      dd <- osp_cu[[loai]]$du_doan
      if (!is.null(pr_luu)) {               # du doan tac gia da luu: +1, round5 cho 2 cot hoi quy
        pl <- pr_luu[[if (loai == "min") 1 else 2]] + 1
        pl[, 2] <- round5(pl[, 2]); pl[, 4] <- round5(pl[, 4])
      }
      for (j in seq_len(ncol(dd))) {
        mh <- colnames(dd)[j]
        loc <- dd[test_idx, j]
        dong <- data.frame(mo_hinh = ten_mh, phuong_phap = mh, nhan = loai,
                           MASE_cu_file = cu$V6[dong_cu[[paste0(mh, "_", loai)]]],
                           MASE_tinh_lai = tb_huu_han(mase_cach_cu(du_bao_chon(d, loc), test_idx, loc)),
                           ty_le_trung_du_doan_luu = NA, MASE_tu_du_doan_luu = NA)
        if (!is.null(pr_luu)) {
          loc_l <- pl[test_idx, j]
          dong$ty_le_trung_du_doan_luu <- mean(loc == loc_l)
          dong$MASE_tu_du_doan_luu <- tb_huu_han(mase_cach_cu(du_bao_chon(d, loc_l), test_idx, loc_l))
        }
        kiem_chung[[length(kiem_chung) + 1]] <- dong
      }
    }
  }

  cat("\nMASE trung binh (mau so co dinh, 30% chuoi kiem tra):\n")
  print(bang[, c("phuong_phap", "nhan", "MASE_moi", "so_voi", "p_wilcoxon", "MASE_cu")],
        row.names = FALSE, digits = 4)
}

# ============================ ghi ket qua ============================
ghi(ket_qua, paste0("ket_qua_buoc1_", tolower(BO), ".csv"))
ghi(theo_khoang, paste0("mase_theo_khoang_", tolower(BO), ".csv"))
ghi(tung_chuoi, paste0("mase_tung_chuoi_buoc1_", tolower(BO), ".csv"))

if (length(kiem_chung) > 0) {
  kc <- do.call(rbind, kiem_chung)
  kc$lech <- abs(kc$MASE_tinh_lai - kc$MASE_cu_file)
  write.csv(kc, file.path(thu_muc_ra, paste0("kiem_chung_mau_so_cu_", tolower(BO), ".csv")), row.names = FALSE)
  cat("\nKIEM CHUNG mau so cu (so voi file ket qua tai hien):\n")
  print(kc, row.names = FALSE, digits = 4)
  if (any(!is.na(kc$MASE_tu_du_doan_luu))) {
    l1 <- max(abs(kc$MASE_tu_du_doan_luu - kc$MASE_cu_file), na.rm = TRUE)
    cat("(1) Cong thuc MASE cu (tu du doan da luu): lech lon nhat =", signif(l1, 3),
        if (l1 < 1e-6) "-> KHOP\n" else "-> CHUA KHOP\n")
  } else {
    cat("(1) Khong co file *_opt_pre_result.RData, bo qua kiem chung cong thuc.\n")
  }
  cat("(2) Huan luyen lai voi nhan cu: lech MASE lon nhat =", signif(max(kc$lech, na.rm = TRUE), 3))
  if (any(!is.na(kc$ty_le_trung_du_doan_luu)))
    cat(" | ty le trung du doan thap nhat =", round(min(kc$ty_le_trung_du_doan_luu, na.rm = TRUE), 3))
  cat("\n")
} else {
  cat("\nKhong tim thay file ket qua tai hien cu, bo qua buoc kiem chung.\n")
}
cat("\nDa ghi ket qua vao:", thu_muc_ra, "\n")
