

if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
source(file.path(THU_MUC_KHO, "scripts", "cai_tien_quarterly", "cai_tien_chung_quarterly.R"))

ket_qua <- list(); tung_chuoi <- list()

for (ten_mh in c("ets", "thetaf")) {
  d <- chuan_bi(ten_mh)
  b <- mo_bang()

  them_total_osp(b, d)
  them(b, d, "TB deu 5 khoang", "Moc", apply(d$fc_ts, c(1, 3), mean), so_voi = "Total series")

  for (loai in c("min", "mean")) {
    P <- d$osp[[loai]]$xs_lgb[test_idx, , drop = FALSE]          # nt x 5 xac suat
    fc <- t(sapply(1:nt, function(a) colSums(P[a, ] * matrix(d$fc_ts[a, , ], nrow = M))))
    them(b, d, "Ket hop mem (lgbcls)", "Cai tien: ket hop mem", fc, loai, so_voi = paste0("lgbcls_", loai))
  }
  them_tran(b, d)

  bang <- bang_ket_qua(b)
  ket_qua[[ten_mh]] <- bang
  tung_chuoi[[ten_mh]] <- bang_tung_chuoi(b, d)
  cat("\nMASE trung binh (mau so co dinh, 30% chuoi kiem tra):\n")
  print(bang[, c("phuong_phap", "nhan", "MASE_moi", "so_voi", "p_wilcoxon")], row.names = FALSE, digits = 4)
}

ghi(ket_qua, paste0("ket_qua_buoc2_", tolower(BO), ".csv"))
ghi(tung_chuoi, paste0("mase_tung_chuoi_buoc2_", tolower(BO), ".csv"))
cat("\nDa ghi ket qua vao:", thu_muc_ra, "\n")
