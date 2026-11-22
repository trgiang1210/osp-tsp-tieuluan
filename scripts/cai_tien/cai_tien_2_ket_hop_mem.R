# Buoc 2 (M4 Yearly): ket hop mem
#
#   OSP goc chon cung 1 khoang. Ket hop mem tron du bao cua 5 khoang theo xac suat lgbcls du doan:
#     du bao = tong (xac suat khoang i x du bao khoang i)
#   Co so: phan ket luan cua bai bao goi y huan luyen them mo hinh gan trong so cho cac du bao cai tien.
#   Moc di kem: trung binh deu 5 khoang (moi khoang trong so 0,2). Neu ket hop mem khong hon moc nay
#   thi cai thien chi den tu viec lay trung binh, khong phai tu xac suat cua mo hinh.
#   Wilcoxon: ket hop mem so voi lgbcls chon cung (cung nhan); TB deu 5 khoang so voi Total.
#   Mau so MASE co dinh (A), giong buoc 1.
#
# Dau ra (results/cai_tien/yearly/):
#   ket_qua_buoc2_yearly.csv           : Total, 8 bien the OSP, TB deu 5 khoang, ket hop mem, Tran
#   mase_tung_chuoi_buoc2_yearly.csv   : MASE cua tung chuoi kiem tra
# Chay:  source("D:/osp-tsp-tieuluan/scripts/cai_tien/cai_tien_2_ket_hop_mem.R")

if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
source(file.path(THU_MUC_KHO, "scripts", "cai_tien", "cai_tien_chung.R"))

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
