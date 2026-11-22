
if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
if (!exists("SO_CHUOI_MAU")) SO_CHUOI_MAU <- 1000
source(file.path(THU_MUC_KHO, "scripts", "cai_tien_monthly", "cai_tien_chung_monthly.R"))
suppressMessages(library(forecast))
options(width = 200)

stopifnot(N == 4)

# du bao tat ca chuoi con cua mot chuoi voi n_diem diem moi khoang; tra ve mang m x n_diem x h
# (cung cac lenh nhu starting_point_ets.r / starting_point_thetaf.r cua tac gia, gom ca accuracy)
du_bao_chuoi_con <- function(l, ten_mh, n_diem) {
  y <- l$x; y_pred <- l$xx; y_l <- length(y)
  m_l <- sum(as.integer((1:y_l) * M / y_l) == 0)
  n_l <- m_l / n_diem
  ph <- array(NA_real_, c(M, n_diem, H))
  for (i in 0:(M - 1)) for (j in 0:(n_diem - 1)) {
    Y <- ts(y[round(i * m_l + j * n_l + 1):y_l], end = end(y), frequency = FREQ)
    pd <- if (ten_mh == "ets") forecast(ets(Y), h = H) else forecast(thetaf(Y, h = H), h = H)
    ph[i + 1, j + 1, ] <- pd$mean
    invisible(accuracy(pd, y_pred)[2, 1:6])
  }
  ph
}

set.seed(2026)
mau <- sort(sample(so_chuoi, min(SO_CHUOI_MAU, so_chuoi)))
cat("Do tren", length(mau), "chuoi Monthly (mau), seed 2026\n")

tong_hop <- list(); tung_chuoi <- list()
for (ten_mh in c("ets", "thetaf")) {
  cat("\n==========", toupper(ten_mh), "==========\n")
  invisible(du_bao_chuoi_con(chuoi[[mau[1]]], ten_mh, 4))     # chay khoi dong, khong tinh gio
  invisible(du_bao_chuoi_con(chuoi[[mau[1]]], ten_mh, 2))

  gio <- data.frame(chuoi = sapply(chuoi[mau], function(l) l$st), do_dai = sapply(chuoi[mau], function(l) length(l$x)),
                    giay_n4 = NA_real_, giay_n2 = NA_real_)
  lech_toi_da <- 0
  dl <- doc_mot_doi_tuong(file.path(thu_muc_vao, paste0(TIEN_TO, "_", ten_mh, "_datalist.RData")))
  for (a in seq_along(mau)) {
    l <- chuoi[[mau[a]]]
    chay <- function(n_diem) {
      kq <- NULL
      t <- system.time(kq <- du_bao_chuoi_con(l, ten_mh, n_diem))["elapsed"]
      list(t = unname(t), ph = kq)
    }
    if (a %% 2 == 1) { r4 <- chay(4); r2 <- chay(2) } else { r2 <- chay(2); r4 <- chay(4) }
    gio$giay_n4[a] <- r4$t; gio$giay_n2[a] <- r2$t
    stored2 <- dl[[2]][mau[a], , c(1, 3), , drop = TRUE]                   # m x 2 x h (diem 1 va 3 cua 4 diem)
    lech_toi_da <- max(lech_toi_da, max(abs(r2$ph - stored2), na.rm = TRUE))
    if (a %% 500 == 0) cat("  da do", a, "/", length(mau), "chuoi |", format(Sys.time(), "%H:%M:%S"), "\n")
  }
  tung_chuoi[[ten_mh]] <- cbind(mo_hinh = ten_mh, gio)

  t4 <- sum(gio$giay_n4); t2 <- sum(gio$giay_n2)
  tong_hop[[ten_mh]] <- data.frame(
    mo_hinh = ten_mh, so_chuoi_mau = length(mau),
    tong_giay_n4 = t4, tong_giay_n2 = t2,
    ty_le_n2_tren_n4 = t2 / t4, giam_thoi_gian_pct = 100 * (1 - t2 / t4),
    giay_moi_chuoi_n4 = t4 / length(mau), giay_moi_chuoi_n2 = t2 / length(mau),
    uoc_tinh_toan_bo_n4_giay = t4 / length(mau) * so_chuoi, uoc_tinh_toan_bo_n2_giay = t2 / length(mau) * so_chuoi,
    lech_toi_da_n2_so_voi_datalist = lech_toi_da)
  cat(sprintf("  n = 4: %.1f giay | n = 2: %.1f giay | n = 2 / n = 4 = %.3f (giam %.1f%%)\n",
              t4, t2, t2 / t4, 100 * (1 - t2 / t4)))
  cat("  Lech toi da giua du bao n = 2 chay lai va du bao da luu (diem 1 va 3):", signif(lech_toi_da, 3), "\n")
  # ghi sau moi mo hinh de neu bi ngat o mo hinh sau thi ket qua mo hinh truoc van con
  write.csv(do.call(rbind, tong_hop), file.path(thu_muc_ra, "thoi_gian_do_n2_monthly.csv"), row.names = FALSE)
  write.csv(do.call(rbind, tung_chuoi), file.path(thu_muc_ra, "thoi_gian_do_n2_tung_chuoi_monthly.csv"), row.names = FALSE)
}

ket_qua <- do.call(rbind, tong_hop)
cat("\nTong hop:\n"); print(ket_qua, row.names = FALSE, digits = 4)
cat("\nDa ghi ket qua vao:", thu_muc_ra, "\n")
