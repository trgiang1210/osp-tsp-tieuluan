

if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
if (!exists("TAN_SUAT")) TAN_SUAT <- "yearly"
if (!exists("SO_CHUOI_MAU")) SO_CHUOI_MAU <- 1000
if (!exists("SO_LOI")) SO_LOI <- 5
TAN_SUAT <- tolower(TAN_SUAT)

file_chung <- switch(TAN_SUAT,
  yearly    = file.path(THU_MUC_KHO, "scripts", "cai_tien", "cai_tien_chung.R"),
  quarterly = file.path(THU_MUC_KHO, "scripts", "cai_tien_quarterly", "cai_tien_chung_quarterly.R"),
  monthly   = file.path(THU_MUC_KHO, "scripts", "cai_tien_monthly", "cai_tien_chung_monthly.R"),
  stop("TAN_SUAT phai la 'yearly', 'quarterly' hoac 'monthly'"))
source(file_chung)
suppressMessages(library(forecast))
suppressMessages(library(parallel))
options(width = 200)
stopifnot(N == 4)

# moc 1 loi tu lan do truoc
file_moc <- file.path(thu_muc_ra, paste0("thoi_gian_do_n2_tung_chuoi_", TAN_SUAT, ".csv"))
if (!file.exists(file_moc)) stop("Chua co ", file_moc, " - can chay buoc do n = 2 (cai_tien_5_*) truoc")
moc <- read.csv(file_moc, stringsAsFactors = FALSE)

# du bao tat ca chuoi con cua mot chuoi voi n_diem diem moi khoang (giong cai_tien_5_*)
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
ds_chuoi <- chuoi[mau]
ma_chuoi <- sapply(ds_chuoi, function(l) l$st)
cat("Song song", SO_LOI, "loi tren", length(mau), "chuoi", BO, "(mau, seed 2026) | may co",
    detectCores(logical = FALSE), "loi that\n")

# khoi dong 5 phien R phu
t_khoi_dong <- system.time({
  cl <- makeCluster(SO_LOI)
  invisible(clusterEvalQ(cl, suppressMessages(library(forecast))))
  clusterExport(cl, c("M", "H", "FREQ", "du_bao_chuoi_con"))
})["elapsed"]
cat("Khoi dong", SO_LOI, "phien R phu:", round(t_khoi_dong, 1), "giay\n")

ket_qua <- list()
tryCatch({
  for (ten_mh in c("ets", "thetaf")) {
    cat("\n==========", toupper(ten_mh), "==========\n")
    m1 <- moc[moc$mo_hinh == ten_mh, ]
    if (nrow(m1) != length(mau) || !all(m1$chuoi == ma_chuoi))
      stop("Mau chuoi khong khop voi file moc 1 loi (", ten_mh, ") - kiem tra SO_CHUOI_MAU")
    dl <- doc_mot_doi_tuong(file.path(thu_muc_vao, paste0(TIEN_TO, "_", ten_mh, "_datalist.RData")))
    # chay khoi dong tren moi phien phu (khong tinh gio)
    invisible(parLapply(cl, rep(ds_chuoi[1], SO_LOI), du_bao_chuoi_con, ten_mh, 2))

    for (n_diem in c(4, 2)) {
      kq <- NULL
      t <- unname(system.time(kq <- parLapply(cl, ds_chuoi, du_bao_chuoi_con, ten_mh, n_diem))["elapsed"])
      diem_luu <- if (n_diem == 4) 1:4 else c(1, 3)        # n = 2 la diem 1 va 3 cua 4 diem da luu
      lech <- 0
      for (a in seq_along(mau)) {
        luu <- dl[[2]][mau[a], , diem_luu, , drop = FALSE]
        lech <- max(lech, max(abs(as.vector(kq[[a]]) - as.vector(luu)), na.rm = TRUE))
      }
      t1 <- sum(if (n_diem == 4) m1$giay_n4 else m1$giay_n2)
      t1_n4 <- sum(m1$giay_n4)
      ket_qua[[paste(ten_mh, n_diem)]] <- data.frame(
        mo_hinh = ten_mh, n = n_diem, so_loi = SO_LOI, so_chuoi_mau = length(mau),
        giay_1_loi = t1, giay_song_song = t,
        tang_toc_lan = t1 / t, giam_thoi_gian_pct = 100 * (1 - t / t1),
        giam_so_voi_n4_1_loi_pct = 100 * (1 - t / t1_n4),
        giay_khoi_dong_cum = unname(t_khoi_dong),
        lech_toi_da_so_voi_datalist = lech)
      cat(sprintf("  n = %d: 1 loi %.1f giay | %d loi %.1f giay | nhanh gap %.2f lan (giam %.1f%%) | lech %s\n",
                  n_diem, t1, SO_LOI, t, t1 / t, 100 * (1 - t / t1), signif(lech, 3)))
      write.csv(do.call(rbind, ket_qua), file.path(thu_muc_ra, paste0("thoi_gian_song_song_", TAN_SUAT, ".csv")),
                row.names = FALSE)
    }
  }
}, finally = stopCluster(cl))

kq_tong <- do.call(rbind, ket_qua)
cat("\nTong hop:\n"); print(kq_tong, row.names = FALSE, digits = 4)
cat("\nDa ghi ket qua vao:", file.path(thu_muc_ra, paste0("thoi_gian_song_song_", TAN_SUAT, ".csv")), "\n")
