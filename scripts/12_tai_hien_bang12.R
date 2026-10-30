# Chay lai thuc nghiem Bang 12: cat chuoi tai diem thay doi tim bang goi cpm, roi du bao bang ETS va thetaf
# Viet lai tu notebook "Complementary experiments/Comparison with other truncation point methods.ipynb"
# cua tac gia, giu nguyen phuong phap (cung kiem dinh, cung cach chia tap kiem tra, cung cach tinh sai so).
# Sua 3 cho trong notebook:
#   - bo dong "data = Hourly_M4" (bien Hourly_M4 khong ton tai vi o dinh nghia da bi comment)
#   - vong lap "for (j in 1:1)" chi chay Hourly -> chay ca 6 tan suat
#   - ket qua bi ghi de sau moi tan suat va chi in ra man hinh -> luu ra file CSV
# Ghi chu: bai bao viet la dung CUSUM, nhung notebook dung cpmType = "Kolmogorov-Smirnov"
# (goi cpm khong co lua chon CUSUM). O day giu dung nhu notebook.

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

suppressMessages({library(forecast); library(M4comp2018); library(cpm)})

KET_QUA <- file.path(THU_MUC_KHO, "results", "bang12")
dir.create(KET_QUA, recursive = TRUE, showWarnings = FALSE)

chay_cpm <- function(tan_suat, so_mau = NULL) {
  tep_chi_tiet <- file.path(KET_QUA, paste0("cpm_", tan_suat, "_chi_tiet.csv"))
  if (file.exists(tep_chi_tiet)) { cat("Da co ket qua", tan_suat, "-> bo qua\n"); return(invisible()) }

  data <- Filter(function(l) l$period == tan_suat, M4)
  # lay mau giong Bang 3, Bang 5 (seed 2026) neu can
  if (!is.null(so_mau) && so_mau < length(data)) {
    set.seed(2026)
    data <- data[sort(sample(length(data), so_mau))]
  }
  h    <- data[[1]]$h
  freq <- frequency(data[[1]]$x)

  # chia tap giong notebook: 70% huan luyen, 30% kiem tra
  set.seed(100)
  index     <- sample(2, length(data), replace = TRUE, prob = c(0.7, 0.3))
  testindex <- c(1:length(data))[index == 2]

  cat("\n==========", tan_suat, "-", length(testindex), "chuoi kiem tra ==========\n")
  bat_dau <- Sys.time()
  kq <- data.frame(chuoi = sapply(data[testindex], function(l) l$st),
                   do_dai = NA_integer_, diem_thay_doi = NA_integer_,
                   mape_ets = NA_real_, mase_ets = NA_real_,
                   mape_theta = NA_real_, mase_theta = NA_real_, loi = "")

  for (k in seq_along(testindex)) {
    i <- testindex[k]
    hdata <- data[[i]]$x
    real  <- data[[i]]$xx
    kq$do_dai[k] <- length(hdata)
    tryCatch({
      cpm.res <- detectChangePoint(hdata, cpmType = "Kolmogorov-Smirnov")
      kq$diem_thay_doi[k] <- cpm.res$changePoint
      hdata <- ts(hdata[cpm.res$changePoint:length(hdata)], frequency = freq, end = end(data[[i]]$x))

      forecast_ets   <- forecast(ets(hdata), h = h)
      forecast_theta <- thetaf(hdata, h = h)
      res_ets   <- accuracy(forecast_ets, real)
      res_theta <- accuracy(forecast_theta, real)
      kq$mape_ets[k]   <- res_ets[2, 5];   kq$mase_ets[k]   <- res_ets[2, 6]
      kq$mape_theta[k] <- res_theta[2, 5]; kq$mase_theta[k] <- res_theta[2, 6]
    }, error = function(e) kq$loi[k] <<- conditionMessage(e))
    if (k %% 500 == 0) cat(" ", k, "/", length(testindex), "chuoi\n")
  }
  giay <- round(as.numeric(difftime(Sys.time(), bat_dau, units = "secs")), 1)
  write.csv(kq, tep_chi_tiet, row.names = FALSE)

  # tinh trung binh nhu notebook: MAPE khong bo NA, MASE bo NA
  tong <- data.frame(
    tan_suat = tan_suat, so_chuoi_kiem_tra = length(testindex),
    so_chuoi_khong_cat = sum(kq$diem_thay_doi == 0, na.rm = TRUE),
    mape_ets = mean(kq$mape_ets), mase_ets = mean(kq$mase_ets, na.rm = TRUE),
    mape_theta = mean(kq$mape_theta), mase_theta = mean(kq$mase_theta, na.rm = TRUE),
    so_na_mase = sum(is.na(kq$mase_ets) | is.na(kq$mase_theta)),
    so_loi = sum(kq$loi != ""), giay = giay)
  tep_tong <- file.path(KET_QUA, "cpm_tong_hop.csv")
  write.csv(if (file.exists(tep_tong)) rbind(read.csv(tep_tong), tong) else tong, tep_tong, row.names = FALSE)
  cat("Xong", tan_suat, "sau", giay, "giay\n")
  print(tong)
}

# Chay ca 6 tan suat tren toan bo M4 (~3 gio, Monthly lau nhat):
# for (ts_ in c("Yearly", "Quarterly", "Weekly", "Daily", "Hourly", "Monthly")) chay_cpm(ts_)
