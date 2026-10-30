# Bang 13: OSP-TSP voi mo hinh co so FFORMA, M4 Weekly
# Viet lai 2 notebook cua tac gia (thu muc "Complementary experiments/FForma/") thanh script R:
#   - Weekly_FForma_opt_forecast.ipynb : gan nhan, huan luyen XGBoost/LightGBM, doan khoang
#   - Weekly_FForma_final_result.ipynb : tinh du bao trung binh va MAPE/MASE
# Khong chay lai Weekly_forma-opt.ipynb (huan luyen FFORMA, du bao 20 diem bat dau): goi M4metalearning
# can ban xgboost tuy bien ("Xgboost CUSTOM version is required!"). Dung 2 file tac gia da luu:
#   - Weekly_FForma_datalist.RData    : du bao + sai so FFORMA cua 137 chuoi huan luyen mo hinh OSP
#   - Weekly_FForma_datatestlist.RData: du bao + sai so FFORMA cua 114 chuoi kiem tra
# Hai cach tai hien (ket qua nam o 2 cot rieng trong so_sanh_bang13.csv):
#   (1) "tac gia luu san": dung khoang do tac gia luu (Weekly_FForma_opt_pre_result.RData),
#       chi chay lai Weekly_FForma_final_result.ipynb
#   (2) "tu huan luyen"  : chay lai ca Weekly_FForma_opt_forecast.ipynb (gan nhan + huan luyen + doan khoang)
#                          roi moi chay Weekly_FForma_final_result.ipynb
# Chia du lieu (theo bai bao, muc 4.3): 30% huan luyen FFORMA, 40% huan luyen mo hinh OSP, 30% kiem tra.
#
# Luu y: "tuong lai" dung de cham diem o Bang 13 la 13 tuan cuoi cua phan qua khu x (ham temp_holdout),
# khong phai phan kiem tra xx chinh thuc cua M4 nhu cac bang khac.
#
# Sua cac cho sau trong notebook:
#   - Weekly_FForma_opt_forecast.ipynb: nhan cho xgb/lgb phan loai viet as.matrix(as.factor(nhan)) -> as.numeric(nhan)
#     (xgboost 3.x khong nhan nhan dang chu); them verbose = -1 cho lightgbm (chi giam in man hinh)
#   - Weekly_FForma_opt_forecast.ipynb, o 59: lgbclsm=matrix(lgbclsmin,m,datalength) chi dung khi predict() tra vector.
#     Trong notebook (o 58) lightgbm tra ma tran 359 x 5, nen lenh nay xao tron xac suat giua cac chuoi:
#     khoang lgbcls tac gia luu trung 85% voi cach xao tron nay, chi 20% voi cach dung.
#     Script dung predict() cua compat.R (doi ma tran thanh vector dung thu tu), nen o cach (2)
#     cot lgbcls duoc tinh dung va khac han bai bao (bai bao MASE 2,00 va 1,95).
#   - o 10: eval(parse(text = nnetaroptl)) -> FFormaoptl (ten bien sai, chep tu notebook nnetar)
#   - o 12: sample(3, dim(...)) -> dim(...)[1] (R moi bao loi "invalid 'size' argument" khi size la vecto)
#   - o 32: testindex = 1:dim(nnetar_pr_min)[1] -> FForma_pr_min (bien nnetar_pr_min khong ton tai)
#   - ham temp_holdout cua goi M4metalearning (chi co tren GitHub, cai rat nang) duoc chep lai o duoi
#     tu https://github.com/robjhyndman/M4metalearning, R/ensemble_classifier.R,
#     commit 61ddc7101680e9df7219c359587d0b509d2b50d6 (giay phep GPL-3)

THU_MUC_TAC_GIA <- "D:/forecasting-with-optimal-starting-point"
THU_MUC_KHO     <- "D:/osp-tsp-tieuluan"

suppressMessages({library(forecast); library(M4comp2018); library(dplyr); library(xgboost); library(lightgbm)})
source(file.path(THU_MUC_KHO, "scripts", "compat.R"))

THU_MUC_FFORMA <- file.path(THU_MUC_TAC_GIA, "Complementary experiments", "FForma")
KET_QUA <- file.path(THU_MUC_KHO, "results", "bang13")
dir.create(KET_QUA, recursive = TRUE, showWarnings = FALSE)

# ---- temp_holdout (chep nguyen tu M4metalearning) ----
# lay h quan sat cuoi cua x lam "tuong lai" xx, phan con lai lam x
temp_holdout <- function(dataset) {
  lapply(dataset, function(seriesentry) {
    frq <- stats::frequency(seriesentry$x)
    if (length(seriesentry$x) - seriesentry$h < max(2 * frq +1, 7)) {
      length_to_keep <- max(2 * stats::frequency(seriesentry$x) +1, 7)
      seriesentry$h <- length(seriesentry$x) - length_to_keep
      if (seriesentry$h < 2) {
        warning( paste( "cannot subset series by",
                        2 - seriesentry$h,
                        " observations, adding a mean constant") )

        seriesentry$x <- stats::ts(c(seriesentry$x, rep(mean(seriesentry$x),2 - seriesentry$h )),
                          frequency = frq)
        seriesentry$h <- 2
      }
    }
    seriesentry$xx <- utils::tail(seriesentry$x, seriesentry$h)
    seriesentry$x <- stats::ts( utils::head(seriesentry$x, -seriesentry$h),
                                frequency = frq)
    if (!is.null(seriesentry$n)) {
      seriesentry$n <- length(seriesentry$x)
    }
    seriesentry
  })
}

load_mot <- function(tep) { e <- new.env(); ten <- load(tep, envir = e); get(ten, envir = e) }

bat_dau <- Sys.time()

# ---- o 1-4: du bao FFORMA tai 20 diem bat dau cua chuoi kiem tra ----
FForma_datalist <- load_mot(file.path(THU_MUC_FFORMA, "Weekly_FForma_datatestlist.RData"))
FForma_predh    <- FForma_datalist[[2]]   # chuoi x 5 khoang x 4 diem x 13 buoc
FForma_pred_res <- FForma_datalist[[1]]   # chuoi x 5 khoang x 4 diem x 6 sai so
m <- 5

# ===== Weekly_FForma_final_result.ipynb: tu khoang du doan -> MAPE/MASE =====
# FForma_opt_pre_list = list(khoang nhan min, khoang nhan mean) cua 359 chuoi
tinh_bang13 <- function(FForma_opt_pre_list) {
  set.seed(100)
  index <- sample(3, dim(FForma_opt_pre_list[[1]])[1], replace = TRUE, prob = c(0.3, 0.4, 0.3))
  FForma_pr_min  <- FForma_opt_pre_list[[1]][index == 3, ] + 1
  FForma_pr_mean <- FForma_opt_pre_list[[2]][index == 3, ] + 1

  round5 <- function(x) { l <- round(x); l[l > 5] <- 5; l[l < 1] <- 1; l }
  FForma_pr_min[, 2]  <- round5(FForma_pr_min[, 2]);  FForma_pr_min[, 4]  <- round5(FForma_pr_min[, 4])
  FForma_pr_mean[, 2] <- round5(FForma_pr_mean[, 2]); FForma_pr_mean[, 4] <- round5(FForma_pr_mean[, 4])
  # cot 1, 3 (phan loai) khong lam tron trong notebook; xgbcls da la so nguyen, lgbcls la which.max - 1

  # ---- o 21-29: du lieu Weekly, vi tri bat dau 5 khoang, tap kiem tra ----
  data <- Filter(function(l) l$period == "Weekly", M4)
  datalength <- length(data)
  h <- data[[1]]$h
  freq <- frequency(data[[1]]$x)

  length5 <- matrix(0, datalength, m)
  for (k in 1:datalength) {
    y <- data[[k]]$x
    m_l <- sum(as.integer((1:length(y)) * m / length(y)) == 0)
    length5[k, ] <- c(0, 1, 2, 3, 4) * m_l
  }
  length5 <- length5[index == 3, ]

  set.seed(100)
  index <- sample(3, length(data), replace = TRUE, prob = c(0.3, 0.4, 0.3))
  tindex <- c(1:length(data))[index == 3]
  M4_test <- temp_holdout(data[tindex])
  stopifnot(length(M4_test) == dim(FForma_pred_res)[1])

  testindex <- 1:dim(FForma_pr_min)[1]

  # ---- o 33-35: ham tinh du bao trung binh va sai so (giu nguyen notebook) ----
  meanpre <- function(pred_array, opt_pr, h, testindex) {
    meanpreh <- matrix(0, length(testindex), h)
    for (i in seq_along(testindex)) {
      idx <- testindex[i]
      predh <- unique(pred_array[idx, opt_pr[idx], , ])
      meanpreh[i, ] <- apply(predh, 2, mean)
    }
    meanpreh
  }

  predres <- function(pred_matrix, data, opt_pr, lengthmatrix, h, testindex, gop = FALSE) {
    pred_res <- matrix(0, length(testindex), 6)
    fore_l <- forecast(ets(data[[1]]$x), h = h)
    freq <- frequency(data[[1]]$x)
    for (i in seq_along(testindex)) {
      idx <- testindex[i]
      loc <- if (gop) min(opt_pr[idx, ]) else opt_pr[idx]   # gop = predres_mul
      start <- lengthmatrix[i, loc]
      y_all <- data[[idx]]$x
      fore_l$x <- ts(y_all[start:length(y_all)], frequency = freq, end = end(y_all))
      real <- data[[idx]]$xx
      fore_l$mean <- ts(pred_matrix[i, ], start = start(real), frequency = freq)
      pred_res[i, ] <- accuracy(fore_l, real)[2, 1:6]
    }
    pred_res
  }

  mean1 <- function(x) mean(x[is.finite(x)], na.rm = TRUE)

  # ---- o 36-50: du bao OSP-TSP va sai so ----
  lengthtest <- length5[testindex, ]
  pp <- c("xgbcls", "xgbreg", "lgbcls", "lgbreg")
  du_bao <- list(); sai_so <- list()
  for (nhan in c("min", "mean")) {
    pr <- if (nhan == "min") FForma_pr_min else FForma_pr_mean
    for (j in 1:4) {
      du_bao[[paste(nhan, pp[j])]] <- meanpre(FForma_predh, pr[, j], h, testindex)
      sai_so[[paste(nhan, pp[j])]] <- predres(du_bao[[paste(nhan, pp[j])]], M4_test, pr[, j], lengthtest, h, testindex)
    }
    # trung binh 4 bien the (dong "Actual mean", "Average mean" cua Bang 13)
    tb <- Reduce(`+`, du_bao[paste(nhan, pp)]) / 4
    sai_so[[paste(nhan, "all")]] <- predres(tb, M4_test, pr, lengthtest, h, testindex, gop = TRUE)
  }

  # ---- o 60-66: tong hop, cot 5 = MAPE, cot 6 = MASE ----
  # Total series: diem bat dau (1,1) = ca chuoi
  tong <- rbind(
    "Total series" = apply(FForma_pred_res[testindex, 1, 1, ], 2, mean1),
    t(sapply(sai_so, function(s) apply(s, 2, mean1))))
  colnames(tong) <- c("ME", "RMSE", "MAE", "MPE", "MAPE", "MASE")
  tong
}


# ===== Weekly_FForma_opt_forecast.ipynb: gan nhan, huan luyen, doan khoang =====
# Doc Weekly_features_matrix.csv + Weekly_FForma_datalist.RData (137 chuoi huan luyen mo hinh OSP),
# tra ve list(khoang nhan min, khoang nhan mean) giong file Weekly_FForma_opt_pre_result.RData
huan_luyen_osp <- function() {
  data <- read.csv(file.path(THU_MUC_FFORMA, "Weekly_features_matrix.csv"))
  data <- data[, 2:dim(data)[2]]
  res <- load_mot(file.path(THU_MUC_FFORMA, "Weekly_FForma_datalist.RData"))[[1]]
  MASE <- res[, , , 6]
  m <- 5

  whichmin <- function(x) { minx <- min(x[x > 0]); which(x == minx)[1] - 1 }
  meanunique <- function(x) mean(unique(x))

  # nhan Actual (min): khoang co MASE nho nhat; nhan Average (mean): khoang co MASE trung binh nho nhat
  gan_nhan <- function(ham_khoang) {
    nhan <- matrix(0, dim(MASE)[1], 1)
    for (i in seq(1, dim(MASE)[1])) {
      v <- apply(MASE[i, , ], 1, ham_khoang)
      if (max(v, na.rm = TRUE) == 0) { nhan[i, ] <- 0 }
      else { v[is.na(v)] <- 100; nhan[i, ] <- whichmin(v) }
    }
    nhan
  }
  realbestmin  <- gan_nhan(min)
  realbestmean <- gan_nhan(meanunique)
  cat("Phan bo nhan min :", table(realbestmin), "\n")

  set.seed(100)
  index <- sample(3, dim(data)[1], replace = TRUE, prob = c(0.3, 0.4, 0.3))
  train_data <- as.matrix(data[index == 2, ])
  stopifnot(nrow(train_data) == length(realbestmin))
  alldata <- as.matrix(data)
  alldataxgb <- xgb.DMatrix(data = alldata)

  hoc_va_doan <- function(nhan) {
    nhan <- as.numeric(nhan)
    xgb_reg <- xgboost(data = xgb.DMatrix(data = train_data, label = nhan), nround = 500)
    xgb_cl  <- xgboost(data = xgb.DMatrix(data = train_data, label = nhan), nround = 500,
                       objective = "multi:softmax", num_class = 5)
    lgb_reg <- lgb.train(data = lgb.Dataset(data = train_data, label = nhan), nrounds = 500,
                         params = list(verbose = -1))
    lgb_cl  <- lgb.train(data = lgb.Dataset(data = train_data, label = nhan), nrounds = 500,
                         params = list(objective = "multiclass", num_class = 5,
                                       metric = "multi_logloss", verbose = -1))
    xgbreg <- predict(xgb_reg, alldataxgb)
    xgbcls <- predict(xgb_cl, alldataxgb)
    lgbreg <- predict(lgb_reg, alldata)
    lgbcls <- predict(lgb_cl, alldata)
    # lightgbm phan loai tra xac suat 5 lop cho tung chuoi -> lop co xac suat lon nhat, tru 1
    lgbclsm <- matrix(lgbcls, m, dim(data)[1])
    lgbclsr <- apply(lgbclsm, 2, which.max) - 1
    pre <- cbind(xgbcls, xgbreg, lgbclsr, lgbreg)
    colnames(pre) <- c("xgbcls", "xgbreg", "lgbcls", "lgbreg")
    pre
  }
  list(hoc_va_doan(realbestmin), hoc_va_doan(realbestmean))
}

# ===== chay hai cach =====
cat("\n--- (1) khoang do tac gia luu san ---\n")
opt_pre_tac_gia <- load_mot(file.path(THU_MUC_FFORMA, "Weekly_FForma_opt_pre_result.RData"))
tong_tac_gia <- tinh_bang13(opt_pre_tac_gia)
write.csv(tong_tac_gia, file.path(KET_QUA, "FForma_Weekly_final_res_tac_gia_luu_san.csv"))

cat("\n--- (2) tu huan luyen (Weekly_FForma_opt_forecast.ipynb) ---\n")
opt_pre_moi <- huan_luyen_osp()
result_list <- opt_pre_moi
save(result_list, file = file.path(KET_QUA, "Weekly_FForma_opt_pre_result_tai_hien.RData"))
tong_moi <- tinh_bang13(opt_pre_moi)
write.csv(tong_moi, file.path(KET_QUA, "FForma_Weekly_final_res_tu_huan_luyen.csv"))

# so khoang du doan giong tac gia (phan tram chuoi kiem tra)
set.seed(100)
idx_kt <- sample(3, 359, replace = TRUE, prob = c(0.3, 0.4, 0.3)) == 3
# hoi quy (cot 2, 4) so sau khi lam tron ve khoang 0-4 nhu buoc tinh sai so
khoang <- function(a) { a[, c(2, 4)] <- pmin(pmax(round(a[, c(2, 4)]), 0), 4); a }
cat("\nTy le chuoi kiem tra co cung khoang du doan voi file cua tac gia:\n")
print(round(rbind(min  = colMeans(khoang(opt_pre_moi[[1]][idx_kt, ]) == khoang(opt_pre_tac_gia[[1]][idx_kt, ])),
                  mean = colMeans(khoang(opt_pre_moi[[2]][idx_kt, ]) == khoang(opt_pre_tac_gia[[2]][idx_kt, ]))), 2))

giay <- round(as.numeric(difftime(Sys.time(), bat_dau, units = "secs")), 1)
cat("\nXong sau", giay, "giay\n")

# ---- so sanh voi Bang 13 ----
# Bai bao ghi ten dong lgbcls va lgbreg nguoc voi code (so bai bao ghi "lgbreg" la ket qua lgbcls
# cua notebook va nguoc lai). O day so theo ten trong code, nen 2 gia tri lgb duoc doi cho.
pp <- c("xgbcls", "xgbreg", "lgbcls", "lgbreg")
bang13 <- data.frame(
  nhan = c(rep("Actual", 4), rep("Average", 4), "Actual", "Average", "Total"),
  phuong_phap = c(pp, pp, "mean", "mean", "Total series"),
  dong_code = c(paste("min", pp), paste("mean", pp), "min all", "mean all", "Total series"),
  MAPE_bai_bao = c(9.82, 10.30, 10.07, 10.48,  9.65, 10.08, 9.95, 10.01,  10.00, 9.76, 9.42),
  MASE_bai_bao = c(1.66, 1.64, 2.00, 1.72,     1.62, 1.61, 1.95, 1.67,    1.82, 1.81, 2.00))
bang13$MAPE_tac_gia_luu_san <- round(tong_tac_gia[bang13$dong_code, "MAPE"], 2)
bang13$MASE_tac_gia_luu_san <- round(tong_tac_gia[bang13$dong_code, "MASE"], 2)
bang13$MAPE_tu_huan_luyen   <- round(tong_moi[bang13$dong_code, "MAPE"], 2)
bang13$MASE_tu_huan_luyen   <- round(tong_moi[bang13$dong_code, "MASE"], 2)
bang13$chenh_MASE_tu_huan_luyen <- round(bang13$MASE_tu_huan_luyen - bang13$MASE_bai_bao, 2)
# so tren gia tri chua lam tron (lgbcls Actual 1,9986 so voi Total 1,9996)
bang13$tot_hon_total_tac_gia  <- tong_tac_gia[bang13$dong_code, "MASE"] < tong_tac_gia["Total series", "MASE"]
bang13$tot_hon_total_tu_huan_luyen <- tong_moi[bang13$dong_code, "MASE"] < tong_moi["Total series", "MASE"]
bang13[bang13$phuong_phap == "Total series", c("tot_hon_total_tac_gia", "tot_hon_total_tu_huan_luyen")] <- NA
bang13$dong_code <- NULL
cat("\nSo o khop bai bao (tac gia luu san):",
    sum(bang13$MAPE_tac_gia_luu_san == bang13$MAPE_bai_bao) + sum(bang13$MASE_tac_gia_luu_san == bang13$MASE_bai_bao),
    "/", 2 * nrow(bang13), "\n")
print(bang13, row.names = FALSE)
write.csv(bang13, file.path(THU_MUC_KHO, "results", "so_sanh_bang13.csv"), row.names = FALSE)
