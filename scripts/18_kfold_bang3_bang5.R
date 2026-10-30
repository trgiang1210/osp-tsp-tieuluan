# Kiem dinh cheo k phan (mac dinh 10-fold) cho thuc nghiem chinh: Bang 3, Bang 5 (va Bang 10)
# OSP-TSP, m = 5, mo hinh co so ETS va thetaf, 6 tan suat M4. Day la phan TAI HIEN: giu nguyen moi cach
# lam cua tac gia (gan nhan, dac trung, cau hinh mo hinh, cach tinh MASE), chi thay 1 lan chia 70/30
# bang k lan chia.
#
# Chia TAP CHUOI thanh k phan (moi chuoi la mot mau); moi lan 1 phan lam kiem tra, k-1 phan huan luyen
# mo hinh chon khoang (XGBoost/LightGBM). Thu tu thoi gian ben trong tung chuoi giu nguyen.
# Khong du bao lai: dung du bao tu 20 diem bat dau da luu trong results/tai_hien_<bo>/<...>_datalist.RData.
# MASE tinh dung nhu ham predres cua tac gia (mau so tren chuoi cat tu dau khoang, tan so thuc cua chuoi).
# Them cac dong ket hop du bao cua Bang 10 (trung binh 4 bien the, nhom phan loai, nhom hoi quy).

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

source(file.path(THU_MUC_KHO, "scripts", "compat.R"))
suppressMessages({library(M4comp2018); library(forecast); library(xgboost); library(lightgbm)})

# ten file theo tung bo (giong code tac gia)
CAU_HINH <- data.frame(
  bo        = c("Yearly", "Quarterly", "Monthly", "Weekly", "Daily", "Hourly"),
  datalist  = c("year", "Quarterly", "Monthly", "week", "Daily", "Hourly"),
  opt       = c("yearly", "Quarterly", "Monthly", "weekly", "Daily", "Hourly"),
  final_ets = c("ets_Yearly", "ets_Quarterly", "ets_Monthly", "ets_week", "ets_Daily", "ets_Hourly"),
  final_th  = c("thetaf_Yearly", "thetaf_Quarterly", "thetaf_Monthly", "thetaf_weekly", "thetaf_Daily", "thetaf_Hourly"),
  tac_gia_bo_cot_tg = c(TRUE, TRUE, TRUE, TRUE, FALSE, TRUE),   # code tac gia co bo 6 cot thoi gian khong (Daily: khong)
  stringsAsFactors = FALSE)

load_mot <- function(tep) { e <- new.env(); ten <- load(tep, envir = e); get(ten, envir = e) }
mean1 <- function(x) mean(x[is.finite(x)])

# gan nhan nhu code tac gia: khoang co MASE nho nhat (nhan min) / MASE trung binh nho nhat (nhan mean), danh so 0-4
gan_nhan <- function(MASE, ham) {
  whichmin <- function(x) { minx <- min(x[x > 0]); which(x == minx)[1] - 1 }
  apply(MASE, 1, function(a) {
    v <- apply(a, 1, ham)
    if (max(v, na.rm = TRUE) == 0) return(0)
    v[is.na(v)] <- 100
    whichmin(v)
  })
}
meanunique <- function(x) mean(unique(x))

# huan luyen 4 mo hinh cho 1 loai nhan, doan khoang (0-4) cho moi chuoi; giong *_opt_forecast.r
hoc_va_doan <- function(dac_trung, train, nhan) {
  X <- as.matrix(dac_trung); Xtr <- X[train, , drop = FALSE]; y <- as.numeric(nhan[train])
  xgb_reg <- xgboost(data = xgb.DMatrix(Xtr, label = y), nround = 100)
  xgb_cl  <- xgboost(data = xgb.DMatrix(Xtr, label = y), nround = 100, objective = "multi:softmax", num_class = 5)
  lgb_reg <- lgb.train(data = lgb.Dataset(Xtr, label = y), nrounds = 100, params = list(verbose = -1))
  lgb_cl  <- lgb.train(data = lgb.Dataset(Xtr, label = y), nrounds = 100,
                       params = list(objective = "multiclass", num_class = 5, metric = "multi_logloss", verbose = -1))
  lgbcls <- apply(matrix(predict(lgb_cl, X), 5, nrow(X)), 2, which.max) - 1
  cbind(xgbcls = predict(xgb_cl, xgb.DMatrix(X)), xgbreg = predict(xgb_reg, xgb.DMatrix(X)),
        lgbcls = lgbcls, lgbreg = predict(lgb_reg, X))
}

# doi du doan (0-4, hoi quy la so thuc) thanh khoang 1-5 nhu *_final_result.r
thanh_khoang <- function(pr) {
  pr <- pr + 1
  for (j in c(2, 4)) pr[, j] <- pmin(pmax(round(pr[, j]), 1), 5)
  pr
}

# mau so MASE theo cach cua tac gia: chuoi cat tu dau khoang (length5), tan so thuc cua chuoi,
# tinh bang chinh forecast::accuracy nhu ham predres trong *_final_result.r
mau_so_tac_gia <- function(chuoi, m = 5) {
  mau <- forecast(ets(chuoi[[1]]$x), h = length(chuoi[[1]]$xx))   # khuon giong code tac gia
  t(sapply(chuoi, function(l) {
    y_all <- l$x; L <- length(y_all); freq <- frequency(y_all)
    m_l <- sum(as.integer((1:L) * m / L) == 0)
    sapply(0:(m - 1), function(i) {
      y <- y_all[(i * m_l):L]                       # i = 0 -> y_all[0:L] = ca chuoi, nhu code tac gia
      mau$x <- ts(y, frequency = freq, end = end(y_all))
      mau$mean <- ts(rep(1, length(l$xx)), start = start(l$xx), frequency = freq)
      a <- accuracy(mau, l$xx)[2, ]
      a[["MAE"]] / a[["MASE"]]
    })
  }))
}

chuan_bi_bo <- function(ch, mh) {
  thu_muc <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(ch$bo)))
  dl <- load_mot(file.path(thu_muc, paste0(ch$datalist, "_", mh, "_datalist.RData")))
  res <- dl[[1]]; predh <- dl[[2]]; n <- dim(res)[1]
  tat_ca <- Filter(function(l) l$period == ch$bo, M4)
  tep_ds <- file.path(thu_muc, "danh_sach_chuoi.txt")
  chuoi <- if (file.exists(tep_ds)) {
    ds <- readLines(tep_ds); tat_ca[match(ds, sapply(tat_ca, function(l) l$st))]
  } else tat_ca
  if (length(chuoi) != n) stop(ch$bo, ": so chuoi (", length(chuoi), ") khac datalist (", n, ")")
  dt <- read.csv(file.path(thu_muc, paste0(ch$bo, "_features_matrix.csv")))
  stopifnot(nrow(dt) == n)
  list(res = res, predh = predh, n = n,
       xx = t(sapply(chuoi, function(l) as.numeric(l$xx))),
       scale_tg = mau_so_tac_gia(chuoi),
       # dac trung dung nhu code tac gia (Daily doc ca 6 cot thoi gian chay, cac bo khac bo di)
       dac_trung = if (ch$tac_gia_bo_cot_tg) dt[, 2:(ncol(dt) - 6)] else dt[, 2:ncol(dt)],
       nhan = list(Actual = gan_nhan(res[, , , 6], min), Average = gan_nhan(res[, , , 6], meanunique)))
}

# sai so trung binh tren cac chuoi kiem tra (giong predres / predres_mul cua tac gia)
# du_bao_ds: ma tran (so chuoi kiem tra x h); khoang_mau: khoang dung de tinh mau so
danh_gia <- function(d, test, du_bao_ds, khoang_mau) {
  e <- abs(d$xx[test, , drop = FALSE] - du_bao_ds)
  c(MASE = mean1(rowMeans(e) / d$scale_tg[cbind(test, khoang_mau)]),
    MAPE = mean1(100 * rowMeans(e / abs(d$xx[test, , drop = FALSE]))))
}

# du bao OSP-TSP: trung binh cac du bao (khong trung nhau) cua 4 diem trong khoang, nhu ham meanpre
du_bao_khoang <- function(d, test, khoang) {
  t(sapply(seq_along(test), function(i) {
    p <- d$predh[test[i], khoang[i], , ]
    if (is.null(dim(p))) p else apply(unique(p), 2, mean)
  }))
}

mot_lan_chia <- function(d, test) {
  train <- !(seq_len(d$n) %in% test)
  pp <- c("xgbcls", "xgbreg", "lgbcls", "lgbreg")
  kq <- list()
  for (nh in c("Actual", "Average")) {
    k <- thanh_khoang(hoc_va_doan(d$dac_trung, train, d$nhan[[nh]]))[test, ]
    f <- lapply(1:4, function(j) du_bao_khoang(d, test, k[, j]))
    them <- function(ten, du_bao, khoang) {
      a <- danh_gia(d, test, du_bao, khoang)
      kq[[length(kq) + 1]] <<- data.frame(nhan = nh, phuong_phap = ten, MASE = a[["MASE"]], MAPE = a[["MAPE"]])
    }
    for (j in 1:4) them(pp[j], f[[j]], k[, j])
    # Bang 10: ket hop du bao; mau so lay o khoang som nhat (nhu predres_mul)
    nhom <- list("ket hop ca 4" = 1:4, "ket hop phan loai" = c(1, 3), "ket hop hoi quy" = c(2, 4))
    for (ten in names(nhom)) {
      j <- nhom[[ten]]
      them(ten, Reduce(`+`, f[j]) / length(j), apply(k[, j, drop = FALSE], 1, min))
    }
  }
  kq[[length(kq) + 1]] <- data.frame(nhan = "Total", phuong_phap = "Total series",
    MASE = mean1(d$res[test, 1, 1, 6]), MAPE = mean1(d$res[test, 1, 1, 5]))
  do.call(rbind, kq)
}

# k: so phan; lap: so lan lap lai (moi lan xao bang seed khac); cac_bo: tan suat can chay
chay_kfold <- function(k = 10, lap = 1, cac_bo = CAU_HINH$bo) {
  thu_muc_kq <- file.path(THU_MUC_KHO, "results", paste0("kfold_k", k, "_lap", lap))
  dir.create(thu_muc_kq, recursive = TRUE, showWarnings = FALSE)
  tep_ct <- file.path(thu_muc_kq, "kfold_chi_tiet.csv")
  da_co <- if (file.exists(tep_ct)) read.csv(tep_ct) else NULL
  for (b in cac_bo) for (mh in c("ets", "thetaf")) {
    if (!is.null(da_co) && any(da_co$bo == b & da_co$mo_hinh == mh)) { cat("Da co", b, mh, "-> bo qua\n"); next }
    cat("==========", b, mh, "==========\n"); t0 <- Sys.time()
    d <- tryCatch(chuan_bi_bo(CAU_HINH[CAU_HINH$bo == b, ], mh),
                  error = function(e) { cat("  LOI:", conditionMessage(e), "\n"); NULL })
    if (is.null(d)) next
    ket_qua <- list()
    for (r in seq_len(lap)) {
      set.seed(2025 + r)
      phan <- sample(rep(seq_len(k), length.out = d$n))
      for (f in seq_len(k)) {
        ket_qua[[length(ket_qua) + 1]] <- cbind(bo = b, mo_hinh = mh, lap = r, phan = f,
                                                so_chuoi_kiem_tra = sum(phan == f), mot_lan_chia(d, which(phan == f)))
      }
    }
    moi <- do.call(rbind, ket_qua)
    da_co <- if (is.null(da_co)) moi else rbind(da_co, moi)
    write.csv(da_co, tep_ct, row.names = FALSE)      # ghi sau moi bo, ngat giua chung khong mat ket qua
    cat("  xong sau", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1), "phut\n")
  }
  tom_tat_kfold(da_co, thu_muc_kq)
}

# trung binh +- do lech chuan qua cac lan chia; so lan tot hon Total series (theo MASE)
tom_tat_kfold <- function(ct, thu_muc_kq) {
  tong <- ct[ct$nhan == "Total", c("bo", "mo_hinh", "lap", "phan", "MASE")]
  names(tong)[5] <- "MASE_total"
  ct <- merge(ct, tong, by = c("bo", "mo_hinh", "lap", "phan"))
  nhom <- split(ct, list(ct$bo, ct$mo_hinh, ct$nhan, ct$phuong_phap), drop = TRUE)
  tom <- do.call(rbind, lapply(nhom, function(g) {
    data.frame(bo = g$bo[1], mo_hinh = g$mo_hinh[1], nhan = g$nhan[1], phuong_phap = g$phuong_phap[1],
               so_lan = nrow(g),
               MASE_tb = round(mean(g$MASE), 3), MASE_sd = round(sd(g$MASE), 3),
               MAPE_tb = round(mean(g$MAPE), 3), MAPE_sd = round(sd(g$MAPE), 3),
               so_lan_tot_hon_total = if (g$nhan[1] == "Total") NA else sum(g$MASE < g$MASE_total))
  }))
  thu_tu_pp <- c("xgbcls", "xgbreg", "lgbcls", "lgbreg", "ket hop ca 4", "ket hop phan loai", "ket hop hoi quy",
                 "Total series")
  tom <- tom[order(match(tom$bo, CAU_HINH$bo), tom$mo_hinh, match(tom$nhan, c("Actual", "Average", "Total")),
                   match(tom$phuong_phap, thu_tu_pp)), ]
  write.csv(tom, file.path(thu_muc_kq, "kfold_tom_tat.csv"), row.names = FALSE)
  print(tom, row.names = FALSE)
  invisible(tom)
}

# Kiem tra: lap lai dung cach chia cua tac gia (set.seed(100), 70/30) va khoang tac gia luu
# -> phai trung file *_final_res.csv da tai hien (de chac chan cach tinh MASE dung nhu tac gia)
kiem_tra_thuoc_do <- function(bo = "Weekly", mh = "ets") {
  ch <- CAU_HINH[CAU_HINH$bo == bo, ]
  d <- chuan_bi_bo(ch, mh)
  thu_muc <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo)))
  opt <- load_mot(file.path(thu_muc, paste0(ch$opt, "_", mh, "_opt_pre_result.RData")))
  fr <- read.csv(file.path(thu_muc, paste0(if (mh == "ets") ch$final_ets else ch$final_th, "_final_res.csv")))
  set.seed(100); test <- which(sample(2, d$n, replace = TRUE, prob = c(0.7, 0.3)) == 2)
  k <- thanh_khoang(opt[[1]])[test, ]
  tinh <- sapply(1:4, function(j) danh_gia(d, test, du_bao_khoang(d, test, k[, j]), k[, j])[["MASE"]])
  print(round(rbind(script = tinh, file_tac_gia = fr$V6[2:5]), 4))
}

# Cach chay:
# kiem_tra_thuoc_do()           # vai giay: 2 dong in ra phai trung nhau
# chay_kfold(k = 10)            # 10-fold, Bang 3 + 5 (+ cac dong Bang 10)
