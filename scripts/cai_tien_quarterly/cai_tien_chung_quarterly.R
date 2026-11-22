# Phan dung chung cho cac buoc cai tien (M4 Quarterly).
# File nay KHONG tu chay thuc nghiem: cac file cai_tien_1_..., cai_tien_2_..., cai_tien_4_... (cung thu muc scripts/cai_tien_quarterly) goi source() file nay.
#
# Lam:
#   - doc du lieu da luu khi tai hien (KHONG chay lai du bao 20 diem): *_datalist.RData, ma tran dac trung
#   - chia 70/30 giong tac gia (set.seed(100), sample(2, ...))
#   - ham chuan_bi(ten_mh): tinh MASE mau so co dinh (A), gan nhan, huan luyen 8 mo hinh nhu tac gia
#   - ham them(): them mot dong vao bang ket qua, kem kiem dinh Wilcoxon so voi mot dong moc
#
# A. Co dinh mau so MASE: moi du bao (ke ca du bao tu chuoi da cat) chia cho sai so naive cua
#    CHUOI DAY DU. Code tac gia chia cho sai so naive cua chuoi da cat (starting_point_ets.r,
#    predres cua Quarterly_ETS_thetaf_final_result.r), con dong Total dung chuoi day du.

if (!exists("THU_MUC_KHO")) THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

# --- tham so cua bo du lieu (Quarterly, tan suat 4, h = 8) ---
BO        <- "Quarterly"
TIEN_TO   <- NA            # tien to ten file datalist: tu tim ben duoi (vd Quarterly_ets_datalist.RData)
M         <- 5             # so khoang
N         <- 4             # so diem trong moi khoang
CAT_6_COT <- TRUE          # file ets_opt_forecast.r bo 6 cot thoi gian chay cuoi cua ma tran dac trung

suppressMessages({library(xgboost); library(lightgbm)})
if (!exists("M4")) suppressMessages(library(M4comp2018))
source(file.path(THU_MUC_KHO, "scripts", "compat.R"))

thu_muc_vao <- file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(BO)))
thu_muc_ra  <- file.path(THU_MUC_KHO, "results", "cai_tien", tolower(BO))
dir.create(thu_muc_ra, recursive = TRUE, showWarnings = FALSE)

# tien to cua file datalist: tu tim, vi ten file tuy cach luu khi tai hien
for (tt in c("Quarterly", "quarter", "quarterly"))
  if (is.na(TIEN_TO) && file.exists(file.path(thu_muc_vao, paste0(tt, "_ets_datalist.RData")))) TIEN_TO <- tt
if (is.na(TIEN_TO)) stop("Khong thay file *_ets_datalist.RData trong ", thu_muc_vao, ". Cac file hien co:\n  ",
                         paste(list.files(thu_muc_vao), collapse = "\n  "))
cat("File datalist:", paste0(TIEN_TO, "_ets_datalist.RData"), "\n")

# ============================ ham phu ============================

doc_mot_doi_tuong <- function(path) {
  env <- new.env(parent = emptyenv())
  ten <- load(path, envir = env)
  if (length(ten) != 1L) stop("Can dung 1 doi tuong trong ", path)
  env[[ten[[1L]]]]
}

# trung binh cac gia tri huu han (giong mean1 cua tac gia)
tb_huu_han <- function(x) mean(x[is.finite(x)])

# mau so MASE: sai so tuyet doi trung binh cua du bao naive tren chuoi day du
# (tan suat 1: do lech bac 1; tan suat > 1: do lech mua vu, giong mac dinh cua forecast::accuracy).
# Buoc kiem tra ben duoi so voi MASE cu cua diem bat dau (1,1) de chac cach tinh dung.
LAG_MS <- NULL    # do lech dung cho mau so; NULL = mac dinh (tan suat > 1: do lech mua vu). chuan_bi() doi sang 1 neu file tai hien dung do lech bac 1
mau_so_naive <- function(x, freq) {
  lag <- if (!is.null(LAG_MS)) LAG_MS else if (freq > 1) freq else 1
  mean(abs(diff(as.numeric(x), lag = lag)))
}

# giong whichmin / meanunique cua tac gia (ets_opt_forecast.r)
whichmin <- function(x) { minx <- min(x[x > 0]); which(x == minx)[1] - 1 }
meanunique <- function(x) mean(unique(x))

# gan nhan 0..m-1 tu mang MASE (chuoi x m x n), giong tac gia
gan_nhan <- function(MASE) {
  so_chuoi <- dim(MASE)[1]
  nhan_min <- numeric(so_chuoi); nhan_mean <- numeric(so_chuoi)
  for (i in 1:so_chuoi) {
    dem <- MASE[i, , ]
    gt_min <- apply(dem, 1, min)
    if (max(gt_min, na.rm = TRUE) == 0) {
      nhan_min[i] <- 0
    } else {
      gt_min[is.na(gt_min)] <- 100
      nhan_min[i] <- whichmin(gt_min)
    }
    gt_mean <- apply(dem, 1, meanunique)
    if (max(gt_mean, na.rm = TRUE) == 0) {
      nhan_mean[i] <- 0
    } else {
      gt_mean[is.na(gt_mean)] <- 100
      nhan_mean[i] <- whichmin(gt_mean)
    }
  }
  list(min = nhan_min, mean = nhan_mean)
}

# dua ket qua predict() cua mo hinh phan loai ve ma tran (chuoi x lop), kiem tra tong moi hang = 1.
# Tranh loi xao tron xac suat khi thu vien tra vector theo thu tu khac nhau.
ma_tran_xac_suat <- function(p, so_chuoi, so_lop) {
  if (is.matrix(p) && all(dim(p) == c(so_chuoi, so_lop))) {
    P <- p
  } else {
    p <- as.numeric(p)
    stopifnot(length(p) == so_chuoi * so_lop)
    Pa <- matrix(p, so_chuoi, so_lop, byrow = TRUE)   # moi chuoi lien tiep nhau
    Pb <- matrix(p, so_chuoi, so_lop)                 # moi lop lien tiep nhau
    ok <- function(P) all(abs(rowSums(P) - 1) < 1e-4)
    if (ok(Pa)) P <- Pa else if (ok(Pb)) P <- Pb else stop("Khong ghep duoc ma tran xac suat")
  }
  stopifnot(all(abs(rowSums(P) - 1) < 1e-4))
  P
}

round5 <- function(x) pmin(pmax(round(x), 1), 5)   # giong round5 cua tac gia (m = 5)

# huan luyen 8 mo hinh nhu tac gia, dua ra du doan khoang cho moi chuoi
chay_osp <- function(feat, nhan, index, m) {
  tong_hop <- list()
  hoc <- as.matrix(feat[index == 1, ])
  tat_ca <- as.matrix(feat)
  dall <- xgb.DMatrix(data = tat_ca)
  so_chuoi <- nrow(feat)
  for (loai in c("min", "mean")) {
    tl <- nhan[[loai]][index == 1]
    dxr <- xgb.DMatrix(data = hoc, label = as.matrix(tl))
    dxc <- xgb.DMatrix(data = hoc, label = as.numeric(tl))
    dlr <- lgb.Dataset(data = hoc, label = as.matrix(tl))
    dlc <- lgb.Dataset(data = hoc, label = as.numeric(tl))

    xgb_reg <- xgboost(data = dxr, nround = 100)
    xgb_cl  <- xgboost(data = dxc, nround = 100, objective = "multi:softmax", num_class = m)
    lgb_reg <- lgb.train(data = dlr, nrounds = 100)
    lgb_cl  <- lgb.train(data = dlc, nrounds = 100,
                         params = list(objective = "multiclass", num_class = m, metric = "multi_logloss"))

    xgbreg <- predict(xgb_reg, dall)
    xgbcls <- predict(xgb_cl, dall)
    lgbreg <- predict(lgb_reg, tat_ca)
    P_lgb  <- ma_tran_xac_suat(predict(lgb_cl, tat_ca), so_chuoi, m)
    lgbcls <- max.col(P_lgb, ties.method = "first") - 1

    # nhan 0..m-1 -> khoang 1..m, round5 cho hai mo hinh hoi quy (nhu tac gia)
    du_doan <- cbind(xgbcls = xgbcls + 1, xgbreg = round5(xgbreg + 1),
                     lgbcls = lgbcls + 1, lgbreg = round5(lgbreg + 1))
    tong_hop[[loai]] <- list(du_doan = du_doan, xs_lgb = P_lgb)   # xs_lgb: xac suat 5 khoang, dung cho ket hop mem
  }
  tong_hop
}

# du bao cua tung khoang = trung binh cac du bao khac nhau cua cac diem trong khoang (giong meanpre)
du_bao_khoang <- function(predh) {
  d <- dim(predh)
  fc <- array(NA_real_, c(d[1], d[2], d[4]))
  for (k in 1:d[1]) for (i in 1:d[2]) {
    mat <- unique(matrix(predh[k, i, , ], nrow = d[3]))
    fc[k, i, ] <- colMeans(mat)
  }
  fc
}

# MASE (mau so co dinh) cua tung chuoi, fc la ma tran (chuoi x h)
mase_chuoi <- function(fc, real, scale_x) rowMeans(abs(real - fc)) / scale_x

# vi tri bat dau cua 5 khoang, giong length5 cua tac gia (Quarterly_ETS_thetaf_final_result.r)
dau_khoang <- function(x, m) {
  L <- length(x)
  m_l <- sum(as.integer((1:L) * m / L) == 0)
  seq.int(0, by = m_l, length.out = m)
}

# MASE theo cach cu cua tac gia (predres): mau so tinh tren chuoi cat tu dau khoang da chon
mase_cach_cu <- function(fc, ks, locs) {
  sapply(seq_along(ks), function(a) {
    k <- ks[a]; x <- as.numeric(chuoi[[k]]$x)
    st <- vi_tri_khoang[k, locs[a]]
    y <- if (st == 0) x else x[st:length(x)]     # x[0:L] cua tac gia = chuoi day du
    mean(abs(real_xx[k, ] - fc[a, ])) / mau_so_naive(y, FREQ)
  })
}

# Wilcoxon cap (2 phia) tren cac chuoi co MASE huu han o ca hai phuong phap
p_wilcoxon <- function(a, b) {
  ok <- is.finite(a) & is.finite(b)
  suppressWarnings(wilcox.test(a[ok], b[ok], paired = TRUE, exact = FALSE)$p.value)
}

# ============================ du lieu ============================

chuoi <- Filter(function(l) l$period == BO, M4)
tep_ds <- file.path(thu_muc_vao, "danh_sach_chuoi.txt")
if (file.exists(tep_ds)) {
  ds <- readLines(tep_ds)
  chuoi <- Filter(function(l) l$st %in% ds, chuoi)
}
so_chuoi <- length(chuoi)
H    <- chuoi[[1]]$h
FREQ <- frequency(chuoi[[1]]$x)
cat(BO, ": so chuoi =", so_chuoi, "| h =", H, "| tan suat =", FREQ, "\n")

real_xx <- t(sapply(chuoi, function(l) as.numeric(l$xx)))
scale_x <- sapply(chuoi, function(l) mau_so_naive(l$x, FREQ))
vi_tri_khoang <- t(sapply(chuoi, function(l) dau_khoang(l$x, M)))   # chuoi x m

feat <- read.csv(file.path(thu_muc_vao, paste0(BO, "_features_matrix.csv")))
feat <- if (CAT_6_COT) feat[, 2:(ncol(feat) - 6)] else feat[, 2:ncol(feat)]
stopifnot(nrow(feat) == so_chuoi)

# cung cach chia 70/30 voi tac gia (set.seed(100), sample(2, ...))
set.seed(100)
index <- sample(2, nrow(feat), replace = TRUE, prob = c(0.7, 0.3))
test_idx <- which(index == 2)
nt <- length(test_idx)
cat("Huan luyen:", sum(index == 1), "chuoi | Kiem tra:", nt, "chuoi\n")

# ============================ ket qua tai hien cu ============================

cach_doc_cu <- function(ten_mh) {
  # file ket qua tai hien cua tac gia: ets_Quarterly_final_res.csv, thetaf_Quarterly_final_res.csv
  ten_tep <- paste0(ten_mh, "_", BO, "_final_res.csv")
  tep <- file.path(thu_muc_vao, ten_tep)
  if (!file.exists(tep)) return(NULL)
  read.csv(tep)
}

# dong cua file ket qua cu cua tac gia (16 dong); chi lay cac dong can so sanh
dong_cu <- c(Total = 1,
             xgbcls_min = 2, xgbreg_min = 3, lgbcls_min = 4, lgbreg_min = 5,
             xgbcls_mean = 6, xgbreg_mean = 7, lgbcls_mean = 8, lgbreg_mean = 9,
             Random = 16)

# ============================ chuan bi cho mot mo hinh co so ============================

# doc datalist, tinh MASE mau so co dinh (A), gan nhan, huan luyen 8 mo hinh.
# Tra ve list d dung cho cac buoc.
chuan_bi <- function(ten_mh) {
  cat("\n==========", toupper(ten_mh), "==========\n")
  dl <- doc_mot_doi_tuong(file.path(thu_muc_vao, paste0(TIEN_TO, "_", ten_mh, "_datalist.RData")))
  pred_cu <- dl[[1]]            # chuoi x m x n x 6 chi so accuracy() (MASE o vi tri 6, mau so tren chuoi da cat)
  predh   <- dl[[2]]            # chuoi x m x n x h du bao cua 20 chuoi con
  stopifnot(dim(predh)[1] == so_chuoi, dim(predh)[2] == M, dim(predh)[3] == N, dim(predh)[4] == H)

  # A: MASE voi mau so co dinh cho 20 diem bat dau
  MASE_moi <- array(NA_real_, dim = dim(predh)[1:3])
  for (k in 1:so_chuoi) for (i in 1:M) for (j in 1:N)
    MASE_moi[k, i, j] <- mean(abs(real_xx[k, ] - predh[k, i, j, ])) / scale_x[k]

  # kiem tra: diem (1,1) la chuoi day du nen MASE moi phai trung MASE cu
  lech <- max(abs(MASE_moi[, 1, 1] - pred_cu[, 1, 1, 6]), na.rm = TRUE)
  cat("Kiem tra mau so (do lech", if (is.null(LAG_MS)) FREQ else LAG_MS, "): lech toi da cua diem (1,1) so voi MASE cu =", signif(lech, 3), "\n")
  if (lech > 1e-6 && is.null(LAG_MS) && FREQ > 1) {
    # thu do lech bac 1 (phien ban goi forecast cu co the tinh MASE khong theo mua vu)
    LAG_MS <<- 1
    scale_x <<- sapply(chuoi, function(l) mau_so_naive(l$x, FREQ))
    for (k in 1:so_chuoi) for (i in 1:M) for (j in 1:N)
      MASE_moi[k, i, j] <- mean(abs(real_xx[k, ] - predh[k, i, j, ])) / scale_x[k]
    lech <- max(abs(MASE_moi[, 1, 1] - pred_cu[, 1, 1, 6]), na.rm = TRUE)
    cat("Thu do lech bac 1: lech toi da =", signif(lech, 3), "\n")
    if (lech > 1e-6) { LAG_MS <<- NULL; scale_x <<- sapply(chuoi, function(l) mau_so_naive(l$x, FREQ)) }
  }
  if (lech > 1e-6) stop("Mau so naive khong trung code tac gia (da thu do lech mua vu va bac 1): gui ket qua nay cho Claude")
  cat("Mau so dang dung: do lech", if (is.null(LAG_MS)) FREQ else LAG_MS, "\n")

  nhan <- gan_nhan(MASE_moi)
  osp <- chay_osp(feat, nhan, index, M)

  fc_ts <- du_bao_khoang(predh)[test_idx, , , drop = FALSE]     # chuoi kiem tra x m x h
  real_t <- real_xx[test_idx, , drop = FALSE]; sc_t <- scale_x[test_idx]
  mase_kh <- sapply(1:M, function(i) mase_chuoi(fc_ts[, i, ], real_t, sc_t))   # nt x m
  khoang_tran <- apply(mase_kh, 1, which.min)

  list(ten_mh = ten_mh, pred_cu = pred_cu, predh = predh, osp = osp, fc_ts = fc_ts,
       real_t = real_t, sc_t = sc_t, mase_kh = mase_kh, khoang_tran = khoang_tran)
}

# du bao cuoi cua chuoi kiem tra khi chon khoang loc (vector do dai nt)
du_bao_chon <- function(d, loc) t(sapply(1:nt, function(a) d$fc_ts[a, loc[a], ]))

# ============================ bang ket qua ============================

mo_bang <- function() { b <- new.env(); b$hang <- list(); b$mase <- list(); b }

# them mot dong; so_voi = ten dong moc de kiem dinh Wilcoxon (NA = khong kiem dinh)
them <- function(b, d, ten, nhom, fc, nhan_loai = NA, khoang_chon = NULL, so_voi = NA) {
  ms <- mase_chuoi(fc, d$real_t, d$sc_t)
  khoa <- if (is.na(nhan_loai)) ten else paste0(ten, "_", nhan_loai)
  b$mase[[khoa]] <- ms
  b$hang[[length(b$hang) + 1]] <- data.frame(
    mo_hinh = d$ten_mh, nhom = nhom, phuong_phap = ten, nhan = nhan_loai,
    MASE_moi = tb_huu_han(ms),
    so_voi = so_voi,
    p_wilcoxon = if (is.na(so_voi)) NA else p_wilcoxon(ms, b$mase[[so_voi]]),
    ty_le_dung_khoang_tran = if (is.null(khoang_chon)) NA else mean(khoang_chon == d$khoang_tran),
    stringsAsFactors = FALSE)
}

# cac dong dung chung: Total va 8 bien the OSP goc (so voi Total)
them_total_osp <- function(b, d) {
  them(b, d, "Total series", "Moc", d$predh[test_idx, 1, 1, , drop = TRUE])
  for (loai in c("min", "mean")) {
    dd <- d$osp[[loai]]$du_doan[test_idx, , drop = FALSE]
    for (mh in colnames(dd))
      them(b, d, mh, "OSP goc (mau so A)", du_bao_chon(d, dd[, mh]), loai, dd[, mh], so_voi = "Total series")
  }
}

them_tran <- function(b, d) {
  them(b, d, "Tran: khoang tot nhat (TB 4 diem)", "Tran (tham khao)", du_bao_chon(d, d$khoang_tran))
}

bang_ket_qua <- function(b) do.call(rbind, b$hang)
bang_tung_chuoi <- function(b, d) do.call(data.frame, c(
  list(mo_hinh = d$ten_mh, chuoi = sapply(chuoi[test_idx], function(l) l$st)),
  b$mase, list(check.names = FALSE)))

ghi <- function(ds_bang, ten_tep) {
  write.csv(do.call(rbind, ds_bang), file.path(thu_muc_ra, ten_tep), row.names = FALSE)
}
