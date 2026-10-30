# So sanh ket qua m=10 voi Bang 4, dat canh ket qua m=5 (Bang 3) da tai hien
# file final_res: dong 1 Total series; 2-5 nhan min (Actual); 6-9 nhan mean (Average)
# thu tu trong moi nhom: xgbcls, xgbreg, lgbcls, lgbreg; V5 = MAPE, V6 = MASE

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"

nhan        <- c("Total", rep("Actual", 4), rep("Average", 4))
phuong_phap <- c("Total series", rep(c("xgbcls", "xgbreg", "lgbcls", "lgbreg"), 2))

# so lieu Bang 4, thu tu: Total, Actual (4 dong), Average (4 dong)
bang4 <- list(
  Yearly = list(
    ETS    = list(MAPE = c(18.06, 17.18, 17.22, 17.32, 17.00, 16.94, 16.80, 16.84, 16.88),
                  MASE = c(3.44, 2.67, 2.70, 2.68, 2.70, 2.63, 2.67, 2.57, 2.67)),
    thetaf = list(MAPE = c(17.06, 17.11, 17.12, 17.57, 17.12, 16.84, 16.93, 16.91, 16.79),
                  MASE = c(3.40, 2.63, 2.65, 2.62, 2.65, 2.65, 2.67, 2.65, 2.67))),
  Quarterly = list(
    ETS    = list(MAPE = c(12.20, 11.86, 11.96, 11.88, 11.93, 11.96, 11.93, 11.92, 11.99),
                  MASE = c(1.15, 1.00, 0.98, 1.00, 0.98, 0.96, 0.97, 0.95, 0.98)),
    thetaf = list(MAPE = c(11.82, 12.35, 12.06, 12.58, 12.03, 12.00, 11.92, 12.00, 11.97),
                  MASE = c(1.22, 1.07, 1.00, 1.08, 1.00, 1.00, 0.99, 1.00, 1.00))),
  Monthly = list(
    ETS    = list(MAPE = c(16.83, 16.75, 16.87, 16.75, 16.89, 16.54, 16.92, 16.52, 16.92),
                  MASE = c(0.95, 0.90, 0.91, 0.91, 0.91, 0.88, 0.91, 0.88, 0.91)),
    thetaf = list(MAPE = c(15.68, 15.88, 15.71, 15.91, 15.72, 15.95, 15.68, 15.92, 15.71),
                  MASE = c(0.97, 0.92, 0.93, 0.94, 0.93, 0.90, 0.92, 0.90, 0.93)))
)

doc_kq <- function(thu_muc, mh, bo) {
  tep <- file.path(THU_MUC_KHO, "results", thu_muc, paste0(tolower(mh), "_", bo, "_final_res.csv"))
  if (!file.exists(tep)) { cat("Chua co ket qua:", tep, "\n"); return(NULL) }
  read.csv(tep)[1:9, ]
}

tat_ca <- list()
for (bo in names(bang4)) {
  for (mh in c("ETS", "thetaf")) {
    m10 <- doc_kq(paste0("tai_hien_", tolower(bo), "_m10"), mh, bo)
    if (is.null(m10)) next
    m5  <- doc_kq(paste0("tai_hien_", tolower(bo)), mh, bo)
    # ban da sua loi round5 (script 13), neu da chay
    sua <- if (dir.exists(file.path(THU_MUC_KHO, "results", paste0("tai_hien_", tolower(bo), "_m10_sua"))))
             doc_kq(paste0("tai_hien_", tolower(bo), "_m10_sua"), mh, bo) else NULL
    b <- data.frame(
      bo = bo, mo_hinh = mh, nhan = nhan, phuong_phap = phuong_phap,
      MAPE_bai_bao  = bang4[[bo]][[mh]]$MAPE,
      MAPE_tai_hien = round(m10$V5, 2),
      MASE_bai_bao  = bang4[[bo]][[mh]]$MASE,
      MASE_tai_hien = round(m10$V6, 2),
      MASE_m5_tai_hien = if (is.null(m5)) NA else round(m5$V6, 2),
      MASE_m10_sua     = if (is.null(sua)) NA else round(sua$V6, 2)
    )
    b$chenh_MASE    <- round(b$MASE_tai_hien - b$MASE_bai_bao, 2)
    b$m10_tru_m5    <- round(b$MASE_tai_hien - b$MASE_m5_tai_hien, 2)
    b$tot_hon_total <- b$MASE_tai_hien < b$MASE_tai_hien[1]
    b$tot_hon_total[1] <- NA
    tat_ca[[paste(bo, mh)]] <- b
  }
}

so_sanh <- do.call(rbind, tat_ca)
print(so_sanh, row.names = FALSE)
write.csv(so_sanh, file.path(THU_MUC_KHO, "results", "so_sanh_bang4.csv"), row.names = FALSE)
