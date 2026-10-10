# 02_so_sanh_bang5.R - So sánh kết quả tái hiện với Bảng 5 của bài báo
# 
# Tệp kết quả của mã gốc có 16 dòng. Theo tác giả:
#   dòng 1      : Total series (dự báo trên toàn bộ chuỗi)
#   dòng 2-5    : nhãn theo sai số NHỎ NHẤT  (xgbcls, xgbreg, lgbcls, lgbreg)
#   dòng 6-9    : nhãn theo sai số TRUNG BÌNH (xgbcls, xgbreg, lgbcls, lgbreg)
#   dòng 10-15  : các cách kết hợp dự báo
#   dòng 16     : chọn điểm bắt đầu ngẫu nhiên
# Mục 4.1 cho biết với Weekly/Daily/Hourly, tác giả chỉ báo cáo cách gán nhãn
# theo sai số trung bình, nên Bảng 5 ứng với dòng 6-9 và dòng 1.
# Các cột V1..V6 là ME, RMSE, MAE, MPE, MAPE, MASE (hàm accuracy()).

THU_MUC_KHO <- "D:/osp-tsp-tieuluan"
KET_QUA <- file.path(THU_MUC_KHO, "results", "tai_hien_weekly")

# Số liệu Bảng 5, cột Weekly chép tay 
# c là hàm gom nhiều giá trị thành 1 danh sách
bang5 <- data.frame(
  mo_hinh     = rep(c("ETS", "thetaf"), each = 5),   #each là mỗi phần tử lặp 5 lần liên tiếp
  phuong_phap = rep(c("xgbcls", "xgbreg", "lgbcls", "lgbreg", "Total series"), 2),    #không có each thì là lặp cả danh sách 2 lần
  MAPE_bai_bao = c(9.17, 9.21, 9.22, 9.11, 9.17,  9.19, 9.25, 9.19, 9.27, 9.05),
  MASE_bai_bao = c(1.98, 2.00, 2.00, 1.99, 2.50,  2.09, 2.12, 2.04, 2.12, 2.66)
)

doc_ket_qua <- function(tep) {
  x <- read.csv(file.path(KET_QUA, tep))
  x[c(6, 7, 8, 9, 1), c("V5", "V6")]          # dòng 6-9 rồi dòng 1
}
cua_toi <- rbind(doc_ket_qua("ets_week_final_res.csv"),
                 doc_ket_qua("thetaf_weekly_final_res.csv"))

so_sanh <- cbind(bang5,
                 MAPE_tai_hien = round(cua_toi$V5, 2),
                 MASE_tai_hien = round(cua_toi$V6, 2))
so_sanh$chenh_MASE <- round(so_sanh$MASE_tai_hien - so_sanh$MASE_bai_bao, 2)

print(so_sanh, row.names = FALSE)
write.csv(so_sanh, file.path(KET_QUA, "so_sanh_bang5.csv"), row.names = FALSE)
