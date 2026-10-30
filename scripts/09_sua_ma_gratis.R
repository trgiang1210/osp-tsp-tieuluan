# Sua code *_gratis5.r cua tac gia (Bang 9) ngay khi doc vao.
# File cua tac gia tren o dia giu nguyen; script 07 goi ham nay truoc khi chay.

# so chuoi GRATIS sinh cho moi chuoi that (code goc dat n_g=50)
N_G <- 5

# noi luu chuoi GRATIS da sinh, de khong phai sinh lai neu chay lai
THU_MUC_GRATIS <- file.path(THU_MUC_KHO, "results", "bang9", "chuoi_gratis")

sua_ma_gratis <- function(ma, ten) {
  # 1. giam so chuoi mo phong
  ma <- sub("^n_g *= *[0-9]+ *$", paste0("n_g=", N_G), ma)

  # 2. Goldtstrain, Goldf duoc dung nhung khong duoc dinh nghia o dau trong code goc
  #    generate_ts_with_target() tim ham my_features o moi truong global
  k <- max(grep("^n_g *=", ma))
  ma <- append(ma, c("Goldtstrain=data_tstrain; Goldf=data_features",
                     "assign('my_features', my_features, envir = globalenv())"), k)

  # 3. dong in bien MASEets dat truoc khi bien nay duoc tao
  ma <- ma[!grepl("^MASEets *$", ma)]

  # 4. luu chuoi GRATIS vao results/bang9/chuoi_gratis (ten file co n_g);
  #    neu file da co thi bo qua buoc sinh chuoi
  dir.create(THU_MUC_GRATIS, recursive = TRUE, showWarnings = FALSE)
  tep <- file.path(THU_MUC_GRATIS, paste0(ten, "_ng", N_G, ".RData"))
  ma <- sub("^save\\(gratislist.*$", sprintf("save(gratislist, file = '%s')", tep), ma)
  ma <- sub("^gratislist <- load_single_object\\(.*$", sprintf("gratislist <- load_single_object('%s')", tep), ma)
  a <- grep("^gratislist=list\\(\\)", ma)[1]
  b <- grep("^save\\(gratislist", ma)[1]
  ma <- append(ma, "}", b)
  ma <- append(ma, c(sprintf("if (file.exists('%s')) {", tep),
                     sprintf("  cat('Dung lai chuoi GRATIS da sinh: %s\\n')", basename(tep)),
                     "} else {"), a - 1)

  # 5. exchange_rate.csv bi doc thanh chu, doi sang so
  if (ten == "exchange_rate") {
    j <- grep("^data=data\\[3:276", ma)[1]
    ma <- append(ma, "data=as.data.frame(lapply(data, function(x) as.numeric(as.character(x))))", j)
  }
  ma
}
