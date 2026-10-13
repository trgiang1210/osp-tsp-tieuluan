# Sua loi khi chay code goc voi xgboost 3.x va lightgbm 4.x

# xgboost 3.x khong nhan cach goi xgboost(data, nround, ...) cu nua
# -> chuyen sang xgb.train
xgboost <- function(data, nround, ...) {
  xgboost::xgb.train(params = list(...), data = data,
                     nrounds = nround, verbose = 0)
}

# lightgbm 4.x tra ve ma tran khi du bao nhieu lop, ban cu tra ve vector
# -> doi lai thanh vector de code goc doc dung
predict <- function(object, ...) {
  p <- stats::predict(object, ...)
  if (inherits(object, "lgb.Booster") && is.matrix(p)) p <- as.vector(t(p))
  p
}
