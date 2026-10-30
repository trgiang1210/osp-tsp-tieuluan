# osp-tsp-tieuluan

Tái hiện thực nghiệm bài báo **"Optimal starting point for time series forecasting"** (OSP-TSP). Tiểu luận môn Phân tích dữ liệu, ĐH Công nghệ – ĐHQGHN. Sinh viên: Nguyễn Trường Giang.

Kho không chép code tác giả; các script chạy code gốc từ bản clone riêng và so sánh với bài báo.

## Chuẩn bị

- Clone https://github.com/feng-li/forecasting-with-optimal-starting-point (commit `d4a393c`) vào `D:/forecasting-with-optimal-starting-point` (hoặc sửa biến `THU_MUC_TAC_GIA` trong script).
- R 4.6.1, xgboost 3.2.1.1, lightgbm 4.7.0. Cài thêm: `imputeTS`, `gratis`, `feasts`, `doParallel`, `cpm`, `jsonlite`.
- `scripts/compat.R` giúp code gốc chạy với xgboost 3.x và lightgbm 4.x.

Sửa tay trong code tác giả:

| File | Sửa |
|---|---|
| `Hourly/Hourly Feature extraction.r` | dòng 4, 6, 46: `Weekly` → `Hourly` |
| `Hourly/starting_point_ets.r` | dòng 100: `12,18` → `24,48` |
| `exchange_rate_M4.r`, `exchange_rate_Train_by_oneself.r` | thêm sau dòng 14: `data=as.data.frame(lapply(data, function(x) as.numeric(as.character(x))))` |
| `Imports_Train_by_oneself.r`, `exchange_rate_Train_by_oneself.r` | bỏ `#` dòng 169–172, 174–177, 184–187 |
| `m=10/Monthly/thetaf_opt_forecast.r` | dòng 30: `m=5` → `m=10` |

## Scripts

| Script | Bảng |
|---|---|
| `01`, `02`, `03`, `04` | 3, 5 |
| `05` | 10 |
| `07`, `08`, `09` | 7, 8, 9 |
| `10`, `11`, `13` (sửa `round5`) | 4 |
| `12`, `14` | 12 |
| `15` | 13 |
| `16` | 11 |
| `18` | Kiểm định chéo 10 phần cho Bảng 3, 5, 10 |

Cách chạy ghi trong chú thích của từng script.

## Lấy mẫu (`set.seed(2026)`)

| Bộ | Bảng 3, 4, 5 | Bảng 11 |
|---|---|---|
| Yearly | 23.000 (đủ) | 1.000 |
| Quarterly | 1.000 | 1.000 |
| Monthly | 1.000 | 500 |
| Weekly | 359 (đủ) | 359 (đủ) |
| Daily | 4.227 (đủ) | 500 |
| Hourly | 200 | 414 (đủ) |

Bảng 9 dùng 5 chuỗi GRATIS thay vì 50. Không tái hiện Bảng 6 và bước huấn luyện FFORMA.

## Kết quả

Trong `results/`; file `so_sanh_bang*.csv` đặt số tái hiện cạnh số bài báo. File `.RData` trung gian không được đưa lên do dung lượng.
