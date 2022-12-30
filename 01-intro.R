library(mapview)
library(mapedit)
library(sen2r)
library(sf)
library(terra)

# editMap()で関心領域の座標情報を取得
m <- editMap()
sf::st_write(m, dsn = "m.geojson") # 取得した座標情報をgeojsonとして保存

# mapview()で取得した座標情報の確認
mapview(m)

# SentinelのOpen Access Hubへのログイン
username <- "*********"
password <- "*********"
write_scihub_login(username, password)

# 指定の条件下でSentinel-2のデータを取得
products <- s2_list(spatial_extent = m,
                    time_interval  = as.Date(c("2021-11-10", "2021-11-20")),
                    time_period    = "full",
                    level          = "L2A",
                    availability   = "check",
                    max_cloud      = 100)

# 取得した衛星データ数の確認（n = 2）
length(products)

# データフレームに変換し、被雲率の低い順に並び替える
products_gdf <- as.data.frame(products)
products_gdf_sorted <- products_gdf[order(products_gdf$clouds, decreasing = FALSE), ]
products_gdf_sorted # 並び替え結果を確認し、取得したい衛星データの行を確認

# 取得したい衛星データをダウンロード（ファイルが重いため時間を要する）
s2_download(products[1], order_lta = TRUE) # s2_listの結果を格納した変数を利用

###########################################################

# 取得した衛星データ（バンド2, 3, 4）を読み込み
b4 <- rast("..\\S2A_MSIL2A_20211113T012911_N0301_R074_T54SUE_20211113T040103.SAFE\\GRANULE\\L2A_T54SUE_A033390_20211113T012913\\IMG_DATA\\R10m\\T54SUE_20211113T012911_B04_10m.jp2")
b3 <- rast("..\\S2A_MSIL2A_20211113T012911_N0301_R074_T54SUE_20211113T040103.SAFE\\GRANULE\\L2A_T54SUE_A033390_20211113T012913\\IMG_DATA\\R10m\\T54SUE_20211113T012911_B03_10m.jp2")
b2 <- rast("..\\S2A_MSIL2A_20211113T012911_N0301_R074_T54SUE_20211113T040103.SAFE\\GRANULE\\L2A_T54SUE_A033390_20211113T012913\\IMG_DATA\\R10m\\T54SUE_20211113T012911_B02_10m.jp2")

# 取得した衛星データのnameを変更
names(b4) <- "b4"
names(b3) <- "b3"
names(b2) <- "b2"

# 取得した衛星データを一つの変数に統合
rgb <- c(b4, b3, b2)

# 取得した衛星データを対象地域のみに切り取り
rgb_reproject <- project(rgb, "EPSG:4326") # rgbのcrsを一般的なEPSG:4326へと変更
rgb_crop <- crop(x = rgb_reproject, y = m)

# 取得した衛星データをヒストグラムで表示
par(mfrow = c(1,3), mar = c(5, 5, 5, 1)) # 複数のヒストグラムの表示、各ヒストグラムの余白設定
hist(rgb_crop[,,1], col = "red", main = "Red")
hist(rgb_crop[,,2], col = "green", main = "Green")
hist(rgb_crop[,,3], col = "blue", main = "Blue")
dev.off() # ヒストグラムの設定を解除

# 取得した衛星データをRGB表示
plotRGB(rgb_crop, axes = TRUE, stretch = "lin")

###########################################################

# STACの利用に必要なrstacパッケージをインストール
install.packages("rstac")
library(rstac)

# 利用するAPIのURLを取得
search <- stac("https://earth-search.aws.element84.com/v0")

# mのbounding boxを確認
m

# 指定の条件下でSTAC経由でSentinel-2のデータを取得
result <- search %>% 
            stac_search(collections = "sentinel-s2-l2a-cogs",
                        bbox        = c(139.7049, 35.47745, 139.8573, 35.66065),
                        datetime    = "2021-11-10/2021-11-20") %>% 
            post_request() %>% 
            items_filter(`eo:cloud_cover` < 100)

# 取得したデータを確認（n = 2）
result

# 取得したSentinel-2データについて被雲率を確認
result$features[[1]]$properties$`eo:cloud_cover`
result$features[[2]]$properties$`eo:cloud_cover`

# 被雲率が低いデータ（0.02）を取得
result_designated <- result %>% 
                       items_filter(`eo:cloud_cover` < 10)

# 被雲率が低いSentinel-2データのB4,3,2をダウンロード
download_B04 <- result_designated %>% 
                  assets_download(asset_names = "B04", overwrite = TRUE)

download_B03 <- result_designated %>% 
                  assets_download(asset_names = "B03", overwrite = TRUE)

download_B02 <- result_designated %>% 
                  assets_download(asset_names = "B02", overwrite = TRUE)

# 取得した衛星データ（バンド2, 3, 4）を読み込み
stac_b04 <- rast("B04.tif")
stac_b03 <- rast("B03.tif")
stac_b02 <- rast("B02.tif")

# 取得した衛星データのnameを変更
names(stac_b04) <- "b4"
names(stac_b03) <- "b3"
names(stac_b02) <- "b2"

# 取得した衛星データを一つの変数に統合
stac_rgb <- c(stac_b04, stac_b03, stac_b02)

# 取得した衛星データを対象地域のみに切り取り
stac_rgb_reproject <- project(stac_rgb, "EPSG:4326")
stac_rgb_crop <- crop(x = stac_rgb_reproject, y = m)

# 取得した衛星データをRGB表示
plotRGB(stac_rgb_crop, axes = TRUE, stretch = "lin")
