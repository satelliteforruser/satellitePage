###APIでSentinel-2の画像を取得する①###
install.packages("mapview")
install.packages("mapedit")
install.packages("sen2r")
install.packages("sf")
install.packages("terra")

###APIでSentinel-2の画像を取得する②###
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


###対象領域の画像を表示する###

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


####STACを利用した衛星データの取得###

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


###Landsat8を用いた各種操作###

# フォルダの中の".TIF"データの一括取得
images <- list.files("フォルダ名", pattern = ".TIF")
b2_image <- rast(images[1])
b3_image <- rast(images[2])
b4_image <- rast(images[3])

# 各データのヒストグラムの表示
par(mfrow = c(3,1)) # 複数のヒストグラムの表示、各ヒストグラムの余白設定
hist(b2_image, breaks = 300, main = "BlueBand", xlim = c(1, 65545))
hist(b3_image, breaks = 300, main = "GreenBand", xlim = c(1, 65545))
hist(b4_image, breaks = 300, main = "RedBand", xlim = c(1, 65545))
dev.off() # ヒストグラムの設定を解除

# 各データの画像表示
par(mfrow = c(1,3)) # 複数の画像の表示
plot(b2_image, range = c(8000, 15000), col = topo.colors(45), main = "BlueBand", legend = FALSE)
plot(b3_image, range = c(7000, 13000), col = terrain.colors(45), main = "GreenBand", legend = FALSE)
plot(b4_image, range = c(6000, 12000), col = heat.colors(45), main = "RedBand", legend = FALSE)
dev.off() # 画像表示の設定を解除

###画像の切り出し###

##4隅の緯度経度を指定した切り出し###

# 4隅の緯度経度からポリゴンを生成
aoi <- as(raster::extent(139.7101, 139.7201, 35.6721, 35.6841), "SpatialPolygons")
aoi <- st_as_sf(aoi, crs = 4612)
st_crs(aoi) <- "+proj=longlat +ellps=WGS84 +no_defs"

mapview(aoi)

# バンド4,8の読み込み
b4_image <- rast(images[4])
b8_image <- rast(images[5])

# ファイルのnameを変更
names(b4_image) <- "b4"
names(b8_image) <- "b8"

# ファイルの座標変更
b4_image_reproject <- project(b4_image, "EPSG:4326")
b8_image_reproject <- project(b8_image, "EPSG:4326")

# 画像の切り出し
b4_image_crop <- crop(b4_image_reproject, aoi)
b8_image_crop <- crop(b8_image_reproject, aoi)

# 画像の表示
par(mfrow = c(1,2))
plot(b4_image_crop, col = gray.colors(100), legend = FALSE)
plot(b8_image_crop, col = gray.colors(100), legend = FALSE)
dev.off()

##カラー合成##

# 関心領域の生成
m2 <- editMap()
st_write(m2, "m2.geojson")

# バンド2,3,4,5の読み込み
images <- list.files("フォルダ名", pattern = ".TIF")
b2_image <- rast(images[1])
b3_image <- rast(images[2])
b4_image <- rast(images[3])
b5_image <- rast(images[4])

# ファイルのnameを変更
names(b2_image) <- "b2"
names(b3_image) <- "b3"
names(b4_image) <- "b4"
names(b5_image) <- "b5"

# ファイルの座標変更
b2_image_reproject <- project(b2_image, "EPSG:4326")
b3_image_reproject <- project(b3_image, "EPSG:4326")
b4_image_reproject <- project(b4_image, "EPSG:4326")
b5_image_reproject <- project(b5_image, "EPSG:4326")

# 画像の切り出し
b2_image_crop <- crop(b2_image_reproject, m2)
b3_image_crop <- crop(b3_image_reproject, m2)
b4_image_crop <- crop(b4_image_reproject, m2)
b5_image_crop <- crop(b5_image_reproject, m2)

# 各画像の合成
allImages <- c(b2_image_crop, b3_image_crop, b4_image_crop, b5_image_crop)

# 画像の表示
par(mfrow = c(1,3))
plotRGB(allImages, r = 3, g = 2, b = 1, stretch = "lin")
plotRGB(allImages, r = 4, g = 3, b = 2, stretch = "lin")
plotRGB(allImages, r = 3, g = 4, b = 2, stretch = "lin")
dev.off()

##パンシャープン画像##
install.packages("RStoolbox")

# パンシャープン用のバンド8の読み込み
b8_image <- rast(images[5])

# ファイルのnameを変更
names(b8_image) <- "b8"

# ファイルの座標変更
b8_image_reproject <- project(b8_image, "EPSG:4326")

# 画像の切り出し
b8_image_crop <- crop(b8_image_reproject, m2)

# パンシャープン画像の生成
allImages_pan <- RStoolbox::panSharpen(allImages, b8_image_crop, r = 3, g = 2, b = 1, method = "brovey")

# 元画像とパンシャープン画像の比較
par(mfrow = c(1,2))
plotRGB(allImages, r = 3, g = 2, b = 1, stretch = "lin")
plotRGB(allImages_pan, r = 3, g = 2, b = 1, stretch = "lin")
dev.off()
