############################
#対象領域（ポリゴン）の作成#
############################

###インタラクティブに作成###

##使用するパッケージのインストール
install.packages("mapview")
install.packages("mapedit")
install.packages("sf")

##使用するパッケージの読み込み
library(mapview)
library(mapedit)
library(sf)

##editMap()を使用
aoi <- editMap() #地図が開く

##作成したポリゴンデータの確認
mapview(aoi)

##作成したポリゴンデータの保存と読込
st_write(aoi, dsn = "aoi.geojson") #geojsonとして保存
aoi <- st_read("aoi.geojson")

###4隅の緯度経度から作成###

##使用するパッケージのインストール
install.packages("mapview")
install.packages("sf")
install.packages("raster")

##使用するパッケージの読み込み
library(mapview)
library(sf)
library(raster)

##4隅の緯度経度からポリゴンデータの作成
aoi <- as(extent(139.7045, 139.8573, 35.47745, 35.66065), "SpatialPolygons")
aoi <- st_as_sf(aoi, crs = 4612)
st_crs(aoi) <- "+proj=longlat +ellps=WGS84 +no_defs"

##作成したポリゴンデータの確認
mapview(aoi)

##作成したポリゴンデータの保存と読込の方法
st_write(aoi, dsn = "aoi.geojson") #geojsonとして保存
aoi <- st_read("aoi.geojson")


###End