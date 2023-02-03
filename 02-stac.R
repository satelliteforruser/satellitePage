#####################################
#STAC(AWS)を使用した衛星データの取得#
#####################################

###基本編###

##使用するパッケージのインストール
install.packages("mapedit")
install.packages("mapview")
install.packages("rstac")
install.packages("terra")
install.packages("sf")

##使用するパッケージの読み込み
library(mapview)
library(mapedit)
library(rstac)
library(terra)
library(sf)

##対象領域の作成・設定

#editMap()を使用
aoi <- editMap()

#作成したポリゴンデータの確認
mapview(aoi)

#bboxの取得（後で使用するため）
aoi_bbox <- st_bbox(aoi)

##衛星データの取得

#利用するAPIのURLを取得
search <- stac("https://earth-search.aws.element84.com/v0")

#指定の条件下でSTAC経由でSentinel-2のデータを取得
sentinel <- search %>% 
              stac_search(collections = "sentinel-s2-l2a-cogs",
                          bbox        = c(aoi_bbox$xmin, aoi_bbox$ymin, aoi_bbox$xmax, aoi_bbox$ymax),
                          datetime    = "2017-01-01/2017-01-18") %>% 
              post_request()

#取得したデータの情報を確認
sentinel
names(sentinel)
sentinel$features[[1]]
names(sentinel$features[[1]])
sentinel$features[[1]]$properties
names(sentinel$features[[1]]$properties)
sentinel$features[[1]]$assets
names(sentinel$features[[1]]$assets)

#GDAL Virtual File Systemsを利用するためにバンド4のURL情報を読込
sentinel$features[[1]]$assets$B04$href
url <- paste0("/vsicurl/", sentinel$features[[1]]$assets$B04$href)

#URL情報を使ってバンド4をラスターデータとして読込
b04 <- rast(url)

#crsがEPSG:32654となっているためEPSG4326へ変更
b04_reproject <- b04 %>% 
                   project(., "EPSG:4326")

#読み込んだデータの確認・表示
b04_reproject
plot(b04_reproject)
plot(aoi$geometry, add = TRUE)

#対象領域だけを切り抜く場合
b04_aoi <- b04_reproject %>% 
             crop(., aoi) %>% 
             mask(., aoi)
plot(b04_aoi)

###応用編###

##使用するパッケージのインストール
install.packages("rstac")
install.packages("terra")
install.packages("tidyverse")

##使用するパッケージの読み込み
library(rstac)
library(terra)
library(tidyverse)

#利用するAPIのURLを取得
search <- stac("https://earth-search.aws.element84.com/v0")

#指定の条件下でSTAC経由でSentinel-2のデータを取得
sentinel <- search %>% 
  stac_search(collections = "sentinel-s2-l2a-cogs",
              bbox        = c(aoi_bbox$xmin, aoi_bbox$ymin, aoi_bbox$xmax, aoi_bbox$ymax),
              datetime    = "2017-01-01/2017-12-31",
              limit       = 500) %>% 
  post_request()

#取得したデータの情報を確認
sentinel

#取得データのproperties情報のみ抽出
sentinel_properties <- sentinel %>% 
                         items_reap(field = c("properties"))

#抽出したproperties情報をtibbleに変換(これで列情報を基に並び替えが可能)
df <- bind_rows(sentinel_properties)

#tibbleに変換できたか確認
df$datetime

#2017年の各月から最も雲量が少ないデータを抽出（n=12のデータを抽出）
#dfの日付データからmonthの列を作成
df$datetime <- as.Date(df$datetime)
df$month <- df$datetime %>% 
              format(., "%m") %>% 
              as.integer()

#month別にデータを抽出し、雲量で並び替えをし、最も雲量が少ないデータを抽出
minId <- c()
getMinCloudItem <- function(df, m){
  df <- df %>% 
         filter(df$month == m) %>% 
         arrange(`eo:cloud_cover`)
  df <- df[1, "sentinel:product_id"]
  return(df)
}
for (m in 1:12){
  minId <- minId %>% 
             rbind(getMinCloudItem(df, m))
}

#想定するデータが抽出できたか確認
minId

#抽出したデータのバンド4のurlを取得
url <- c()
for (x in 1:nrow(minId)){
  sentinel_selected <- sentinel %>% 
                         items_filter(`sentinel:product_id` == minId[x,])
  url <- url %>% 
           rbind(paste0("/vsicurl/", sentinel_selected$features[[1]]$assets$B04$href))
}

#取得したデータ(1月～3月)をラスターデータとして読み込み
b04_01 <- rast(url[1,])
b04_02 <- rast(url[2,])
b04_03 <- rast(url[3,])

#crsの変更
b04_01_reproject <- b04_01 %>% 
                      project(., "EPSG:4326")
b04_02_reproject <- b04_02 %>% 
                      project(., "EPSG:4326")
b04_03_reproject <- b04_03 %>% 
                      project(., "EPSG:4326")

#対象地域のみを切取
b04_01_aoi <- b04_01_reproject %>% 
                crop(., aoi) %>% 
                mask(., aoi)
b04_02_aoi <- b04_02_reproject %>% 
                crop(., aoi) %>% 
                mask(., aoi)
b04_03_aoi <- b04_03_reproject %>% 
                crop(., aoi) %>% 
                mask(., aoi)

#切り取った対象地域を確認
par(mfrow = c(1, 3))
plot(b04_01_aoi)
plot(b04_02_aoi)
plot(b04_03_aoi)
dev.off()

#ヒストグラムを確認したい
par(mfrow = c(3, 1))
hist(b04_01_aoi)
hist(b04_02_aoi)
hist(b04_03_aoi)
dev.off()
