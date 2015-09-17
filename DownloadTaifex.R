TAIFEX_F_TbyT_ZIPs_URL = "http://www.taifex.com.tw/eng/eng3/eng3_1_3.asp"
TAIFEX_FC_TbyT_ZIPs_URL = "http://www.taifex.com.tw/eng/eng3/eng3_1_5.asp"

library(httr)
library(rvest)
library(stringr)

res = GET(TAIFEX_F_TbyT_ZIPs_URL)
downloadUrls = html(res) %>% html_nodes(".table_c input") %>% html_attr("onclick")
downloadUrls = sapply(downloadUrls,function(xx) str_replace_all(xx,"window.open[(]'../..","http://www.taifex.com.tw"))
downloadUrls = sapply(downloadUrls,function(xx) str_replace_all(xx,"'[)]",""))
names(downloadUrls) = NULL

downloadUrls = downloadUrls[grep("DataInformation.doc",downloadUrls,invert = T)]

downloadFilenames = sapply(downloadUrls, function(url){
  xx = unlist(strsplit(url,"/"))
  xx[length(xx)]
})
names(downloadFilenames) = NULL

dnFilesDF = data.frame(url=downloadUrls,dest=downloadFilenames,stringsAsFactors = F)
View(downloadUrls)

DownloadPATH = "TAIFEX"

dnFilesDF$dest = sprintf("./TAIFEX/zip/%s",dnFilesDF$dest)
View(dnFilesDF)

rptFiles = apply(dnFilesDF,1,function(xx){
  do.call(download.file,as.list(xx))
  unzip(xx[2], exdir = "./TAIFEX/rpt/")
})

sapply(rptFiles,function(rptF){
  TaifexFutureTByT_df = read.csv(rptF, na.strings = "-",stringsAsFactors=FALSE)  
  TaifexFutureTByT_df$Time.of.Trades = sapply(TaifexFutureTByT_df$Time.of.Trades,function(time){
    ifelse(str_length(time) < 6,sprintf("0%s",time),time)
  })
  
  TaifexFutureTByT_df$Time = apply(TaifexFutureTByT_df,1,function(row){
    str_replace_all(paste(row[1],row[4],collapse = "")," ","")
  })
  
  TaifexFutureTByT_df$Time = strptime(TaifexFutureTByT_df$Time,"%Y%m%d%H%M%S",tz = "CST")

  rowData = cbind(time = TaifexFutureTByT_df$Time,
                  price = TaifexFutureTByT_df$Trade.Price, 
                  pcode = str_replace_all(TaifexFutureTByT_df$Product.Code," ",""),
                  exMW = str_replace_all(TaifexFutureTByT_df$Contract.Month.Week.," ",""),
                  volume=TaifexFutureTByT_df$Volume.Buy.Sell./2,
                  pNM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                  pFM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                  OCA = TaifexFutureTByT_df$Opening.Call.Auction)
  
  rowDataX = cbind(price = TaifexFutureTByT_df$Trade.Price, 
                  pcode = str_replace_all(TaifexFutureTByT_df$Product.Code," ",""),
                  exMW = str_replace_all(TaifexFutureTByT_df$Contract.Month.Week.," ",""),
                  volume=TaifexFutureTByT_df$Volume.Buy.Sell./2,
                  pNM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                  pFM = TaifexFutureTByT_df$Price.for.Nearer.Delivery.Month.Contract,
                  OCA = TaifexFutureTByT_df$Opening.Call.Auction)
  
  Xt = xts(rowDataX,TaifexFutureTByT_df$Time)
  
  dest = str_replace_all(rptF,"rpt","RData")
  save(TaifexFutureTByT_df, rowData, Xt ,file = dest)
})

