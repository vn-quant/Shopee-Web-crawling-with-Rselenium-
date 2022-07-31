
############## rselenium
library(rvest)
library(tidyverse)
library(fable)
library(fabletools)
library(tsibble)
library(ggtext)
library(janitor)
library(RSelenium)
library(netstat)
library(tictoc)
library(beepr)
############# login infor 
username <- "******@gmail.com"
password <- "******"


rs_driver_object <- rsDriver(
  browser = "chrome",
  chromever = "103.0.5060.53",
  verbose = F,
  port = free_port()
)

remDr <- rs_driver_object$client

#remDr$open()
remDr$open()
remDr$maxWindowSize()
remDr$navigate("https://shopee.vn/buyer/login?next=https%3A%2F%2Fshopee.vn%2F")


session_username <- remDr$findElement(using = "class", "pDzPRp")
session_username$clickElement()
session_username$sendKeysToElement(list(username))

#### password
session_password <- remDr$findElement(using = "name", "password")
session_password$clickElement()
session_password$sendKeysToElement(list(password,
                                        key = "enter"))


### mi pham
remDr$navigate("https://shopee.vn/S%E1%BA%AFc-%C4%90%E1%BA%B9p-cat.11036279")
#remDr$findElement()

### chuyen sang tieng anh 


### lay link cac san pham


link_path <- remDr$findElements(using = "xpath", '//a[@data-sqe="link"]')

link <- rep(NA,length(link_path))
for(i in 1:length(link_path)){
  link[i] <- link_path[[i]]$getElementAttribute('href') %>% unlist()
}


###### scrape

tic()

data_overal <- data.frame() # store data
#data_crawl <- data.frame() # u dont need it, i use it when testing 
waiting_time <- 3000 #(in miliseconds)

for (i in 1:length(link[1:5])) {
  i = 10
  ### open link
  remDr$navigate(link[i])
  remDr$setTimeout(type = "implicit", milliseconds = waiting_time) # set wait time 
  
  ### product name
  text_R <- remDr$findElements(using = "xpath",'//*[@class="VCNVHn"]')
  text <- text_R[[1]]$getElementText() %>% unlist()
  text
  
  ### product star review
  reivew_star_R<- remDr$findElements(using = "class name", "product-rating-overview__rating-score")
  review_star = reivew_star_R[[1]]$getElementText() %>% unlist()
  
  ### mall or not
  mall <- remDr$findElements(using = "xpath",'//a[@class="ofs-header__page-name"]') 
  # mall[[1]]$getElementAttribute('href') %>% unlist() get link mall
  shopee_mall <- case_when( length(mall) == 1 ~ "Shopee Mall",
                     length(mall) == 0 ~ "Not shopee Mall")
  
  ### sold
  sold_R <- remDr$findElements(using = "class name", "_45NQT5")
  sold <- sold_R[[1]]$getElementText() %>% unlist()
  
  ### addition information 
  shop_inf_R <- remDr$findElements(using = "class name", "_32ZDbL")
  
  review <- shop_inf_R[[1]]$getElementText() %>% unlist() # review
  product <- shop_inf_R[[2]]$getElementText() %>% unlist() # products left
  respone_rate <- shop_inf_R[[3]]$getElementText() %>% unlist() # Response Rate
  respone_time <- shop_inf_R[[4]]$getElementText() %>% unlist() # Response Time
  joined <- shop_inf_R[[5]]$getElementText() %>% unlist() # Joined
  followed <- shop_inf_R[[6]]$getElementText() %>% unlist() # Follower
  
  ### shop name
  shop_name_R<- remDr$findElements(using = "class name", "_6HeM6T")
  shop_name <- shop_name_R[[1]]$getElementText() %>% unlist()
  
  ### utilites
  utilities_R <- remDr$findElements(using = "class name", case_when( length(mall) == 1 ~ "X343QF",
                                                                     length(mall) == 0 ~ "zevbuo"))
  utilities <- ""
  for(j in 1:length(utilities_R)){
    utilities <- paste(utilities,
                       "-",
                       utilities_R[[j]]$getElementText() %>% unlist()
                       )
  }
  
  ### price
  price_R <- remDr$findElements(using = "class name","pmmxKx") 
  (price <- price_R[[1]]$getElementText() %>% unlist())

  ### let put all together 
  data_crawl <- data.frame(text,price,shopee_mall,sold,review,product,respone_rate,respone_time,joined,followed,shop_name,utilities,link_product = link[i]) 
  data_overal <- rbind(data_overal,data_crawl)

}

toc()

View(data_overal)
beep(3)

data_overal %<>% 
  mutate(link_product = link)

data_overal 

file_name <- paste0("data_shopee ",Sys.Date(),".csv")
write.csv(data_overal,file_name)




