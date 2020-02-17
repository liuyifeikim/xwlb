#内页链接抓取
in_url_scraping_fun <- function(i){                                 #i为页码
  out_url <- str_glue("http://www.xwlb.net.cn/video_{i}.html")      #列表页url
  page <- read_html(out_url)                                        #读取列表页内容
  page %>% html_nodes("h2 a") %>% html_attr("href") -> in_url       #提取每页的内页链接
}

#新闻内容抓取
news_scraping_fun <- function(in_url){
  #读取网页
  news <- read_html(in_url)
  #提取日期
  news %>% 
    html_node(".xwlb h2") %>% 
    html_text(trim = TRUE) %>% 
    str_replace_all(c("《新闻联播》主要内容" = "",  "[年|月|日]" = "")) %>% 
    ymd() -> date
  #提取内容
  news %>% 
    html_node(".content") %>% 
    html_text(trim = TRUE) %>% 
    str_replace_all(c("\\W" = "", "\\d" = "", "[a-zA-Z]" = "")) -> content
    news_tib <- tibble(date, content)
}

#总函数
all_scraping_fun_new <- function(){
  #末页页码
  out_url <- str_glue("http://www.xwlb.net.cn/video_1.html")        #列表页url
  page <- read_html(out_url)                                        #读取列表页内容
  last_page <- page %>% html_nodes("a:nth-child(13)") %>% html_attr("href") %>% str_extract("\\d+") %>% as.integer()
  #调用多线程:snow的sock集群
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  #循环所有列表页，提取内页url列表
  in_url_vec <- foreach(i = seq(1, last_page), .combine = c) %dopar% in_url_scraping_fun(i)
  #从内页循环提取新闻内容，并按行堆叠
  result <- foreach(j = seq(1, length(in_url_list)), .combine = rbind) %dopar% news_scraping_fun(in_url_vec[j])
  #停止集群，一定要在return前，否则无法停止集群
  stopCluster(cl)  
  #返回结果，一定要有，否则结果为空
  return(result)  
}