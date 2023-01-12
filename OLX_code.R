
install.packages("tibble") 
install.packages("httr") 
install.packages("rvest") 
install.packages("dplyr") 
install.packages("ggplot2") 
install.packages("sjmisc") 
install.packages("stringr")
install.packages("assertr")
install.packages("curl") 
install.packages("RDCOMClient")
install.packages("RSelenium")
install.packages("lubridate")
install.packages("splitstackshape")

library(tibble)
library(httr)
library(rvest)
library(dplyr)
library(ggplot2)
library(sjmisc)
library(stringr)
library(assertr)
library(curl)
library(RDCOMClient)
library(RSelenium)
library(lubridate)
library(splitstackshape)


#Defining variables
streets <- c("travessa cunha galvao", "araguaia", "firmino do amaral", "marmore", "xingu", "tirol", "tres rios", "francisca sales", "juvencio de brito", "masaryk", "potiguara", "ituverava", "rubens silva", "alcides lima", "antonio cordeiro", "teodomiro pereira", "ricardo barbosa", "fortunato de brito", "geminiano gois", "zoroastro pamplona", "geremario dantas")

max_value <- 2000
min_value <- 500

path <- paste0("/Users/arthurandrade/Desktop/OLX/Imoveis/")
file_pattern <- paste0(streets,"_")

#Create the server
rD <- rsDriver(port = 4444L,
               browser = "firefox", 
               verbose = F)


#Create drive server
remDr <- remoteDriver(
  remoteServerAddr = "localhost",
  port = 4444L,
  browserName = "firefox"
)

#Open the server
remDr$open()

#Defining listing table
listings <- data.frame(matrix(ncol=1, dimnames=list(NULL, c("links"))))

#Defining listing details table
df <- data.frame(matrix(ncol=21, dimnames=list(NULL, 
                                              c(
                                                "link", 
                                                "id", 
                                                "listing_type",
                                                "title", 
                                                "published_at", 
                                                "contact_name",
                                                "contact_phone",
                                                "rent_value",
                                                "condo_value",
                                                "IPTU_value",
                                                "description",
                                                "category",
                                                "type",
                                                "area",
                                                "bedroom",
                                                "bathroom",
                                                "CEP",
                                                "city",
                                                "district",
                                                "street",
                                                "has_photo"
                                                ))))

#Fixing column type
df$published_at <- as_datetime(df$published_at)
df$rent_value <- as.numeric(df$rent_value)
df$condo_value <- as.numeric(df$condo_value)
df$IPTU_value <- as.numeric(df$IPTU_value)

########### PART I: WEB SCRAPPING ############

print("##### SEARCHING YOUR NEW HOME #####")
#For each street
for (r in streets) {

#Main listing page
  URL <- paste0('https://www.olx.com.br/imoveis/aluguel/estado-rj/rio-de-janeiro-e-regiao/zona-oeste?pe=', max_value, '&ps=', min_value, '&q=', str_replace_all(r, " ", "%20"))
  
  remDr$navigate(URL)

  #Getting listing total number
  total_listing <- remDr$findElements(using = "class name",
                                      value = "sc-1mi5vq6-0")
  
  total_listing <- total_listing[[1]]$getElementText()[[1]]
  
  total_listing <- as.numeric(substr(total_listing, 
                          unlist(gregexpr('de', total_listing))[1]+3,
                          unlist(gregexpr('resultados', total_listing))[1]-2))
  
  #Print status for each street
  print(paste0(
    ifelse(total_listing < 1,
           "No listing found ",
    paste0(total_listing, ifelse(total_listing >1,
                                     " listings found ",
                                     " listing found "))), 
               "for the street: ", str_to_title(r)))
  
  #Getting second page if exists (over 50 listings)
  if (total_listing > 50) {

    #Geting the maximum number of pages
    maxpages <- remDr$findElement(using = "class name",
                                  value = "sc-1bofr6e-0") 
    
    maxpages <- maxpages$findElement(using = "tag name", value = "a") 
    maxpages <- maxpages$getElementAttribute("href")[[1]]
    
    maxpages <- substr(maxpages, 
                       unlist(gregexpr('o=', maxpages))[1]+2, 
                       unlist(gregexpr('&ps', maxpages))[1]-1)
    
    for (p in 1:maxpages)  {
      
      # Navigate to OLX
      URL <- paste0('https://www.olx.com.br/imoveis/aluguel/estado-rj/rio-de-janeiro-e-regiao/zona-oeste?pe=', max_value, '&ps=', min_value, '&q=', str_replace_all(r, " ", "%20"), '&o=',p)
      
      remDr$navigate(URL)
      
      #Getting the list of links
      list <- remDr$findElements(using = "class name", value = "sc-eKZiaR")
      
      total <- length(list)
      

      for (i in 1:total) {
        listings <- rbind(listings, 
                          unlist(list[[i]]$getElementAttribute("href")))
        
        i = i + 1
      }
    }
    
  } else {
    
    # Navigate to OLX
    URL <- paste0('https://www.olx.com.br/imoveis/aluguel/estado-rj/rio-de-janeiro-e-regiao/zona-oeste?pe=', max_value, '&ps=', min_value, '&q=', str_replace_all(r, " ", "%20"))
    
    remDr$navigate(URL)
    
    #Getting the list of links
    list <- remDr$findElements(using = "class name", value = "sc-12rk7z2-1")

    total <- length(list)
    
    if (total < 1){
    } else {
    for (i in 1:total) {
      
      listings <- rbind(listings, 
                        unlist(list[[i]]$getElementAttribute("href")))
      
      i = i + 1
      }
    }
  }
}

listings <- drop_na(listings)

########### PART II: WEB SCRAPPING ############
print(paste0("##### ", length(listings$links), " HOUSE LISTINGS #####"))

#Opening each link e getting more info
i = 1

for (link in listings$links){

  
  remDr$navigate(link)
  
### LISTING ID
  id <- remDr$findElements(using = "class name", value = "ad__sc-16iz3i7-0")
  id <- id[[1]]$getElementText()[[1]]
  id <- str_extract(id, "[[:digit:]]+")

### LISTING TYPE (PROFESSIONAL OR NOT)
  listing_type <- remDr$findElements(using = "class name", value = "ad__sc-16bj9n5-0")
  listing_type <- ifelse(
    length(listing_type)==1,1,0)

### LISTING TITLE
  title <- remDr$findElements(using = "class name", value = "ad__sc-45jt43-0")
  title <- title[[1]]$getElementText()[[1]]

### PUBLISHED AT
  published_at <- remDr$findElements(using = "class name", value = "ad__sc-1oq8jzc-0")
  published_at <- published_at[[2]]$getElementText()[[1]]
  published_at <- extract_numeric(published_at)
  published_at <- as_datetime(
    paste0(
      "2022-",
      substr(published_at, 3,4),"-",
      substr(published_at, 1,2)," ",
      substr(published_at, 5,6),":",
      substr(published_at, 7,8)),
    format = "%Y-%m-%d %H:%M")
  

### RENT VALUE 
  rent_value <- remDr$findElements(using = "class name", value = "ad__sc-1wimjbb-1")
  rent_value <- rent_value[[1]]$getElementText()[[1]]
  rent_value <- str_extract(rent_value, "\\d+([.,]\\d+)?")
  
  details_label <-  remDr$findElements(using = "class name", value = "ad__sc-1f2ug0x-0")

  get_all_elements_label <- function (X){
    X$getElementText()[[1]]
  }
  
  details_label <- as.data.frame(
    unlist(
      lapply(details_label, get_all_elements_label)
      )
    )
  
  details_label$index <- 1:nrow(details_label)
  
  colnames(details_label) <- c("label", "index")
    
  details <- remDr$findElements(using = "class name", value = "ad__sc-1f2ug0x-1")
  

# Geting more details  
  details <- remDr$findElements(using = "class name", value = "ad__duvuxf-0")
  details <- unlist(lapply(details, get_all_elements_label))
  details <- data.frame(details,do.call(rbind,str_split(details,'\n'))[,1:2])
  colnames(details) <- c("original_column", "column", "value")
  
  
### CONDO VALUE
  
  condo_value <- details %>% 
    filter(column == "Condomínio") %>%
    select(value)
  
  condo_value <-  ifelse(length(condo_value$value) == 0, 0,
                    str_extract(condo_value, 
    "\\d+([.,]\\d+)?"))

### IPTU VALUE
  
  IPTU_value <- details %>% 
    filter(column == "IPTU") %>%
    select(value)
  
  IPTU_value <- 
    ifelse(length(IPTU_value$value) == 0, 0,
    str_extract(IPTU_value, 
                             "\\d+([.,]\\d+)?"))

### DESCRIPTION
  description <- remDr$findElements(using = "class name", value = "ad__sc-1sj3nln-1")
  description <- description[[1]]$getElementText()[[1]]
  
### CATEGORY
  
  category <- details %>% 
    filter(column == "Categoria") %>%
    select(value)
  
  category <- ifelse(length(category$value) == 0, "NA", category$value)

### TYPE

  type <- details %>% 
    filter(column == "Tipo") %>%
    select(value)
  
  type <- ifelse(length(type$value) == 0, "NA", type$value)
  
### AREA 
  
  area <- details %>% 
    filter(column == "Área construída") %>%
    select(value)
  
  area <- ifelse(length(area$value) == 0, 0,
    str_extract(area, 
                      "\\d+([.,]\\d+)?"))
  
### BEDROOM
  
  bedroom <- details %>% 
    filter(column == "Quartos") %>%
    select(value)
  
  bedroom <- ifelse(length(bedroom$value) == 0, 0,
    str_extract(bedroom, 
                      "\\d+([.,]\\d+)?"))
  
  
### BATHROOM
  
  bathroom <- details %>% 
    filter(column == "Banheiros") %>%
    select(value)
  
  bathroom <- ifelse(length(bathroom$value) == 0, 0,
    str_extract(bathroom, 
                         "\\d+([.,]\\d+)?"))
  
### CEP (ZIP CODE)
  
  CEP <- details %>% 
    filter(column == "CEP") %>%
    select(value)
  
  CEP <- ifelse(length(CEP$value) == 0, 0,
    str_extract(CEP, 
                          "\\d+([.,]\\d+)?"))
  
### CITY
  
  city <- details %>% 
    filter(column == "Município") %>%
    select(value)
  
  city <- ifelse(length(city$value) == 0, "NA", city$value)
  
### DISTRICT
  
  district <- details %>% 
    filter(column == "Bairro") %>%
    select(value)

  district <- ifelse(length(district$value) == 0, "NA", district$value)

### STREET
  
  street <- details %>% 
    filter(column == "Logradouro") %>%
    select(value)
  
  street <- ifelse(length(street$value) == 0, "NA", street$value)
  
  #Building the dataframe
  
  df[i, 1] <- link
  df[i, 2] <- id
  df[i, 3] <- listing_type
  df[i, 4] <- title
  df[i, 5] <- published_at 
  df[i, 6] <- "NA" #contact_name
  df[i, 7] <- "NA" #contact_phone
  df[i, 8] <- rent_value
  df[i, 9] <- condo_value
  df[i, 10] <- IPTU_value
  df[i, 11] <- description
  df[i, 12] <- category
  df[i, 13] <- type
  df[i, 14] <- area
  df[i, 15] <- bedroom
  df[i, 16] <- bathroom
  df[i, 17] <- CEP
  df[i, 18] <- city
  df[i, 19] <- district
  df[i, 20] <- street
  df[i, 21] <- "NA" #has_photo
  
  
  i <- i + 1
  
}


########### PART III: ANALYTICS ############
options(OutDec=".")
filtered_df <- df

filtered_df$rent_value <- as.numeric(
  gsub("\\.", "", filtered_df$rent_value))

filtered_df$condo_value <- as.numeric(
  gsub("\\.", "", filtered_df$condo_value))

filtered_df$IPTU_value <- as.numeric(
  gsub("\\.", "", filtered_df$IPTU_value))

filtered_df <- filtered_df %>% filter(district %in% 
                               c("Jacarepaguá", "Freguesia (Jacarepaguá)")) %>%
  mutate(total_value = rent_value + condo_value + IPTU_value)

filtered_df <- filtered_df %>% filter(total_value <= max_value)

filtered_df$area <-
ifelse(filtered_df$area==0,
str_extract(
  substr(filtered_df$description,
    str_locate(filtered_df$description, "m²")[,1]-4,
    str_locate(filtered_df$description, "m²")[,1]),
  "\\d+([.,]\\d+)?"),filtered_df$area)

#Remove studio listings (or T0 listings)

filtered_df <- filtered_df %>% filter(
  !str_detect(filtered_df$description, "(?i)Kitnet"))

########### PART IV: DATA VIZ ############

#To be continued...

#Export
write.csv(filtered_df, 
          paste0(path, "house_listings.csv"),
                 row.names = FALSE,
          sep = "|"
          )

