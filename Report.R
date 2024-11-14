
data <- read.csv("D:/file copy R.csv")
data <- data.frame(data)
tail(data)
head(data)
dim(data)
names(data)
str(data)
summary(data)



#----------------------------------------
#chuyển đổi kiểu dữ liệu
data$School_Year <- as.factor(data$School_Year)

#-----------------------------------------

#xử lý giá trị bị thiếu
colSums(is.na(data))  #tổng số lượng các giá trị thiếu (NA) trong từng cột của bộ dữ liệu.
colSums(data == "") ##tổng số lượng giá trị rỗng hoặc chuỗi trống trong từng cột.
mean_AVG_increase <- mean(data$AVG_increase, na.rm = TRUE)
mean_AVG_decrease <- mean(data$AVG_decrease, na.rm =TRUE)
# gán các dữ liệu missing thành dữ liệu trung bình
data$AVG_increase[is.na(data$AVG_increase)] <- mean_AVG_increase
data$AVG_decrease[is.na(data$AVG_decrease)] <- mean_AVG_decrease

#------------------------------------------------------
#Tính số lượng giá trị duy nhất (unique values) trong từng cột 
apply(data,2, function(x) length(unique(x))) 

#--------------------------------------------------------

#Kiểm tra và xử lý dữ liệu trùng lặp để bỏ các giá trị trùng nhau trong cột chuỗi 
duplicated_rows <- duplicated(data$String)
unique(data$String)
length(unique(data$String))

#----------------------------------------------------------
#CHUẨN HÓA dữ liệu là không cần thiết
#------------------------------------------------------

#để năm 2 là  mảng tham chiếu
levels(data$School_Year)
relevel(data$School_Year, ref = "Năm 2")

#----------------------------------------------------
#xử lý chuổi 
library(stringr)
library(readr)
df <- read.csv("D:/file copy R.csv", header = TRUE, encoding = "UTF-8")
column_names <- colnames(df)
print(column_names)
print("-------------")
sequences <- df$String
# Loại bỏ các thứ không liên quan
processed_sequences <- c()
for (seq in sequences) {
  # Loại bỏ dấu chấm, dấu phẩy
  processed_seq <- str_replace_all(seq, "[.,]", "")
  # Loại bỏ các câu haha, hehe
  processed_seq <- str_replace_all(processed_seq, "\\b([Hh][Aa]ha+|[Hh]ehe|hjhj|hoho|Vhh|hmm|goihoihoijoljio)\\b", "")
  # Những chuỗi không liên quan đến câu hỏi
  processed_seq <- str_replace_all(processed_seq, "\\b[Đđ]úng|Có|Đíng|[Oo]k|Next]\\b", "")
  if (nchar(processed_seq) > 5) {
    processed_sequences <- c(processed_sequences, processed_seq)
  }
}
print(processed_sequences)
data_string <-data.frame(opinion = processed_sequences)
tich_cuc <- c("Tốt", "Hay","giúp",  "thật sự tốt", "hưởng lợi", "đột phá", "hài lòng","hay"
              ,"hữu ích", "tuyệt vời", "phát triển","thông minh","tien ich",
              "giải thích","tiện lợi","hiệu quả")

tieu_cuc <- c("thất nghiệp", "khủng hoảng", "ngu","sai", "phụ thuộc", "Không tốt","có thể",
              "giảm","sai", "quan ngại")

trung_tinh <-c("Bình thường", "tùy", "Chính xác", "Không có ý kiến")

khonglq <- c("Ko", "No problem","Ngủ đi", "Thơm", "nothing")
# Đếm số lượng
sl_tich_cuc <- 0
sl_tieu_cuc <-0
sl_trung_tinh <-0
sl_khong_lien_quan <- 0

# Hàm phân loại ý kiến
phan_loai_y_kien <- function(text) {
  tich_cuc_count <- sum(grepl(paste(tich_cuc, collapse = "|"), text, ignore.case = TRUE))
  tieu_cuc_count <- sum(grepl(paste(tieu_cuc, collapse = "|"), text, ignore.case = TRUE))
  trung_tinh_count <- sum(grepl(paste(trung_tinh, collapse = "|"), text, ignore.case = TRUE))
  klq_count <- sum(grepl(paste(khonglq, collapse = "|"), text, ignore.case = TRUE))
  if( klq_count >0&&trung_tinh_count==0){
    return ("Ý kiến không liên quan")
  }
  
  
  if (tich_cuc_count > tieu_cuc_count) {
    return("Ý kiến tích cực")
  } else if (tich_cuc_count < tieu_cuc_count) {
    return("Ý kiến tiêu cực")
  } else {
    return("Ý kiến trung tính")
  }
  
}

trung_tinh<- c()
for (i in 1:nrow(data_string)) {
  opinion <- as.character(data_string[i, "opinion"])
  
  # Phân loại ý kiến
  loai_y_kien <- phan_loai_y_kien(opinion)
  
  # Tăng biến đếm tương ứng
  if (loai_y_kien == "Ý kiến tích cực") {
    sl_tich_cuc <- sl_tich_cuc + 1
  } else if (loai_y_kien == "Ý kiến tiêu cực") {
    sl_tieu_cuc <- sl_tieu_cuc + 1
  } else if (loai_y_kien == "Ý kiến trung tính") {
    sl_trung_tinh <- sl_trung_tinh + 1
    trung_tinh <- c(trung_tinh, opinion)
  } else {
    sl_khong_lien_quan <- sl_khong_lien_quan + 1
  }
}

# Hiển thị kết quả
cat("Số lượng ý kiến tích cực:", sl_tich_cuc, "\n")
cat("Số lượng ý kiến tiêu cực:", sl_tieu_cuc, "\n")
cat("Số lượng ý kiến trung tính:", sl_trung_tinh, "\n")
cat("Số lượng ý kiến không liên quan:", sl_khong_lien_quan, "\n")
 
#------------------------------------------------------------------
#Boxplot 
# Boxplot
boxplot(data$Classification, col = "lightblue")
boxplot(data$Self_assessment, col = "lightblue")
boxplot(data$AVG_increase, col = "lightblue")
boxplot(data$AVG_decrease, col = "lightblue")
boxplot(data$frequency_in1day, col = "lightblue")

#----------------------------------------------------------------------
data[,10]
var(data[,c(11:16,21:22,24:25,27:28)])
cov(data[,c(11:16,21:22,24:25,27:28)])
cor(data[,c(11:16,21:22,24:25,27:28)])

#-----------------------------------------------------------------------
desc <- function(x)   
{   
  av <- mean(x)   
  sd <- sd(x)    
  se <- sd/sqrt(length(x))  
  c(MEAN=av, 
    SD=sd,
    SE=se)  
}
#hàm tính sai số chuẩn 
desc(data$frequency_in1day) #gọi hàm tính sai số chuẩn
#SD độ lệch chuẩn
#SE sai số chuẩn
by(data, data$Gender, summary)  #gom nhóm theo giới tính

#---------------------------------------------------------------------
#Barplot 
barplot(table(data$Gender), beside = FALSE)
barplot(table(data$School_Year), beside = FALSE)
barplot(table(data$Use.), beside = FALSE)
barplot(table(data$Classification), beside = FALSE)
barplot(table(data$AVG_increase), beside = FALSE)
barplot(table(data$AVG_decrease), beside = FALSE)
barplot(table(data$Self_assessment), beside = FALSE)
barplot(table(data$frequency_in1day), beside = FALSE)
barplot(table(data$Another.website), beside = FALSE)
barplot(table(data$PrioritizeofChatGPTvsAnotherWebsite), beside = FALSE)
barplot(table(data$Trust), beside = FALSE)
barplot(table(data$Effective), beside = FALSE)
barplot(table(data$Affect), beside = FALSE)
barplot(table(data$Satisfied), beside = FALSE)
barplot(table(data$Fit), beside = FALSE)
barplot(table(data$Inspiration), beside = FALSE)
barplot(table(data$thinking_influence), beside = FALSE)
barplot(table(data$Manage_study_time), beside = FALSE)
barplot(table(data$Replace_teacher), beside = FALSE)
barplot(table(data$Save_time), beside = FALSE)
barplot(table(data$Negative_affects), beside = FALSE)
barplot(table(data$Make_lazy), beside = FALSE)
barplot(table(data$improve_learning), beside = FALSE)
barplot(table(data$Satisfied_with_results), beside = FALSE)
barplot(table(data$HowChatGPTwork), beside = FALSE)
barplot(table(data$Consistent_with_understanding), beside = FALSE)
barplot(table(data$Contend_GG_vs_ChatGPT), beside = FALSE)
barplot(table(data$Use_for_work), beside = FALSE)
barplot(table(data$Make_question), beside = FALSE)
barplot(table(data$String), beside = FALSE)

#----------------------------------------------------------------------------
#biểu đồ  tròn
percent = round(prop.table(table(data$Gender))*100,2)
legend_text <- paste(names(table(data$Gender)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Gender))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$School_Year))*100,2)
legend_text <- paste(names(table(data$School_Year)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$School_Year))
legend("topright", legend = legend_text, cex = 0.8)

#school name phải vẽ biểu đồ khác

percent = round(prop.table(table(data$Use.))*100,2)
legend_text <- paste(names(table(data$Use.)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Use.))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Classification))*100,2)
legend_text <- paste(names(table(data$Classification)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Classification))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$AVG_increase))*100,2)
legend_text <- paste(names(table(data$AVG_increase)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$AVG_increase))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$AVG_decrease))*100,2)
legend_text <- paste(names(table(data$AVG_decrease)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$AVG_decrease))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Self_assessment))*100,2)
legend_text <- paste(names(table(data$Self_assessment)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Self_assessment))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$frequency_in1day))*100,2)
legend_text <- paste(names(table(data$frequency_in1day)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$frequency_in1day))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Another.website))*100,2)
legend_text <- paste(names(table(data$Another.website)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Another.website))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$PrioritizeofChatGPTvsAnotherWebsite))*100,2)
legend_text <- paste(names(table(data$PrioritizeofChatGPTvsAnotherWebsite)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$PrioritizeofChatGPTvsAnotherWebsite))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Trust))*100,2)
legend_text <- paste(names(table(data$Trust)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Trust))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Effective))*100,2)
legend_text <- paste(names(table(data$Effective)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Effective))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Affect))*100,2)
legend_text <- paste(names(table(data$Affect)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Affect))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Satisfied))*100,2)
legend_text <- paste(names(table(data$Satisfied)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Satisfied))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Fit))*100,2)
legend_text <- paste(names(table(data$Fit)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Fit))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Inspiration))*100,2)
legend_text <- paste(names(table(data$Inspiration)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Inspiration))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$thinking_influence))*100,2)
legend_text <- paste(names(table(data$thinking_influence)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$thinking_influence))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Manage_study_time))*100,2)
legend_text <- paste(names(table(data$Manage_study_time)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Manage_study_time))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Replace_teacher))*100,2)
legend_text <- paste(names(table(data$Replace_teacher)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Replace_teacher))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Save_time))*100,2)
legend_text <- paste(names(table(data$Save_time)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Save_time))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Negative_affects))*100,2)
legend_text <- paste(names(table(data$Negative_affects)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Negative_affects))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Make_lazy))*100,2)
legend_text <- paste(names(table(data$Make_lazy)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Make_lazy))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$improve_learning))*100,2)
legend_text <- paste(names(table(data$improve_learning)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$improve_learning))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Satisfied_with_results))*100,2)
legend_text <- paste(names(table(data$Satisfied_with_results)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Satisfied_with_results))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$HowChatGPTwork))*100,2)
legend_text <- paste(names(table(data$HowChatGPTwork)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$HowChatGPTwork))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Consistent_with_understanding))*100,2)
legend_text <- paste(names(table(data$Consistent_with_understanding)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Consistent_with_understanding))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Contend_GG_vs_ChatGPT))*100,2)
legend_text <- paste(names(table(data$Contend_GG_vs_ChatGPT)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Contend_GG_vs_ChatGPT))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Use_for_work))*100,2)
legend_text <- paste(names(table(data$Use_for_work)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Use_for_work))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$Make_question))*100,2)
legend_text <- paste(names(table(data$Make_question)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$Make_question))
legend("topright", legend = legend_text, cex = 0.8)

percent = round(prop.table(table(data$String))*100,2)
legend_text <- paste(names(table(data$String)), paste0(" (", percent, "%)"), sep = "")
pie(table(data$String))
legend("topright", legend = legend_text, cex = 0.8)

#---------------------------------------------------------
#histogram
op<- par(mfrow = c(1,3)) #phân phối hormones và chỉ số sinh hóa
hist(data$Gender,col = "lightblue",
     ylab = "Number of surveys", xlab = "rating level")
hist(data$Self_assessment, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating level", main = "Self_assessment")
hist(data$frequency_in1day, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating_level", main = "Frequency_in1day")
hist(data$PrioritizeofChatGPTvsAnotherWebsite, breaks = 3, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating level", main = "PrioritizeofChatGPTvsAnotherWebsite")
hist(data$Trust, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating_level", main = "Reliability of ChatGPT")
hist(data$Effective, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating_level", main = "The effectiveness of ChatGPT")
hist(data$Affect, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating_level", main = "The affect of ChatGPT")
hist(data$Satisfied, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating_level", main = "The satisfaction level")
hist(data$Fit, col = "lightblue",
     ylab = "Number of surveys", xlab = "rating_level", main = "the Suitable of the content")

#----------------------------------------
# Gis

library(tmap)
library(readxl)
data <- read.csv("D:/file copy R.csv", header = TRUE, encoding = "UTF-8")

data_geophy <- data$School_Name
options(digits = 16)
data_gis <-read.csv("D:/GisData.csv",header= FALSE, skip=1, encoding = "UTF-8", sep =",",
                    col.names = c("Latitude", "Longitude"))
data_gis$Longitude <- gsub(";", "", data_gis$Longitude)
data_gis$Longitude <- as.numeric(data_gis$Longitude)



library(leaflet)
# Tạo bản đồ và thêm ảnh nền 
map <- leaflet(data_gis) %>%
  addTiles() %>%
  setView(lng = 106.705, lat = 10.762, zoom = 12)

# Đánh dấu các điểm trên bản đồ
map <- map %>% addMarkers(lng = ~Longitude, lat = ~Latitude)

# Hiển thị bản đồ
map

#---------------------------------------------------------

# Tidy data
library(tidyverse)
library(dplyr)
library(tidyr)


df <- read.csv("D:/file copy R.csv", header = TRUE, encoding = "UTF-8")
head(df)
df["Another.website"]
data <-  df[, c("Gender","School_Year", "School_Name","Use.","Another.website",
                "PrioritizeofChatGPTvsAnotherWebsite")]

data_web <- data %>%
  mutate(Wikipedia.org = as.numeric(grepl("Wikipedia.org", Another.website)),
         Google.com = as.numeric(grepl("Google.com", Another.website)),
         Youtube.com = as.numeric(grepl("Youtube.com", Another.website)),
         `Google Scholar` = as.numeric(grepl("Google Scholar", Another.website)),
         khác = as.numeric(grepl("Khác", Another.website))) %>%
  select(-Another.website)


tidydata_long <-data_web %>%
  pivot_longer(cols = starts_with(c("Wikipedia.org", "Google.com", 
                                    "Youtube.com", "Google Scholar", "khác")),
               names_to = "Website",
               values_to = "Value")


# Vẻ biểu đồ bậc thang
library(ggplot2)

# Tính tổng của 5 cột
totals <- colSums(data_web[, c("Wikipedia.org", "Google.com", "Youtube.com", "Google Scholar", "khác")])

# Tạo data frame mới chứa tổng của 5 cột
totals_df <- data.frame(Website = names(totals), Total = totals)

# Vẽ biểu đồ cột nằm ngang
ggplot(totals_df, aes(x = Total, y = Website)) +
  geom_col(fill = "steelblue") +
  labs(x = "Total", y = "Website") +
  ggtitle("Total Counts of Websites")


# Biểu đồ hình bánh

library(ggplot2)

# Tính tổng của 5 cột
totals <- colSums(data_web[, c("Wikipedia.org", "Google.com", "Youtube.com", "Google Scholar", "khác")])

# Tạo data frame mới chứa tổng của 5 cột
totals_df <- data.frame(Website = names(totals), Total = totals)

# Vẽ biểu đồ hình bánh
ggplot(totals_df, aes(x = "", y = Total, fill = Website)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Website", x = NULL, y = NULL) +
  ggtitle("Distribution of Websites")