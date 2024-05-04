library("dplyr")
library("car")

GPU_data <- read.csv("C:/Users/WIN 11/Documents/All_GPUs.csv")
head(GPU_data, 10)

#Lọc biến cần phân tích
new_data <- All_GPUs %>% select('Manufacturer','Memory','Memory_Bandwidth','Memory_Speed','Open_GL','Pixel_Rate','Process','Shader','TMUs','Texture_Rate')
str(new_data)

# Xóa đơn vị của các biến
new_data$Memory <- as.numeric(sub(" MB", "", new_data$Memory))
new_data$Memory_Bandwidth <- as.numeric(sub("GB/sec", "", new_data$Memory_Bandwidth))
new_data$Memory_Speed <- as.numeric(sub(" MHz", "", new_data$Memory_Speed))
new_data$Pixel_Rate <- as.numeric(sub(" GPixel/s", "", new_data$Pixel_Rate))
new_data$Process <- as.numeric(sub("nm", "", new_data$Process))
new_data$Texture_Rate <- as.numeric(sub(" GTexel/s", "", new_data$Texture_Rate))

# Thống kê số lượng dữ liệu khuyết 
colSums(is.na(new_data))

# Tỷ lệ dữ liệu khuyết của từng biến
apply(is.na(new_data), 2, mean)

# thay giá trị trung vị vào các biến có tỉ lệ N/A lớn

new_data$Memory <- ifelse(is.na(new_data$Memory), median(new_data$Memory, na.rm = TRUE), new_data$Memory)
new_data$Pixel_Rate <- ifelse(is.na(new_data$Pixel_Rate), median(new_data$Pixel_Rate, na.rm = TRUE), new_data$Pixel_Rate)
new_data$Process <- ifelse(is.na(new_data$Process), median(new_data$Process, na.rm = TRUE), new_data$Process)
new_data$TMUs <- ifelse(is.na(new_data$TMUs), median(new_data$TMUs, na.rm = TRUE), new_data$TMUs)
new_data$Texture_Rate <- ifelse(is.na(new_data$Texture_Rate), median(new_data$Texture_Rate, na.rm = TRUE), new_data$Texture_Rate)

# Xóa quan sát có biến mà có dữ liệu khuyết 
new_data <- na.omit(new_data)

str(new_data)

#General information of the variables in the dataset
summary(new_data)

#Descriptive statistics for Continuous variables (Memory, Memory_Bandwidth, Memory_Speed, Open_GL, Pixel_Rate, Process, Shader, TMUs, Texture_Rate)
sd <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, sd) #Standard Deviation(Độ lệch chuẩn)
mean <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, mean) #Mean(Trung bình mẫu)
Q1 <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, quantile, probs = 0.25) #1st Quartile(Phân vị 1)
Q2 <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, median) #Median(Trung vị)
Q3 <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, quantile, probs = 0.75) #3rd Quartile(Phân vị 3)
min <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, min) #Min(Minimum)
max <- apply(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")], 2, max) #Max(Maximum)
#Create a table to show the descriptive statistics
descriptive_statistics <- data.frame(mean, sd, Q1, Q2, Q3, min, max)
print(descriptive_statistics) #Print the table

#Statistics for each type of variable
table(new_data$Manufacturer) #Frequency table for Manufacturer
table(new_data$Memory) #Frequency table for Memory
table(new_data$Memory_Bandwidth) #Frequency table for Memory_Bandwidth
table(new_data$Memory_Speed) #Frequency table for Memory_Speed
table(new_data$Open_GL) #Frequency table for Open_GL
table(new_data$Pixel_Rate) #Frequency table for Pixel_Rate
table(new_data$Process) #Frequency table for Process
table(new_data$Shader) #Frequency table for Shader
table(new_data$TMUs) #Frequency table for TMUs
table(new_data$Texture_Rate) #Frequency table for Texture_Rate

#Correlation chart between Continuous variables described by Corrplot
# install.packages("corrplot") #Install the package
library(corrplot) #Load the package
correlation_matrix <- cor(new_data[, c("Memory", "Memory_Bandwidth", "Memory_Speed", "Open_GL", "Pixel_Rate", "Process", "Shader", "TMUs", "Texture_Rate")]) #Calculate the correlation matrix
corrplot(correlation_matrix, method = "color", main = "Correlation between Continuous variables"
         , addCoef.col = "yellowgreen", tl.col = "red", tl.cex = 0.8, tl.srt = 45, mar = c(0, 0, 1, 0)) #Correlation chart
#Confirm Pixel_Rate is the main analysis variable - The correlation coefficient is one of the highest with the other variables

#Histogram for Pixel_Rate variable
fq_table <- hist(new_data$Pixel_Rate, main = "Histogram of Pixel_Rate", col = "lightgreen", xlab = "Pixel_Rate(GPixel/s)", border = "black"
                 , xlim = c(0, 300), breaks = 50)         #Basic setting for the histogram
labels <- ifelse(fq_table$counts > 0, fq_table$counts, "") #Set the labels for the histogram
text(fq_table$mids, fq_table$counts, labels = labels, adj = c(0.5, -0.5)) #Add the labels to the histogram

#Outliers detection for Pixel_Rate variable
IQR = IQR(new_data$Pixel_Rate) #Calculate the Interquartile Range
lower_bound <- quantile(new_data$Pixel_Rate, 0.25) - 1.5 * IQR #Calculate the Lower Bound
upper_bound <- quantile(new_data$Pixel_Rate, 0.75) + 1.5 * IQR #Calculate the Upper Bound
outliers <- new_data$Pixel_Rate[new_data$Pixel_Rate < lower_bound | new_data$Pixel_Rate > upper_bound] #Detect the outliers
table(outliers) #Frequency table of the outliers

#Boxplot display distribution between Pixel_Rate and Categorical Variables - Manufacturer
boxplot(new_data$Pixel_Rate ~ new_data$Manufacturer, main = "Boxplot of Pixel_Rate by Manufacturer"
        , xlab = "Manufacturer", ylab = "Pixel_Rate(GPixel/s)", ylim = c(0, 300), col = "lightblue", border = "black")

#Scatter plot display distribution between Pixel_Rate and Continuous Variables - Memory, Memory_Bandwidth, Memory_Speed, Open_GL, Process, Shader, TMUs, Texture_Rate
par(mfrow = c(1, 2)) #Set the layout for the scatter plots
plot(new_data$Memory, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Memory"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Memory(GB)", col = "violet", pch = 19) #Pixel_Rate and Memory
plot(new_data$Memory_Bandwidth, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Memory_Bandwidth"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Memory_Bandwidth(GB/s)", col = "pink", pch = 19) #Pixel_Rate and Memory_Bandwidth
plot(new_data$Memory_Speed, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Memory_Speed"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Memory_Speed(MHz)", col = "violet", pch = 19) #Pixel_Rate and Memory_Speed
plot(new_data$Texture_Rate, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Texture_Rate"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Texture_Rate(GT/s)", col = "pink", pch = 19) #Pixel_Rate and Texture_Rate
plot(new_data$Open_GL, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Open_GL"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Open_GL", col = "pink", pch = 19) #Pixel_Rate and Open_GL
plot(new_data$Process, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Process"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Process(nm)", col = "violet", pch = 19) #Pixel_Rate and Process
plot(new_data$Shader, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to Shader"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "Shader", col = "pink", pch = 19) #Pixel_Rate and Shader
plot(new_data$TMUs, new_data$Pixel_Rate, main = "Distribution of Pixel_Rate according to TMUs"
     , ylab = "Pixel_Rate(GPixel/s)", xlab = "TMUs", col = "violet", pch = 19) #Pixel_Rate and TMUs

# Done
# result = aggregate(Pixel_Rate ~ Manufacturer, data = new_data, FUN = max)
# result <- tapply(new_data$Pixel_Rate, new_data$Manufacturer, function(x) quantile(x, probs = c(0.25, 0.5, 0.75)))
# print(result)

### KIểm định 1 mẫu
# Tính các thông số của z_qs 
n <- length(new_data$Pixel_Rate)
x_tbm <- mean(new_data$Pixel_Rate)
s <- sd(new_data$Pixel_Rate)
data.frame(n,x_tbm,s)
# Kiểm định phân phối chuẩn 
shapiro.test(new_data$Pixel_Rate)
# Tính giá trị z_qs 
z_qs <- (x_tbm - 20)/(s/sqrt(n))
print(z_qs)
# xác định miền bác bỏ RR 
qnorm(p=0.05, lower.tail = FALSE)

### Kiểm định 2 mẫu 
# Tạo tập dữ liệu cho 2 biến Memory và Manufacturer 
twoPieces = c("Memory", "Manufacturer")
twoSamples <- new_data[ , twoPieces]
twoSamples <- twoSamples[twoSamples$Manufacturer %in% c("Nvidia", "AMD"), ]
summary(twoSamples)

M_Nvidia <- subset(twoSamples, twoSamples$Manufacturer=="Nvidia")
M_AMD <- subset(twoSamples, twoSamples$Manufacturer=="AMD")
# Tính các thông số cho z_qs1 và z_qs2 
n1 <- length(M_Nvidia$Memory)
x_tbm1 <- mean(M_Nvidia$Memory)
s1 <- sd(M_Nvidia$Memory)

n2 <- length(M_AMD$Memory)
x_tbm2 <- mean(M_AMD$Memory)
s2 <- sd(M_AMD$Memory)

data.frame(n1, x_tbm1, s1, n2, x_tbm2, s2)
# KIểm định phân phối chuẩn cho Memory của 2 hãng Nvidia và AMD 
shapiro.test(M_Nvidia$Memory)
shapiro.test(M_AMD$Memory)
# Tính giá trị thống kê kiểm định z_qs2 
z_qs2 <- (x_tbm1-x_tbm2)/sqrt(s1^2/n1 + s2^2/n2)
print(z_qs2)
# Xác định miền bác bỏ RR 
qnorm(p=0.05/2, lower.tail = FALSE)

# Kiểm định dạng phân phối chuẩn 
var.test(M_Nvidia$Memory, M_AMD$Memory, alternative = "greater")
# tính giá trị t_qs2 
s_P <- ((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2)
t_qs2 <- (x_tbm1 - x_tbm2)/sqrt(s_P/n1 + s_P/n2)
print(t_qs2)
# Xác định miền bác bỏ RR
qt(p=0.05/2, df=2901, lower.tail = FALSE)

### anova 1 nhân tố 
Mem_anova = c("Memory_Speed", "Manufacturer")
Memory_anova <- new_data[ , Mem_anova]
Memory_anova <- Memory_anova[Memory_anova$Manufacturer %in% c("ATI","Intel","Nvidia", "AMD"), ]
summary(Memory_anova)

MSpeed_ATI <- subset(Memory_anova, Memory_anova$Manufacturer=="ATI")
MSpeed_Intel <- subset(Memory_anova, Memory_anova$Manufacturer=="Intel")
MSpeed_Nvidia <- subset(Memory_anova, Memory_anova$Manufacturer=="Nvidia")
MSpeed_AMD <- subset(Memory_anova, Memory_anova$Manufacturer=="AMD")

# Kiểm định giả định các biến có phân phối chuẩn
# KIểm định bằng dồ thị Q-Q Plot 
qqnorm(MSpeed_ATI$Memory_Speed)
qqline(MSpeed_ATI$Memory_Speed)

qqnorm(MSpeed_Intel$Memory_Speed)
qqline(MSpeed_Intel$Memory_Speed)

qqnorm(MSpeed_Nvidia$Memory_Speed)
qqline(MSpeed_Nvidia$Memory_Speed)

qqnorm(MSpeed_AMD$Memory_Speed)
qqline(MSpeed_AMD$Memory_Speed)
# KIểm định bằng bài toán kiểm định 
shapiro.test(MSpeed_ATI$Memory_Speed)
shapiro.test(MSpeed_Intel$Memory_Speed)
shapiro.test(MSpeed_Nvidia$Memory_Speed)
shapiro.test(MSpeed_AMD$Memory_Speed)
# Kiểm định tính đông nhất phương sai 
leveneTest(Memory_Speed ~ Manufacturer,data = Memory_anova)
# phân tích anova 
anova_model <- aov(Memory_Speed ~ Manufacturer, data = Memory_anova)
summary(anova_model)

qf(p=0.05, df1 = 3, df2 = 3189, lower.tail = FALSE) #Tính giá trị F(alpha, k-1, N-K) trong miền bác bỏ RR
# so sánh bội 
TukeyHSD(anova_model)

### Hồi quy tuyến tính đơn 
lm <- lm(Pixel_Rate~Memory_Bandwidth, new_data)
summary(lm)
plot(lm)

### Hồi quy tuyến tính bội 
lm_model<-lm(Pixel_Rate~Manufacturer+Memory+Memory_Bandwidth+Memory_Speed+Open_GL+Process+Shader+TMUs+Texture_Rate,new_data)
summary(lm_model)

# mô hình mới bỏ đi biến Shader 
lm_model_new<-lm(Pixel_Rate~Manufacturer+Memory+Memory_Bandwidth+Memory_Speed+Open_GL+Process+TMUs+Texture_Rate,new_data)
summary(lm_model_new)
# so sánh 2 mô hình cũ và mới 
anova(lm_model, lm_model_new)

plot(lm_model_new) # vẽ đồ thị kiểm tra giả định cho mô hình mới 
# Dự báo 
X = data.frame("Manufacturer"='Nvidia', "Memory"=1024, "Memory_Bandwidth"=35.2, "Memory_Speed"=1100, "Open_GL"=2.3, "Process"=50, "TMUs"=15, "Texture_Rate"=10)
predict(lm_model_new,X, interval = "confidence")
