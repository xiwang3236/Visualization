############### draw complex heatmap
# install.packages("readxl")

library("pheatmap")
library("readxl")

file_path <- 'Data/col_mean_demo.xlsx'
# data <- read.csv(file_path, fileEncoding = "GBK")

raw_data <- read_excel(file_path)

############### Set the first column as row names
raw_data <- data.frame(raw_data)
rownames(raw_data) <- raw_data[[1]] 
raw_data <- raw_data[,-1] 

############### Change columns name
colnames(raw_data) <- c("G1", "G2", "G3", "G4", "G5")


############### row-wise min-max normalization
data <- t(apply(raw_data, 1, function(x)(x-min(x))/(max(x)-min(x))))
# data <- raw_data

# Add group information and group color
group <- data.frame(type = c(rep("G1",1), rep("G2",1), rep("G3",1),rep("G4",1),rep("G5",1)))
rownames(group) <- colnames(data)
group_colors <- list(type = c(G1 = "wheat4",
                              G2 = "paleturquoise3",
                              G3 = "thistle4",
                              G4 = "indianred",
                              G5 = "khaki2"
                              ))
# head(group)
# group_colors

############### 1. Draw original color style heatmap
pheatmap(data,
         # annotation_legend = TRUE,
         cluster_cols = F,
         cluster_rows = F,
         # display_numbers = round(data, 2), # Keep 2 digit number
         annotation_col = group, # Group Information
         annotation_color = group_colors # Group color
         )

############### 2. dual gradient
############### 2.1 Calculate the middle value (median/mean) of the data 
middle_value <- median(unlist(data))

############### 2.2 Create the breaks based on min, median, and max of the data
bk <- c(seq(min(data), middle_value, length.out = 50), 
        seq(middle_value, max(data), length.out = 50))
# bk <- c(seq(min(data ),0.50,length.out = 50), seq(0.51, max(data),length.out = 50)) # abosolute value, need change before apply

############### 2.3 Draw heatmap
pheatmap(data, 
         cluster_cols = T, 
         cluster_rows = T,
         color = c(colorRampPalette(colors = c("royalblue3","white"))(length(bk)/2),
           colorRampPalette(colors = c("white","firebrick3"))(length(bk)/2)),
         # display_numbers = round(data, 2),
         annotation_col = group, # Group Information
         annotation_color = group_colors, # Group color
         annotation_legend = TRUE)
