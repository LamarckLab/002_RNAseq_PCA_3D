# 安装并加载必要的包
# install.packages(c("readr", "ggplot2", "vegan", "plotly"))
library(readr)
library(ggplot2)
library(vegan)
library(plotly)

# 1. 读取数据
file_path <- "C:/Users/Lamarck/Desktop/pca.csv"  # 替换为实际文件路径
data <- read_csv(file_path)

# 设置基因名称为行名，并移除第一列
rownames(data) <- data$NAME
data <- data[, -1]  # 移除基因名称列

# 删除全为 0 的行
data <- data[rowSums(data != 0) > 0, ]

# 提取分组信息
group <- ifelse(grepl("^control", colnames(data)), "Control", "Treated")  # 根据列名生成分组信息

# 确保数据为数值型矩阵
data <- as.matrix(data)

# 2. 数据标准化 (对样本进行标准化)
data_scaled <- t(scale(t(data)))  # 对行（基因）进行标准化

# 移除包含 NaN 的行
data_scaled <- data_scaled[complete.cases(data_scaled), ]

# 3. PCA 分析
pca_result <- prcomp(t(data_scaled), center = TRUE, scale. = TRUE)

# 查看 PCA 解释的方差比例
summary(pca_result)

# 提取 PCA 数据
pca_data <- data.frame(
  Sample = colnames(data),
  PC1 = pca_result$x[, 1],
  PC2 = pca_result$x[, 2],
  PC3 = pca_result$x[, 3],  # 提取第三主成分
  Group = group
)

pca_3d_plot <- plot_ly(
  data = pca_data,
  x = ~PC1,
  y = ~PC2,
  z = ~PC3,
  color = ~Group,
  colors = c("red", "blue"),
  text = ~Sample,
  type = "scatter3d",
  mode = "markers+text",
  marker = list(size = 5)
) %>%
  layout(
    title = "3D PCA Analysis of Gene Expression",
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

# 显示 3D 图
pca_3d_plot

# 导入 htmlwidgets 包
library(htmlwidgets)

# 保存为 HTML 文件
saveWidget(pca_3d_plot, "3D_PCA_Plot.html", selfcontained = TRUE)
