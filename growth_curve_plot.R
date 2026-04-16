install.packages(c("ggplot2", "dplyr", "tidyr"))
library(ggplot2)
library(dplyr)
library(tidyr)
growth_data <- data.frame(
  时间 = c(0,8,12,16,20,30,40,50,60,70,80,96),
  # OD600 吸光度值
  X0 = c(0.00,1.12,2.21,2.72,3.01,2.71,2.76,2.76,2.66,2.66,2.66,2.66),
  X50 = c(0.00,0.66,1.62,2.23,2.51,2.46,2.36,2.46,2.51,2.51,2.51,2.46),
  X100 = c(0.00,0.44,1.41,2.01,2.41,2.36,2.31,2.36,2.41,2.41,2.41,2.31),
  X150 = c(0.00,0.21,1.26,1.61,2.11,2.16,2.16,2.16,2.16,2.16,2.16,2.11),
  X200 = c(0.00,0.14,1.11,1.42,1.46,1.61,1.63,1.63,1.63,1.63,1.63,1.61),
  X250 = c(0.00,0.10,0.34,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44,0.44),
  X300 = c(0.00,0.10,0.39,0.44,0.64,0.64,0.64,0.64,0.64,0.64,0.64,0.64),
  # 误差棒
  err0 = c(0.00,0.05,0.08,0.10,0.08,0.07,0.08,0.07,0.06,0.06,0.06,0.06),
  err50 = c(0.00,0.04,0.07,0.09,0.08,0.07,0.06,0.07,0.06,0.06,0.06,0.06),
  err100 = c(0.00,0.04,0.06,0.08,0.07,0.06,0.06,0.06,0.06,0.06,0.06,0.06),
  err150 = c(0.00,0.03,0.06,0.07,0.07,0.06,0.06,0.06,0.06,0.06,0.06,0.06),
  err200 = c(0.00,0.03,0.05,0.06,0.06,0.05,0.05,0.05,0.05,0.05,0.05,0.05),
  err250 = c(0.00,0.02,0.04,0.05,0.05,0.04,0.04,0.04,0.04,0.04,0.04,0.04),
  err300 = c(0.00,0.02,0.04,0.05,0.05,0.04,0.04,0.04,0.04,0.04,0.04,0.04)
)
# 1. 先提取OD值数据（X开头的列）
od_data <- growth_data %>%
  select(时间, starts_with("X")) %>%
  pivot_longer(cols = -时间, names_to = "浓度", values_to = "X") %>%
  mutate(浓度 = gsub("X", "", 浓度)) # 去掉X前缀
# 2. 再提取误差数据（err开头的列）
err_data <- growth_data %>%
  select(时间, starts_with("err")) %>%
  pivot_longer(cols = -时间, names_to = "浓度", values_to = "err") %>%
  mutate(浓度 = gsub("err", "", 浓度)) # 去掉err前缀
# 3. 合并两个数据框，生成干净的长格式数据
plot_data <- left_join(od_data, err_data, by = c("时间", "浓度")) %>%
  mutate(
    浓度 = factor(浓度, levels = c("0","50","100","150","200","250","300")),
    # 此时X和err已经是纯数值，无需强制转换，这里仅做保险
    X = as.numeric(X),
    err = as.numeric(err)
  )
# 【关键验证】确保数据完全正确，无列表列、无NA
str(plot_data) # 查看数据结构，X和err应为num类型
sum(is.na(plot_data$X)) # 应为0
sum(is.na(plot_data$err)) # 应为0
# ===================== 点形状与填充设置 =====================
point_shapes <- c("0"=15,"50"=16,"100"=17,"150"=6,"200"=18,"250"=21,"300"=22)
# 仅填充型形状(21,22)设置白色填充，其余为NA
point_fills <- c("0"=NA,"50"=NA,"100"=NA,"150"=NA,"200"=NA,"250"="white","300"="white")
# ===================== 开始绘图（无报错版）=====================
ggplot(plot_data, aes(x = 时间, y = X, shape = 浓度, fill = 浓度, group = 浓度)) +
  # 误差棒
  geom_errorbar(aes(ymin = X - err, ymax = X + err), 
                width = 1, color = "black", linewidth = 0.3) +
  # 生长曲线
  geom_line(linewidth = 0.5, color = "black") +
  # 数据点
  geom_point(size = 2, stroke = 0.5, color = "black") +
  # 手动设置形状和填充
  scale_shape_manual(values = point_shapes) +
  scale_fill_manual(values = point_fills, guide = "none") +
  # 坐标轴
  scale_x_continuous(limits = c(0,100), breaks = seq(0,100,20)) +
  scale_y_continuous(limits = c(0,3.5), breaks = seq(0,3.5,0.5)) +
  # 标签
  labs(
    x = "时间/h",
    y = expression(OD[600~nm]),
    title = "图5  TP17菌株在不同镉浓度下的生长曲线",
    caption = "Fig. 5  Growth curves of strain TP17 at different cadmium concentrations"
  ) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14),
    plot.caption = element_text(hjust = 0.5, size = 11, face = "italic"),
    legend.title = element_blank(),
    legend.position = "right"
  )
# ggsave("TP17_growth_curve.png", width = 8, height = 6, dpi = 300)
