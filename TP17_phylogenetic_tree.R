# ========================
# TP17 系统发育树 最终整合版
# 一次运行成功 | 文字紧贴线条 | 自动导出桌面高清图
# ========================

# 加载必需包
library(ggtree)
library(treeio)
library(ape)
library(ggplot2)

# 1. 构建发育树（完全正确格式）
newick_text <- "((MZ203534.1_Pseudomonas_sp._strain_PNP:0.02,(MN252105.1_Pseudomonas_sp._strain_MR15:0.01,(HM152676.1_Uncultured_Pseudomonas_sp._clone_Filt.89:0.01,((KC189961.1_Pseudomonas_putida:0.01,HM152671.1_Uncultured_Pseudomonas_sp._clone_Filt.84:0.01)52:0.005,(KF956584.1_Pseudomonas_plecoglossicida_strain_S20411:0.01,(KT273281.1_Pseudomonas_putida_strain_Y-4:0.01,MW383601.1_Pseudomonas_plecoglossicida_strain_BKP_NB22:0.01)52:0.005)79:0.005)37:0.005):0.005,(HM152701.1_Uncultured_Pseudomonas_sp._clone_Filt.114:0.01,(TP17:0.01,HM152688.1_Uncultured_Pseudomonas_sp._clone_Filt.101:0.01)37:0.005):0.005):0.005)100:0.01,(MN176467.1_Serratia_sp._strain_RJ-10-14:0.02,(MN176468.1_Serratia_sp._strain_RJ-10-16:0.02,ON197880.1_Serratia_marcescens_strain_ZJC2:0.02):0.02):0.02);"

tree <- read.tree(text = newick_text)
tree$node.label[tree$node.label == ""] <- NA

# 2. 绘图（文字紧贴分支 + 无空格 + 文字完整显示）
p <- ggtree(tree, linewidth = 0.8, color = "black") +
  geom_nodelab(aes(label = label), size = 3.5, hjust = 1.1, vjust = 0.5) +
  geom_tiplab(
    size = 3.5,
    offset = 0,        # 文字紧贴线条，无空隙
    align = FALSE,     # 跟随自己的分支，不强制对齐
    linetype = "blank"
  ) +
  geom_treescale(x = 0, y = -1, width = 0.01, offset = 0.002, fontsize = 3.5) +
  labs(title = "图4 TP17菌株的系统发育树\nFig.4 Phylogenetic tree of the TP17 strain") +
  theme_tree2() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid = element_blank(),
    plot.margin = unit(c(1,2,1,1), "cm")
  ) +
  xlim(0, 0.09)

# 3. 显示图片
print(p)

# 4. 自动导出 → 桌面（绝对能找到！）
ggsave(
  filename = file.path(Sys.getenv("USERPROFILE"), "Desktop", "TP17_final_tree.png"),
  plot = p,
  width = 14, height = 8, dpi = 300, bg = "white"
)
