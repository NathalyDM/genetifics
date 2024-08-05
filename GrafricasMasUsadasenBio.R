

library(EnhancedVolcano)

# Generar Volcano Plot
EnhancedVolcano(data,
                lab = data$Gene,          # Etiquetas (nombres de genes)
                x = 'logFC',             # Eje X (Fold Change)
                y = 'pval',              # Eje Y (p-value)
                pCutoff = 0.05,          # Umbral para p-value
                FCcutoff = 1.5,          # Umbral para Fold Change
                title = "Volcano Plot"   # Título del gráfico
)


# Cargar librerías necesarias
library(ComplexHeatmap)  # Para heatmaps avanzados
library(RColorBrewer)    # Para paletas de colores

# Crear Heatmap con ComplexHeatmap
Heatmap(data,
        name = "Expression",  # Nombre de la leyenda
        show_row_names = TRUE,  # Mostrar nombres de filas
        show_column_names = TRUE,  # Mostrar nombres de columnas
        cluster_rows = TRUE,  # Agrupamiento de filas
        cluster_columns = TRUE,  # Agrupamiento de columnas
)







# Cargar librerías necesarias
library(Rtsne)  # Para t-SNE

# Realizar t-SNE
set.seed(123)
tsne <- Rtsne(data, dims = 2, pca = TRUE, perplexity = 5)  # Ajusta perplexity según tus datos

# Crear gráfico t-SNE con ggplot2
tsne_data <- data.frame(tsne$Y)
tsne_data$Group <- group

ggplot(tsne_data, aes(x = V1, y = V2, color = Group, label = rownames(tsne_data))) +
  geom_point(size = 3) +  # Puntos en el gráfico
  geom_text(vjust = 2) +  # Etiquetas
  ggtitle("t-SNE Plot") +  # Título del gráfico
  labs(color = "Group") +  # Leyenda para el grupo
  theme_bw()


# Crear datos para boxplots
data_df <- data.frame(t(data))  # Transponer para tener muestras en filas
data_df$Group <- group

# Crear boxplot con ggplot2
ggplot(data_df, aes(x = Group, y = value, fill = Group)) +
  geom_boxplot() +  # Boxplot
  facet_wrap(~variable, scales = 'free') +  # Un gráfico por variable
  ggtitle("Boxplots") +  # Título del gráfico
  theme_bw()





# Cargar librerías necesarias
library(ggplot2)  # Para gráficos

# Crear datos simulados
set.seed(123)
genes <- paste0("Gene", 1:10)  # Nombres de genes
samples <- paste0("Sample", 1:20)  # Nombres de muestras
group <- factor(rep(c("Control", "Treatment"), each = 10))  # Dos grupos

# Crear una matriz de expresión de genes
expression_data <- matrix(rnorm(200, mean = 5, sd = 2), nrow = 20, ncol = 10)
rownames(expression_data) <- samples
colnames(expression_data) <- genes

# Convertir a data.frame y agregar grupo
expression_df <- as.data.frame(expression_data)
expression_df$Group <- group

# Convertir datos a formato largo para ggplot2
library(reshape2)
expression_long <- melt(expression_df, id.vars = "Group")

# Crear boxplot
ggplot(expression_long, aes(x = variable, y = value, fill = Group)) +
  geom_boxplot() +  # Boxplot
  ggtitle("Boxplot de Expresión de Genes") +  # Título del gráfico
  xlab("Gene") +  # Etiqueta del eje X
  ylab("Expression") +  # Etiqueta del eje Y
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + # Rotar nombres de genes
  theme_bw()




# Cargar librerías necesarias
library(ggplot2)  # Para gráficos
library(stats)    # Para PCA

# Crear datos simulados con dos grupos
set.seed(123)
data <- matrix(rnorm(100), nrow=10)  # Matriz de datos simulados
rownames(data) <- paste0("Sample", 1:10)  # Nombres de muestras
group <- factor(rep(c("Control", "Treatment"), each = 5))  # Dos grupos

# Realizar PCA
pca <- prcomp(data, scale. = TRUE)  # PCA con escalado de datos

# Crear gráfico PCA con colores por grupo
pca_data <- data.frame(PC1 = pca$x[,1], PC2 = pca$x[,2], Group = group)
ggplot(pca_data, aes(x = PC1, y = PC2, color = Group, label = rownames(pca_data))) +
  geom_point(size = 3) +  # Puntos en el gráfico
  geom_text(vjust = 2) +  # Etiquetas
  ggtitle("PCA Plot") +  # Título del gráfico
  labs(color = "Group")+  # Leyenda para el grupo
  theme_bw()



