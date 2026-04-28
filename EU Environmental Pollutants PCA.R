
library("readxl")
environment <- read_excel("environment.xlsx")  ### διαβάζω το αρχείο που είναι σε excel

library("FactoMineR")
library("factoextra")

data(environment)
summary(environment)
ncol(environment)
names(environment)
length(environment$Population)

data.frame(environment)

# variables
is.numeric(environment$Population)
is.na(environment$Population)
is.null(environment$Population)
## true / false
is.logical(environment$Population)
## NRG_consumption
is.numeric(environment$NRG_consumption)
is.na(environment$NRG_consumption)
is.null(environment$NRG_consumption)

## true / false
is.logical(environment$NRG_consumption)


environment2 <- environment[2:8]

# head(environment2)
head(environment2,n=28)


# covariance matrix
cov(environment2)

# correlation matrix
cor(environment2)

## pca results
res.pca <- PCA(environment2, graph = FALSE)
print(res.pca)

eig.val <- get_eigenvalue(res.pca)
eig.val

## scree plot
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 40))

## From the plot above, we might want to stop at the fifth principal component 
## 99.7% of the information (variances) contained in the data 
## are retained by the first five principal components

res.pca <- prcomp(environment2, scale = TRUE)


## scree plot
fviz_eig(res.pca)
var <- get_pca_var(res.pca)
var

## The quality of representation of the variables on factor map is called cos2 
## (square cosine, squared coordinates)
var$cos2
var$contrib
var$coord
## The larger the value of the contribution, the more the variable contributes to the component

## initial look
fviz_pca_var(res.pca, col.var = "black")

## Positively correlated variables are grouped together.
## Negatively correlated variables are positioned on opposite sides 
## of the plot origin (opposed quadrants)
## The distance between variables and the origin measures 
## the quality of the variables on the factor map
## Variables that are away from the origin are well represented on the factor map.

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE) 
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2)
## A high cos2 indicates a good representation of the variable on the principal component 
## In this case the variable is positioned close to the circumference of the correlation circle
## A low cos2 indicates that the variable is not perfectly represented by the PCs
## In this case the variable is close to the center of the circle

## The cos2 values are used to estimate the quality of the representation
## The closer a variable is to the circle of correlations, 
## the better its representation on the factor map 
## (and the more important it is to interpret these components)
## Variables that are closed to the center of the plot are less important for the first components

# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)

## C1 and C2 are the contributions of the variable on PC1 and PC2, respectively
## Eig1 and Eig2 are the eigenvalues of PC1 and PC2, respectively
## Recall that eigenvalues measure the amount of variation retained by each PC
## Specifically, It can be seen that the variables NRG_consumption, Population, Real_GDP_PC, GDP_K 
## contribute the most to the dimensions 1 and 2

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)
fviz_pca_var(res.pca)

fviz_pca_ind(res.pca, col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
              )                            
fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)                             
                             
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster"
              )
fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = environment$Country, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#8A2BE2", "#006400", "#7CFC00", "#191970", "#EE82EE", "#CD853F", "#8B4513", "#FFEFD5", "#8B0000", "#FF0000", "#EEE685","#E0FFFF", "#00FFFF", "#FFFF00","#9D8CED", "#213465", "#098734", "#FACA81", "#99FF51", "#8754AC", "#0881BB", "#D45DA7", "510334", "#EEDA31", "#59946B"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
             )

