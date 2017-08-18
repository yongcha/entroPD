# End-to-end visualization using ggplot2
`r Sys.Date()`  




 * `ggplot2`는 데이터 시각화할때 자주 사용되는 패키지이다. 
 
 * 복잡한 데이터를 핸들링하고나서, 각기 다른 측면에서 데이터를 살펴볼 때 시각화를 한다. 
 
 * `plot()`, `matplot()`, `barplot()` 등 간단한 데이터를 시각화할 때는 좋지만, `ggplot2`에 익숙해지면 훨씬 더 파워풀한 시각화 패키지가 된다.
 
 * 본 문서에서는 `ggplot2`를 이용해서 데이터를 시각화하는 것들을 정리하였다.
 
 * 데이터 핸들링은 `data.table`로 하였다.


## 1. Pima Indian Diabetes data

 * R에 내장되어있는 데이터들 중 Pima Indian Diabetes data를 한번 살펴보고, 시각화를 해보도록하자.

 * Pima Indian Diabetes data는 WHO가 Pima족 Indian의 후손이자 Phoenix, Arizona 근처에 살고 최소 21세이상인 여성을 대상으로 당뇨병 여부를 테스트 한 것에 대한 데이터이다.
 
 * 결측값은 없으며, 자세한 것은 `help(Pima.te)`을 통해 살펴보면 된다.
 
 * `ggplot2`를 사용해서 우리가 잘 모르는 데이터에 대해서 시각적으로 살펴본다.
 
 * 이 과정은 데이터 모델링을 준비하는데에 도움이 되는 데이터 탐색의 일부이다.
 

```
     npreg             glu              bp              skin      
 Min.   : 0.000   Min.   : 65.0   Min.   : 24.00   Min.   : 7.00  
 1st Qu.: 1.000   1st Qu.: 96.0   1st Qu.: 64.00   1st Qu.:22.00  
 Median : 2.000   Median :112.0   Median : 72.00   Median :29.00  
 Mean   : 3.485   Mean   :119.3   Mean   : 71.65   Mean   :29.16  
 3rd Qu.: 5.000   3rd Qu.:136.2   3rd Qu.: 80.00   3rd Qu.:36.00  
 Max.   :17.000   Max.   :197.0   Max.   :110.00   Max.   :63.00  
      bmi             ped              age         type    
 Min.   :19.40   Min.   :0.0850   Min.   :21.00   No :223  
 1st Qu.:28.18   1st Qu.:0.2660   1st Qu.:23.00   Yes:109  
 Median :32.90   Median :0.4400   Median :27.00            
 Mean   :33.24   Mean   :0.5284   Mean   :31.32            
 3rd Qu.:37.20   3rd Qu.:0.6793   3rd Qu.:37.00            
 Max.   :67.10   Max.   :2.4200   Max.   :81.00            
```


```
   npreg glu bp skin  bmi   ped age type
1:     6 148 72   35 33.6 0.627  50  Yes
2:     1  85 66   29 26.6 0.351  31   No
3:     1  89 66   23 28.1 0.167  21   No
4:     3  78 50   32 31.0 0.248  26  Yes
5:     2 197 70   45 30.5 0.158  53  Yes
6:     5 166 72   19 25.8 0.587  51  Yes
```

 * `type`변수가 우리가 Target으로 하는 변수이다.
 
## 2. Distributions across categories

 * Target 변수가 범주형일때 (여기서는 `type`), 연속형 inputs 변수들에 대해 각각 분포를 조사해보는 것이 좋은 시작점이 될 수 있다.(저자의 생각)
 
 * 이는 우리에게 유용할 것 같은 input 변수들이 무엇인지에 대해 전체적인 느낌을 줄 수 있다.
 
 * 이를 위해 각기 다른 목적으로 boxplots과 density plot을 둘 다 그리는 것이 좋다.(저자의 생각)

### 2.1. Boxplots

 * 먼저, boxplots을 `ggplot2-style`로 그려보자.
 
 * 전통적인 `boxplot()`보다 plotting에 필요한 arguments를 이용해서 그래프를 잘 가꾸고 여러 개의 plots을 하나의 grid에 균일하게 그려냄으로써, 보기 좋은 plot에 도달한다.
 
 * 아래의 그래프들을 살펴보면, Target내의 2개 범주를 잘 분류하거나 그렇지 않은 input 변수들을 볼 수 있다.

```
SubsetDT <- copy(PimaIndDT[, -c(1)])

library(gridExtra)
library(ggplot2)

p <- list()

invisible(
  sapply(names(SubsetDT)[-7], 
         function(inp){
           p[[inp]] <- ggplot(data=SubsetDT, aes_string(x="type", y=inp)) + 
             geom_boxplot(aes(fill=factor(type))) + guides(fill=FALSE) + 
             theme(axis.title.y = element_text(face="bold", size=14))
          assign("p", p, envir = .GlobalEnv)
}))

do.call(grid.arrange, c(p, ncol=3))
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### 2.2. Density plots

 * `ggplot2`를 이용해서 overlay-density를 그렸다.
 
 * 이를 통해 이 데이터에서 당뇨병 환자들은 임신횟수(npreg)와 연관이 있다고 얘기할 수 있다.
 
 * `alpha`라는 argument를 통해 투명도를 조절하여 plotting 할 수 있다.

```
g <- ggplot(data = PimaIndDT, aes(npreg))
g + geom_density(aes(fill=factor(type)), alpha=0.8) +
  labs(title="Density plot",
       subtitle="# Pregnancies Grouped by Diabetes Type",
       x="# Pregnancies",
       fill="Diabetes Type")
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


## 3. Grid views

 * 이번에는 multi-dimensional views가 가능하도록 시각화를 해보자.
    - 3-way table과 비슷하지만 숫자를 사용하지않고 멋지게 시각화
 
 * 이를 통해 Target에 대해 중요한 input 변수가 몇개인지 알아보도록 한다.
 
 * `facet_grid()`를 이용하였으며, `ggplot2`의 유연한 arguments를 조절하여 더욱 괜찮은 그래프를 그릴 수 있다.


### 3.1. Data preparation

 * Facet-wrapping 과 gridding은 더 깊게 데이터를 보기 위해 꼭 필요한 것들이다.
 
 * 프로세스가 여러 단계로 나누어져있지만 그렇게 복잡하지 않다.
 
 * 1개의 연속형 변수를 이용하여 새로운 범주형 변수를 생성하였다.
 
 * 변수를 생성하는데 있어서 원래 문서에서는 `magrittr`, `plyr`, `dplyr`을 이용하였으나 여기서는 `data.table`로 처리하였다.


```
PimaIndDT[, c("Skin", "BMI", "Ped") :=
            list(as.factor(ifelse(skin <= 29, "low skin fold", "high skin fold")),
                 as.factor(ifelse(bmi <= 33, "low BMI", "high BMI")),
                 as.factor(ifelse(ped <= 0.31, "low pedigree",
                           ifelse(ped > 0.3134 & ped <= 0.5844, 
                           "medium pedigree", "high pedigree"))))
          ]

head(PimaIndDT)
```


```
   npreg glu bp skin  bmi   ped age type           Skin      BMI
1:     6 148 72   35 33.6 0.627  50  Yes high skin fold high BMI
2:     1  85 66   29 26.6 0.351  31   No  low skin fold  low BMI
3:     1  89 66   23 28.1 0.167  21   No  low skin fold  low BMI
4:     3  78 50   32 31.0 0.248  26  Yes high skin fold  low BMI
5:     2 197 70   45 30.5 0.158  53  Yes high skin fold  low BMI
6:     5 166 72   19 25.8 0.587  51  Yes  low skin fold  low BMI
               Ped
1:   high pedigree
2: medium pedigree
3:    low pedigree
4:    low pedigree
5:    low pedigree
6:   high pedigree
```

### 3.2. Reshaping the data

 * `facet_grid()`을 활용하기 좋게 데이터 형태를 약간 바꿔야한다.
    - "wide"-form data를 "long"-form data로 바꿔줘야함

 * `tidyr` 패키지를 이용할 수 있으나 여기서는 `melt()`를 이용하여 형태를 바꿔주었다.

```
GridDT <- copy(PimaIndDT[, c("Skin", "BMI", "Ped", "type"), with = F])
GridDT <- melt(data = GridDT, measure.vars = "type", value.name = "DiabetValue")

GridDT[, size := rep(1.5, nrow(GridDT))]
s <- 1.5

head(GridDT)
```


```
             Skin      BMI             Ped variable DiabetValue size
1: high skin fold high BMI   high pedigree     type         Yes  1.5
2:  low skin fold  low BMI medium pedigree     type          No  1.5
3:  low skin fold  low BMI    low pedigree     type          No  1.5
4: high skin fold  low BMI    low pedigree     type         Yes  1.5
5: high skin fold  low BMI    low pedigree     type         Yes  1.5
6:  low skin fold  low BMI   high pedigree     type         Yes  1.5
```

### 3.3. Facet Grid

 * 여기서 Facet Grid를 하는 목적은 BMI분포를 type에 대해서 "matrix" 혹은 "grid" 형태로 살펴보기 위함이다.
    - pedigree/skin fold의 2X3 조합 table 형태

 * 즉, 낮은 pedigree / 낮은 skin fold에 대해서 BMI가 type에 따라 어떻게 분포하는지를 보는 것이다.
 
 * 이러한 정보를 단 1개의 plot으로 묶어서 볼수 있다.

```
ggplot(data = GridDT, aes(x=DiabetValue, fill=BMI)) + geom_bar() + 
  facet_grid(Skin ~ Ped)
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

 * 괜찮은 그래프지만, 눈에 잘 띄지 않는다.
 
 * 그래서 각각의 box의 outline과 글씨들을 굵게하였다.

```
p <- ggplot(data=GridDT, aes(x=DiabetValue, fill=BMI)) + geom_bar() +  # Barplot
  geom_rect(aes(fill=NA, size=size),
            xmin =-Inf,xmax=Inf,ymin=-Inf,ymax=Inf,
            alpha = 0.0002, colour="black",show.legend = F) + 
  scale_size(range=c(s,s), guide=FALSE) + 
  facet_grid(Skin ~ Ped) +
  theme(strip.text.x = element_text(face="bold", size=12)) +
  theme(strip.text.y = element_text(face="bold", size=12)) 

plot(p)
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

 * 각 box의 outline이 부각되고 글자들도 굵어져서 꽤 잘보이게 되었다.
 
 * 위 그래프로부터 해석해보자면,
    - 1) 높은 피하지방의 두께(skin fold)과 높은 BMI지수는 서로 연관이 있고, 이 둘은 당뇨병 환자의 수가 많은 것과도 연관이 있다.
    - 2) 위의 내용은 당뇨병 혈통기능이 높을수록 더욱 그렇다.

 * 이러한 종류의 grid plot은 다차원 데이터를 보는데에 매우 파워풀하다.

## 4. Heatmaps

 * `ggplot2`를 이용해서 Heatmaps을 시각화해보자.
 
 * Heatmaps을 통해 기본 분석전에 패턴 감지가 가능

 * 이 그래프를 그릴때, 각각의 input 변수들을 type에 따라 패턴을 보여주고자 한다.

### 4.1. Data preparation

 * 보통 `ggplot2`를 사용하기 전에 데이터를 준비해야한다.
 
 * 아래는 Heatmaps를 시각화하는데에 필요한 데이터로 준비하는 코드이다.

```
PimaIndDT <- PimaIndDT[, 1:8]

HeatDT <- copy(PimaIndDT[, lapply(.SD, as.double), .SDcols = c(1:7)])
HeatDT[, id := 1:nrow(PimaIndDT)]

HeatDT <- melt(HeatDT, id.vars = "id")
HeatDT[, rescale := scale(value), by = "variable"]

# Color scale for heatmap
library(RColorBrewer)
colors <- brewer.pal(9, 'Reds')

# Lines to split patients into diabetic/non-diabetic
my.lines <- data.table(x1 = 0.5, x2 = 7.5, y1 = 223.5, y2 = 223.5)
```




### 4.2. Rendering the heatmap

```
# Basic plot
p <- ggplot(HeatDT, aes(as.factor(variable), as.factor(id), group=id)) + 
  geom_tile(aes(fill = rescale), colour = "white") +
  scale_fill_gradient(low="green", high="red")

# Make adjustments
base_size <- 9
p_adj <- p + theme_grey(base_size = base_size) + labs(x = "", y = "") + 
  scale_x_discrete(expand = c(0,0)) + 
  geom_segment(data=my.lines, aes(x = x1, y = y1, xend=x2, yend=y2), 
               size=1, inherit.aes = F) +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

plot(p_adj)
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


 * Heatmaps에서 우리가 관심있는 부분을 명확하게 볼 수 있다.
 
 * Heatmaps의 각각 두께는 무시하였다.
 
 * viewer에서 좀 더 튀게보이게끔하기 위해 `geom_rect()`를 이용한다.
 
```
# Borders of rectangles to indicate areas of interest on heatmap
my.lines.rect.1 <- data.table(xmin = 1.5, xmax = 2.5, ymin = 223.5, ymax = 255.5)
my.lines.rect.2 <- data.table(xmin = 3.5, xmax = 4.5, ymin = 223.5, ymax = 332)
my.lines.rect.3 <- data.table(xmin = 5.5, xmax = 6.5, ymin = 223.5, ymax = 280.5)

p_adj <- p_adj + geom_rect(data=my.lines.rect.1, aes(xmin = xmin, xmax = xmax, 
            ymin = ymin, ymax = ymax), fill = NA, col = "black", lty=2, inherit.aes = F) +
  geom_rect(data=my.lines.rect.2, aes(xmin = xmin, xmax = xmax, 
            ymin = ymin, ymax = ymax), fill = NA, col = "black", lty=5, inherit.aes = F) +
  geom_rect(data=my.lines.rect.3, aes(xmin = xmin, xmax = xmax, 
            ymin = ymin, ymax = ymax), fill = NA, col = "black", lty=4, inherit.aes = F)

plot(p_adj)
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-11-1.png)<!-- -->



## 5. Segmentation in a scatterplot

 * 마지막으로, 기본적인 수준의 클러스터링을 해보자.
 
 * 이는 model-based 클러스터링이 아니라 scatterplot을 기반으로 간단하게 해보는 것이다.
 
 * `ggplot2`를 통해 시각화하는데 클러스터들을 시각화하고 표시함으로써 모호함이 거의 없게끔 했다.
 
 * 여기서 예로 보여줄 것은 age, BMI, glucose 등을 가지고 여러 층의 정보에 따라 "클러스터"들을 좀 더 잘 보이게끔 시각화하는 것입니다.

```
PimaIndDT[, Age := ifelse(age < 30, "<30 yrs", ">= 30 yrs")]


l <- ggplot(PimaIndDT, aes(x = glu, y = bmi)) + 
  geom_point(aes(col = factor(type), shape = factor(Age)), size = 3) +
  scale_color_brewer(name = "Type", palette = "Set1") +
  scale_shape(name = "Age") +
  scale_linetype_manual(values = c("High BMI - Diabetic" = "dotted", "Low BMI - Not Diabetic" = "dashed"),
                        name = "Segment")

plot(l)
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

 * 위 그림을 봤을때 어느정도 클러스터가 보이는 것을 확인 할 수 있다.
 
 * 클러스터를 표시해보면 아래와 같다.

```
l + geom_rect(aes(linetype = "High BMI - Diabetic"), 
              xmin = 160, ymax = 40, fill = NA, xmax = 200, 
              ymin = 25, col = "black") + 
  geom_rect(aes(linetype = "Low BMI - Not Diabetic"), 
            xmin = 0, ymax = 25, fill = NA, xmax = 120, 
            ymin = 10, col = "black")
```

![](E2Eggplot2_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

 * 여기까지 해서 간단한 data를 가지고 `ggplot2`를 이용하여 여러가지를 시각화해봤다.
 
 * 깔끔한 시각화를 통해 여러 정보를 다양한 측면에서 얻을 수 있었다.


<br>

******************************************************************

Reference : [Rviews](https://rviews.rstudio.com/2017/08/14/end-to-end-visualization-using-ggplot2/)

******************************************************************



