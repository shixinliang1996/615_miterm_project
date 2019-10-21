
library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(magrittr)
library(GGally)
library(ggmosaic)
library(psych)
library(GPArotation)

##########################
## Make plots
Data <- read.csv("Data.csv", header = TRUE)
Data <- Data[, 1:332]
Data <- Data[, -c(1,2,3,4,46,59,61,129:145,248:281,286,313,315:317,320,322,325,326,330,331)]

## Satisfaction with life  
# Satisfaction.with.your.life  
Data$V23..Satisfaction.with.your.life <- as.factor(Data$V23..Satisfaction.with.your.life)
levels(Data$V23..Satisfaction.with.your.life) <- c("NA", 1,2,3,4,5,6,7,8,9,10)
f1 <- ggplot(Data) +
    aes(x = V23..Satisfaction.with.your.life, fill = V23..Satisfaction.with.your.life) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Satisfaction level", y = "Frequency") + 
    ggtitle("Satisfaction Frequency")+ labs(caption = "Figure 1")

# Feeling.of.happiness
Data$V10..Feeling.of.happiness <- as.factor(Data$V10..Feeling.of.happiness)
levels(Data$V10..Feeling.of.happiness) <- c("NA", "very happy", "fairly happy", "not very happy", "completely not happy")
f2 <- ggplot(Data) +
    aes(x = V10..Feeling.of.happiness, fill = V10..Feeling.of.happiness) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Feeling of Happiness", y = "Frequency") + 
    ggtitle("Feeling of Happiness Frequency")+ labs(caption = "Figure 2")

# How.much.freedom.of.choice.and.control.over.own.life
Data$V55..How.much.freedom.of.choice.and.control.over.own.life <- as.factor(Data$V55..How.much.freedom.of.choice.and.control.over.own.life)
levels(Data$V55..How.much.freedom.of.choice.and.control.over.own.life) <- c("NA", 1,2,3,4,5,6,7,8,9,10)
f3 <- ggplot(Data) +
    aes(x = Data$V55..How.much.freedom.of.choice.and.control.over.own.life, fill = Data$V55..How.much.freedom.of.choice.and.control.over.own.life) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Freedom to Choose", y = "Frequency") + 
    ggtitle("Freedom to Choose")

## Financial situation 
# Scale.of.incomes
Data$V239..Scale.of.incomes <- as.factor(Data$V239..Scale.of.incomes)
levels(Data$V239..Scale.of.incomes) <- c("NA", 1,2,3,4,5,6,7,8,9,10)
f4 <- ggplot(Data) +
    aes(x = V239..Scale.of.incomes, fill = V239..Scale.of.incomes) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Incomes", y = "Frequency") + 
    ggtitle("Incomes")

# Satisfaction.with.financial.situation.of.household
Data$V59..Satisfaction.with.financial.situation.of.household <- as.factor(Data$V59..Satisfaction.with.financial.situation.of.household)
levels(Data$V59..Satisfaction.with.financial.situation.of.household) <- c("NA", 1,2,3,4,5,6,7,8,9,10)
f5 <- ggplot(Data) +
    aes(x = V59..Satisfaction.with.financial.situation.of.household, fill = V59..Satisfaction.with.financial.situation.of.household) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Satisfaction with financial situation", y = "Frequency") + 
    ggtitle("Satisfaction with financial situation")

# Are.you.the.chief.wage.earner.in.your.house
Data$V235..Are.you.the.chief.wage.earner.in.your.house <- as.factor(Data$V235..Are.you.the.chief.wage.earner.in.your.house)
levels(Data$V235..Are.you.the.chief.wage.earner.in.your.house) <- c("NA", "Yes", "No")
f6 <- ggplot(Data) +
    aes(x = V235..Are.you.the.chief.wage.earner.in.your.house, fill = V235..Are.you.the.chief.wage.earner.in.your.house) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Chief Wage Earner", y = "Frequency") + 
    ggtitle("Chief Wage Earner")

# Is.the.chief.wage.earner.employed.now.or.not
Data$V236..Is.the.chief.wage.earner.employed.now.or.not <- as.factor(Data$V236..Is.the.chief.wage.earner.employed.now.or.not)
levels(Data$V236..Is.the.chief.wage.earner.employed.now.or.not) <- c("NA", "Yes", "No")
f7 <- ggplot(Data) +
    aes(x = V236..Is.the.chief.wage.earner.employed.now.or.not, fill = V236..Is.the.chief.wage.earner.employed.now.or.not) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Chief Wage Earner Employment", y = "Frequency") + 
    ggtitle("Chief Wage Earner Employment")

# Family.savings.during.past.year
Data$V237..Family.savings.during.past.year <- as.factor(Data$V237..Family.savings.during.past.year)
levels(Data$V237..Family.savings.during.past.year) <- c("NA", "have savings", "make ends meet", "spend some savings", "spend all savings and borrow money")
f8 <- ggplot(Data) +
    aes(x = V237..Family.savings.during.past.year, fill = V237..Family.savings.during.past.year) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Family Savings", y = "Frequency") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
    ggtitle("Family Savings")

## Identity  
# How.proud.of.nationality
Data$V211..How.proud.of.nationality <- as.factor(Data$V211..How.proud.of.nationality)
levels(Data$V211..How.proud.of.nationality) <- c("NA", "very proud", "rather proud", "not very proud", "not proud at all", "I am not a Hong Konger.")
f9 <- ggplot(Data) +
    aes(x = V211..How.proud.of.nationality, fill = V211..How.proud.of.nationality) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Proud of Nationality", y = "Frequency") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
    ggtitle("Proud of Nationality")

# I see myself as a part of the local community, a Hong Konger
Data$V213..I.see.myself.as.part.of.my.local.community <- as.factor(Data$V213..I.see.myself.as.part.of.my.local.community)
levels(Data$V213..I.see.myself.as.part.of.my.local.community) <- c("NA", "strongly agree", "agree", "disagree", "strongly disagree")
f10 <- ggplot(Data) +
    aes(x = V213..I.see.myself.as.part.of.my.local.community, fill = V213..I.see.myself.as.part.of.my.local.community) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Identity of Hong Konger", y = "Frequency") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
    ggtitle("Identity of Hong Konger")

# I see myself as a part of the Chinese nation
Data$V214..I.see.myself.as.part.of.the..country..nation <- as.factor(Data$V214..I.see.myself.as.part.of.the..country..nation)
levels(Data$V214..I.see.myself.as.part.of.the..country..nation) <- c("NA", "strongly agree", "agree", "disagree", "strongly disagree")
f11 <- ggplot(Data) +
    aes(x = V214..I.see.myself.as.part.of.the..country..nation, fill = V214..I.see.myself.as.part.of.the..country..nation) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Identity of part of the Chinese nation", y = "Frequency") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8)) +
    ggtitle("Identity of part of the Chinese nation")

## Confidence in the government   
# Confidence..The.police
Data$V113..Confidence..The.police <- as.factor(Data$V113..Confidence..The.police)
levels(Data$V113..Confidence..The.police) <- c("NA", "a lot", "some", "not very much", "none at all")
f12 <- ggplot(Data) +
    aes(x = V113..Confidence..The.police, fill = V113..Confidence..The.police) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Confidence in the Police", y = "Frequency") + 
    ggtitle("Confidence in the Police")

# Confidence..Hong Kong government
Data$V115..Confidence..The.government..in.your.nation.s.capital. <- as.factor(Data$V115..Confidence..The.government..in.your.nation.s.capital.)
levels(Data$V115..Confidence..The.government..in.your.nation.s.capital.) <- c("NA", "a lot", "some", "not very much", "none at all")
f13 <- ggplot(Data) +
    aes(x = V115..Confidence..The.government..in.your.nation.s.capital., fill = V115..Confidence..The.government..in.your.nation.s.capital.) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Confidence in Hong Kong government", y = "Frequency") + 
    ggtitle("Confidence in Hong Kong government")

# Confidence..Mainland government
Data$V125_17..Confidence..Mainland.government <- as.factor(Data$V125_17..Confidence..Mainland.government)
levels(Data$V125_17..Confidence..Mainland.government) <- c("NA", "a lot", "some", "not very much", "none at all")
f14 <- ggplot(Data) +
    aes(x = V125_17..Confidence..Mainland.government, fill = V125_17..Confidence..Mainland.government) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Confidence in Mainland government", y = "Frequency") +
    ggtitle("Confidence in Mainland government")

## Attitude towards democracy  
# Importance.of.democracy
Data$V140..Importance.of.democracy <- as.factor(Data$V140..Importance.of.democracy)
levels(Data$V140..Importance.of.democracy) <- c("NA", 1,2,3,4,5,6,7,8,9,10)
f15 <- ggplot(Data) +
    aes(x = V140..Importance.of.democracy, fill = V140..Importance.of.democracy) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Importance of Democracy", y = "Frequency") +
    ggtitle("Importance of Democracy")

# How.democratically.is.this.country.being.governed.today
Data$V141..How.democratically.is.this.country.being.governed.today <- as.factor(Data$V141..How.democratically.is.this.country.being.governed.today)
levels(Data$V141..How.democratically.is.this.country.being.governed.today) <- c("NA", 1,2,3,4,5,6,7,8,9,10)
f16 <- ggplot(Data) +
    aes(x = V141..How.democratically.is.this.country.being.governed.today, fill = V141..How.democratically.is.this.country.being.governed.today) +
    geom_bar() +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Today's Democracy Level", y = "Frequency") +
    ggtitle("Today's Democracy Level")

## ggpairs plot between Confidence in Mainland government and other parts
new_data <- Data[,c(122,112,110,223,55,249,20)]
# rename the variables
names(new_data)[names(new_data)=="V125_17..Confidence..Mainland.government"]="Confidence.in.Mainland.government"
names(new_data)[names(new_data)=="V115..Confidence..The.government..in.your.nation.s.capital."]="Confidence.in.HongKong.government"
names(new_data)[names(new_data)=="V113..Confidence..The.police"]="Confidence.in.Police"
names(new_data)[names(new_data)=="V214..I.see.myself.as.part.of.the..country..nation"]="Part.of.the.Chinese.nation"
names(new_data)[names(new_data)=="V59..Satisfaction.with.financial.situation.of.household"]="Satisfaction.in.Financial.Situation"
names(new_data)[names(new_data)=="V239..Scale.of.incomes "]="Incomes.Level"
names(new_data)[names(new_data)=="V23..Satisfaction.with.your.life"]="Satisfaction.with.Life"
as.factor(new_data$Confidence.in.Mainland.government)
as.factor(new_data$Confidence.in.HongKong.government)
as.factor(new_data$Confidence.in.Police)
as.factor(new_data$Part.of.the.Chinese.natio)
as.factor(new_data$Satisfaction.in.Financial.Situation)
as.factor(new_data$Incomes.Level)
as.factor(new_data$Satisfaction.with.Life)
# ggpairs plot
gplot <- GGally::ggpairs(subset(new_data,is.na(new_data$Confidence.in.Mainland.government)==FALSE), mapping = aes(color=Confidence.in.Mainland.government))
gplot <- gplot + theme(axis.text.x = element_text(angle = 90, hjust = 1, size=8))
gplot$nrow <- 1
gplot$yAxisLabels <- gplot$yAxisLabels[1]
for (j in 1:gplot$ncol){
    gplot[1, j] <- gplot[1, j] +
        scale_fill_viridis_d(option = "viridis")
}
gplot + ggtitle("ggpairs plots between Confidence in Mainland government and other main variables")

## Age and other variables
# satisfaction by age
a1 <- ggplot(Data, mapping = aes(x = V23..Satisfaction.with.your.life, y = V242..Age, fill = V23..Satisfaction.with.your.life)) +
    geom_boxplot(mapping = aes(group = V23..Satisfaction.with.your.life)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Satisfaction level", y = "Age") + 
    ggtitle("Age and Satisfaction level")

# Scale.of.incomes by age
a2 <- ggplot(Data, mapping = aes(x = V239..Scale.of.incomes, y = V242..Age, fill = V239..Scale.of.incomes)) +
    geom_boxplot(mapping = aes(group = V239..Scale.of.incomes)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Incomes", y = "Age") + 
    ggtitle("Age and Incomes")

# Identity of part of the Chinese nation of Nationality by age
a3 <- ggplot(Data, mapping = aes(x = V214..I.see.myself.as.part.of.the..country..nation, y = V242..Age, fill = V214..I.see.myself.as.part.of.the..country..nation)) +
    geom_boxplot(mapping = aes(group = V214..I.see.myself.as.part.of.the..country..nation)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Identity of part of the Chinese nation", y = "Age") + 
    ggtitle("Age and Identity of part of the Chinese nation")+ labs(caption = "Figure 16") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size=8))

# Confidence in the Police by age
a4 <- ggplot(Data, mapping = aes(x = V113..Confidence..The.police, y = V242..Age, fill = V113..Confidence..The.police)) +
    geom_boxplot(mapping = aes(group = V113..Confidence..The.police)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Confidence in the Police", y = "Age") + 
    ggtitle("Age and Confidence in the Police")

# Confidence in Hong Kong government by age
a5 <- ggplot(Data, mapping = aes(x = V115..Confidence..The.government..in.your.nation.s.capital., y = V242..Age, fill = V115..Confidence..The.government..in.your.nation.s.capital.)) +
    geom_boxplot(mapping = aes(group = V115..Confidence..The.government..in.your.nation.s.capital.)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Confidence in Hong Kong government", y = "Age") + 
    ggtitle("Age and Confidence in Hong Kong government")

# Confidence in Mainland government by age
a6 <- ggplot(Data, mapping = aes(x = V125_17..Confidence..Mainland.government, y = V242..Age, fill = V125_17..Confidence..Mainland.government)) +
    geom_boxplot(mapping = aes(group = V125_17..Confidence..Mainland.government)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Confidence in Mainland government", y = "Age") + 
    ggtitle("Age and Confidence in Mainland government")

# Today's Democracy Leve by age
a7 <- ggplot(Data, mapping = aes(x = V141..How.democratically.is.this.country.being.governed.today, y = V242..Age, fill = V141..How.democratically.is.this.country.being.governed.today)) +
    geom_boxplot(mapping = aes(group = V141..How.democratically.is.this.country.being.governed.today)) +
    scale_fill_viridis_d(option = "viridis") + 
    theme_minimal()+labs(x = "Today's Democracy Level", y = "Age") + 
    ggtitle("Age and Today's Democracy Level")

##########################

ui <- fluidPage(
    
    titlePanel("Exploration on Hong Kong People’s Values and Belief"),
    
    navlistPanel(
        "EDA of Five Main Parts",
        tabPanel("Satisfaction with Life", 
                 plotOutput("Satisfaction_life"),
                 plotOutput("Feeling_happines"),
                 plotOutput("Freedom_life")),
        
        tabPanel("Financial Situation", 
                 plotOutput("Incomes"),
                 plotOutput("Satisfaction_financial_situation"),
                 plotOutput("Chief_Wage_Earne"),
                 plotOutput("Employment"),
                 plotOutput("Family_Savings")),
        
        tabPanel("Identity", 
                 plotOutput("Proud_of_Nationality"),
                 plotOutput("Identity_Hong_Konger"),
                 plotOutput("Identity_Chinese_nation")),
        
        tabPanel("Confidence in The Government", 
                 plotOutput("Confidence_Police"),
                 plotOutput("Confidence_Hong_Kong_government"),
                 plotOutput("Confidence_Mainland_government")),
        
        tabPanel("Attitude Towards Democracy", 
                 plotOutput("Importance_Democracy"),
                 plotOutput("Democracy_Level")),
        
        "EDA of Correlations between Key Variables",
        
        tabPanel("Plots between Age & Other Parts",
                 plotOutput("A1"),
                 plotOutput("A2"),
                 plotOutput("A3"),
                 plotOutput("A4"),
                 plotOutput("A5"),
                 plotOutput("A6"),
                 plotOutput("A7")),
        
        tabPanel("Plots between Confidence in Mainland government & Other Parts", 
                 plotOutput("ggpairs"))
    )
)

server <- function(input, output) {
    
    output$Satisfaction_life <- renderPlot({f1})
    output$Feeling_happines <- renderPlot({f2})
    output$Freedom_life <- renderPlot({f3})
    
    output$Incomes <- renderPlot({f4})
    output$Satisfaction_financial_situation <- renderPlot({f5})
    output$Chief_Wage_Earne <- renderPlot({f6})
    output$Employment <- renderPlot({f7})
    output$Family_Savings <- renderPlot({f8})
    
    output$Proud_of_Nationality <- renderPlot({f9})
    output$Identity_Hong_Konger <- renderPlot({f10})
    output$Identity_Chinese_nation <- renderPlot({f11})
    
    output$Confidence_Police <- renderPlot({f12})
    output$Confidence_Hong_Kong_government <- renderPlot({f13})
    output$Confidence_Mainland_government <- renderPlot({f14})
    
    output$Importance_Democracy <- renderPlot({f15})
    output$Democracy_Level <- renderPlot({f16})
    
    output$A1 <- renderPlot({a1})
    output$A2 <- renderPlot({a2})
    output$A3 <- renderPlot({a3})
    output$A4 <- renderPlot({a4})
    output$A5 <- renderPlot({a5})
    output$A6 <- renderPlot({a6})
    output$A7 <- renderPlot({a7})
    output$A8 <- renderPlot({a8})
    
    output$ggpairs <- renderPlot({gplot})
    
}


# Run the application 
shinyApp(ui = ui, server = server)
