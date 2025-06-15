library(ggplot2)
library(treemapify)

# Assuming total_churned is a continuous variable, you can summarize the counts
dataset_summary <- dataset %>%
  group_by(Churn, yes) %>%
  summarise(count = n())

# Bar Plot of Unchurned vs. Churned customers
ggplot(dataset_summary, aes(Churn , count, fill = Churn)) +
  geom_bar(position = position_dodge(width = 0.8), stat = "identity", width = 0.7) +
  geom_text(aes(label = count),
            position = position_dodge(width = 0.8), vjust = -0.2, color = "black") +
  scale_fill_manual(values = c("blue", "orange")) +
  labs(title = "Bar Plot of Unchurned vs. Churned customers",caption = "Total customers = 7032",
       x = "Churn",
       y = "Count") +
  theme_minimal()
  

# Create a data frame with the counts of churned and non-churned customers
churn_data <- data.frame(label = c("Churned", "Non-Churned"),
                         count = c(total_churned, total_customers - total_churned))

# Calculate the percentage
churn_data$percent <- (churn_data$count / sum(churn_data$count)) * 100

# treemap of total churn rate
treemap_plot <- ggplot(churn_data, aes(area = percent, fill = label, label = paste0(round(percent, 2), "%"))) +
  geom_treemap() +
  geom_treemap_text(place = "centre", size = 20, color = "black") +
  labs(title = "Churn Rate",
       caption = paste("Total Customers: ", total_customers))
print(treemap_plot)




# Churn in Female vs Male Customers
ggplot(dataset, aes(x = gender, fill = Churn)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.5,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn in Female vs Male Customers") +
  theme_minimal()


# Treemap of senior citizen customer's Churn rate
ggplot(data, aes(area = Count, fill = SeniorCitizen, label = paste0(Percent, "%"))) +
  geom_treemap(layout = "squarified") +
  geom_treemap_text(place = "centre", size = 15, color = "black") +
  labs(title = "TreeMap of Churned vs. Unchurned Senior Citizens",
       caption = "Total senior citizens = 1142") +
  theme_minimal()


# barplot  of Churn in Customers with and without Partners
ggplot(partner_data, aes(x = PartnerStatus, y = ChurnRate, fill = PartnerStatus)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(ChurnRate, "%")), position = position_dodge(width = 0.8), vjust = -0.2, color = "black") +
  labs(title = "Percentage of Churn in Customers with and without Partners",
       x = "Partner Status", y = "Churn Rate") +
  theme_minimal()


# Create a bar plot using ggplot2

dependent_data <- data.frame(
  DependentStatus = c("With Dependents", "Without Dependents"),
  ChurnRate = c(15.53, 31.28),  # Churn rates in percentages
  ChurnedCount = c(326, 1543),
  TotalCount = c(2099, 4933)
)


ggplot(dependent_data, aes(x = DependentStatus, y = ChurnRate, fill = DependentStatus)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(ChurnRate, "%")), position = position_dodge(width = 0.8), vjust = -0.2, color = "black") +
  labs(title = "Percentage of Churn in Customers with and without dependents",
       x = "Dependent Status", y = "Churn Rate") +
  theme_minimal()

# Churn Rate Across Tenure Ranges
result <- data.frame(
  tenure_range = c("0-12", "12-24", "24-36", "36-48", "48+"),
  tenure_range_merged = c("1 to 12", "13 to 24", "25 to 36", "37 to 48", "49 to 72"),
  churn_rate = c(47.68, 28.71, 21.63, 19.03, 9.51)
)


ggplot(result, aes(x = tenure_range, y = churn_rate, fill = tenure_range)) +
  geom_bar(stat = "identity", width = 0.7) +
  geom_text(aes(label = paste0(churn_rate, "%")), vjust = -0.2, color = "black") +
  labs(title = "Churn Rate Across Tenure Ranges",
       x = "Tenure Range(in months)", y = "Churn Rate") +
  theme_minimal()

# visualization of internet service type 
ggplot(dataset,aes(Churn,fill = InternetService))+
  geom_bar() +
  labs(title = "Customers using different types of internet service") +
  facet_wrap(~InternetService) +
  theme_minimal()



# visualization of Churn based on Online Security
ggplot(dataset, aes(x = Churn, fill = OnlineSecurity )) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  facet_wrap(~OnlineSecurity)

# visualization of device protection services
ggplot(dataset, aes(x =Churn , fill = DeviceProtection)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to device protection ") +
  facet_wrap ( ~ DeviceProtection) +
  theme_minimal()



# visualization of Onlinne backup services

ggplot(dataset, aes(x =Churn , fill = OnlineBackup)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to Online Backup ") +
  facet_wrap ( ~ OnlineBackup) +
  theme_minimal()

# visualization of tech support services
ggplot(dataset,aes(TechSupport,fill = Churn)) +
  geom_bar()
ggplot(dataset, aes(x =Churn , fill = TechSupport)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to Tech Support ") +
  facet_wrap ( ~ TechSupport) +
  theme_minimal()

# visualization of TV Streaming
ggplot(dataset,aes(StreamingTV,fill = Churn)) +
  geom_bar()
ggplot(dataset, aes(x =Churn , fill = StreamingTV)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to TV Streaming ") +
  facet_wrap ( ~ StreamingTV) +
  theme_minimal()

# visualization of Movie streaming

ggplot(dataset,aes(StreamingMovies,fill = Churn)) +
  geom_bar()
ggplot(dataset, aes(x =Churn , fill = StreamingMovies)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to Streaming Movies ") +
  facet_wrap ( ~ StreamingMovies) +
  theme_minimal()

#CONTRACT TYPE

ggplot(dataset, aes(x =Churn , fill = Contract)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to Contract type ") +
  facet_wrap ( ~ Contract) +
  theme_minimal()

#PAYMENT METHODS
ggplot(dataset,aes(PaymentMethod, fill = Churn)) +
  geom_bar()
ggplot(dataset, aes(x =Churn , fill = PaymentMethod)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to Payment Method ") +
  facet_wrap ( ~ PaymentMethod) +
  theme_minimal()

#PAPERLESS BILLLING
ggplot(dataset, aes(x =PaperlessBilling , fill =Churn)) +
  geom_bar(stat = "count", position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = after_stat(count)),
    stat = "count",
    position = position_dodge(width = 0.8),
    vjust = -0.2,   # Adjust vertical positioning of labels
    color = "black") +
  labs(title = "Churn with respect to  paperless billing ") +
 theme_minimal()


#MONTHLY CHARGES
ggplot(dataset, aes(x = Churn, y = MonthlyCharges , fill = Churn)) +
  geom_violin() +
  labs(title = "Violin Plot of Monthly Charges by Churn",
       x = "Churn",
       y = "Monthly Charges")+
  theme_minimal()

#TOTAL CHARGES
ggplot(dataset, aes(x = Churn, y = TotalCharges , fill = Churn)) +
  geom_violin() +
  labs(title = "Violin Plot of Total Charges by Churn",
       x = "Churn",
       y = "TotalCharges ") +
  theme_minimal()

# Independent two-sample t-test between monthly charges and churn
t_test_result <- t.test(MonthlyCharges ~ Churn, data = dataset)
print(t_test_result)

# # Independent two-sample t-test between total charges and churn
Total_charges <- t.test(TotalCharges ~ Churn, data = dataset)
print(Total_charges)
