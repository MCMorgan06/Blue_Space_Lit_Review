## Bring in df

library(readxl)
library(dplyr)

##
Green_space_search_4comparison_20240327_DA <- read_excel("Green_space_search_4comparison_20240327_DA.xls")
Blue_space_search_4comparison_20240327_DA <- read_excel("Blue_space_search_4comparison_20240327_DA.xls")
master_search_output <- read_excel("master_search_output.xlsx")

##
green <- Green_space_search_4comparison_20240327_DA
blue <- Blue_space_search_4comparison_20240327_DA
original <- master_search_output

## head
head(green)

green_list <- unique(green$DOI)
blue_list <- unique(blue$DOI)
original_list <-unique(original$DOI)
## check for duplicates

# Check for duplicates in the DOI column
duplicates_b <- blue[duplicated(blue$DOI), ]
duplicates_g <- green[duplicated(green$DOI), ]
duplicates_o <- original[duplicated(original$DOI), ]

# If you want to see the duplicated rows
print(duplicates_b)
print(duplicates_g)
print(duplicates_o)

# Count the number of rows in the DOI column
num_rows <- length(blue$DOI)
# Count the number of rows in the DOI column
num_rows <- length(green$DOI)
print(num_rows)

# creating a search output df with no duplicates 
master_search_final <- master_search_output[!duplicated(master_search_output$DOI), ]
write.csv(master_search_final, file = "master_search_final.csv")

# check new blue against original df

num_common1 <- sum(original_list %in% blue_list)
num_common2 <- sum(original_list %in% green_list)
print(num_common1)
print(num_common2)

# Find the elements in list1 that are not in list2
missing_values <- setdiff(blue_list, original_list)
print(missing_values)

# missing papers in new blue list
missing_values <- as.data.frame(missing_values)
missing_values <- missing_values %>% 
  rename(DOI = missing_values)

# missing papers in new blue and original (missing papers which were deleted from original search)
filtered_blue <- blue[blue$DOI %in% missing_values$DOI, ]

# 2 papers not in original search, (although 4 were returned, the 2 from landscape plan came up as different)


## now that the data has been checked it is time to plot

library(dplyr)

# Example dataframes blue and green
# Replace this with your actual dataframes
blue_comparison <- subset(blue, select = c("Publication Year", "DOI"))
blue_comparison <- blue_comparison %>%
  mutate(type = "blue")

green_comparison <- subset(green, select = c("Publication Year", "DOI"))
green_comparison <- green_comparison %>%
  mutate(type = "green")

# Pool the data together
pooled_data <- bind_rows(blue_comparison, green_comparison)
unique_values <- unique(pooled_data$DOI)

library(dplyr)

# Example dataframe pooled_data with Year, DOI, and type columns
# Replace this with your actual dataframe

# Group by DOI and count the number of unique types for each DOI
shared_DOI <- pooled_data %>%
  group_by(DOI) %>%
  summarise(num_types = n_distinct(type)) %>%
  filter(num_types > 1) %>%
  pull(DOI)

# Display the shared DOI values
print(shared_DOI)
shared_DOI <- as.data.frame(shared_DOI)
shared_DOI <- shared_DOI %>%
  rename(DOI = shared_DOI)
shared_DOI <- shared_DOI %>% mutate(type = "shared")

#merge

# Merge the dataframes by matching values in the "DOI" column
merged_data <- merge(pooled_data, shared_DOI, by = c("DOI"), all.x = TRUE)

# Remove duplicate values in the "DOI" column
merged_data <- distinct(merged_data, DOI, .keep_all = TRUE)


library(dplyr)

merged_data <- merged_data %>% mutate(type.y = coalesce(type.y, type.x))

unique(merged_data$type.y)
summary(merged_data$type.y)
table(merged_data$type.y)



# Assuming merged_data is your dataframe

# simplified dataframe

simple <- merged_data %>% select("Publication Year", 'type.y') 

# Count the number of values in "type.y" for each unique value in "year"
summary <- simple %>%
  count(`Publication Year`, type.y)

# final

final <- summary %>% 
  group_by(`Publication Year`) %>% 
  summarise(total = sum(n))

# join summary and final by year

plot <- summary %>% 
  left_join(final, by = "Publication Year") %>% 
  mutate(percentage = (n/total))

# change year to factor
plot$`Publication Year` <- as.factor(plot$`Publication Year`)


# Assuming plot is your dataframe

p <- ggplot(plot, aes(x = `Publication Year`, y = percentage, fill = type.y)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Publication Year", y = "Percentage") +
  ggtitle("Stacked Barplot of Publication Year vs. Percentage")

print(p)

### getting total rows in seperate df
total_rows <- plot %>%
  group_by(`Publication Year`) %>%
  summarize(total_n = sum(n))

####

# Reorder levels of type.y variable
plot$type.y <- factor(plot$type.y, levels = c("green", "shared", "blue"))

# Calculate total number of rows per publication year
library(dplyr)

# Define custom colors
custom_colors <- c("green" = "#75D05480", "shared" = "#3c998d", "blue" = "#3B528BFF")

# define scales for secondary axis
max_count <- max(plot$n)


# Create ggplot object
p <- ggplot(plot, aes(x = `Publication Year`, y = percentage, fill = type.y)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(x = "Publication Year", y = "Percentage") +
  ggtitle("Stacked Barplot of Publication Year vs. Percentage")
p


# Calculate the total count from total_rows
total_count <- sum(total_rows$total_n)

# Create the plot
# Calculate the total count from total_rows
total_count <- sum(total_rows$total_n)

# add a common grouping to total rows df to draw the line
total_rows <- total_rows %>%
  mutate(type.y = "pooled")


# Define a scaling factor to convert count to percentage
scaling_factor <- 5 / total_count

# Create the plot
p <- ggplot(plot, aes(x = `Publication Year`, y = percentage)) +
  geom_bar(position = "fill", aes(fill = type.y), stat = "identity") +
  scale_fill_manual(values = custom_colors) +  # Apply custom colors
  labs(x = "Publication Year", y = "Percentage") +
  ggtitle("") +
  theme_minimal()+
  geom_line(data = total_rows, aes(x = `Publication Year`, y = total_n * scaling_factor, group = type.y), color = "black", size = 0.5) +
  geom_point(data = total_rows, aes(x = `Publication Year`, y = total_n * scaling_factor), color = "black", size = 3, shape = 17) +
  scale_y_continuous(
    name = "Percentage",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count", labels = scales::comma)
  )
p

# Final plot 
# Create the plot
p <- ggplot(plot, aes(x = `Publication Year`, y = percentage)) +
  geom_bar(position = "fill", aes(fill = type.y), stat = "identity") +
  scale_fill_manual(
    values = custom_colors,  # Apply custom colors
    labels = c("green" = "Articles Unique to green space search", "shared" = "Articles found in both searches", "blue" = "Articles unique to blue space search") #+
      #theme(legend.position="bottom")# Custom legend labels
  ) +
  labs(x = "Publication Year", y = "Percentage", fill = "") +  # Custom legend title
  ggtitle("") +
  theme_minimal() +
  geom_line(data = total_rows, aes(x = `Publication Year`, y = total_n * scaling_factor, group = type.y), color = "black") +
  geom_point(data = total_rows, aes(x = `Publication Year`, y = total_n * scaling_factor), color = "blue") +
  scale_y_continuous(
    name = "Percentage",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count", labels = scales::comma)
  ) +
  theme(
    axis.title.x = element_text(size = 14, margin = margin(t = 10)),  # Adjust text size and distance for x-axis label
    axis.title.y = element_text(size = 14, margin = margin(r = 10)),  # Adjust text size and distance for y-axis label
    axis.text.x = element_text(size = 12, angle =35),  # Adjust text size for x-axis ticks
    axis.text.y = element_text(size = 12),   # Adjust text size for y-axis ticks
    legend.text = element_text(size = 12)
  )
p
# Save the plot as A4 with custom DPI
ggsave("publication_plot_a4_labels.png", plot = p, dpi = 600)  # Save as A4 landscape with 300 DPI

## for final plot
theme_match <- theme(
  axis.title.x = element_text(size = 14, margin = margin(t = 10)),
  axis.title.y = element_text(size = 14, margin = margin(r = 10)),
  axis.text.x  = element_text(size = 12, angle = 35, margin = margin(t = 5)),
  axis.text.y  = element_text(size = 12, margin = margin(r = 5)),
  legend.text  = element_text(size = 12)
)

p <- ggplot(plot, aes(x = `Publication Year`, y = percentage)) +
  geom_bar(position = "fill", aes(fill = type.y), stat = "identity") +
  scale_fill_manual(values = custom_colors,
                    labels = c("green"="Articles Unique to green space search",
                               "shared"="Articles found in both searches",
                               "blue"="Articles unique to blue space search")) +
  labs(x = "Publication Year", y = "Percentage", fill = "") +
  ggtitle("") +
  theme_minimal() +
  geom_line(data = total_rows,
            aes(x = `Publication Year`, y = total_n * scaling_factor, group = 1),
            color = "black") +
  geom_point(data = total_rows,
             aes(x = `Publication Year`, y = total_n * scaling_factor),
             color = "blue") +
  scale_y_continuous(
    name = "Percentage",
    labels = scales::percent_format(),
    sec.axis = sec_axis(~ . / scaling_factor, name = "Count", labels = scales::comma)
  ) +
  theme_match

ggsave("publication_plot_a4_final.png", plot = p, dpi = 600)  # Save as A4 landscape with 300 DPI


