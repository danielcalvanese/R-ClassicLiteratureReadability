# Load data.r.
source('data.R')

# Order books by downloads descending.
books_by_download <- books %>% arrange(desc(downloads))

# Select for author, title, words, syllables, and sentences of books by download.
books_refined <- books_by_download %>% select(author, title, words, syllables, sentences)

# Take rows of the authors of the top ten books.
top_ten_authors <- books_refined %>% head(10) %>% pull(author)

# Select all rows of authors of the top ten books.
authors_books <- books_refined %>% filter(author %in% top_ten_authors) %>% arrange(author)

# Add a column called the flesch reading ease to the authors of the top ten books.
reading_ease <- authors_books %>% mutate(flesch_reading_ease = 206.835 - 1.015 * (words / sentences) - 84.6 * (syllables / words))

# Add a column called the flesch kincaid grade level to the authors of the top ten books & group by author.
reading_grade <- reading_ease %>% mutate(flesch_kincaid_grade_level = 0.39 * (words / sentences) + 11.8 * (syllables / words) - 15.59)
reading_grouped <- reading_grade %>% group_by(author)

# Aggregate averages into the reading summary.
reading_summary <- reading_grouped %>% summarise(
  flesch_reading_ease = mean(flesch_reading_ease),
  flesch_kincaid_grade_level = mean(flesch_kincaid_grade_level)
)

# Reshape the reading summary.  Not sure where type and score are coming from.
reading_long <- reading_summary %>% gather(type, score, flesch_reading_ease:flesch_kincaid_grade_level)

# Create a plot object on the long reading summary with a proportional column layer on the plot.  
# Map author to score.
p <- ggplot(reading_long, aes(author, score)) + geom_bar(stat = 'identity') 

# Separate out the flesch scores using a facet grid.
p <- p + facet_grid(rows = vars(type))

# Customize the plot by adding a theme that rotates the x-axis 45 degrees.
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the plot.
plot(p)
