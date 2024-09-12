
#PART 1
lines <- readLines(con = "brown_sentences.txt")
letter_frequencies = read.csv("letter_frequencies.csv")

calculate_chi_squared = function(sentence, freq_table) {
  
  # Ensure letter frequencies are normalized and sum to 1
  freq_table$Probability = freq_table$Probability / sum(freq_table$Probability)
  
  # Remove non-letters and convert to uppercase
  clean_sentence = gsub("[^A-Za-z]", "", sentence)
  clean_sentence = toupper(clean_sentence)
  
  # Count the occurrences of each letter in the sentence
  observed_counts = table(factor(strsplit(clean_sentence, "")[[1]], levels = freq_table$Letter))
  
  # Calculate expected counts
  total_letters = sum(observed_counts)
  expected_counts = total_letters * freq_table$Probability
  
  # Chi-squared statistic
  chi_squared_stat = sum((observed_counts - expected_counts)^2 / expected_counts)
  
  return(chi_squared_stat)
}

chi_squared_stats <- numeric(length(lines))

for (i in 1:length(lines)){
  chi_squared_stats[i] <- calculate_chi_squared(lines[i], letter_frequencies)
}
# print(chi_squared_stats) - check to print

#PART 2
#simulation = do(100000)*nflip(n=2021, prob=0.024)
#head(simulation)
#ggplot(simulation) + geom_histogram(aes(x=nflip), binwidth=1)

sentences <- c(
  "She opened the book and started to read the first chapter, eagerly anticipating what might come next.",
  "Despite the heavy rain, they decided to go for a long walk in the park, crossing the main avenue by the fountain in the center.",
  "The museum’s new exhibit features ancient artifacts from various civilizations around the world.",
  "He carefully examined the document, looking for any clues that might help solve the mystery.",
  "The students gathered in the auditorium to listen to the guest speaker’s inspiring lecture.",
  "Feeling vexed after an arduous and zany day at work, she hoped for a peaceful and quiet evening at home, cozying up after a quick dinner with some TV, or maybe a book on her upcoming visit to Auckland.",
  "The chef demonstrated how to prepare a delicious meal using only locally sourced ingredients, focusing mainly on some excellent dinner recipes from Spain.",
  "They watched the sunset from the hilltop, marveling at the beautiful array of colors in the sky.",
  "The committee reviewed the proposal and provided many points of useful feedback to improve the project’s effectiveness.",
  "Despite the challenges faced during the project, the team worked tirelessly to ensure its successful completion, resulting in a product that exceeded everyone’s expectations."
)

p_values <- c(
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[1],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[2],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[3],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[4],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[5],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[6],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[7],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[8],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[9],letter_frequencies)))/length(chi_squared_stats),
  
  sum(1*(chi_squared_stats >= calculate_chi_squared(sentences[10],letter_frequencies)))/length(chi_squared_stats) 
  
)

rounded_p_values <- round(p_values, 3)

sentences_index <- 1:10
p_values_table <- data.frame(P_Value = rounded_p_values)

print(p_values_table)




