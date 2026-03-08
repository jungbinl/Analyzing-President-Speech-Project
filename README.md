# President Speech Analyzing project

## introduction
Analyzing-President-Speech-Project is a Natural Language Processing (NLP) initiative designed to uncover patterns within the speeches of U.S. presidents. Also This project leverages Machine Learning techniques to explore and categorize the core themes found in U.S. presidential speeches.

## Objective

* Basic Analysis : analyzing basic information such as word frequency, pronoun usage, pos ratio and etc

* Advanced Aanlysis : anlayzing advanced method such as parse tree depth, sentiment anlaysis and etc.

* Comparative Study: Contrasting the linguistic styles of different presidents or political parties.

* Topic Discovery via Machine Learning: Using unsupervised learning to automatically group speeches into distinct themes.

## Tech Stack
* R: Serves as the primary tool for data analysis, including text mining, statistical modeling, and Natural Language Processing (NLP).
* SQL: Utilized for database management, ensuring structured storage and efficient retrieval of large-scale speech datasets.
* Python: Used for automation and pipeline management, specifically for Microsoft auto-scheduling and seamless database updates.
* Power BI: Employed for advanced data visualization, transforming complex analysis results into interactive and insightful dashboards.

## Project structure
```text
├── Analysis/       # Core scripts for Text Mining & Machine Learning (R)
├── data/           # Datasets (Raw presidential speeches & tokenized text)
├── database/       # SQL scripts for data management & storage
├── result/         # Analysis outputs (Visualizations & Power BI reports)
└── README.md       # Project documentation and overview
```

## Analysis Method
### Basic Analysis
Focuses on the foundational structural and lexical characteristics of presidential speeches.

* Word Frequency: Identifies the most commonly used words to capture the primary focus of each speech.

* TTR (Type-Token Ratio): Measures lexical diversity to evaluate the range of vocabulary used by different presidents.

* Avg. Sentence Length: Analyzes the complexity and readability of the speeches based on average sentence structure.

* POS Ratio (Part-of-Speech): Examines the distribution of nouns, verbs, and adjectives to understand the rhetorical style.

* Pronoun Analysis: Tracks the usage of pronouns (e.g., "I" vs. "We") to determine the level of inclusivity or personal authority.

### Advanced Analysis
Employs Natural Language Processing (NLP) and statistical models to uncover deeper patterns.

* TF-IDF: Determines the most unique and significant words for each president by penalizing commonly occurring terms across all speeches.

* Sentiment Analysis: Quantifies the overall positive or negative tone of the speeches to track emotional shifts over historical eras.

* Emotion Analysis: Goes beyond binary sentiment to categorize specific emotions such as Trust, Fear, Joy, or Anger.

* Odd Ratio: Uses statistical odds to compare the likelihood of specific keywords appearing democratic, repulican party

* Co-occurrence: Visualizes the relationships and networks between words that frequently appear together inlcuding correlation, n-gram

* Parse Tree Depth: Measures the syntactic complexity of sentences to evaluate the sophistication of the political discourse.

## Machine Learning Method

Utilizes unsupervised learning algorithms to automatically discover hidden themes and topics across thousands of documents.

* LDA (Latent Dirichlet Allocation): A generative statistical model that allows sets of observations to be explained by unobserved groups (topics). It identifies which topics dominate each president's tenure.

* STM (Structural Topic Modeling): An advanced topic model that allows for the inclusion of "metadata" (such as the president's party or the year of the speech) to see how topic usage varies across different political contexts.


## Databases
To support multifaceted NLP analysis, I designed a robust relational data model use primary key and foreign key in the SQL

Data Engineering & Schema Architecture
To ensure data integrity and optimized query performance for NLP analysis, I implemented a comprehensive ETL and Relational Modeling process using SQL.

### Data Integration & Preprocessing (ETL)
I unified disparate speech datasets (Inaugural, Union, Weekly, Spoken) into centralized party-specific tables (demo_data, repu_data) in order to analyze easy when I use this raw data in R

Dynamic Tagging: Added metadata columns like id and type (Inaugural, Weekly, etc.) to track source origins.

Automated Updates: Performed bulk updates using LEFT JOIN to synchronize token data with speech metadata (Name, Year, Party).

### Relational Schema Design (Star Schema)
I established a complex network of relationships centered around the Presidential Master Data to enable seamless filtering across various NLP metrics.

Primary Key/Foreign Key Constraints: Unified all analysis results (Sentiment, TTR, TF-IDF, Topic Modeling) by referencing the president_party_data table.

Hierarchical Graph Data Modeling:

Managed network analysis data (Nodes/Edges) for Bigrams, Correlations, and Pairwise words.

Implemented auto-incrementing unique IDs to maintain referential integrity between graph nodes and edges.

Machine Learning Integration: Successfully mapped LDA and STM (Structural Topic Modeling) outputs to individual presidents and parties for granular trend analysis.

<img width="201" height="405" alt="image" src="https://github.com/user-attachments/assets/524ca442-41d3-41bc-bf94-7a7782bf1bf8" />


## Visualization (Power BI)

We integrated the analysis results into a Power BI Dashboard to provide an interactive experience.

<table>
<tr>
<td><img src="https://github.com/user-attachments/assets/0339900e-5767-4e18-a73d-12142a4bc156" width="100%"></td>
<td><img src="https://github.com/user-attachments/assets/5766efd4-a5d1-40a9-bb22-ac0891bed74a" width="100%"></td>
</tr>
<tr>
<td><img src="https://github.com/user-attachments/assets/64173fd7-d22b-4b5e-9bbf-dd84d27dfaee" width="100%"></td>
<td><img src="https://github.com/user-attachments/assets/f505dcd4-799a-405a-9c20-c4d8ae6995a6" width="100%"></td>
</tr>
</table>

## Result
This project demonstrates how Machine Learning and NLP can transform historical text into quantifiable data. By analyzing centuries of presidential speeches, we can better understand the shifting priorities of American leadership.

### collaborator
Seounghyun kim
