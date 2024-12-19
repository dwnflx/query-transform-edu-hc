# Supporting Analysis Files for Master's Thesis

These are the supporting analysis files for my master's thesis:  
**"Optimizing Information Retrieval in Retrieval-Augmented Generation Systems with Query Transformations for Healthcare and Education."**

---

## Setup

- **Python Version**: 3.12.5

### Quick Environment Installation
To set up the environment, run the following commands:

```bash
python -m venv env
source env/bin/activate  # On Windows: .\env\Scripts\activate
pip install -r requirements.txt
```

### Environment Variables
Create a .env file in the root directory with the following keys:
- OPENAI_KEY=<Your OpenAI API Key>
- CLAUDE_KEY=<Your Anthropic API Key>
- GOOGLE_API_KEY=<Your Gemini API Key>

## Project Structure
data/
├── raw/
└── cleaned/
    Contains raw and cleaned data. Note that due to file size constraints, the raw and cleaned dataset of PubMedQA has been removed. However, these are not necessary for analysis, as the final testing datasets are located under notebooks/data.

models/
    Contains the embeddings for each sector's Retrieval-Augmented Generation (RAG) system.

notebooks/
├── data/
│   └── (contains testing datasets)
├── Preprocessing
├── Exploratory Analysis
├── RAG Setup
├── Answer Generation
└── Analysis
    Contains Jupyter Notebooks covering:
        Preprocessing
        Exploratory Analysis
        RAG Setup
        Answer Generation
        Analysis

.R-files/
    Contains the statistical analysis scripts for each sector