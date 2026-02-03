# world-values-ai-usage

Anthropic usage dataset files (note that they are "long"):
- data/anthropic-economic-index/release_2025_09_15/data/output/aei_enriched_claude_ai_2025-08-04_to_2025-08-11.csv
- data/anthropic-economic-index/release_2026_01_15/data/output/aei_raw_claude_ai_2025-11-13_to_2025-11-20.csv

(see data_documentation.md files in those subfolders as needed)

World Values Survey data:
- data/wvs/Trends_VS_1981_2022_rds_v4_0.rds

World Values Survey column info:
- Trends_VS_1981_2022_labels_questions.csv
Trends_VS_1981_2022_labels_values.csv

Analysis notes (from wvs_trends_brainstorm.md #17):
- Combine WVS values with country-level AI usage to test whether pro-tech or secular-rational values predict adoption/exposure.
- Candidate WVS variables: E217/E218/E234 (science/technology optimism), E220 (faith vs science), A189-A198 (Schwartz), E032/E035/E037 (economic values).

Initial analysis script (R):
- drafts/world-values-ai-usage/analysis_wvs_ai_usage.R

Prep script (run immediately before analysis):
- drafts/world-values-ai-usage/prep_wvs_ai_usage.R

Analysis outputs include:
- Coefficient pointrange plot
- Partial-correlation plot (GDP-controlled) across many WVS items
