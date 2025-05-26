# System-Level Prompt (use before any prompts)

I'm going to now ask you questions about the public opinion research you've analyzed in deep research or that I've uploaded to you.
In all responses, follow these guidelines:

- Only summarize what is directly supported by the source data (survey results, documented findings, quotes).

- Do not fabricate survey questions, sample sizes, or subgroup findings that aren’t explicitly stated.

- Provide caveats in your statements whenever necessary. For example if a finding is not universally supported by all studies, if it is only weakly supported in general (e.g. a slim margin, from relatively small sample sizes).

- Distinguish clearly between:
	- What respondents said
	- What authors concluded
	- What you are inferring based on patterns

- When interpreting sentiment (e.g., “positive,” “skeptical,” “concerned”), indicate:
	- Whether this is **derived from quantitative data** (e.g., % concerned)
	- Or a **summary of framing** used by the report authors

- If multiple sources **disagree or show different trends**, call that out.

- If sample size or method is **unclear or not reported**, say so.

- Always include **geographic scope** (e.g., U.S., UK, Turkey, Global) and timing of data collection when summarizing findings.

Be concise, transparent, and rigorous — treat this as if writing for a public-facing report or policy briefing.

# Exploratory Prompts
## Overview & Summarization

- List all the survey questions asked across the sources, if available.

- Summarize the top 5 findings that all sources agree on.

- Identify the most counter-intuitive or surprising overall findings in the research you've done, based on what an average person might think or assume.

- Extract the specific wording of attitude questions used in these studies.

- What types of AI technologies were respondents asked about?

- Which sources ask about AI in specific domains (e.g., jobs, health, media)?

- Summarise where and how the sources disagree. Include details (e.g. reported survey statistics) on why and how they disagree. Note whether these are substantively big or small changes and/or whether they might be statistically noisy due to small sample sizes. 

- Which domains of AI (e.g., health, jobs, media) evoke the most concern, if any?

- List key descriptive words or phrases used to summarize public attitudes (e.g., ‘curious,’ ‘concerned,’ ‘distrustful’). Say whether they came from survey questions, respondent open-ends, or analyst summaries.

Verification:
- If a source didn’t include survey questions or specific items, just say so instead of inferring or fabricating them.

## Subgroup & Demographic Analysis

- List all the subgroups (demographics or psychographics) examined across the sources.

- What patterns emerge by {subgroup} across these sources?

- Are there any subgroup variables (e.g. age, political identification, education) for which there is sharp divergence or polarization of attitudes between different subgroups (e.g. more than a 20% difference in responses to a major question about attitudes)?

- List subgroups in order of most positive to negative sentiment (confidence, trust, etc.) about AI.

- Identify the most counter-intuitive or surprising subgroup patterns in the research you've done, based on what an average person might think or assume.

Verification:
- Please only list subgroups that were explicitly reported, not those that might have been used in weighting or assumed.

## Temporal

- Accounting for the time period that these surveys took place, summarize the top 5 ways that opinions about AI have changed. Include details (e.g. reported survey statistics) and note whether these are substantively big or small changes and/or whether they might be statistically noisy due to small sample sizes. 

## Extrapolation & Hypothesis Generation

- Based on available research, synthesize some qualitative keywords that describe the state of public opinion about AI right now.

- Are any of these findings an artifact of survey methodology? If so, hypothesize which ones, why, how they might be corrected and, if corrected, how findings might differ?

- How would you extrapolate from current data to predict how public opinion toward AI might evolve over the next five years? Please give multiple competing hypotheses, be specific about particular attitudes (trust vs. comfort) and note what would support each.

- How might different AI domains (healthcare, elections, hiring, education) diverge or converge in terms of public support and trust over time? What early signals support this divergence?

- How might different subgroups (high vs. low education, knowledge vs. blue collar workers) diverge or converge in terms of public support and trust over time? What early signals support this divergence?

- How might different countries (industrialized vs. less developed) diverge or converge in terms of public support and trust over time? What early signals support this divergence?

- Can you hypothesize how increased public understanding of AI (e.g., greater knowledge of how models are trained) might shift attitudes, based on the data?

- Let's take a step back: are any of these findings at all unique to AI (versus, say, technologies of the past like the Internet)?

- Based on the survey evidence compiled and any other knowledge of social science and public opinion formation you have, what are some plausible causal mechanisms that help explain how people form attitudes toward artificial intelligence? Please propose a few hypotheses and link them to specific findings, subgroups, or theoretical concepts (e.g., trust, knowledge, exposure, ideology). Indicate where the evidence is strong, suggestive, or speculative.

- Here's a hypothesis I have about AI attitudes - I want you to give it a fair assessment both based on what the research you've done supports and based on other social science and public opinion research you're aware of. Do not just agree with me, uncritically. `{hypothesis}`

