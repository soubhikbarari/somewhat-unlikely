# My LLM Research Assistant: Methodology

For this blog post, I wanted to see whether and how tools from ChatGPT (specifically the latest 4o model and Deep Research) could (a) provide me with high quality syntheses of primary research, and (b) help with 'exploratory' queries about the resulting knowledge. Proprietary vendor-based tools like ChatGPT are [inherently irreplicable](https://github.com/ArthurSpirling/LargeLanguageReplication/blob/main/explainer/explainer.md) -- still, I think there is value in researchers being *transparent* about their usage.

A quick overview:

- `prompts/01_primary_research_prompts.md`: contains a cleaned up version of my initial research prompt(s) which, in short, asks 4o to ingest and summarise several manually curated source PDFs (mostly reports or paywalled academic papers) and then for Deep Research to continue the process with new sources that it discovers. My hunch is that the first 4o step helps to 'anchor' the second, but a more systematic evaluation would be needed to say for sure. The output deep research report and the summary tables are in `outputs/01_primary_research*`.

- `prompts/02_exploratory_prompts.md`: has several lists of the prompts I asked next to 'explore' the synthesis that GPT generated. This really ran the gamut - both topline and subgroup questions, attitude-specific and general, 'summarization' and 'extrapolation'. I copied and pasted the outputs from these questions in `output/02.{X}_exploratory_outputs*`. You can see that my system-level prompt seems to  generate some helpful caveats and hedges in claims being made. Still, it may be the case that the model is hallucinating or overclaiming, which motivates the next step--

- `outputs/03_verified_claims.md`: here, I took a sample of claims (either source-specific or aggregate across sources) generated and manually verified them by cross-checking with original sources. This is both a check of whether the model is *interpreting* information correctly and, more banally, whether the model is simply *extracting* the information (numbers, sources, question text) properly.

I performed all of these prompts on ChatGPT's GPT-4o (default parameters) and Deep Research (default parameters) between Friday May 23rd and Monday May 26th.
