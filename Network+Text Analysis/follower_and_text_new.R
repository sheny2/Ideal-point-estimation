library(tidyverse)
library(irlba)
library(stringi)

contains_non_english <- function(s) {
  any(stri_detect_regex(s, "[^a-zA-Z]"))
}


##### prepare WtW
load("sparse_W.RData")

dim(sparse_W)

##### Remove rare words
all_row_sum_W = rowSums(sparse_W > 0)
great_5_W = which(all_row_sum_W > 5)
W = sparse_W[great_5_W, ]
dim(W)


##### Remove just numbers
W <- W[!grepl("^[0-9]+$", rownames(W)), ]
W_clean = W[493:nrow(W),]
dim(W_clean)

W_clean = W_clean[!grepl("^http", rownames(W_clean)), ]
dim(W_clean)
W_clean = W_clean[!grepl("^[#@]", rownames(W_clean)), ]

W_clean = W_clean[!grepl("^[0-9]", rownames(W_clean)), ]
dim(W_clean)

non_english_rows <- rownames(W_clean)[sapply(rownames(W_clean), contains_non_english)]
W_clean <- W_clean[!(rownames(W_clean) %in% non_english_rows), , drop = FALSE]
dim(W_clean)


# rownames(sparse_W) %>% view()
# rownames(W) %>% view()



W_small = W_clean[(rownames(W_clean) %in% lexicon), , drop = FALSE]
W_small = W_clean

six_ppl = c(
"29442313",  # Bernie
"939091",   # Biden
"1339835893", # Clinton

"15976697", 
"41634520",
"22703645"
)

W_sub = W_small[, six_ppl]>0

W_sub2 =  W_small[, c("130617778")]>0 # Keith Olbermann
mean(W_sub2)

mean(W_sub2 == W_sub[,3])
mean(W_sub2 == W_sub[,1])
mean(W_sub2 == W_sub[,2])

mean(W_sub[,2] == W_sub[,6])


mean((W_small[,1]>0 ) == (W_small[,3]>0) )
mean((W_small[,124]>0 ) == (W_small[,355]>0) )


sum(W_sub2)
colSums(W_sub)
sum(W_sub2 == W_sub[6,])


W_bernie = W_small[, c("29442313")]
W_bernie_words = names(W_bernie[W_bernie != 0])

W_biden = W_small[, c("939091")]
W_biden_words = names(W_biden[W_biden != 0])

W_Olbermann = W_small[, c("130617778")]
W_Olbermann_words = names(W_Olbermann[W_Olbermann != 0])

W_tucker = W_small[, c("22703645")]
W_tucker_words = names(W_tucker[W_tucker != 0])


mean(W_Olbermann_words %in% W_biden_words)
mean(W_Olbermann_words %in% W_bernie_words)
mean(W_Olbermann_words %in% W_tucker_words)

mean(W_bernie_words %in% W_biden_words)
mean(W_biden_words %in% W_bernie_words)


mean(W_bernie_words %in% W_biden_words)
mean(W_biden_words %in% W_bernie_words)


mean(W_tucker_words %in% W_biden_words)
mean(W_tucker_words %in% W_bernie_words)

mean(W_biden_words %in% W_tucker_words)
mean(W_bernie_words %in% W_tucker_words)

##### Choose one way of WTW

# WtW_count = t(W) %*% (W)  # word count
# 
# # Apply Laplacian 
# rsWtW_count = rowSums(WtW_count)
# lap_WtW_c = diag(rsWtW_count^{-1/2})%*%WtW_count%*%diag(rsWtW_count^{-1/2})



# set.seed(123)
# sampled_indices <- sample(1:nrow(W_clean),  nrow(W_clean) / 10)
# W_clean2 <- W_clean[sampled_indices, , drop = FALSE]



WtW = t(W_small>0) %*% (W_small>0) # word occurrence
WtW = t(W_clean>0) %*% (W_clean>0) # word occurrence

# Apply Laplacian 
rsWtW = rowSums(WtW)
lap_WtW = diag(rsWtW^{-1/2})%*%WtW%*%diag(rsWtW^{-1/2})

# Spectral clustering and the high-dimensional stochastic blockmodel

dim(lap_WtW)







### prepare yty

load("sparsematrix_1m_933.RData")

dim(y)

yty = t(y) %*% y

## Apply Laplacian 
rsyty = rowSums(yty)
lap_yty = diag(rsyty^{-1/2})%*%yty%*%diag(rsyty^{-1/2})



# Combine them 
comb_lap_new = lap_yty + lap_WtW


comb_new = yty + WtW





  Word1 <- c("action", "agree", "balance", "balanced", "banks", "bipartisan", "budget", "century", "challenge", "challenges",
             "chamber", "change", "child", "children", "childrens", "china", "college", "common", "communities", "community",
             "congress", "credit", "crime", "crisis", "decisions", "deficit", "differences", "economic", "economy", "education",
             "effort", "efforts", "europe", "families", "fellow", "financial", "forward", "future", "generation", "global", "growth",
             "health", "helped", "homes", "house", "increase", "internet", "invest", "investment", "issue", "issues", "leadership",
             "loans", "longterm", "markets", "means", "million", "moment", "national", "opportunity", "pakistan", "parents", "perfect",
             "politics", "private", "propose", "prosperity", "provide", "qaeda", "question", "racial", "recession", "recognize",
             "recovery", "reduce", "responsibility", "reverend", "revolution", "school", "schools", "science", "sense", "service",
             "simply", "single", "solve", "spending", "standards", "start", "steps", "streets", "strengthen", "stronger", "students",
             "support", "teachers", "technology", "tonight", "trade", "troops", "union", "values", "welfare", "white", "black",
             "forge", "lines", "ultimately", "guantanamo", "package", "detainees", "accountable", "actions", "address", "administration",
             "afford", "affordable", "answer", "approach", "begin", "bridge", "building", "chance", "checks", "citizen", "classroom",
             "clean", "combat", "consensus", "consumers", "corps", "costs", "critical", "decade", "decline", "depression", "difficult",
             "direction", "dollar", "doubt", "enormous", "entire", "environment", "expand", "extraordinary", "family", "finish",
             "fundamental", "generations", "housing", "hundred", "ideas", "incentives", "individual", "initiative", "international",
             "investments", "journey", "kinds", "largest", "lasting", "learning", "legacy", "lives", "market", "measure", "millennium",
             "months", "moved", "neighborhoods", "opportunities", "partners", "partnership", "poverty", "preserve", "prevent",
             "programs", "proposal", "public", "quality", "raise", "reach", "restore", "review", "rural", "sector", "serving",
             "shape", "shortterm", "strategy", "street", "strongest", "succeed", "surplus", "teacher", "transparency", "unprecedented",
             "worlds", "finance", "fiscal", "harder", "immigrants", "lowest", "planet", "waste", "bosnia", "discrimination", "doors",
             "exist", "globalization", "recommend", "stimulus", "summit", "treaty", "wright", "commissions", "compensation", "interrogation",
             "lending", "anger", "assure", "authority", "blame", "built", "campaign", "cancer", "capacity", "choose", "churches", "civil",
             "complete", "confidence", "courts", "creating", "determination", "discipline", "earlier", "economies", "effective", "enable",
             "enduring", "ensure", "european", "expanded", "exports", "federal", "foundation", "government", "ground", "hands", "havens",
             "includes", "incomes", "investing", "knowledge", "legitimate", "lifetime", "maintain", "marines", "mutual", "pleased", "police",
             "policies", "proposed", "proud", "remain", "renew", "rewards", "rolls", "saving", "served", "spirit", "spring", "student",
             "summer", "surely", "thousand", "times", "tomorrow", "tools", "truth", "tuition", "watching", "closer", "michael", "payments",
             "space", "aging", "americorps", "asian", "boldly", "brady", "classrooms", "efficient", "endeavor", "kenya", "organized", "richard",
             "tobacco", "trouble", "whove", "biden", "brown", "document", "economists", "elkhart", "extraordinarily", "french", "michelle",
             "oversight", "spectrum", "sustainable", "ability", "accountability", "africanamerican", "alliances", "arguments", "ashley",
             "assistance", "begins", "bottom", "break", "burden", "camps", "capital", "carry", "carrying", "celebrate", "charter", "church",
             "completely", "comprehensive", "conflicts", "constructive", "creed", "crises", "debate", "decide", "defeat", "designed", "diplomacy",
             "disagree", "districts", "divide", "documents", "doesnt", "dreams", "establish", "failure", "falling", "father", "finest", "firms",
             "fought", "founders", "goals", "grade", "grant", "guarantee", "guarantees", "improved", "india", "language", "legislation", "listening",
             "local", "losing", "match", "methods", "miles", "mistake", "mother", "mothers", "notion", "operate", "partner", "pension", "pollution",
             "possibility", "powerful", "precious", "presidents", "pursue", "raised", "range", "rates", "relationship", "release", "reminds", "report",
             "republicans", "requires", "research", "reward", "rooted", "safer", "savings", "scores", "senate", "short", "sitting", "social", "soldiers",
             "stood", "stopped", "stories", "struggling", "table", "taxpayer", "teach", "technologies", "toxic", "training", "traveled", "treasures", "trust",
             "unemployment", "weakened", "worth", "acknowledge", "aisle", "announce", "computer", "convention", "cycle", "effectively", "flowing", "intend", "joint",
             "minimum", "partnerships", "reality", "regulatory", "shores", "adversaries", "alive", "applaud", "applied", "captain", "commonsense", "connect", "conversation",
             "deepen", "dialogue", "economically", "empowerment", "endanger", "expensive", "extending", "graduates", "greenhouse", "harness", "kosovo", "launching", "library",
             "memories", "mortgage", "native", "privacy", "publicly", "recognition", "repeat", "responded", "restoring", "reverse", "rivers", "salaries", "scholarships",
             "shareholders", "warming", "blacks", "coordinated", "detention", "diplomatic", "executives", "homeowners", "laying", "reinvestment", "sermons", "strasbourg",
             "techniques", "wrights", "accept", "access", "accomplished", "accounts", "active", "additional", "adopted", "afghan", "agencies", "agents", "aggression",
             "aggressive", "ahead", "allowing", "alternative", "amount", "baghdad", "ballistic", "beliefs", "benefits", "bigotry", "broader", "brought", "charging",
             "chief", "civilization", "closely", "condemned", "confronted", "congratulations", "consequences", "conservative", "construction", "dangerous", "dealing",
             "deepest", "defeating", "defended", "defiance", "delivered", "dictators", "director", "dominate", "duties", "eliminate", "employees", "encourages", "entrepreneurs",
             "entry", "environmental", "equally", "event", "eventually", "faith", "field", "fight", "frivolous", "fulfill", "funding", "glorious", "governing", "heart",
             "homeland", "honorable", "independent", "institution", "judge", "kuwait", "lawsuits", "leader", "lieutenant", "looked", "louisiana", "loyalty", "mankind",
             "matter", "mercy", "minute", "miracle", "names", "opponent", "optimism", "organization", "outcome", "outdated", "outlaw", "palestine", "pennsylvania",
             "permanent", "permitting", "personnel", "political", "prevail", "progrowth", "promise", "promote", "protection", "protects", "proudly", "providing", "punish",
             "putting", "quiet", "rebuild", "recently", "region", "rejects", "relations", "religion", "responders", "rulers", "ruling", "salute", "secretary", "senators",
             "share", "shown", "souls", "stability", "standard", "stated", "steven", "stock", "strive", "struggle", "supplies", "telling", "terrorism", "tyranny", "ultimate",
             "undermine", "unfairly", "unlike", "update", "virginia", "voted", "western", "wishes", "wounded", "affected", "aliens", "altleft", "canada", "circuit", "cleanest",
             "fence", "flynn", "green", "havent", "indiana", "jessica", "julie", "keystone", "mentioned", "miners", "offshore", "pipeline", "product", "reserves", "robert",
             "scott", "selfinflicted", "southern", "statue", "terrific", "thrilled", "vetting", "walmart", "watched", "advance", "allies", "amazing", "ambitions", "assembly",
             "attack", "biological", "broke", "capable", "catastrophic", "charge", "cheney", "choices", "close", "compliance", "confident", "conflict", "confront", "contributions",
             "court", "crucial", "decision", "delegates", "destruction", "directed", "elections", "enemies", "equipment", "faithbased", "folks", "framework", "friend", "growing",
             "hardworking", "hearts", "honest", "immediately", "importantly", "inauguration", "information", "intimidate", "iraqs", "joining", "judges", "liability", "lowincome",
             "medicare", "message", "moral", "oppressed", "ownership", "peoples", "phase", "positive", "power", "promised", "quickly", "repeal", "replace", "represent",
             "represented", "respected", "sanctions", "saudi", "signed")
  
  Word2 = c("sovereignty", "surprised", "texas", "travel", "unfair", "witnessed", "wonderful", "wrote", "badly", "bannon", 
           "disadvantage", "friendly", "harold", "jobkilling", "protest", "putin", "reporters", "tennessee", "advantage", 
           "allowed", "alqaida", "announced", "benefit", "bless", "borders", "broken", "build", "business", "businesses", 
           "chaos", "choice", "civilized", "commitments", "compassionate", "compete", "correct", "couple", "danger", "death", 
           "decades", "declared", "deliver", "desire", "determined", "dictator", "disarm", "disaster", "elected", "election", 
           "encourage", "expected", "focus", "forgotten", "found", "friends", "giving", "governor", "happened", "happy", 
           "heard", "highway", "honored", "hopes", "illegal", "income", "innocent", "inspectors", "israel", "killers", 
           "leaving", "manufacturing", "medicine", "mexico", "morning", "muslim", "negotiate", "officials", "oppression", 
           "palestinian", "peaceful", "period", "personal", "plants", "pouring", "prayers", "prescription", "pride", 
           "process", "produce", "production", "promises", "reforms", "regulation", "representative", "resolution", 
           "retirement", "ronald", "security", "serve", "signs", "sources", "starting", "supposed", "takes", "taliban", 
           "theyve", "thousands", "threat", "totally", "treatment", "understand", "unite", "uranium", "victims", "watch", 
           "whats", "worker", "world", "anymore", "audience", "charlottesville", "classified", "deals", "factories", 
           "fantastic", "guess", "happening", "inaudible", "ivanka", "kelly", "leaks", "mattis", "neonazis", "pipelines", 
           "restrictions", "steel", "theyll", "account", "agreement", "americas", "attacks", "billions", "border", "bring", 
           "called", "calling", "character", "cities", "citizens", "coalition", "coming", "commitment", "companies", 
           "compassion", "competitive", "control", "council", "countries", "courage", "coverage", "current", "defend", 
           "democracy", "democratic", "deserve", "didnt", "dignity", "dollars", "excuse", "foreign", "freedom", "governments", 
           "happen", "hatred", "healthcare", "honor", "horrible", "human", "hussein", "immigration", "including", "incredible", 
           "infrastructure", "intelligence", "iraqi", "justice", "laughter", "leaders", "level", "liberty", "lower", 
           "marriage", "massive", "media", "meeting", "middle", "military", "millions", "model", "money", "murder", "nation", 
           "nations", "paris", "peace", "percent", "person", "press", "reagan", "reform", "regime", "regimes", "regulations", 
           "relief", "remember", "resolutions", "respect", "rights", "russia", "saddam", "senator", "seniors", "societies", 
           "society", "story", "taxes", "terrible", "terror", "terrorist", "terrorists", "theyre", "tremendous", "trillion", 
           "violence", "vital", "wages", "wealth", "weapons", "women", "workers", "youre", "accord", "approved", "beautiful", 
           "dakota", "obamacare", "statement", "steve", "talking", "trillions")
             
  lexicon = c(Word1, Word2)
  