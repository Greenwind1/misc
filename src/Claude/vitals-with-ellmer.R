# library ----
library(vitals)
library(ellmer)
library(tibble)



# simple task ----
# simple_addition <- tibble(
#     input = c("What's 2+2?", "What's 2+3?", "What's 2+4?"),
#     target = c("4", "5", "6")
# )

# tsk <- Task$new(
#     dataset = simple_addition,
#     solver = generate(chat_anthropic(model = "claude-sonnet-4-20250514")),
#     scorer = model_graded_qa()
# )
# 
# tsk$eval()



# An R Eval ----
# An R Eval is a dataset of challenging R coding problems. 
# Each input is a question about R code which could be solved on first-read only by 
# experts and, with a chance to read documentation and run some code, by fluent data scientists. 
# Solutions are in target() and enable a fluent data scientist to evaluate whether the solution deserves full, partial, or no credit.

claude_3_7_sonnet <- chat_anthropic(model = "claude-3-7-sonnet-latest")
claude_latest <- chat_anthropic()

are_task <- Task$new(
    dataset = are,
    solver = generate(),
    scorer = model_graded_qa(
        scorer_chat = claude_3_7_sonnet, 
        partial_credit = TRUE
    ),
    epochs = 3,
    name = "An R Eval"
)

are_task

are_task$eval(solver_chat = claude_latest)
save(are_task, file = "output/are_claude_4_sonnet.rda")
