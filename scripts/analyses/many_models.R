library(purrr)
library(broom)

model <- function(x) tidy(lm(x ~ arm + fev1.pct.pred2 + centre,
                             data = c))

cfqr_models <- map_df(select(c, ends_with("_s")), model) %>%
  filter(term == "armActive") %>%
  mutate(ul = estimate + (2 * std.error),
         ll = estimate - (2 * std.error)) %>%
  mutate(out = factor(names(select(c, ends_with("_s"))), labels = Subs))

ggplot(cfqr_models, aes(y = estimate, ymax = ul, ymin = ll,
                        x = out)) +
  geom_pointrange() +
  theme_minimal() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  scale_color_brewer(guide = FALSE, palette = "Set1") +
  theme(panel.grid = element_blank()) +
  ylab("Difference in mean component score, Active vs. Control") +
  xlab("") +
  coord_flip()