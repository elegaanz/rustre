// Lustre syntax highlighting

#let colors = (
  keyword: red,
  comment: gray,
)

#let lustre(content) = {
  show regex("\bfunction\b"): set text(colors.keyword)
  show regex("\blet\b"): set text(colors.keyword)
  show regex("\btel\b"): set text(colors.keyword)
  show regex("\breturns\b"): set text(colors.keyword)

  show regex("--.*"): set text(colors.comment)

  content
};