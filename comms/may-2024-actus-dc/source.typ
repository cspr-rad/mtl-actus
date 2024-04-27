#import "@preview/polylux:0.3.1": *
#import "@preview/diagraph:0.2.0": *
#import themes.university: *

#let today = datetime.today().display()

#show: university-theme.with(
  short-title: "Toward Formally Verified Finance",
  short-author: "Quinn Dougherty",
  short-date: today,
)

#title-slide(
  title: "Towards Formally Verified Finance",
  subtitle: "Lessons from Cyber-Physical Systems",
  authors: [Quinn Dougherty],
  institution-name: "Casper Association",
  date: today,
  logo: image("./cspr-favicon.png"),
)


#slide(title: [Slide title], new-section: [The section])[
  #lorem(40)
]

#matrix-slide[
  left
][
  middle
][
  right
]

#matrix-slide(columns: 1)[
  top
][
  bottom
]

#matrix-slide(columns: (1fr, 2fr, 1fr), ..(lorem(8),) * 9)

it'd be good to have a section on the different usecases of formal verification, like modeling vs verification of prod code
