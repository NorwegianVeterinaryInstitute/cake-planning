
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Cake Planning at VI

<!-- badges: start -->
<!-- badges: end -->

The Cake planning app is a R Shiny application that can be used for
planning cake, coffee, or similar events at a workplace.

However, its main purpose is pedagogical. We use the app to learn
software development in a team, and specifically how to write
[production grade shiny apps](https://engineering-shiny.org/), and how
to deploy apps to Posit Connect using CI/CD.

Initially the app was meant to be an example of how the `pins` package
can be used to read and write that to Posit Connect. But the app has
since grown its teaching domain to include `renv`, `github actions`,
`{golem}`, and maybe some other things.

## How to run the app

The main branch of the app is being deployed to Posit Connect, and
requires a pin that is also hosted there. This is why, the app from the
main branch won’t work for you if you try to run it.

We maintain a mostly-up-to-date
[local-testing](https://github.com/NorwegianVeterinaryInstitute/cake-planning/tree/local-testing)
branch that works with an in-memory pin using `pins::board_temp()`.
