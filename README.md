# EC 361&mdash;001: Applied Macroeconomic Forecasting, Spring 2024

Welcome to EC 361&mdash;001: Applied Macroeconomic Forecasting!<br>
*Spring 2024 - Dr. Marcio Santetti - [Economics Department](https://www.skidmore.edu/economics/), [Skidmore College](https://www.skidmore.edu/)*


## Course Syllabus

<br>

  - [EC 361&mdash;001 Syllabus](https://raw.githack.com/marciosantetti/ec361-sp24/main/syllabus/ec361-syllabus-sp24.pdf)

<br>

## Lecture notes/slides

<br>

*Note*: Slides and assignment files created with the `xaringan` *R* package, with templates mainly inspired by [Ed Rubin](https://github.com/edrubin)'s CSS file.

<br>

**001: Course Logistics**

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/001-logistics/001-logistics.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/001-logistics/001-logistics.pdf)
      - [`Video lecture`](https://youtu.be/9gAjB7F9dq0)

<br>

**002: Forecasting methods and steps**

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/002-intro-forecast/002-intro-forecast.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/002-intro-forecast/002-intro-forecast.pdf)


<br>

**003: Time series graphics I**

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/003-graphics/003-graphics.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/003-graphics/003-graphics.pdf)

<br>

**004: Time series graphics II**

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/003-graphics/003-graphics-2.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/003-graphics/003-graphics-2.pdf)

<br>

**005: Time series decomposition I**

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/004-decomposition/004-decomposition-1.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/004-decomposition/004-decomposition-1.pdf)

<br>

**006: Time series decomposition II**

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/004-decomposition/004-decomposition-2.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec361-sp24/main/lectures/004-decomposition/004-decomposition-2.pdf)

<br>

## Applied lectures

<br>

The following code will install the main packages needed for the applied lectures.

```r
install.packages(c("janitor", "feasts", "tsibble", "fable", "fabletools", "fpp3", "sugrrants"))
```



<br>

**001: Time series graphics**

<br>

  - [`Part 1`](https://youtu.be/GuK0qH8jj8I)
  - [`Part 2`](https://youtu.be/mGwx6daVB8Q)

<br>



## Problem Sets

<br>

- [`Problem Set 0`](https://raw.githack.com/marciosantetti/ec361-sp24/main/problem-sets/ps0-ec361-sp24.pdf) (due 02/16, 12:20 PM)
- [`Problem Set 1`](https://raw.githack.com/marciosantetti/ec361-sp24/main/problem-sets/ps1-ec361-sp24.pdf) (due 03/01, 12:20 PM)


<br>



## R basics

<br>

*Note*: Slides created using [Quarto](https://quarto.org/), with templates mainly inspired by [Mine Çetinkaya-Rundel](https://mine-cr.com/)'s SCSS file.

<br>

**001**: Introduction to `R`, `RStudio`, and the `tidyverse`

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/001-tidyverse/001-tidyverse.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/001-tidyverse/001-tidyverse.pdf)
  - Data: [`toy_data.csv`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/001-tidyverse/toy_data.csv) | Data can also be downloaded on `theSpring`
  - [`Video lecture`](https://youtu.be/SCOCBd1t7Ew)
  - [`Practice 1`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-1-sp24.pdf)


**002**: Manipulating data in the `tidyverse`
 
  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/002-data-manipulation/002-data-manipulation.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/002-data-manipulation/002-data-manipulation.pdf)
  - [`Video lecture`](https://youtu.be/h8em0bYRgvY)
  - [`Practice 2`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-2-sp24.pdf)
  
**003**: Manipulating economic data with the `tidyverse`

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/003-data-manipulation-2/003-data-manipulation-2.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/003-data-manipulation-2/003-data-manipulation-2.pdf)
  - Data: [`gdp-data.csv`](https://raw.githack.com/marciosantetti/ec103-fall22/main/lab/003-data-manipulation-2/gdp-data.csv) | Data can also be downloaded on `theSpring`
  - [`Video lecture`](https://youtu.be/z5ON3xXREiA)
  - [`Practice 3`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-3-sp24.pdf)
  

**004**: The "grammar of graphics"
  
   - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/004-graphics/004-ggraphics.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/004-graphics/004-graphics.pdf)
   - [`Video lecture`](https://www.youtube.com/watch?v=u2DRXvWXAPQ&ab_channel=MarcioSantetti)
   - [`Practice 4`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-4-sp24.pdf)
   
   
**005**: Making plots informative
 
  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/005-informative-plots/005-informative-plots.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/005-informative-plots/005-informative-plots.pdf)
  - Data: [`unemp-data.csv`](https://raw.githack.com/marciosantetti/ec103-fall22/main/lab/005-informative-plots/unemp-data.csv) | Data can also be downloaded on `theSpring`
  - [`Video lecture`](https://youtu.be/pMhebKitqqU)
  - [`Practice 5`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-5-sp24.pdf)
  
**006**: Customizing and saving plots
 
   - Slides:  [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/006-custom-plots/006-customizing-plots.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/006-custom-plots/006-customizing-plots.pdf)
   - Data: [`lab6_data.csv`](https://raw.githack.com/marciosantetti/ec103-fall22/main/lab/006-custom-plots/lab6_data.csv) | Data can also be downloaded on `theSpring`
   - [`Video lecture`](https://youtu.be/HMc9_Zumfa4)
   - [`Practice 6`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-6-sp24.pdf)
   
 **007**: Dealing with dates
 
  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/007-dates/007-dates.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/007-dates/007-dates.pdf)
  - [`Video lecture`](https://youtu.be/h0jWmgYe3zI)
  - [`Practice 7`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-7-sp24.pdf)
 
**008**: Variable classes

  - Slides: [`html`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/008-variable-classes/008-variable-classes.html) | [`pdf`](https://raw.githack.com/marciosantetti/ec103-sp23/main/lab/008-variable-classes/008-variable-classes.pdf)
  - [`Video lecture`](https://youtu.be/ZJ-AyQC4OOs)
  - [`Practice 8`](https://raw.githack.com/marciosantetti/ec361-sp24/main/r-bootcamp/practice-8-sp24.pdf)


<br>

