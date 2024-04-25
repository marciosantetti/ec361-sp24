- *In-class pratice* (04/26):

---



**Data sets**:



1. [`New One Family Houses Sold: United States`](https://fred.stlouisfed.org/series/HSN1FNSA)
2. [`U.S. Phillips curve data`](https://raw.githubusercontent.com/marciosantetti/ec361-sp24/main/lectures/010-dyn-reg/phillips_data.csv) (also available on `theSpring`)



---
---



**Proposed practice**:

The *accelerationist* version of the Phillips curve may be written in regression form as follows:

<br>

$$ \Delta \pi_t = \beta_0 + \beta_1 u_t + \varepsilon_t $$

<br>

where $\pi_t$ is the inflation rate; $\Delta \pi_t$ is the change in the inflation rate $(\pi_t - \pi_{t-1})$; $u_t$ is the unemployment rate; and $\varepsilon_t$ is a white noise residual term.

<br>

Suppose the goal is to produce a 5-year ahead forecast for the model's dependent variable based off of the regression model above. With this in mind, do the following:

- *Plot* the two model variables over time;
- Check their *stationarity*;
- Estimate the model above assuming a *well-behaved* (i.e., white noise) residual term;
- In case you find evidence of the residual term being statistically *different* from a white-noise process, estimate the regression model assuming ARIMA errors.
- Check the *residual autocorrelation* from this new model;
- In case your residuals are well-behaved, produce a *5-year ahead forecast* for $\Delta \pi_t$ assuming that the unemployment rate will follow its historical *median* value;
- Lastly, produce a *scenario-based* forecast where the unemployment rate follows a *pessimistic* and an *optimistic* path. The choice of values for these scenarios are yours.

---



Moving on to the **houses sold** data:



- Plot the data over time, checking relevant features;
- Graph the ACF and PACF plots;
- Come up with possible ARIMA model specifications
- Select the best ARIMA model, minimizing the AICc criterion;
- Check the selected model's residuals;
- Compute a 24-month ahead forecast and graph it.

