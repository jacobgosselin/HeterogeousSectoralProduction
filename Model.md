# Overview

Static GE model of small open economy with $N$ sectors, each producing a differentiated good used for intermediate inputs and final consumption. Each sector's good can be sourced domestically or imported. Households supply labor inelastically and consume a CES aggregate of domestic and foreign goods. Firms produce using labor and a CES aggregate of intermediate inputs, which themselves are CES aggregates of domestic and foreign variants. Treat the world as exogenous, with fixed foreign prices.

# Model
## Domestic Household

The household consumes by maximizing a CES aggregator across domestic sectoral goods, subject to its budget constraint.
$$
C_t = \max_{\{C_{it}\}} \left(\sum_i^N \beta_i^{\frac{1}{\nu}} C_{it}^{\frac{\nu-1}{\nu}}\right)^{\frac{\nu}{\nu-1}} \text{ subject to } \sum_i^N P_{it} C_{it} = \sum_i W_{it} L_i
$$

## Domestic Firms

Each domestic sector $i$ is composed of a representative firm who's production function is 3-nest CES...

$$
\begin{aligned}
& Y_{it} = Z_{it} \left(\gamma_i^{\frac{1}{\sigma}} L_{it}^{\frac{\sigma-1}{\sigma}} + (1-\gamma_i)^{\frac{1}{\sigma}} M_{it}^{\frac{\sigma - 1}{\sigma}} \right)^{\frac{\sigma}{\sigma-1}} \\

& M_{it} = \left(\sum_j^N \omega_{ij}^{\frac{1}{\theta_i}} \bar X_{ijt}^{\frac{\theta_i-1}{\theta_i}} \right)^{\frac{\theta_i}{\theta_i-1}} \quad \bar X_{ijt} = \left(\phi_{ij}^{\frac{1}{\xi}} X_{ijt}^{\frac{\xi - 1}{\xi}} + (1-\phi_{ij})^{\frac{1}{\xi}} \tilde X_{ijt}^{\frac{\xi - 1}{\xi}} \right)^{\frac{\xi}{\xi - 1}}
\end{aligned}
$$

## Rest of World

Foreign-currency prices $\{\tilde P_{it}\}_{i=1}^N$ are exogenous. The domestic-currency price of an imported good is $E_t \tilde P_{it}$, where $E_t$ is the real exchange rate (the price of foreign goods in units of the domestic numeraire). Exports are determined by a downward-sloping foreign demand schedule. For tradeable goods foreign demand for the domestic variety is isoelastic in its price (in foreign units):
$$
C^f_{it} = \phi^f_i \left(\frac{P_{it}}{E_t}\right)^{-\tilde \xi}
$$
Where $\phi^f_i$ is a demand shifter and $\tilde \xi$ is the export-demand elasticity.

## Equilibrium 

An equilibrium is as prices $\{P_i\}_{i=0}^N$ and $E$, and quantities $\{Y_{i}\}$, such that...
1) Prices are equal to marginal cost, i.e. $P_i = MC_i$ for all $i$; $N$ equations.
2) Markets clear, given optimal household + firm decisions: $N$ equations.
3) Set numeraire (unimportant, but choose $P_C = 1$): 1 equation.

## Calibration

I calibrate by picking a base year in my data (2024), and assuming it correspond to the non-stochastic equilibrium of the model, where all prices ($W_{it}, P_{it}, E_t, \tilde P_{it}$) and productivities equal 1. This is equivalent to normalizing GDP and price responses as relative to the base year.

Under this normalization, all share parameters equal their expenditure shares at the base year.
$$
\gamma_i = \frac{W_i L_i}{P_i Y_i}; \quad \omega_{ij} = \frac{P_{j} X_{ij}}{Q_i M_i}; \quad \psi_{ij} = \frac{P_{jd} X_{ijd}}{P_j X_{ij}}; \quad \beta_i = \frac{P_i C_i}{P C}
$$

I can back out sectoral labor allocations $L_i$ from these expenditure shares, **assuming they correspond to the closed economy** (i.e. in the absence of trade, labor allocations would be the same).
$$
\begin{aligned}
L_i = \gamma_i Y_i \\
X_{ijd} = (1-\gamma_i) \omega_{ij} Y_i \\
Y_i = C_i + \sum_j X_{jid} = \beta_i + \sum_j (1-\gamma_j) \omega_{ji} Y_j \\
\mathbb Y = \boldsymbol \beta + (1-\gamma) \boldsymbol \Omega \mathbb Y \\\
\rightarrow \mathbb Y = (\mathbb I - (1-\gamma) \boldsymbol \Omega)^{-1} \boldsymbol \beta
\end{aligned}
$$
With share parameters and labor allocations fixed, I am left with the parameters that determine the foreign demand schedule for exports of tradeable goods: demand shifters $\psi_i^f$ and export demand elasticity $\tilde \xi$. I back out demand shifters $\psi_i^f$ via market clearing in the base year. I assume the export demand elasticity $\tilde \xi$ equals the Armington elasticity $\xi$. 

# Theorem Derivations

Intermediate expenditure shares will be used repeatedly throughout my derivations, so I define them here to simplify notations.
$$
\Omega_{ijt} = \frac{P_{jt} X_{ijt}}{\sum_k P_{kt} X_{ikt} + E_t \tilde P_{kt} \tilde X_{ikt}}, \Delta_{ijt} = \frac{P_{jt} X_{ijt}}{P_{jt} X_{ijt} + E_t \tilde P_{jt} \tilde X_{ijt}}
$$
Our main object of interest will be the **input-output matrix**, defined as 
$$
\mathbf A_t := a_{ijt} = \frac{P_{jt} X_{ijt}}{P_{it} Y_{it}}; \space \mathbf {\tilde A}_t := \tilde a_{ijt} = \frac{E_t \tilde P_{jt} X_{ijt}}{P_{it} Y_{it}}
$$
Note that row $0$ and column $0$ of the input-output matrix corresponds to the household, i.e. 
$$
a_{0jt} = \frac{P_{jt} C_{jt}}{\sum_k P_{kt} C_{kt}}; \space a_{i0t} = 0
$$
## Expenditure Shares

| Definition                     | Notation                                                                                                                                                         |
| ------------------------------ | ---------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Labor Share                    | $\Gamma_{it} = \frac{W_{it} L_{it}}{P_{it} Y_{it}}$                                                                                                              |
| Intermediate Expenditure Share | $\Omega_{ijt} = \frac{P_{jt} X_{ijt}}{\sum_k P_{kt} X_{ikt} + E_t \tilde P_{kt} \tilde X_{ikt}}$                                                                 |
| Import Ratio                   | $\Phi_{ijt} = \frac{P_{jt} X_{ijt}}{P_{jt} X_{ijt} + E_t \tilde P_{jt} \tilde X_{ijt}}$                                                                          |
| Input Output Matrix            | $\mathbf A_t := a_{ijt} = \frac{P_{jt} X_{ijt}}{P_{it} Y_{it}}; \space \mathbf {\tilde A}_t := \tilde a_{ijt} = \frac{E_t \tilde P_{jt} X_{ijt}}{P_{it} Y_{it}}$ |
| Leontief Inverse               | $\mathbb \Psi_t := \psi_{ijt} = (\mathbb I - \mathbf A_t)^{-1}$                                                                                                  |

## Proof of Theorem 1:

Setting aggregate expenditure as the numeraire, $\sum_i P_{it} C_{it} = 1$, market clearing delivers the following expression relating Domar weights, the input-output matrix, and export expenditures.

$$
\begin{align*}

Y_{jt} = \sum_i X_{ijt} + C_{jt}^f \rightarrow \lambda_{jt} = \sum_i a_{ijt} \lambda_{it} + P_{jt} C_{jt}^f \\

\boldsymbol \lambda_t = \boldsymbol \lambda'_t \mathbf A_t + \mathbf{NX}_t

\end{align*}
$$
Taking the total derivative, we can express changes in Domar weights in terms of changes in export expenditures and changes in the input-output matrix.

$$

d \boldsymbol \lambda_t = (\boldsymbol \lambda^T_t d \mathbf A_t + d \mathbf{NX}_t) \boldsymbol \Psi_t \rightarrow d \lambda_{it} = \sum_j \psi_{jit} \left(\sum_k \lambda_{kt} d a_{kjt} + d NX_{jt}\right)

$$

By definition of export demand $C^f_{it} = \phi^f_i \left(\frac{P_{it}}{E_t}\right)^{-\tilde \xi}$, changes in export expenditures can be expressed in terms of current export expenditures and changes in prices.

$$

\partial NX_{jt} = NX_{jt} \partial \log NX_{jt} = NX_{jt} ((1-\tilde \xi) \partial \log P_{jt} + \partial \log E_t)

$$

And by cost-minimization, changes in the input-output matrix can be expressed in terms of current expenditure shares and changes in prices.

$$

\partial a_{ijt} = a_{ijt} \partial \log a_{ijt} = a_{ijt}\left((\sigma - 1) \partial \log P_{it} + (\theta_i - \sigma) \partial \log Q_{it} + (\xi-\theta_i) \partial \log \bar P_{jt} + (1-\xi) \partial \log P_{jt}\right)

$$

Cost-minimization also allows us to write changes in CES price indices in terms of changes in input prices and expenditure shares.

$$

\partial \log Q_{it} = \sum_j \Omega_{ijt} \partial \log \bar P_{jt}, \space \partial \log \bar P_{jt} = \Phi_{ijt} \partial \log P_{jt} + (1 - \Phi_{ijt}) (\partial \log \tilde P_{jt} + \partial \log E_t)

$$

## Proof of Theorem 2

By cost-minimization of the firm problem, we can write changes in prices as follows:

$$
\begin{align*}

\partial \log P_{it} &= \Gamma_{it} \partial \log W_{it} + (1-\Gamma_{it}) \partial \log Q_{it} - \partial \log Z_{it} \\

\partial \log Q_{it} &= \sum_j \Omega_{ijt} \partial \log \bar P_{jt} \\

\partial \log \bar P_{jt} &= \Phi_{ijt} \partial \log P_{jt} + (1 - \Phi_{ijt}) (\partial \log \tilde P_{jt} + \partial \log E_t)

\end{align*}
$$
Sector-specific wages (factor prices) are equal to the marginal revenue of labor:

$$
\begin{aligned}

W_{it} &= P_{it} Z_{it}^{1-\frac 1 \sigma} \left(\gamma_i \frac {Y_{it}}{L_{it}}\right)^{\frac 1 \sigma} \\

\partial \log W_{it} &= \partial \log P_{it} + \left(1-\frac 1 \sigma\right) \partial \log Z_{it} + \frac 1 \sigma \left(\partial \log Y_{it}\right)

\end{aligned}
$$

Observe that we can write changes in sectoral output in terms of changes in sales share (remember that we've set nominal expenditures as our numeraire).

$$

\lambda_{it} = P_{it} Y_{it} \rightarrow \partial \log Y_{it} = \partial \log\lambda_{it} - \partial \log P_{it}

$$

Plugging into the wage equation, we have

$$
\partial \log W_{it} = \left(1-\frac 1 \sigma\right) (\partial \log P_{it} + \partial \log Z_{it}) + \frac 1 \sigma \partial \log \lambda_{it}
$$

Plugging this definition of wages into the price equation, we have:

$$
\begin{aligned}

& \partial \log P_{it} = \Gamma_{it}\!\left[\left(1-\tfrac{1}{\sigma}\right)(\partial \log P_{it} + \partial \log Z_{it}) + \tfrac{1}{\sigma}\partial \log \lambda_{it}\right] + (1-\Gamma_{it})\partial \log Q_{it} - \partial \log Z_{it} \\

& \underbrace{\left(1 - \Gamma_{it}\!\left(1-\tfrac{1}{\sigma}\right)\right)}_{\kappa_{it}}\partial \log P_{it} = \tfrac{\Gamma_{it}}{\sigma}\partial \log \lambda_{it} + (1-\Gamma_{it})\partial \log Q_{it} - \underbrace{\left(1 - \Gamma_{it}\!\left(1-\tfrac{1}{\sigma}\right)\right)}_{\kappa_{it}}\partial \log Z_{it} \\

& \partial \log P_{it} = \frac{\Gamma_{it}/\sigma}{\kappa_{it}}\partial \log \lambda_{it} + \frac{1-\Gamma_{it}}{\kappa_{it}}\partial \log Q_{it} - \partial \log Z_{it}

\end{aligned}
$$

By expanding the price equation definition we get the following equation:

$$
\begin{aligned}

\partial \log P_{it} &= \frac{\Gamma_{it}/\sigma}{\kappa_{it}}\partial \log \lambda_{it} + \frac{1-\Gamma_{it}}{\kappa_{it}}\sum_j \Omega_{ijt} (\Phi_{ijt} \partial \log p_{jt} + (1-\Delta_{ij})\partial \log E_t \tilde P_{jt}) - \partial \log Z_{it} \\

&= \frac{\Gamma_{it}/\sigma}{\kappa_{it}}\partial \log \lambda_{it} + \frac{1-\Gamma_{it}}{\kappa_{it}}\sum_j \!\left(a_{ijt}\,\partial \log P_{jt} + \tilde{a}_{ijt}\,\partial \log E_t\tilde{P}_{jt}\right) - \partial \log Z_{it}

\end{aligned}
$$

Vectorizing and inverting we recover the forward propagation equation, with $a^\kappa_{ijt} = \frac{1-\Gamma_{it}}{\kappa_{it}} a_{ijt}$ and $\tilde{a}^\kappa_{ijt} = \frac{1}{\kappa_{it}} \tilde{a}_{ijt}$:

$$
\begin{aligned}

(\mathbb{I} - \mathbf{A}^\kappa_t)\,\partial \log \mathbf{P}_t &= \frac{\boldsymbol{\Gamma}_t/\sigma}{\boldsymbol{\kappa}_t}\partial \log \boldsymbol{\lambda}_t + \tilde{\mathbf{A}}^\kappa_t(\partial \log \tilde{\mathbf{P}}_t + \partial \log E_t) - \partial \log \mathbf{Z}_t \\

\partial \log \mathbf{P}_t &= \boldsymbol{\Psi}^\kappa_t\!\left(\frac{\boldsymbol{\Gamma}_t/\sigma}{\boldsymbol{\kappa}_t}\partial \log \boldsymbol{\lambda}_t + \tilde{\mathbf{A}}^\kappa_t(\partial \log \tilde{\mathbf{P}}_t + \partial \log E_t) - \partial \log \mathbf{Z}_t\right)

\end{aligned}
$$

## Proof of Theorem 3

Since our economy is efficient, the decentralized equilibrium corresponds to the solution to the following social planner problem.

$$
\begin{aligned}

&\max_{C_{it}, X_{ijt}} C_t \\

&\text{ subject to } Y_{it} = F_{it}( Z_{it}, L_{it}, X_{ijt}, \tilde X_{ijt}) = C_{it} + \sum_j X_{jit} + C_{it}^f\quad [\mu_{it}] \\

&\text{ and } L_{it} = \bar L_i \quad [\iota_{it}]

\end{aligned}
$$

The envelope theorem implies that

$$
\frac{\partial C_t}{\partial \log Z_{it}} = Z_{it} \frac{\partial C_t}{\partial Z_{it}} = \mu_{it} Y_{it}
$$

First order conditions further imply that

$$
\mu_{jt} = \mu_{it} \frac{\partial{F_{it}}}{\partial X_{ijt}} \text{ for every } i, j
$$

Which correspond with the firm first-order conditions in the decentralized equilibrium, implying that $\mu_{it} = P_{it}$. Thus

$$
\frac{\partial C_t}{\partial \log Z_{it}} = P_{it} Y_{it} = \lambda_{it}
$$

## Empirical Specification

Cost minimization by the representative firm in sector $i$ implies that their input expenditure share on sector $j$ is a function of of relative prices, share parameters, and substitution elasticities:

$$
\Omega_{ijt} = \left(\frac{P_{jt}}{\bar P_{jt}}\right)^{1-\xi} \left(\frac{\bar P_{jt}}{Q_{it}}\right)^{1-\theta_i} \phi_{ij} \omega_{ij}
$$

Cost minimization also implies that the CES price index $\bar P_{jt}$ can be written as a function of domestic prices, share parameters, and the import ratio:

$$
\bar P_{jt} = P_{jt} \Phi_{ijt}^{\frac{1}{\xi- 1}} \phi_{ij}^{\frac{1}{1 - \xi}}
$$

Combining these two conditions, taking logs, and taking the total derivative, yields the following result:

$$
\partial \log \Omega_{ijt} = (1-\theta_i) \partial \log P_{jt} + \frac{\xi- \theta_i}{\xi- 1} \partial \log \Phi_{ijt} + \eta_{it} \text{ where } \eta_{it} = (\theta_i - 1) \partial \log Q_{it}
$$

We estimate this with year-over-year changes in industry-to-industry spending shares and prices. 

$$
\Delta \log \Omega_{ijt} = (1-\theta_i) \Delta \log P_{jt} + \frac{\xi- \theta_i}{\xi- 1} \Delta \log \Phi_{ijt} + \eta_{it} + \epsilon_{ijt}
$$