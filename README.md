# Online-cryptocurreny-portfolio
Mean-Variance portfolio optimization models are sensitive to uncertainty in risk-return estimates, which may result in poor out-of-sample performance. 
In particular, the estimates may suffer when the number of assets considered is high and the length of the return time series is not sufficiently long. 
This is precisely the case in the cryptocurrency market, where there are hundreds of crypto assets that have been traded for a few years. 

We propose enhancing the mean-variance (MV) model with a pre-selection stage that uses a prototype-based clustering algorithm to reduce the number 
of crypto assets considered at each investment period. In the pre-selection stage, we run a prototype-based clustering algorithm where the assets 
are described by variables representing the profit-risk duality. The prototypes of the clustering partition are automatically examined and the one 
that best suits our risk-aversion preference is selected. We then run the MV portfolio optimization with the crypto assets of the selected cluster.
The proposed approach is tested for a period of 17 months in the whole cryptocurrency market and two selections of the cryptocurrencies with the higher 
market capitalization (175 and 250 cryptos). We compare the results against three methods applied to the whole market: classic MV, risk parity, and 
hierarchical risk parity methods. We also compare our results with those from investing in the market index.

The simulation results generally favor our proposal in terms of profit and risk-profit financial indicators. This result reaffirms the convenience of 
using machine learning methods to guide financial investments in complex and highly-volatile environments such as the cryptocurrency market.
