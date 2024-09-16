#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double gelman_rubin_cpp(List chains) {
  int n_chains = chains.size();
  int n = as<NumericVector>(chains[0]).size();

  // Calculate chain means and overall mean
  NumericVector chain_means(n_chains);
  double overall_mean = 0;

  for (int i = 0; i < n_chains; i++) {
    NumericVector chain = chains[i];
    chain_means[i] = mean(chain);
    overall_mean += chain_means[i];
  }
  overall_mean /= n_chains;

  // Calculate between-chain and within-chain variance
  double B = 0;
  double W = 0;

  for (int i = 0; i < n_chains; i++) {
    NumericVector chain = chains[i];
    B += pow(chain_means[i] - overall_mean, 2);
    W += var(chain);
  }

  B *= n / (n_chains - 1);
  W /= n_chains;

  // Calculate potential scale reduction factor
  double V = (1 - 1.0/n) * W + B/n;
  double R = sqrt(V / W);

  return R;
}
