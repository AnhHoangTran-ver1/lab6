#include <Rcpp.h>
#include <algorithm>
#include <vector>
using namespace Rcpp;

// C++ version of the dynamic programming knapsack.
// Called from knapsack_dynamic() in R when fast = TRUE.

// [[Rcpp::export]]
List knapsack_dynamic_cpp(IntegerVector w, NumericVector v, int W) {
  int n = w.size();

  // Table with n + 1 rows and W + 1 columns, filled with 0
  NumericMatrix m(n + 1, W + 1);

  for (int i = 1; i <= n; i++) {
    for (int j = 0; j <= W; j++) {
      if (w[i - 1] > j) {
        // Item i does not fit: same as without it
        m(i, j) = m(i - 1, j);
      } else {
        // Best of skipping item i or taking it
        m(i, j) = std::max(m(i - 1, j),
          m(i - 1, j - w[i - 1]) + v[i - 1]);
      }
    }
  }

  std::vector<int> elements;
  int j = W;
  for (int i = n; i >= 1; i--) {
    if (m(i, j) != m(i - 1, j)) {
      elements.push_back(i);  // store item numbers counted from 1, like R
      j -= w[i - 1];
    }
  }
  // Items were found from last to first, so reverse to increasing order
  std::reverse(elements.begin(), elements.end());

  return List::create(Named("value") = m(n, W),
                      Named("elements") = elements);
}
