#include <Rcpp.h>
#include <R_ext/Arith.h>
#include <cmath>
#include <vector>

using namespace Rcpp;

inline bool is_missing(double x) {
    return R_IsNA(x) || R_IsNaN(x);
}

// [[Rcpp::export]]
NumericMatrix row_scale_dgC(
        S4 x,
        bool center = true,
        bool scale = true,
        bool add_attr = true,
        bool drop_na_rows = true
) {
    if (!x.inherits("dgCMatrix")) {
        stop("`x` must inherit from Matrix::dgCMatrix.");
    }

    IntegerVector dims = x.slot("Dim");
    IntegerVector p = x.slot("p");
    IntegerVector i = x.slot("i");
    NumericVector values = x.slot("x");
    List input_dimnames = x.slot("Dimnames");

    const int nr = dims[0];
    const int nc = dims[1];

    // Dense output is necessary when centering sparse data.
    NumericMatrix work(nr, nc);

    // Expand the CSC sparse representation.
    for (int col = 0; col < nc; ++col) {
        for (int k = p[col]; k < p[col + 1]; ++k) {
            work(i[k], col) = values[k];
        }
    }

    NumericVector means(nr);
    NumericVector sds(nr);

    SEXP row_names = R_NilValue;
    SEXP col_names = R_NilValue;

    if (input_dimnames.size() >= 2) {
        row_names = input_dimnames[0];
        col_names = input_dimnames[1];
    }

    for (int row = 0; row < nr; ++row) {
        long double total = 0.0L;
        int observations = 0;
        bool has_missing = false;
        bool has_infinite = false;

        for (int col = 0; col < nc; ++col) {
            const double value = work(row, col);

            if (is_missing(value)) {
                has_missing = true;
            } else {
                total += static_cast<long double>(value);
                ++observations;

                if (!std::isfinite(value)) {
                    has_infinite = true;
                }
            }
        }

        // Corresponds to rowMeans(..., na.rm = TRUE).
        means[row] = observations == 0
        ? R_NaN
        : static_cast<double>(total / observations);

        if (!scale) {
            sds[row] = 1.0;
        } else if (has_missing || nc < 2) {
            // stats::sd() uses na.rm = FALSE by default.
            sds[row] = NA_REAL;
        } else if (has_infinite) {
            sds[row] = R_NaN;
        } else {
            // Welford's algorithm for sample standard deviation.
            long double running_mean = 0.0L;
            long double m2 = 0.0L;

            for (int col = 0; col < nc; ++col) {
                const long double value = work(row, col);
                const long double count = col + 1.0L;
                const long double delta = value - running_mean;

                running_mean += delta / count;
                m2 += delta * (value - running_mean);
            }

            sds[row] = std::sqrt(
                static_cast<double>(m2 / (nc - 1.0L))
            );
        }
    }

    // Apply centering and scaling.
    for (int row = 0; row < nr; ++row) {
        const double row_center = center ? means[row] : 0.0;
        const double row_scale = scale ? sds[row] : 1.0;

        for (int col = 0; col < nc; ++col) {
            work(row, col) =
                (work(row, col) - row_center) / row_scale;
        }
    }

    // Match the original condition:
    // rowSums(is.na(x)) < ncol(x)
    std::vector<int> keep;
    keep.reserve(nr);

    for (int row = 0; row < nr; ++row) {
        bool has_nonmissing = false;

        for (int col = 0; col < nc; ++col) {
            if (!is_missing(work(row, col))) {
                has_nonmissing = true;
                break;
            }
        }

        if (!drop_na_rows || has_nonmissing) {
            keep.push_back(row);
        }
    }

    NumericMatrix result(keep.size(), nc);

    for (std::size_t out_row = 0; out_row < keep.size(); ++out_row) {
        const int input_row = keep[out_row];

        for (int col = 0; col < nc; ++col) {
            result(out_row, col) = work(input_row, col);
        }
    }

    // Preserve and subset dimnames.
    List output_dimnames(2);

    if (row_names != R_NilValue) {
        CharacterVector old_names(row_names);
        CharacterVector new_names(keep.size());

        for (std::size_t row = 0; row < keep.size(); ++row) {
            new_names[row] = old_names[keep[row]];
        }

        output_dimnames[0] = new_names;
    } else {
        output_dimnames[0] = R_NilValue;
    }

    output_dimnames[1] = col_names;
    result.attr("dimnames") = output_dimnames;

    // rowMeans() and apply(..., 1, sd) ordinarily carry row names.
    if (row_names != R_NilValue) {
        means.attr("names") = row_names;
        sds.attr("names") = row_names;
    }

    if (add_attr) {
        if (center) {
            result.attr("scaled:center") = means;
        }

        if (scale) {
            result.attr("scaled:scale") = sds;
        }
    }

    return result;
}
