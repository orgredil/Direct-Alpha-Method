# R translation of irr_mata.do
# Solves for (1+IRR) given a vector of equally spaced cash flows named 'cf_'.
# Cash flows in 'cf_' need be sorted DESCENDING time-wise (e.g., CF_T, CF_T-1, ..., CF_0).
# Creates a result which is the 'properroot' (1+IRR) or NA.

irr_mata <- function(cf_) {
  # Handle cases where IRR cannot be calculated
  # Need at least two cash flows for a meaningful polynomial (e.g., CF0 and CF1+NAV)
  # or if all cash flows are zero or NA.
  if (length(cf_) < 2 || all(is.na(cf_)) || all(cf_ == 0, na.rm = TRUE)) {
    return(NA_real_)
  }
  
  # Remove trailing NAs as polyroot cannot handle them, but keep leading/internal NAs if any (polyroot will error)
  # However, for IRR, typically we expect a dense vector of CFs.
  # If there are internal NAs, polyroot will fail. The input should be clean.
  # For this function, we assume cf_ is a clean vector of numbers.
  # If cf_ could have NAs that need specific handling before polyroot, that logic would be here.
  # For now, we assume cf_ is ready for polyroot or polyroot will handle errors for non-numeric.
  
  # Calculate invested and returned amounts
  # Ensure cf_ is numeric for these operations
  cf_numeric <- as.numeric(cf_[!is.na(cf_)]) # Use only non-NA values for sum
  if(length(cf_numeric) == 0) return(NA_real_) # All were NA
  
  invested <- -sum(cf_numeric[Re(cf_numeric) < 0])
  returned <- sum(cf_numeric[Re(cf_numeric) > 0])
  
  # T is the degree of the polynomial, which is number of periods - 1
  # Or, if CFs are [CF_T, ..., CF_0], then T is length - 1.
  # T <- length(cf_) - 1 # Not directly used in polyroot logic below but good for context
  
  # Perturb the final distribution (first cash flow, CF_T) to check root monotonicity
  # Ensure cf_[1] exists and is numeric. The length check above helps.
  # If cf_[1] is NA, this perturbation might not be meaningful or could error.
  # Assuming cf_[1] is a valid number.
  uptick <- invested * 0.001 # Match Stata's uptick value
  
  # Ensure cf_ has at least one element for cf_[1]
  # The length(cf_) < 2 check at the start should ensure this.
  
  cf_up <- cf_ # Create copies to modify
  cf_dn <- cf_
  
  cf_up[1] <- cf_[1] + uptick
  cf_dn[1] <- cf_[1] - uptick
  
  # Using polyroot function from base R.
  # polyroot expects coefficients in increasing order of power (c0, c1*x, c2*x^2, ...).
  # If cf_ is [CF_T, CF_T-1, ..., CF_0], then to solve P(x) = CF_T + CF_T-1*x + ... + CF_0*x^T = 0
  # where x = (1+IRR), we need to pass cf_ as is to polyroot.
  # polyroot(c(c0, c1, ..., cT)) finds roots of c0 + c1*z + ... + cT*z^T = 0.
  # So, if cf_ = [CF_T, ..., CF_0], then polyroot(cf_) finds roots of CF_T + CF_(T-1)*z + ... + CF_0*z^T = 0.
  # This means z = (1+IRR). This matches the Stata intent.
  
  # Suppress warnings from polyroot if it can't find roots or for other issues, handle via NA
  try_polyroot <- function(p_coeffs) {
    res <- tryCatch(polyroot(p_coeffs), warning = function(w) NULL, error = function(e) NULL)
    if (is.null(res) || length(res) == 0) return(complex(0)) # Return empty complex if no roots
    return(res)
  }
  
  roots <- try_polyroot(cf_)
  rootsup <- try_polyroot(cf_up)
  rootsdn <- try_polyroot(cf_dn)
  
  # If any polyroot failed to return comparable roots, we can't proceed with consistency checks
  if (length(roots) == 0 || length(rootsup) == 0 || length(rootsdn) == 0 || 
      length(roots) != length(rootsup) || length(roots) != length(rootsdn)) {
    # Fallback: use only 'roots' if consistency check isn't possible
    # This part of the logic might need refinement based on how Stata handles such cases.
    # For now, if consistency cannot be checked, we proceed with 'roots' only.
    rrsFlag <- (abs(Im(roots)) < 1e-9) & (Re(roots) > 1e-9) # Real and positive (1+IRR)
    
    properroot <- NA_real_
    if (sum(rrsFlag) > 0) {
      realroots_val <- Re(roots[rrsFlag])
      posroots_values <- realroots_val[realroots_val >= 1.0]
      negroots_values <- realroots_val[realroots_val < 1.0 & realroots_val > 1e-9] # (1+IRR) > 0
      
      if (returned >= invested) { # Expect (1+IRR) >= 1
        properroot <- if (length(posroots_values) > 0) min(posroots_values) else NA_real_
      } else { # Expect (1+IRR) < 1 (but > 0)
        properroot <- if (length(negroots_values) > 0) max(negroots_values) else NA_real_
      }
    }
    return(properroot)
  }
  
  # Create flags for real and positive roots
  # Using a small tolerance for Im(roots) == 0 and Re(roots) > 0
  # (1+IRR) should be positive.
  tol <- 1e-9 # Tolerance for checking if imaginary part is zero and real part is positive
  rrsFlag <- (abs(Im(roots)) < tol) & (Re(roots) > tol)
  rruFlag <- (abs(Im(rootsup)) < tol) & (Re(rootsup) > tol)
  rrdFlag <- (abs(Im(rootsdn)) < tol) & (Re(rootsdn) > tol)
  
  # Consistent roots across perturbations
  # This implies we are looking for roots that are present and real+positive in all three sets
  # and maintain their relative order.
  # Stata's logic: rrFlag=rrsFlag:*rruFlag:*rrdFlag implies selecting indices where all are true.
  # This assumes the roots are ordered similarly or can be matched.
  # polyroot doesn't guarantee order, so direct element-wise product might be problematic
  # if the number of real positive roots changes.
  
  # Let's find common real positive roots first, then check consistency.
  # This part is tricky because polyroot doesn't guarantee order of roots.
  # Stata's approach might rely on implicit matching or specific behavior of its polyroots.
  
  # A simpler interpretation of Stata's "rrFlag=rrsFlag:*rruFlag:*rrdFlag"
  # is to find roots that are real & positive in `roots` AND whose counterparts
  # (if they exist and can be matched) are also real & positive in `rootsup` and `rootsdn`.
  # Given the difficulty in matching roots, the Stata code might be implicitly
  # filtering `roots` based on `rrsFlag` and then checking conditions.
  
  properroot <- NA_real_
  
  # First, consider roots that are real and positive from the original CF
  if (sum(rrsFlag) > 0) {
    candidate_roots <- Re(roots[rrsFlag])
    
    # Try to find corresponding roots in up/dn sets for consistency check
    # This is an approximation of Stata's logic without explicit root matching
    consistent_roots_list <- c()
    for (r_val in candidate_roots) {
      # Check if a "close" root exists in rootsup and rootsdn that is also real & positive
      # And if the monotonicity holds: Re(r_val) < Re(r_up_match) and Re(r_val) > Re(r_dn_match)
      
      # Check monotonicity: root < root_up and root > root_dn
      # This implies that as the last cash flow (CF_T) increases, (1+IRR) should increase.
      is_consistent <- FALSE
      
      # Find closest real positive root in rootsup
      real_rootsup_vals <- Re(rootsup[rruFlag])
      if (length(real_rootsup_vals) > 0) {
        diff_up <- abs(real_rootsup_vals - r_val)
        closest_up <- real_rootsup_vals[which.min(diff_up)]
        # Check if this closest root satisfies monotonicity and is indeed "close"
        if (r_val < closest_up && min(diff_up) < 0.1 * abs(r_val) + 0.01) { # Heuristic for "close"
          # Now check rootsdn
          real_rootsdn_vals <- Re(rootsdn[rrdFlag])
          if (length(real_rootsdn_vals) > 0) {
            diff_dn <- abs(real_rootsdn_vals - r_val)
            closest_dn <- real_rootsdn_vals[which.min(diff_dn)]
            if (r_val > closest_dn && min(diff_dn) < 0.1 * abs(r_val) + 0.01) {
              is_consistent <- TRUE
            }
          }
        }
      }
      
      if (is_consistent) {
        consistent_roots_list <- c(consistent_roots_list, r_val)
      }
    }
    
    if (length(consistent_roots_list) > 0) {
      posroots_values <- consistent_roots_list[consistent_roots_list >= 1.0]
      negroots_values <- consistent_roots_list[consistent_roots_list < 1.0 & consistent_roots_list > tol] # (1+IRR) > 0
      
      if (returned >= invested) { # Expect (1+IRR) >= 1
        properroot <- if (length(posroots_values) > 0) min(posroots_values) else NA_real_
      } else { # Expect (1+IRR) < 1 (but > 0)
        properroot <- if (length(negroots_values) > 0) max(negroots_values) else NA_real_
      }
    } else {
      # Fallback if no "consistent" roots found by the above logic,
      # use the original Stata's simpler filter if rrFlag sum > 0
      # This part tries to mimic the Stata code's direct element-wise logic for rrFlag
      # which assumes roots are somewhat aligned or that this filtering is robust enough.
      # This is a direct translation attempt of the Stata `if (sum(select(rrFlag,rrFlag:>=0))>0)` block
      
      # We need to ensure roots, rootsup, rootsdn have same length for element-wise ops
      # The check at the beginning of polyroot results should handle this.
      # If they are not same length, this block should be skipped or handled.
      # For now, assuming they are same length if we reached here without early exit.
      
      combined_flag <- rrsFlag & rruFlag & rrdFlag # Element-wise AND
      if (sum(combined_flag) > 0) {
        realroots_s <- Re(roots[combined_flag])
        realrootsup_s <- Re(rootsup[combined_flag])
        realrootsdn_s <- Re(rootsdn[combined_flag])
        
        below <- realroots_s < realrootsup_s
        above <- realroots_s > realrootsdn_s
        
        consroots_s_val <- realroots_s[below & above]
        
        if (length(consroots_s_val) > 0) {
          posroots_s_values <- consroots_s_val[consroots_s_val >= 1.0]
          negroots_s_values <- consroots_s_val[consroots_s_val < 1.0 & consroots_s_val > tol]
          
          if (returned >= invested) {
            properroot <- if (length(posroots_s_values) > 0) min(posroots_s_values) else NA_real_
          } else {
            properroot <- if (length(negroots_s_values) > 0) max(negroots_s_values) else NA_real_
          }
        }
      } else if (sum(rrsFlag) > 0) { # Stata's "else if (sum(select(rrsFlag,rrsFlag:>=0))>0)"
        # This is the block from Stata when full consistency (rrFlag) fails, but original has real roots.
        # It tries to apply consistency checks individually.
        
        current_real_roots <- Re(roots[rrsFlag])
        pos_roots_cand <- current_real_roots[current_real_roots >= 1.0]
        pos_root <- if (length(pos_roots_cand) > 0) min(pos_roots_cand) else NA_real_
        
        neg_roots_cand <- current_real_roots[current_real_roots < 1.0 & current_real_roots > tol]
        neg_root <- if (length(neg_roots_cand) > 0) max(neg_roots_cand) else NA_real_
        
        # Check with rootsup
        if (sum(rruFlag) > 0) {
          real_rootsup_val <- Re(rootsup[rruFlag])
          pos_rootsup_cand <- real_rootsup_val[real_rootsup_val >= 1.0]
          pos_rootsup <- if (length(pos_rootsup_cand) > 0) min(pos_rootsup_cand) else NA_real_
          
          if (!is.na(pos_root) && !is.na(pos_rootsup) && pos_root > pos_rootsup) {
            pos_root <- NA_real_ # Inconsistent
          }
        }
        
        # Check with rootsdn
        if (sum(rrdFlag) > 0) {
          real_rootsdn_val <- Re(rootsdn[rrdFlag])
          neg_rootsdn_cand <- real_rootsdn_val[real_rootsdn_val < 1.0 & real_rootsdn_val > tol]
          neg_rootsdn <- if (length(neg_rootsdn_cand) > 0) max(neg_rootsdn_cand) else NA_real_
          
          if (!is.na(neg_root) && !is.na(neg_rootsdn) && neg_root < neg_rootsdn) {
            neg_root <- NA_real_ # Inconsistent
          }
        }
        
        if (returned >= invested) {
          properroot <- pos_root
        } else {
          properroot <- neg_root
        }
      }
    }
  }
  
  # Ensure a single value is returned, or NA
  if (length(properroot) != 1) {
    return(NA_real_)
  }
  if (is.complex(properroot)) { # Should not happen if logic is correct
    return(NA_real_)
  }
  
  return(as.numeric(properroot))
}