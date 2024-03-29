// Generated by rstantools.  Do not edit by hand.

/*
    bmnr is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    bmnr is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with bmnr.  If not, see <http://www.gnu.org/licenses/>.
*/
#ifndef MODELS_HPP
#define MODELS_HPP
#define STAN__SERVICES__COMMAND_HPP
#ifndef USE_STANC3
#define USE_STANC3
#endif
#include <rstan/rstaninc.hpp>
// Code generated by stanc v2.26.1-4-gd72b68b7-dirty
#include <stan/model/model_header.hpp>
namespace model_interface_gp_matern32_cov_ard_namespace {
inline void validate_positive_index(const char* var_name, const char* expr,
                                    int val) {
  if (val < 1) {
    std::stringstream msg;
    msg << "Found dimension size less than one in simplex declaration"
        << "; variable=" << var_name << "; dimension size expression=" << expr
        << "; expression value=" << val;
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
inline void validate_unit_vector_index(const char* var_name, const char* expr,
                                       int val) {
  if (val <= 1) {
    std::stringstream msg;
    if (val == 1) {
      msg << "Found dimension size one in unit vector declaration."
          << " One-dimensional unit vector is discrete"
          << " but the target distribution must be continuous."
          << " variable=" << var_name << "; dimension size expression=" << expr;
    } else {
      msg << "Found dimension size less than one in unit vector declaration"
          << "; variable=" << var_name << "; dimension size expression=" << expr
          << "; expression value=" << val;
    }
    std::string msg_str(msg.str());
    throw std::invalid_argument(msg_str.c_str());
  }
}
using std::istream;
using std::string;
using std::stringstream;
using std::vector;
using std::pow;
using stan::io::dump;
using stan::math::lgamma;
using stan::model::model_base_crtp;
using stan::model::rvalue;
using stan::model::cons_list;
using stan::model::index_uni;
using stan::model::index_max;
using stan::model::index_min;
using stan::model::index_min_max;
using stan::model::index_multi;
using stan::model::index_omni;
using stan::model::nil_index_list;
using namespace stan::math;
using stan::math::pow; 
stan::math::profile_map profiles__;
static int current_statement__= 0;
static const std::vector<string> locations_array__ = {" (found before start of program)",
                                                      " (in 'string', line 74, column 2 to column 35)",
                                                      " (in 'string', line 78, column 4 to column 69)",
                                                      " (in 'string', line 77, column 9 to line 79, column 3)",
                                                      " (in 'string', line 76, column 4 to column 56)",
                                                      " (in 'string', line 75, column 16 to line 77, column 3)",
                                                      " (in 'string', line 75, column 2 to line 79, column 3)",
                                                      " (in 'string', line 64, column 2 to column 24)",
                                                      " (in 'string', line 65, column 2 to column 19)",
                                                      " (in 'string', line 66, column 2 to column 19)",
                                                      " (in 'string', line 67, column 23 to column 26)",
                                                      " (in 'string', line 67, column 9 to column 17)",
                                                      " (in 'string', line 67, column 2 to column 28)",
                                                      " (in 'string', line 68, column 23 to column 26)",
                                                      " (in 'string', line 68, column 9 to column 17)",
                                                      " (in 'string', line 68, column 2 to column 28)",
                                                      " (in 'string', line 69, column 2 to column 16)",
                                                      " (in 'string', line 70, column 9 to column 17)",
                                                      " (in 'string', line 70, column 2 to column 29)",
                                                      " (in 'string', line 74, column 9 to column 12)",
                                                      " (in 'string', line 74, column 14 to column 29)",
                                                      " (in 'string', line 13, column 4 to column 24)",
                                                      " (in 'string', line 14, column 4 to column 24)",
                                                      " (in 'string', line 15, column 11 to column 14)",
                                                      " (in 'string', line 15, column 16 to column 19)",
                                                      " (in 'string', line 15, column 4 to column 25)",
                                                      " (in 'string', line 17, column 4 to column 40)",
                                                      " (in 'string', line 18, column 4 to column 14)",
                                                      " (in 'string', line 22, column 8 to column 64)",
                                                      " (in 'string', line 23, column 8 to line 24, column 72)",
                                                      " (in 'string', line 21, column 24 to line 25, column 7)",
                                                      " (in 'string', line 21, column 6 to line 25, column 7)",
                                                      " (in 'string', line 20, column 22 to line 26, column 5)",
                                                      " (in 'string', line 20, column 4 to line 26, column 5)",
                                                      " (in 'string', line 28, column 4 to column 15)",
                                                      " (in 'string', line 12, column 96 to line 29, column 1)",
                                                      " (in 'string', line 42, column 4 to column 22)",
                                                      " (in 'string', line 43, column 11 to column 14)",
                                                      " (in 'string', line 43, column 16 to column 19)",
                                                      " (in 'string', line 43, column 4 to column 25)",
                                                      " (in 'string', line 45, column 4 to column 40)",
                                                      " (in 'string', line 46, column 4 to column 14)",
                                                      " (in 'string', line 49, column 6 to column 30)",
                                                      " (in 'string', line 52, column 8 to column 58)",
                                                      " (in 'string', line 53, column 8 to column 82)",
                                                      " (in 'string', line 54, column 8 to column 30)",
                                                      " (in 'string', line 51, column 29 to line 55, column 7)",
                                                      " (in 'string', line 51, column 6 to line 55, column 7)",
                                                      " (in 'string', line 48, column 27 to line 56, column 5)",
                                                      " (in 'string', line 48, column 4 to line 56, column 5)",
                                                      " (in 'string', line 58, column 4 to column 32)",
                                                      " (in 'string', line 60, column 4 to column 15)",
                                                      " (in 'string', line 41, column 72 to line 61, column 1)"};
template <typename T0__, typename T1__, typename T2__, typename T3__>
Eigen::Matrix<stan::promote_args_t<T0__, T1__, T2__,
stan::value_type_t<T3__>>, -1, -1>
gp_matern32_cov_ard_general(const std::vector<Eigen::Matrix<T0__, -1, 1>>& x_r,
                            const std::vector<Eigen::Matrix<T1__, -1, 1>>& x_c,
                            const T2__& gp_scale,
                            const T3__& gp_length_arg__,
                            std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__,
          T1__,
          T2__,
          stan::value_type_t<T3__>>;
  const auto& gp_length = to_ref(gp_length_arg__);
  const static bool propto__ = true;
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  (void) DUMMY_VAR__;  // suppress unused var warning
  
  try {
    int n_r;
    n_r = std::numeric_limits<int>::min();
    
    current_statement__ = 21;
    n_r = stan::math::size(x_r);
    int n_c;
    n_c = std::numeric_limits<int>::min();
    
    current_statement__ = 22;
    n_c = stan::math::size(x_c);
    current_statement__ = 23;
    validate_non_negative_index("out", "n_r", n_r);
    current_statement__ = 24;
    validate_non_negative_index("out", "n_c", n_c);
    Eigen::Matrix<local_scalar_t__, -1, -1> out;
    out = Eigen::Matrix<local_scalar_t__, -1, -1>(n_r, n_c);
    stan::math::fill(out, DUMMY_VAR__);
    
    local_scalar_t__ gp_scale_sq;
    gp_scale_sq = DUMMY_VAR__;
    
    current_statement__ = 26;
    gp_scale_sq = pow(gp_scale, 2);
    local_scalar_t__ dist;
    dist = DUMMY_VAR__;
    
    current_statement__ = 33;
    for (int ir = 1; ir <= n_r; ++ir) {
      current_statement__ = 31;
      for (int ic = 1; ic <= n_c; ++ic) {
        current_statement__ = 28;
        dist = stan::math::sqrt(
                 dot_self(
                   elt_divide(subtract(x_r[(ir - 1)], x_c[(ic - 1)]),
                     gp_length)));
        current_statement__ = 29;
        assign(out,
          cons_list(index_uni(ir),
            cons_list(index_uni(ic), nil_index_list())),
          ((gp_scale_sq * (1 + (stan::math::sqrt(3) * dist))) *
            stan::math::exp(((-1 * stan::math::sqrt(3)) * dist))),
          "assigning variable out");}}
    current_statement__ = 34;
    return out;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
  }
  
}
struct gp_matern32_cov_ard_general_functor__ {
template <typename T0__, typename T1__, typename T2__, typename T3__>
Eigen::Matrix<stan::promote_args_t<T0__, T1__, T2__,
stan::value_type_t<T3__>>, -1, -1>
operator()(const std::vector<Eigen::Matrix<T0__, -1, 1>>& x_r,
           const std::vector<Eigen::Matrix<T1__, -1, 1>>& x_c,
           const T2__& gp_scale, const T3__& gp_length,
           std::ostream* pstream__)  const 
{
return gp_matern32_cov_ard_general(x_r, x_c, gp_scale, gp_length, pstream__);
}
};
template <typename T0__, typename T1__, typename T2__>
Eigen::Matrix<stan::promote_args_t<T0__, T1__,
stan::value_type_t<T2__>>, -1, -1>
gp_matern32_cov_ard(const std::vector<Eigen::Matrix<T0__, -1, 1>>& x,
                    const T1__& gp_scale, const T2__& gp_length_arg__,
                    std::ostream* pstream__) {
  using local_scalar_t__ = stan::promote_args_t<T0__,
          T1__,
          stan::value_type_t<T2__>>;
  const auto& gp_length = to_ref(gp_length_arg__);
  const static bool propto__ = true;
  (void) propto__;
  local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
  (void) DUMMY_VAR__;  // suppress unused var warning
  
  try {
    int n_r;
    n_r = std::numeric_limits<int>::min();
    
    current_statement__ = 36;
    n_r = stan::math::size(x);
    current_statement__ = 37;
    validate_non_negative_index("out", "n_r", n_r);
    current_statement__ = 38;
    validate_non_negative_index("out", "n_r", n_r);
    Eigen::Matrix<local_scalar_t__, -1, -1> out;
    out = Eigen::Matrix<local_scalar_t__, -1, -1>(n_r, n_r);
    stan::math::fill(out, DUMMY_VAR__);
    
    local_scalar_t__ gp_scale_sq;
    gp_scale_sq = DUMMY_VAR__;
    
    current_statement__ = 40;
    gp_scale_sq = pow(gp_scale, 2);
    local_scalar_t__ dist;
    dist = DUMMY_VAR__;
    
    current_statement__ = 49;
    for (int i = 1; i <= (n_r - 1); ++i) {
      current_statement__ = 42;
      assign(out,
        cons_list(index_uni(i), cons_list(index_uni(i), nil_index_list())),
        gp_scale_sq, "assigning variable out");
      current_statement__ = 47;
      for (int j = (i + 1); j <= n_r; ++j) {
        current_statement__ = 43;
        dist = stan::math::sqrt(
                 dot_self(
                   elt_divide(subtract(x[(i - 1)], x[(j - 1)]), gp_length)));
        current_statement__ = 44;
        assign(out,
          cons_list(index_uni(i), cons_list(index_uni(j), nil_index_list())),
          ((gp_scale_sq * (1 + (stan::math::sqrt(3) * dist))) *
            stan::math::exp(((-1 * stan::math::sqrt(3)) * dist))),
          "assigning variable out");
        current_statement__ = 45;
        assign(out,
          cons_list(index_uni(j), cons_list(index_uni(i), nil_index_list())),
          rvalue(out,
            cons_list(index_uni(i),
              cons_list(index_uni(j), nil_index_list())), "out"),
          "assigning variable out");}}
    current_statement__ = 50;
    assign(out,
      cons_list(index_uni(n_r), cons_list(index_uni(n_r), nil_index_list())),
      gp_scale_sq, "assigning variable out");
    current_statement__ = 51;
    return out;
  } catch (const std::exception& e) {
    stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
  }
  
}
struct gp_matern32_cov_ard_functor__ {
template <typename T0__, typename T1__, typename T2__>
Eigen::Matrix<stan::promote_args_t<T0__, T1__,
stan::value_type_t<T2__>>, -1, -1>
operator()(const std::vector<Eigen::Matrix<T0__, -1, 1>>& x,
           const T1__& gp_scale, const T2__& gp_length,
           std::ostream* pstream__)  const 
{
return gp_matern32_cov_ard(x, gp_scale, gp_length, pstream__);
}
};
#include <stan_meta_header.hpp>
class model_interface_gp_matern32_cov_ard final : public model_base_crtp<model_interface_gp_matern32_cov_ard> {
private:
  int n_inputs;
  int n_r;
  int n_c;
  std::vector<Eigen::Matrix<double, -1, 1>> x_r;
  std::vector<Eigen::Matrix<double, -1, 1>> x_c;
  double gp_scale;
  Eigen::Matrix<double, -1, 1> gp_length;
  int out_2dim__;
 
public:
  ~model_interface_gp_matern32_cov_ard() { }
  
  inline std::string model_name() const final { return "model_interface_gp_matern32_cov_ard"; }
  inline std::vector<std::string> model_compile_info() const noexcept {
    return std::vector<std::string>{"stanc_version = stanc3 v2.26.1-4-gd72b68b7-dirty", "stancflags = "};
  }
  
  
  model_interface_gp_matern32_cov_ard(stan::io::var_context& context__,
                                      unsigned int random_seed__ = 0,
                                      std::ostream* pstream__ = nullptr) : model_base_crtp(0) {
    using local_scalar_t__ = double ;
    boost::ecuyer1988 base_rng__ = 
        stan::services::util::create_rng(random_seed__, 0);
    (void) base_rng__;  // suppress unused var warning
    static const char* function__ = "model_interface_gp_matern32_cov_ard_namespace::model_interface_gp_matern32_cov_ard";
    (void) function__;  // suppress unused var warning
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
      current_statement__ = 7;
      context__.validate_dims("data initialization","n_inputs","int",
          context__.to_vec());
      n_inputs = std::numeric_limits<int>::min();
      
      current_statement__ = 7;
      n_inputs = context__.vals_i("n_inputs")[(1 - 1)];
      current_statement__ = 7;
      current_statement__ = 7;
      check_greater_or_equal(function__, "n_inputs", n_inputs, 1);
      current_statement__ = 8;
      context__.validate_dims("data initialization","n_r","int",
          context__.to_vec());
      n_r = std::numeric_limits<int>::min();
      
      current_statement__ = 8;
      n_r = context__.vals_i("n_r")[(1 - 1)];
      current_statement__ = 8;
      current_statement__ = 8;
      check_greater_or_equal(function__, "n_r", n_r, 1);
      current_statement__ = 9;
      context__.validate_dims("data initialization","n_c","int",
          context__.to_vec());
      n_c = std::numeric_limits<int>::min();
      
      current_statement__ = 9;
      n_c = context__.vals_i("n_c")[(1 - 1)];
      current_statement__ = 9;
      current_statement__ = 9;
      check_greater_or_equal(function__, "n_c", n_c, 0);
      current_statement__ = 10;
      validate_non_negative_index("x_r", "n_r", n_r);
      current_statement__ = 11;
      validate_non_negative_index("x_r", "n_inputs", n_inputs);
      current_statement__ = 12;
      context__.validate_dims("data initialization","x_r","double",
          context__.to_vec(n_r, n_inputs));
      x_r = std::vector<Eigen::Matrix<double, -1, 1>>(n_r, Eigen::Matrix<double, -1, 1>(n_inputs));
      stan::math::fill(x_r, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> x_r_flat__;
        current_statement__ = 12;
        assign(x_r_flat__, nil_index_list(), context__.vals_r("x_r"),
          "assigning variable x_r_flat__");
        current_statement__ = 12;
        pos__ = 1;
        current_statement__ = 12;
        for (int sym1__ = 1; sym1__ <= n_inputs; ++sym1__) {
          current_statement__ = 12;
          for (int sym2__ = 1; sym2__ <= n_r; ++sym2__) {
            current_statement__ = 12;
            assign(x_r,
              cons_list(index_uni(sym2__),
                cons_list(index_uni(sym1__), nil_index_list())),
              x_r_flat__[(pos__ - 1)], "assigning variable x_r");
            current_statement__ = 12;
            pos__ = (pos__ + 1);}}
      }
      current_statement__ = 13;
      validate_non_negative_index("x_c", "n_c", n_c);
      current_statement__ = 14;
      validate_non_negative_index("x_c", "n_inputs", n_inputs);
      current_statement__ = 15;
      context__.validate_dims("data initialization","x_c","double",
          context__.to_vec(n_c, n_inputs));
      x_c = std::vector<Eigen::Matrix<double, -1, 1>>(n_c, Eigen::Matrix<double, -1, 1>(n_inputs));
      stan::math::fill(x_c, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> x_c_flat__;
        current_statement__ = 15;
        assign(x_c_flat__, nil_index_list(), context__.vals_r("x_c"),
          "assigning variable x_c_flat__");
        current_statement__ = 15;
        pos__ = 1;
        current_statement__ = 15;
        for (int sym1__ = 1; sym1__ <= n_inputs; ++sym1__) {
          current_statement__ = 15;
          for (int sym2__ = 1; sym2__ <= n_c; ++sym2__) {
            current_statement__ = 15;
            assign(x_c,
              cons_list(index_uni(sym2__),
                cons_list(index_uni(sym1__), nil_index_list())),
              x_c_flat__[(pos__ - 1)], "assigning variable x_c");
            current_statement__ = 15;
            pos__ = (pos__ + 1);}}
      }
      current_statement__ = 16;
      context__.validate_dims("data initialization","gp_scale","double",
          context__.to_vec());
      gp_scale = std::numeric_limits<double>::quiet_NaN();
      
      current_statement__ = 16;
      gp_scale = context__.vals_r("gp_scale")[(1 - 1)];
      current_statement__ = 17;
      validate_non_negative_index("gp_length", "n_inputs", n_inputs);
      current_statement__ = 18;
      context__.validate_dims("data initialization","gp_length","double",
          context__.to_vec(n_inputs));
      gp_length = Eigen::Matrix<double, -1, 1>(n_inputs);
      stan::math::fill(gp_length, std::numeric_limits<double>::quiet_NaN());
      
      {
        std::vector<local_scalar_t__> gp_length_flat__;
        current_statement__ = 18;
        assign(gp_length_flat__, nil_index_list(),
          context__.vals_r("gp_length"),
          "assigning variable gp_length_flat__");
        current_statement__ = 18;
        pos__ = 1;
        current_statement__ = 18;
        for (int sym1__ = 1; sym1__ <= n_inputs; ++sym1__) {
          current_statement__ = 18;
          assign(gp_length, cons_list(index_uni(sym1__), nil_index_list()),
            gp_length_flat__[(pos__ - 1)], "assigning variable gp_length");
          current_statement__ = 18;
          pos__ = (pos__ + 1);}
      }
      current_statement__ = 19;
      validate_non_negative_index("out", "n_r", n_r);
      current_statement__ = 20;
      out_2dim__ = std::numeric_limits<int>::min();
      
      current_statement__ = 20;
      out_2dim__ = (n_c ? n_c : n_r);
      current_statement__ = 20;
      validate_non_negative_index("out", "n_c ? n_c : n_r", out_2dim__);
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    num_params_r__ = 0U;
    
    try {
      
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
  }
  template <bool propto__, bool jacobian__, typename VecR, typename VecI, stan::require_vector_like_t<VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline stan::scalar_type_t<VecR> log_prob_impl(VecR& params_r__,
                                                 VecI& params_i__,
                                                 std::ostream* pstream__ = nullptr) const {
    using T__ = stan::scalar_type_t<VecR>;
    using local_scalar_t__ = T__;
    T__ lp__(0.0);
    stan::math::accumulator<T__> lp_accum__;
    static const char* function__ = "model_interface_gp_matern32_cov_ard_namespace::log_prob";
(void) function__;  // suppress unused var warning
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    lp_accum__.add(lp__);
    return lp_accum__.sum();
    } // log_prob_impl() 
    
  template <typename RNG, typename VecR, typename VecI, typename VecVar, stan::require_vector_like_vt<std::is_floating_point, VecR>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr, stan::require_std_vector_vt<std::is_floating_point, VecVar>* = nullptr>
  inline void write_array_impl(RNG& base_rng__, VecR& params_r__,
                               VecI& params_i__, VecVar& vars__,
                               const bool emit_transformed_parameters__ = true,
                               const bool emit_generated_quantities__ = true,
                               std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.resize(0);
    stan::io::reader<local_scalar_t__> in__(params_r__, params_i__);
    static const char* function__ = "model_interface_gp_matern32_cov_ard_namespace::write_array";
(void) function__;  // suppress unused var warning
    (void) function__;  // suppress unused var warning
    double lp__ = 0.0;
    (void) lp__;  // dummy to suppress unused var warning
    stan::math::accumulator<double> lp_accum__;
    local_scalar_t__ DUMMY_VAR__(std::numeric_limits<double>::quiet_NaN());
    (void) DUMMY_VAR__;  // suppress unused var warning
    
    try {
      if (logical_negation((primitive_value(emit_transformed_parameters__) ||
            primitive_value(emit_generated_quantities__)))) {
        return ;
      } 
      if (logical_negation(emit_generated_quantities__)) {
        return ;
      } 
      Eigen::Matrix<double, -1, -1> out;
      out = Eigen::Matrix<double, -1, -1>(n_r, out_2dim__);
      stan::math::fill(out, std::numeric_limits<double>::quiet_NaN());
      
      current_statement__ = 6;
      if (logical_eq(n_c, 0)) {
        current_statement__ = 4;
        assign(out, nil_index_list(),
          gp_matern32_cov_ard(x_r, gp_scale, gp_length, pstream__),
          "assigning variable out");
      } else {
        current_statement__ = 2;
        assign(out, nil_index_list(),
          gp_matern32_cov_ard_general(x_r, x_c, gp_scale,
            gp_length, pstream__), "assigning variable out");
      }
      for (int sym1__ = 1; sym1__ <= out_2dim__; ++sym1__) {
        for (int sym2__ = 1; sym2__ <= n_r; ++sym2__) {
          vars__.emplace_back(
            rvalue(out,
              cons_list(index_uni(sym2__),
                cons_list(index_uni(sym1__), nil_index_list())), "out"));}}
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // write_array_impl() 
    
  template <typename VecVar, typename VecI, stan::require_std_vector_t<VecVar>* = nullptr, stan::require_vector_like_vt<std::is_integral, VecI>* = nullptr>
  inline void transform_inits_impl(const stan::io::var_context& context__,
                                   VecI& params_i__, VecVar& vars__,
                                   std::ostream* pstream__ = nullptr) const {
    using local_scalar_t__ = double;
    vars__.clear();
    vars__.reserve(num_params_r__);
    
    try {
      int pos__;
      pos__ = std::numeric_limits<int>::min();
      
      pos__ = 1;
    } catch (const std::exception& e) {
      stan::lang::rethrow_located(e, locations_array__[current_statement__]);
      // Next line prevents compiler griping about no return
      throw std::runtime_error("*** IF YOU SEE THIS, PLEASE REPORT A BUG ***"); 
    }
    } // transform_inits_impl() 
    
  inline void get_param_names(std::vector<std::string>& names__) const {
    
    names__.clear();
    names__.emplace_back("out");
    } // get_param_names() 
    
  inline void get_dims(std::vector<std::vector<size_t>>& dimss__) const {
    dimss__.clear();
    dimss__.emplace_back(std::vector<size_t>{static_cast<size_t>(n_r),
                                             static_cast<size_t>(out_2dim__)});
    
    } // get_dims() 
    
  inline void constrained_param_names(
                                      std::vector<std::string>& param_names__,
                                      bool emit_transformed_parameters__ = true,
                                      bool emit_generated_quantities__ = true) const
    final {
    
    
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= out_2dim__; ++sym1__) {
        {
          for (int sym2__ = 1; sym2__ <= n_r; ++sym2__) {
            {
              param_names__.emplace_back(std::string() + "out" + '.' + std::to_string(sym2__) + '.' + std::to_string(sym1__));
            }}
        }}
    }
    
    } // constrained_param_names() 
    
  inline void unconstrained_param_names(
                                        std::vector<std::string>& param_names__,
                                        bool emit_transformed_parameters__ = true,
                                        bool emit_generated_quantities__ = true) const
    final {
    
    
    if (emit_transformed_parameters__) {
      
    }
    
    if (emit_generated_quantities__) {
      for (int sym1__ = 1; sym1__ <= out_2dim__; ++sym1__) {
        {
          for (int sym2__ = 1; sym2__ <= n_r; ++sym2__) {
            {
              param_names__.emplace_back(std::string() + "out" + '.' + std::to_string(sym2__) + '.' + std::to_string(sym1__));
            }}
        }}
    }
    
    } // unconstrained_param_names() 
    
  inline std::string get_constrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"out\",\"type\":{\"name\":\"matrix\",\"rows\":" << n_r << ",\"cols\":" << out_2dim__ << "},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_constrained_sizedtypes() 
    
  inline std::string get_unconstrained_sizedtypes() const {
    stringstream s__;
    s__ << "[{\"name\":\"out\",\"type\":{\"name\":\"matrix\",\"rows\":" << n_r << ",\"cols\":" << out_2dim__ << "},\"block\":\"generated_quantities\"}]";
    return s__.str();
    } // get_unconstrained_sizedtypes() 
    
  
    // Begin method overload boilerplate
    template <typename RNG>
    inline void write_array(RNG& base_rng,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& params_r,
                            Eigen::Matrix<double,Eigen::Dynamic,1>& vars,
                            const bool emit_transformed_parameters = true,
                            const bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      std::vector<double> vars_vec(vars.size());
      std::vector<int> params_i;
      write_array_impl(base_rng, params_r, params_i, vars_vec,
          emit_transformed_parameters, emit_generated_quantities, pstream);
      vars.resize(vars_vec.size());
      for (int i = 0; i < vars.size(); ++i) {
        vars.coeffRef(i) = vars_vec[i];
      }
    }
    template <typename RNG>
    inline void write_array(RNG& base_rng, std::vector<double>& params_r,
                            std::vector<int>& params_i,
                            std::vector<double>& vars,
                            bool emit_transformed_parameters = true,
                            bool emit_generated_quantities = true,
                            std::ostream* pstream = nullptr) const {
      write_array_impl(base_rng, params_r, params_i, vars, emit_transformed_parameters, emit_generated_quantities, pstream);
    }
    template <bool propto__, bool jacobian__, typename T_>
    inline T_ log_prob(Eigen::Matrix<T_,Eigen::Dynamic,1>& params_r,
                       std::ostream* pstream = nullptr) const {
      Eigen::Matrix<int, -1, 1> params_i;
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
    template <bool propto__, bool jacobian__, typename T__>
    inline T__ log_prob(std::vector<T__>& params_r,
                        std::vector<int>& params_i,
                        std::ostream* pstream = nullptr) const {
      return log_prob_impl<propto__, jacobian__>(params_r, params_i, pstream);
    }
  
    inline void transform_inits(const stan::io::var_context& context,
                         Eigen::Matrix<double, Eigen::Dynamic, 1>& params_r,
                         std::ostream* pstream = nullptr) const final {
      std::vector<double> params_r_vec(params_r.size());
      std::vector<int> params_i;
      transform_inits_impl(context, params_i, params_r_vec, pstream);
      params_r.resize(params_r_vec.size());
      for (int i = 0; i < params_r.size(); ++i) {
        params_r.coeffRef(i) = params_r_vec[i];
      }
    }
    inline void transform_inits(const stan::io::var_context& context,
                                std::vector<int>& params_i,
                                std::vector<double>& vars,
                                std::ostream* pstream = nullptr) const final {
      transform_inits_impl(context, params_i, vars, pstream);
    }        
};
}
using stan_model = model_interface_gp_matern32_cov_ard_namespace::model_interface_gp_matern32_cov_ard;
#ifndef USING_R
// Boilerplate
stan::model::model_base& new_model(
        stan::io::var_context& data_context,
        unsigned int seed,
        std::ostream* msg_stream) {
  stan_model* m = new stan_model(data_context, seed, msg_stream);
  return *m;
}
stan::math::profile_map& get_stan_profile_data() {
  return model_interface_gp_matern32_cov_ard_namespace::profiles__;
}
#endif
#endif
