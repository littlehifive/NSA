// This software was prepared for the Institute of Education Sciences (IES) under Contract 91990020F0052 by Mathematica.
// 
// It accompanies the report (https://ies.ed.gov/ncee/pubs/2022005/):
// Deke, J., Finucane, M. & Thal, D. (2022). The BASIE (BAyeSian Interpretation of Estimates) framework for interpreting findings from impact evaluations: A practical guide for education researchers. (NCEE 2022-005). Washington, DC: U.S. Department of Education, Institute of Education Sciences, National Center for Education Evaluation and Regional Assistance. Retrieved from http://ies.ed.gov/ncee.
//  
// THIS SOFTWARE IS PROVIDED ON AN “AS IS” BASIS WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, AND NON-INFRINGEMENT.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE SOFTWARE IS WITH YOU.  SHOULD THE SOFTWARE PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING OR CORRECTION.
// IN NO EVENT WILL MATHEMATICA, IT AGENTS, SUBCONTRACTORS OR EMPLOYEES, OR ANY OTHER AGENT WHO MAY REDISTRIBUTE OR MODIFY THIS SOFTWARE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, CONSEQUENTIAL, INCIDENTAL, OR INDIRECT DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE SOFTWARE (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE), IN EACH CASE REGARDLESS OF WHETHER MATHEMATICA WAS ADVISED OF THE POSSIBILITY OF SUCH LOSSES OR DAMAGES OR SUCH LOSSES OR DAMAGES WERE OTHERWISE FORESEEABLE.


functions {
	// taken from https://discourse.mc-stan.org/t/skewed-distribution/1383/19
	// This function already incorporates the v/m adjustments, so sd = sigma and mean=mu, approximately. Doesn't work so great at low q
	real sgt_log(vector x, real mu, real s, real l, real p, real q) {
		// Skewed generalised t
		int N;
		real lz1;
		real lz2;
		real v;
		real m;
		real r;
		real out;
		N = dims(x)[1];
		lz1 = lbeta(1.0/p,q);
		lz2 = lbeta(2.0/p,q-1.0/p);
		v = q^(-1.0/p)*((3*l^2+1)*exp(lbeta(3.0/p,q-2.0/p)-lz1)-4*l^2*exp(lz2-lz1)^2)^(-0.5);
		m = 2*v*s*l*q^(1.0/p)*exp(lz2-lz1);
		out = 0;
		for (n in 1:N) {
			r = x[n]-mu+m;
			if (r<0)
				out = out+log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(-1)+1)^p)+1)^(1.0/p+q));
			else
				out = out+log(p)-log(2*v*s*q^(1.0/p)*exp(lz1)*(fabs(r)^p /(q*(v*s)^p*(l*(1)+1)^p)+1)^(1.0/p+q));
		}
		return out;
	}
	// Skew t is just sgt with p fixed at 2
	real skew_t_log(vector x, real mu, real s, real l, real q) {
		// Skewed generalised t
		int N;
		real lz1;
		real lz2;
		real v;
		real m;
		real r;
		real out;
		real l2;
		real simp1;
		real simp2;
		N = dims(x)[1];
		lz1 = lbeta(0.5,q);
		lz2 = lbeta(1.0,q-0.5);
		v = q^-.5*((3*l^2+1)*exp(lbeta(1.5,q-1)-lz1)-4*l^2*exp(lz2-lz1)^2)^(-0.5);
		m = 2*v*s*l*sqrt(q)*exp(lz2-lz1);
		out = 0;
		
		// Some of the pieces in the loop can be pulled back out
		l2 = log(2);
		simp1 = 2*v*s*sqrt(q)*exp(lz1);
		simp2 = q*(v*s)^2;
		for (n in 1:N) {
			r = x[n]-mu+m;
			// With p=2, abs(r)^p == r^p so no need for abs 
			if (r<0)
				out = out+l2-log(simp1*(r^2 /(simp2*(l*(-1)+1)^2)+1)^(0.5+q));
			else
				out = out+l2-log(simp1*(r^2 /(simp2*(l*(1)+1)^2)+1)^(0.5+q));
		}
		return out;
	}
}

data {
  real impact_est;               // estimated treatment effects 
  real<lower=0> se_est;  // s.e. of effect estimates 
  real mu; //user-specified prior mean of true effect distribution
  real sigma; //user-specified prior std dev
  real lambda; //user-specified prior skew
  real q; //user-specified prior taily
  real p; //user-specified prior plateau
  real df; 
}
parameters {
  vector[1] impact_true;	
}
model {
  if (p==2) impact_true ~ skew_t(mu, sigma, lambda, q);
  else impact_true ~ sgt(mu, sigma, lambda,p, q);
  impact_est - impact_true ~ student_t(df,0,se_est);
  
}
