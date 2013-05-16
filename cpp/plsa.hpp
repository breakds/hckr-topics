#include <vector>
#include <random>

namespace plsa
{
  
  namespace
  {
    inline void normalize( double *x, int len )
    {
      double s = 0.0;
      for ( int i=0; i<len; i++) s += x[i];
      for ( int i=0; i<len; i++) x[i] /= s;
    }

    inline void normalize_at0( std::vector<double> &x, int k )
    {
      int n = static_cast<int>( x.size() );
      for ( int i=0; i<n; i+=k ) normalize( &x[i], k );
    }

    inline void fullrand( std::vector<double> &x, int n, std::mt19937 &rng )
    {
      static std::uniform_real_distribution<double> udist( 0.0, 1.0 );
      x.resize( n );
      for ( auto& ele : x ) {
        ele = udist( rng );
      }
    }


    inline void EStep( int N,
                       int W,
                       int K,
                       const std::vector<double> &pw_z, 
                       const std::vector<double> &pd_z,
                       const std::vector<double> &pz,
                       std::vector<double> &pz_dw )
    {
      for( int i=0; i<N; i++ ) {
        int base = i * W * K;
        for ( int j=0; j<W; j++ ) {
          for ( int k=0; k<K; k++ ) {
            pz_dw[base++] = pw_z[k*W+j] * pd_z[k*N+i] * pz[k];
          }
        }
      }
      normalize_at0( pz_dw, K );
    }

    inline void MStep( int N,
                       int W,
                       int K,
                       const std::vector<std::vector<std::pair<int,double > > > &X,
                       std::vector<double> &pw_z, 
                       std::vector<double> &pd_z,
                       std::vector<double> &pz,
                       const std::vector<double> &pz_dw )
    {
      std::fill( pw_z.begin(), pw_z.end(), 0.0 );
      std::fill( pd_z.begin(), pd_z.end(), 0.0 );
      std::fill( pz.begin(), pz.end(), 0.0 );
      for ( int i=0; i<N; i++ ) {
        for ( auto& ele : X[i] ) {
          int j = ele.first;
          for ( int k=0; k<K; k++ ) {
            double product = ele.second * pz_dw[( i * W + j ) * K + k];
            pw_z[k*W+j] += product;
            pd_z[k*N+i] += product;
          }
        }
      }
      normalize_at0( pw_z, W );

      for ( int k=0; k<K; k++ ) {
        for ( int i=0; i<N; i++ ) {
          pz[k] += pd_z[k * N + i];
        }
      }
      normalize( &pz[0], K );

      normalize_at0( pd_z, N );
    }

    inline double calc_energy(  int N,
                                int W,
                                int K,
                                const std::vector<std::vector<std::pair<int,double > > > &X,
                                const std::vector<double> &pw_z, 
                                const std::vector<double> &pd_z,
                                const std::vector<double> &pz )
    {
      double energy = 0.0;
      for ( int i=0; i<N; i++ ) {
        for ( auto& ele : X[i] ) {
          int j = ele.first;
          for ( int k=0; k<K; k++ ) {
            energy += ele.second * pz[k] * pw_z[k * W + j] * pd_z[ k * N + i ];
          }
        }
      }
      return energy;
    }
  }

  struct Options
  {
    int maxIter;
    Options() : maxIter(10) {}
  };

  void train( const std::vector<std::vector<std::pair<int,double> > >& X,
              const std::vector<std::string> &dict,
              int K,
              std::vector<double> &pw_z, std::vector<double> &pd_z, std::vector<double> &pz,
              Options options )
  {
    std::mt19937 rng;
    
    int N = static_cast<int>( X.size() );
    int W = static_cast<int>( dict.size() );
    
    // init pz;
    fullrand( pz, K, rng );
    normalize( &pz[0] , K);

    // init pw _z
    fullrand( pw_z, K * W, rng );
    normalize_at0( pw_z, W );

    // init pd_z
    fullrand( pd_z, K * N, rng );
    normalize_at0( pd_z, N );
    
    // init pz_dw
    std::vector<double> pz_dw( N * W * K, 0.0 );

    for ( int iter=0; iter<options.maxIter; iter++ ) {
      EStep( N, W, K, pw_z, pd_z, pz, pz_dw );
      MStep( N, W, K, X, pw_z, pd_z, pz, pz_dw );
      printf( "iter %d: ernergy = %.6lf\n", iter, calc_energy( N, W, K, 
                                                               X,
                                                               pw_z,
                                                               pd_z,
                                                               pz ) );
    }
  }
              
}



