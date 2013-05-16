#include "LLPack/utils/extio.hpp"
#include "LLPack/utils/pathname.hpp"
#include "LLPack/algorithms/sort.hpp"
#include "plsa.hpp"
#include <vector>
#include <string>

int main( int argc, char ** argv )
{
  if ( argc < 2 ) {
    Error( "Please specify the archive path" );
    exit( -1 );
  }
  
  // read dictionary
  std::vector<std::string> dict = std::move( readlines( strf( "%s/dict.txt", argv[1] ) ) );
  Done( "dictionary loaded." );
  Info( "%lu words.", dict.size() );

  // read X
  std::vector<std::string> filenames = std::move( readlines( strf( "%s/list.txt", argv[1] ) ) );
  filenames = std::move( path::FFFL( std::string( argv[1] ), filenames ) );
  std::vector<int> points;
  std::vector<std::vector<std::pair< int, double > > > X;
  for ( auto& ele : filenames ) {
    int tmp = 0;
    int cnt = 0;
    WITH_OPEN( in, ele.c_str(), "r" );
    fscanf( in, "%d", &tmp );
    points.push_back( tmp );
    X.emplace_back();
    while ( fscanf( in, "%d %d", &tmp, &cnt ) != EOF ) {
      X.back().emplace_back( tmp, cnt * points.back() );
    }
    END_WITH( in );
  }
  Done( "archive loaded." );
  Info( "%lu documents.", X.size() );

  // trainning
  int K = 10;
  int W = static_cast<int>( dict.size() );
  plsa::Options options;
  options.maxIter = 120;
  std::vector<double> pw_z;
  std::vector<double> pd_z;
  std::vector<double> pz;
  plsa::train( X, dict, K, pw_z, pd_z, pz, options );

  // points statistics
  // std::vector<double> scores( K, 0.0 );
  // for ( int i=0; i<

  
  // show result

  for ( int k=0; k<K; k++ ) {
    printf( "--------------------------------------------------\n" );
    printf( "topic %d: (%.4lf)\n", k, pz[k] );
    std::vector<double> pw_zk( dict.size() );
    for ( int j=0; j<W; j++ ) {
      pw_zk[j] = pw_z[k*W+j];
    }
    std::vector<int> ordered = std::move( sorting::index_sort( pw_zk ) );
    for ( int j=0; j<20; j++ ) {
      printf( "%1.5lf   %s\n", pw_zk[ordered[j]], dict[ordered[j]].c_str() );
    }
  }
  return 0;
}
