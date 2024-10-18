#include <cmath>
#ifndef __numerics_mod
#define __numerics_mod

namespace numerics{


  inline int finagle_widgets(double A, int B){

    // Do mock work - here busy sleep for A seconds, B times
    int slp = floor(A*1000.0);
    for(int i=0; i<B; i++){
      sleeper::busy_sleep(slp);
      std::cout<<"CALCULATING "<<std::string(i, '.')<<std::endl;//Make sure to flush
    }
    return 0;
  }

  inline void reticulate_splines(double A, int B){

    // As above, mock work
    int slp = floor(A*1000.0);
    for(int i=0; i<B; i++){
      sleeper::busy_sleep(slp);
      std::cout<<"Processing..."<<std::endl;//Make sure to flush
    }
  }

  inline void repair_fabric_of_reality(double A){

    sleeper::busy_sleep(floor(A*1000.0));
  }

  inline void square_the_circle(double A, int B){

    // As above, mock work
    for(int i=0; i<B; i++){
      repair_fabric_of_reality(A/B/2.0);
      sleeper::busy_sleep(floor(A/B/2.0*1000.0));
    }
  }

};
#endif
