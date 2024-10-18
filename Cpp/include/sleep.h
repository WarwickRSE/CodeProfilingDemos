#include <chrono>
#include <fstream>

#ifndef __sleep_mod
#define __sleep_mod

namespace sleeper{

  using namespace std::literals::chrono_literals;
  // Busy sleeping - doing something to keep process alive
  inline void busy_sleep(int milliseconds){

    std::chrono::time_point<std::chrono::high_resolution_clock> start, end, curr;

    start = std::chrono::high_resolution_clock::now();// Start time

    //Calculate end time
    end = start + std::chrono::milliseconds(milliseconds);

    int poll = 1e2;//Set clock polling interval from number of dumb ops
    double tmp;
    for(int last = 0;;last++){
      // calling to chrono::now is NOT going to be optimised out!
      if(last > poll){
        curr = std::chrono::high_resolution_clock::now();
	last = 0;
      }
      if(curr >= end) break;
    }

  }
};
#endif
