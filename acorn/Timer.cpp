#include "Timer.h"

void acorn::Timer::start() {
    start_time = std::chrono::steady_clock::now();
}

void acorn::Timer::stop() {
    tp end_time = std::chrono::steady_clock::now();
    auto elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(end_time - start_time);
    total_time += elapsed.count();
}
