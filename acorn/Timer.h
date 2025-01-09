#ifndef TIMER_H
#define TIMER_H

#include <chrono>

namespace acorn {
    class Timer {
        using tp = std::chrono::steady_clock::time_point;
    public:

        void start();

        void stop();

        double took_ms() const {
            return total_time / 1e6;
        }

    private:
        tp start_time;
        long long total_time = 0;
    };
}

#endif // TIMER_H
