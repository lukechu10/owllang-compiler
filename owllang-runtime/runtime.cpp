#include <iostream>

extern "C" {
    long long println(long long val) {
        std::cout << val << std::endl;
        return 0;
    }
}