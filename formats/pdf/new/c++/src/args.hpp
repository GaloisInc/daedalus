#pragma once

#include <string>

struct Args {
    bool extractText;
    std::string outputFile;
    std::string inputFile;

    Args();
};

Args parse_args(int &argc, char ** &argv);
