#include "args.hpp"

#include <getopt.h>
#include <cstdlib>
#include <iostream>

Args::Args() : extractText(false), outputFile() {}

namespace {
    char const* const optstring = "ht";

    const struct option longopts[] = {
        { "text-output", required_argument, NULL, 'o'},
        {}
    };

    [[noreturn]] void usage() {
        std::cerr << "Usage: parser-test [-h] [-t] [--output-file=OUTPUTFILE] INPUTFILE" << std::endl;
        exit(EXIT_FAILURE);
    }
}

Args parse_args(int &argc, char ** &argv)
{
    Args args;

    int ch;
    while ((ch = getopt_long(argc, argv, optstring, longopts, NULL)) != -1) {
        switch (ch) {
        case 'o': args.outputFile = optarg; break;
        case 't': args.extractText = true; break;
        case 'h': usage();
        default: usage();
        }
    }

    argc -= optind;
    argv += optind;

    if (argc != 1) usage();

    args.inputFile = argv[0];

    return args;
}
