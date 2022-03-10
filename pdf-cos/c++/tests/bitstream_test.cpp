#include "lzw.hpp"
#include "ascii85.hpp"

#include <vector>
#include <iostream>

namespace {
    char sample[] = "8;Z]\">n4ap%\"ip,$g'M)b9dGhEP\\O&2Et/U+o\"\\AD//dV3a)Qj2^$4boWmQGH(,$T &n9b;g\"mOk=c9`*SPjk]kGKk'WTLk;3ON_fOHmo>`J1d1AEV%2b.5NW(@2/_'HF5# _)BVR;o*tk%)Kh3;@\"4]:LJCaGVF^&l:Sg'P+Aj0%QX]RR?-/CmlqZ&ZT4)hi:nV: `#a6;>TU=\\odY1qqc(CLdsKA1ZFIT#m:\"D%)$R(P%-kYO@7(E:YV53'Tk#0V-t48V qSeiC<6>YZduDg@N<O&kl2LNG4U(n%cHkD~>";
}

int main()
{
    std::vector<uint8_t> buffer;
    ASCII85Decode(sample, std::size(sample), buffer);

    for (auto &&x : buffer) { 
        std::cout << std::hex << int(x) << " ";
    }
/*
    BitStream bs{buffer.data(), buffer.size()};
    try {
    std::cout << decompress(bs) << std::endl;
    } catch (LzwException const& e) {
        std::cerr << "Failed with " << e.what() << std::endl;
    }

*/
    uint8_t abexample[] = {0x80, 0x0b, 0x60, 0x50, 0x22, 0x0c, 0x0c, 0x85, 0x01};
    std::cout << decompress(abexample, std::size(abexample)) << std::endl;
}
