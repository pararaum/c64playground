// cellmatch.cpp
//
// Reads a bicolor (black & white) PNG image, splits it into 8x8 pixel cells
// ("characters"), and reduces the image to a limited palette of unique
// character glyphs:
//
//   1. Cells are clustered using single-linkage clustering on Hamming
//      distance: starting at distance 0, the allowed distance is increased
//      one step at a time until the number of resulting clusters is less
//      than or equal to the requested target (default 256, e.g. a typical
//      8-bit character set).
//   2. Each cluster is collapsed to a single representative pattern, chosen
//      bit-by-bit via majority vote among the cluster's members (this
//      minimizes total visual distortion versus just picking one member).
//   3. Every cell in the image is replaced by its cluster's representative
//      pattern, and the result is written out as a new PNG.
//
// No symmetry / rotation / flip / inversion handling is performed: cells are
// compared exactly as they are, pixel position by pixel position.
//
// Image I/O is done via ImageMagick's Magick++ C++ API.
// Command-line parsing is done via CLI11 (header-only).
//
// Build:
//   g++ -O2 -std=c++17 cellmatch.cpp -o cellmatch \
//       -Iinclude $(pkg-config --cflags --libs Magick++)
//
// (Adjust -Iinclude to wherever the CLI/ header directory lives, or point it
//  at a single CLI11.hpp if you're using the single-header distribution
//  instead - just change the #include below to "CLI11.hpp".)
//
// Usage:
//   ./cellmatch -i image.png -o output.png [options]
//
// Run with --help to see all options.

#include <Magick++.h>
#include "CLI/CLI.hpp"

#include <cstdint>
#include <cstdio>
#include <string>
#include <vector>
#include <numeric>
#include <algorithm>

static const int CELL_W = 8;
static const int CELL_H = 8;
static const int BITS_PER_CELL = CELL_W * CELL_H; // 64

struct Cell {
    int row, col;        // position in the character grid
    uint64_t bits;        // 64 bits, 1 = black, 0 = white, row-major within cell
};

// Popcount wrapper (uses compiler builtin where available).
static inline int popcount64(uint64_t x) {
#if defined(__GNUC__) || defined(__clang__)
    return __builtin_popcountll(x);
#else
    int c = 0;
    while (x) { x &= (x - 1); c++; }
    return c;
#endif
}

// --- Simple union-find (disjoint set union) ---
struct DSU {
    std::vector<int> parent;
    std::vector<int> rank_;
    int numComponents;

    explicit DSU(int n) : parent(n), rank_(n, 0), numComponents(n) {
        std::iota(parent.begin(), parent.end(), 0);
    }

    int find(int x) {
        while (parent[x] != x) {
            parent[x] = parent[parent[x]]; // path halving
            x = parent[x];
        }
        return x;
    }

    // Returns true if a merge actually happened (i.e. they were in
    // different components before).
    bool unite(int a, int b) {
        a = find(a);
        b = find(b);
        if (a == b) return false;
        if (rank_[a] < rank_[b]) std::swap(a, b);
        parent[b] = a;
        if (rank_[a] == rank_[b]) rank_[a]++;
        numComponents--;
        return true;
    }
};

int main(int argc, char** argv) {
    CLI::App app{
        "Split a bicolor PNG into 8x8 character cells, cluster visually "
        "similar cells together (increasing the allowed Hamming distance "
        "until the cell count fits a target palette size), replace every "
        "cell with its cluster's representative pattern, and write the "
        "result out as a new PNG."
    };

    std::string inputPath;
    std::string outputPath = "output.png";
    int blackThreshold = 128;
    int targetCells = 256;
    int startDistance = 0;
    int maxDistance = 64; // safety cap; 64 = every cell could merge into one
    bool verbose = false;

    app.add_option("-i,--image", inputPath, "Input PNG image path")
        ->required()
        ->check(CLI::ExistingFile);
    app.add_option("-o,--output", outputPath,
                    "Output PNG image path (default: output.png)");
    app.add_option("-b,--black-threshold", blackThreshold,
                    "Luminance (0-255) below which a pixel counts as black "
                    "(default: 128)")
        ->check(CLI::Range(0, 255));
    app.add_option("-t,--target", targetCells,
                    "Maximum number of unique cells to reduce the image to "
                    "(default: 256)")
        ->check(CLI::PositiveNumber);
    app.add_option("-s,--start-distance", startDistance,
                    "Initial Hamming distance to start clustering at "
                    "(default: 0)")
        ->check(CLI::Range(0, BITS_PER_CELL));
    app.add_option("-m,--max-distance", maxDistance,
                    "Maximum Hamming distance to try before giving up "
                    "(default: 64, the theoretical maximum)")
        ->check(CLI::Range(0, BITS_PER_CELL));
    app.add_flag("-v,--verbose", verbose,
                  "Print per-cell-group details while clustering");

    CLI11_PARSE(app, argc, argv);

    Magick::InitializeMagick(nullptr);

    Magick::Image image;
    try {
        image.read(inputPath);
    } catch (Magick::Exception& e) {
        std::fprintf(stderr, "Failed to load image '%s': %s\n",
                      inputPath.c_str(), e.what());
        return 1;
    }

    image.type(Magick::GrayscaleType);

    const int width = (int)image.columns();
    const int height = (int)image.rows();

    std::printf("Loaded '%s': %dx%d pixels\n", inputPath.c_str(), width, height);

    if (width % CELL_W != 0 || height % CELL_H != 0) {
        std::fprintf(stderr,
            "Warning: image dimensions %dx%d are not exact multiples of "
            "%dx%d. Trailing partial rows/columns will be ignored.\n",
            width, height, CELL_W, CELL_H);
    }

    const int cols = width / CELL_W;
    const int rows = height / CELL_H;
    const int numCells = cols * rows;

    std::printf("Grid: %d columns x %d rows = %d characters (8x8 each)\n",
                cols, rows, numCells);

    if (targetCells >= numCells) {
        std::printf(
            "Target (%d) already covers every cell (%d) - no clustering "
            "needed, output will be a faithful (binarized) copy.\n",
            targetCells, numCells);
    }

    // Pull pixel data into a flat 0-255 grayscale buffer. We export a
    // single 8-bit "Intensity" channel directly rather than reading the
    // red channel out of a full RGBA PixelPacket: after the GrayscaleType
    // conversion above R == G == B for every pixel, so reading .red would
    // also work, but asking Magick++ for the intensity channel explicitly
    // is clearer about intent and only fetches the one byte per pixel we
    // actually need.
    std::vector<unsigned char> gray(width * height);
    image.write(0, 0, width, height, "I", Magick::CharPixel, gray.data());

    // Binarize and pack every cell into a 64-bit value.
    std::vector<Cell> cells;
    cells.reserve(numCells);

    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            uint64_t bits = 0;
            int bitIndex = 0;
            for (int y = 0; y < CELL_H; y++) {
                int py = r * CELL_H + y;
                for (int x = 0; x < CELL_W; x++) {
                    int px = c * CELL_W + x;
                    unsigned char lum = gray[py * width + px];
                    bool isBlack = lum < blackThreshold;
                    if (isBlack) bits |= (uint64_t(1) << bitIndex);
                    bitIndex++;
                }
            }
            cells.push_back(Cell{r, c, bits});
        }
    }

    // --- Precompute all pairwise Hamming distances once, sorted ascending.
    // For ~1000 cells this is ~500k pairs, trivial to hold in memory.
    struct Edge { int a, b; int dist; };
    std::vector<Edge> edges;
    edges.reserve((size_t)numCells * (numCells - 1) / 2);
    for (int i = 0; i < numCells; i++) {
        for (int j = i + 1; j < numCells; j++) {
            int d = popcount64(cells[i].bits ^ cells[j].bits);
            if (d <= maxDistance) edges.push_back({i, j, d});
        }
    }
    std::sort(edges.begin(), edges.end(),
              [](const Edge& a, const Edge& b) { return a.dist < b.dist; });

    // --- Single-linkage clustering: merge edges in increasing distance
    // order. Because merging is monotonic (more edges only ever reduces the
    // cluster count), processing all edges with dist <= startDistance first,
    // then continuing to merge edges in ascending distance order until the
    // cluster count drops to the target, is exactly equivalent to "redo
    // full clustering at threshold = 0, 1, 2, ... until it fits".
    DSU dsu(numCells);
    size_t edgeIdx = 0;
    int finalDistance = startDistance;

    // Apply all edges with distance <= startDistance immediately.
    while (edgeIdx < edges.size() && edges[edgeIdx].dist <= startDistance) {
        dsu.unite(edges[edgeIdx].a, edges[edgeIdx].b);
        edgeIdx++;
    }

    if (verbose) {
        std::printf("After start distance %d: %d unique cluster(s)\n",
                    startDistance, dsu.numComponents);
    }

    while (dsu.numComponents > targetCells && finalDistance < maxDistance) {
        finalDistance++;
        bool mergedAny = false;
        while (edgeIdx < edges.size() && edges[edgeIdx].dist <= finalDistance) {
            if (dsu.unite(edges[edgeIdx].a, edges[edgeIdx].b)) mergedAny = true;
            edgeIdx++;
        }
        if (verbose) {
            std::printf("Hamming distance <= %d: %d unique cluster(s)%s\n",
                        finalDistance, dsu.numComponents,
                        mergedAny ? "" : " (no new merges)");
        }
    }

    if (dsu.numComponents > targetCells) {
        std::printf(
            "\nWarning: could not reach target of %d unique cells even at "
            "the maximum distance of %d. Stopped at %d unique cells.\n",
            targetCells, maxDistance, dsu.numComponents);
    } else {
        std::printf(
            "\nReached %d unique cell(s) (target: %d) at Hamming distance "
            "%d.\n",
            dsu.numComponents, targetCells, finalDistance);
    }

    // --- Build cluster -> representative pattern (majority vote per bit).
    // First, group cell indices by their DSU root.
    std::vector<std::vector<int>> clusterMembers(numCells); // indexed by root
    for (int i = 0; i < numCells; i++) {
        clusterMembers[dsu.find(i)].push_back(i);
    }

    std::vector<uint64_t> representative(numCells, 0); // indexed by root
    int uniqueClusterCount = 0;
    for (int root = 0; root < numCells; root++) {
        auto& members = clusterMembers[root];
        if (members.empty()) continue;
        uniqueClusterCount++;

        if (members.size() == 1) {
            representative[root] = cells[members[0]].bits;
            continue;
        }

        int counts[BITS_PER_CELL] = {0};
        for (int idx : members) {
            uint64_t bits = cells[idx].bits;
            for (int b = 0; b < BITS_PER_CELL; b++) {
                if (bits & (uint64_t(1) << b)) counts[b]++;
            }
        }
        uint64_t rep = 0;
        int half = (int)members.size();
        for (int b = 0; b < BITS_PER_CELL; b++) {
            // Majority vote; ties (exactly half) resolved towards black.
            if (counts[b] * 2 >= half) rep |= (uint64_t(1) << b);
        }
        representative[root] = rep;
    }

    std::printf("Final unique cell count: %d\n", uniqueClusterCount);

    if (verbose) {
        // List clusters with more than one member, largest first.
        std::vector<int> roots;
        for (int root = 0; root < numCells; root++) {
            if (clusterMembers[root].size() > 1) roots.push_back(root);
        }
        std::sort(roots.begin(), roots.end(), [&](int a, int b) {
            return clusterMembers[a].size() > clusterMembers[b].size();
        });
        std::printf("\n=== Clusters with more than one member ===\n");
        for (int root : roots) {
            std::printf("Cluster (representative pattern 0x%016llX), %zu member(s):\n",
                        (unsigned long long)representative[root],
                        clusterMembers[root].size());
            for (int idx : clusterMembers[root]) {
                std::printf("    (row=%d, col=%d)\n", cells[idx].row, cells[idx].col);
            }
        }
    }

    // --- Replace every cell with its cluster's representative pattern and
    // write the resulting image out.
    Magick::Image outImage(Magick::Geometry(width, height), "white");
    outImage.type(Magick::GrayscaleType);
    outImage.modifyImage();

    Magick::PixelPacket* outPixels = outImage.getPixels(0, 0, width, height);
    for (int i = 0; i < numCells; i++) {
        int root = dsu.find(i);
        uint64_t bits = representative[root];
        int r = cells[i].row;
        int c = cells[i].col;
        int bitIndex = 0;
        for (int y = 0; y < CELL_H; y++) {
            int py = r * CELL_H + y;
            for (int x = 0; x < CELL_W; x++) {
                int px = c * CELL_W + x;
                bool isBlack = (bits & (uint64_t(1) << bitIndex)) != 0;
                Magick::Quantum v = isBlack ? 0 : (Magick::Quantum)65535;
                Magick::PixelPacket& outP = outPixels[py * width + px];
                outP.red = outP.green = outP.blue = v;
                bitIndex++;
            }
        }
    }
    outImage.syncPixels();

    try {
        outImage.write(outputPath);
    } catch (Magick::Exception& e) {
        std::fprintf(stderr, "Failed to write output image '%s': %s\n",
                      outputPath.c_str(), e.what());
        return 1;
    }

    std::printf("\nWrote reduced-palette image to '%s'\n", outputPath.c_str());

    return 0;
}
