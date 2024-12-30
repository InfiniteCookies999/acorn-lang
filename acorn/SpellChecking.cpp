#include "SpellChecking.h"

#include "Logger.h"

namespace acorn {

    static const std::string DidYouMeanStr = "Did you mean '";
    static const std::string InsteadOfStr = "' instead of '";

    // Computes the levenstein distance.
    static size_t lev_row(const llvm::StringRef s1, const llvm::StringRef s2) {
    
        const size_t m = s1.size(), n = s2.size();

        if (m == 0) return n;
        if (n == 0) return m;

        llvm::SmallVector<size_t, 32> costs;
        costs.resize(s2.size() + 1);

        for (size_t k = 0; k <= n; ++k) {
            costs[k] = k;
        }

        // Using std::tolower to normalize the comparisons so it does
        // not take into account case sensitivity.
        for (size_t i = 0; i < s1.size(); ++i) {
            char c1 = std::tolower(s1[i]);

            costs[0] = i + 1;
            size_t corner = i;

            for (size_t j = 0; j < s2.size(); ++j) {
                char c2 = std::tolower(s2[j]);

                size_t upper = costs[j + 1];
                if (c1 == c2) {
                    costs[j + 1] = corner;
                } else {
                    // min(upper, corner, left) + 1
                    size_t t = upper < corner ? upper : corner;
                    costs[j + 1] = (costs[j] < t ? costs[j] : t) + 1;
                }

                corner = upper;
            }
        }

        return costs[n];
    }

    static void add_comparison_underscoring(Logger& logger, llvm::StringRef search, llvm::StringRef found) {
        logger.print(std::string(DidYouMeanStr.size(), ' '));

        llvm::SmallVector<llvm::SmallVector<size_t>> table;
        for (size_t i = 0; i < search.size() + 1; ++i) {
            table.push_back(llvm::SmallVector<size_t>(found.size() + 1, 0));
        }

        for (size_t i = 0; i <= search.size(); ++i) {
            table[i][found.size()] = search.size() - i;
        }
        for (size_t i = 0; i <= found.size(); ++i) {
            table[search.size()][i] = found.size() - i;
        }

        using isize = long long;
        for (isize i = search.size() - 1; i >= 0; --i) {
            for (isize j = found.size() - 1; j >= 0; --j) {
                size_t right  = table[i][j + 1];
                size_t down   = table[i + 1][j];
                size_t corner = table[i + 1][j + 1];

                bool match = search[i] == found[j];
                size_t value = match ? 0 : 1;
                if (right < down && right < corner) {
                    table[i][j] = value + right;
                } else if (down < right && down < corner) {
                    table[i][j] = value + down;
                } else {
                    table[i][j] = value + corner;
                }
            }
        }

        std::string pluses, minuses;
        size_t i = 0, j = 0;
        while (i < search.size() && j < found.size()) {
            if (search[i] == found[j]) {
                // Nothing to do they match!
                pluses  += " ";
                minuses += " ";
                ++i;
                ++j;
            } else {
                size_t right  = table[i][j + 1];
                size_t down   = table[i + 1][j];
                size_t corner = table[i + 1][j + 1];
                if (right < down && right < corner) {
                    // Add
                    pluses += "+";
                    ++j;
                } else if (down < right && down < corner) {
                    // Delete
                    minuses += "-";
                    ++i;
                } else {
                    // Replace
                    minuses += "-";
                    pluses  += "+";
                    ++i;
                    ++j;
                }
            }
        }

        size_t delete_trail = search.size() - i;
        size_t add_trail    = found.size() - j;
        
        pluses  += std::string(add_trail, '+');
        minuses += std::string(delete_trail, '-');

        logger.set_color(Stream::StdErr, Color::BrightGreen);
        logger.print(pluses);
        logger.print(std::string(InsteadOfStr.size(), ' '));
        logger.set_color(Stream::StdErr, Color::BrightRed);
        logger.print(minuses);
        logger.set_color(Stream::StdErr, Color::BrightWhite);

    }
}

llvm::StringRef acorn::find_closest_spelling_match(const llvm::SmallVector<llvm::StringRef, 64>& comparisons,
                                                   const llvm::StringRef& find_for) {
    
    if (comparisons.empty()) {
        return "";
    }
    
    size_t smallest_cost = std::numeric_limits<size_t>::max();
    size_t selected_index = 0, index = 0;

    for (const llvm::StringRef comparison : comparisons) {
        size_t cost = lev_row(find_for, comparison);
        if (cost < smallest_cost) {
            smallest_cost = cost;
            selected_index = index;
        }
        ++index;
    }

    if (smallest_cost > find_for.size() / 2) {
        return "";
    }
    
    return comparisons[selected_index];
}

bool acorn::ErrorSpellChecker::search(Logger& logger, Identifier search_identifier) {
    if (!should_show) {
        all_searches.clear();
        return false;
    }

    auto search = search_identifier.to_string();
    auto found = find_closest_spelling_match(all_searches, search);
    if (!found.empty()) {
        logger.add_line([found, search](Logger& logger) {
            logger.fmt_print("%s%s%s%s'?", DidYouMeanStr, found, InsteadOfStr, search);
        }).remove_period();
        logger.add_line([found, search](Logger& logger) {
            add_comparison_underscoring(logger, search, found);
        }).remove_period();
    }
    
    all_searches.clear();
    return !found.empty();
}
