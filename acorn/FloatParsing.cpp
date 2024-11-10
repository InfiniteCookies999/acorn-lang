#include "FloatParsing.h"

#include "Source.h"
#include "PageAllocator.h"
#include "Util.h"

#include <iostream>

namespace acorn {

const size_t MAX_SMALL_DOUBLE_DIGITS = 16;
const size_t MAX_SMALL_SINGLE_DIGITS = 8;

// Used to scale whole values when trailing zeros
// are encountered.
const uint64_t POW10_INT64_TABLE[] = {
    1,
    10,
    100,
    1000,
    10000,
    100000,
    1000000,
    10000000,
    100000000,
    1000000000,
    10000000000,
    100000000000,
    1000000000000,
    10000000000000,
    100000000000000,
    1000000000000000,
    10000000000000000,
    100000000000000000,
    1000000000000000000,
    10000000000000000000,
};

const int64_t SINGLE_POW10_TABLE_LIMIT = 10;
const float POW10_SINGLE_TABLE[SINGLE_POW10_TABLE_LIMIT + 1] = {
    1.0E0f, 1.0E1f, 1.0E2f, 1.0E3f, 1.0E4f , 1.0E5f,
    1.0E6f, 1.0E7f, 1.0E8f, 1.0E9f, 1.0E10f,
};

const int64_t DOUBLE_POW10_TABLE_LIMIT = 22;
const double POW10_DOUBLE_TABLE[DOUBLE_POW10_TABLE_LIMIT + 1] = {
    1.0E0 , 1.0E1 , 1.0E2 , 1.0E3 , 1.0E4 , 1.0E5,
    1.0E6 , 1.0E7 , 1.0E8 , 1.0E9 , 1.0E10, 1.0E11,
    1.0E12, 1.0E13, 1.0E14, 1.0E15, 1.0E16, 1.0E17,
    1.0E18, 1.0E19, 1.0E20, 1.0E21, 1.0E22,
};

const double POW10_DOUBLE_BIG_TABLE[] = {
      1.0E16, 1.0E32, 1.0E64, 1.0E128, 1.0E256
};
const double POW10_DOUBLE_TINY_TABLE[] = {
    1E-16, 1E-32, 1E-64, 1E-128, 1E-256
};

const size_t SMALL_POW5_TABLE_SIZE = 14;
const uint32_t SMALL_POW5_TABLE[SMALL_POW5_TABLE_SIZE] = {
    1,
    5,
    5 * 5,
    5 * 5 * 5,
    5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5,
    5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5 * 5
};

struct ParseData {
    FloatParseError error;
    uint64_t        small_value;
    size_t          small_digits_count;
    char*           big_digits;
    size_t          big_digits_count;
    int64_t         exp;
    bool            is_negative;
};

struct StretchyBuffer {

    char* expand(PageAllocator& allocator, size_t new_size) {
        if (new_size > size) {
            digits = static_cast<char*>(allocator.allocate(new_size));
            size = new_size;
        }
        return digits;
    }

    char* digits = nullptr;;
    size_t size = 0;
};

static StretchyBuffer digits_buffer;

// TODO: improve allocations.
class BigIntFD {
    static const uint64_t U64_MASK = 0xffffffffull;

    // Cache for 5^n that cannot fit into a uint32_t.
    static const size_t POW5_CACHE_SIZE = 100;
    static BigIntFD POW5_CACHE[POW5_CACHE_SIZE];

public:

    static void initialize_cache(PageAllocator& allocator) {
        // TODO: This is copying the memory this should be calling std::move!!!
        for (size_t i = 0; i < SMALL_POW5_TABLE_SIZE; i++) {
            BigIntFD big_pow5;
            big_pow5.blocks = new uint32_t[1];
            big_pow5.length = 1;
            *big_pow5.blocks = SMALL_POW5_TABLE[i];
            POW5_CACHE[i] = big_pow5;
        }
        // TODO: We are also making uneeded copying of every previous value.
        BigIntFD prev_pow5 = POW5_CACHE[SMALL_POW5_TABLE_SIZE - 1];
        for (size_t i = SMALL_POW5_TABLE_SIZE; i < POW5_CACHE_SIZE; i++) {
            prev_pow5 = prev_pow5.multiply(5);
            POW5_CACHE[i] = prev_pow5;
        }
    }

    ~BigIntFD() {
        delete[] blocks;
        blocks = nullptr;
    }

    BigIntFD(const BigIntFD& b) {
        this->length = b.length;
        this->blocks = new uint32_t[b.length];
        memcpy(this->blocks, b.blocks, b.length * sizeof(uint32_t));
    }

    BigIntFD(BigIntFD&& b) noexcept {
        this->length = std::exchange(b.length, 0);
        this->blocks = std::exchange(b.blocks, nullptr);
    }

    BigIntFD& operator=(const BigIntFD& b) {
        if (this == &b) {
            return *this;
        }
        
        delete[] this->blocks;
        this->length = b.length;
        this->blocks = new uint32_t[b.length];
        memcpy(this->blocks, b.blocks, b.length * sizeof(uint32_t));
        return *this;
    }

    BigIntFD& operator=(BigIntFD&& b) noexcept {
        if (this == &b) {
            return *this;
        }

        delete[] this->blocks;
        this->length = std::exchange(b.length, 0);
        this->blocks = std::exchange(b.blocks, nullptr);
        return *this;
    }

    static BigIntFD zero() {
        BigIntFD result;
        result.length = 0;
        return result;
    }

    static BigIntFD from_digits(uint64_t small_value, uint64_t small_digits_count,
                                char* big_digits, uint64_t big_digits_count) {
        BigIntFD result;

        uint64_t total_digits = small_digits_count + big_digits_count;
        size_t block_count = std::max((total_digits + 8) / 9, 2ull);

        // The small value behaves as the high digits.
        result.blocks = new uint32_t[block_count];
        result.blocks[0] = static_cast<uint32_t>(small_value);
        result.blocks[1] = static_cast<uint32_t>(small_value >> 32ull);
        result.length = 2;

        size_t idx = 0;
        while (idx + 5 < big_digits_count) {
            
            uint32_t value = static_cast<uint32_t>(big_digits[idx++]) - '0';
            for (size_t i = 1; i < 5; i++) {
                value *= 10;
                value += static_cast<uint32_t>(big_digits[idx++]) - '0';
            }

            // Shift by 10^5 then add by the value into the lower end
            // of the lower end that was just freed up.
            result.multiply_and_add_assume_capacity(100000, value);
        }

        // Same process except there isn't 5 digits left.
        uint32_t shift = 1;
        uint32_t value = 0;
        while (idx < big_digits_count) {
            value *= 10;
            value += static_cast<uint32_t>(big_digits[idx++]) - '0';
            shift *= 10;
        }
        if (shift != 1) { // Have to check for != 1 because its possible there where 5*n digits.
            result.multiply_and_add_assume_capacity(shift, value);
        }

        result.trim_leading_zeros();
        return result;
    }

    static BigIntFD from_pow5(int64_t p5) {
        if (p5 < POW5_CACHE_SIZE) {
            return POW5_CACHE[p5];
        }

        // 5^pow5 = 5^(q+r) = 5^q * 5^r
        // divide the power in 2.
        // q = pow5/2, r = pow5 - q
        int64_t q = p5 >> 1;
        int64_t r = p5 - q;

        BigIntFD bq = from_pow5(q);
        if (r < SMALL_POW5_TABLE_SIZE) {
            return bq.multiply(SMALL_POW5_TABLE[r]);
        } else {
            BigIntFD br = from_pow5(r);
            return bq.multiply(br);
        }
    }

    static BigIntFD from_sigf_pow5_pow2(uint64_t sigf, int64_t p5, int64_t p2) {
        uint32_t b1 = static_cast<uint32_t>(sigf);
        uint32_t b2 = static_cast<uint32_t>(sigf >> 32ull);
        int64_t  shift_block_idx = p2 >> 5;
        uint32_t shift_mod_idx = p2 & 31;

        if (p5 != 0) {
            if (p5 < SMALL_POW5_TABLE_SIZE) {
                uint64_t pow5 = SMALL_POW5_TABLE[p5] & U64_MASK;
                uint64_t product = pow5 * (b1 & U64_MASK);
                b1 = static_cast<uint32_t>(product);
                uint64_t carry = product >> 32;
                product = pow5 * (b2 & U64_MASK) + carry;
                b2 = static_cast<uint32_t>(product);
                uint32_t b3 = static_cast<uint32_t>(product >> 32);
                if (shift_mod_idx == 0) {
                    // Shift is block aligned so can optimize by
                    // just moving the values over the correct number
                    // of blocks.
                    return BigIntFD(b1, b2, b3, shift_block_idx);
                } else {
                    BigIntFD result;
                    result.blocks = new uint32_t[shift_block_idx + 4];
                    result.blocks[shift_block_idx + 0] = b1 << shift_mod_idx;
                    result.blocks[shift_block_idx + 1] = (b2 << shift_mod_idx) | (b1 >> (32 - shift_mod_idx));
                    result.blocks[shift_block_idx + 2] = (b3 << shift_mod_idx) | (b2 >> (32 - shift_mod_idx));
                    result.blocks[shift_block_idx + 3] = b3 >> (32 - shift_mod_idx);
                    result.length = shift_block_idx + 4;
                    // Zero lower shifted blocks.
                    memset(result.blocks, 0, shift_block_idx * sizeof(uint32_t));
                    // Trim the leading zeros.
                    result.trim_leading_zeros();
                    return result;
                }
            } else {
                BigIntFD pow5 = from_pow5(p5);
                if (b2 == 0) {
                    pow5 = pow5.multiply(b1);
                } else {
                    pow5 = pow5.multiply(b1, b2);
                }

                return pow5.left_shift(p2);
            }
        } else if (p2 != 0) {
            if (shift_mod_idx == 0) {
                // Shift is block aligned so can optimize by
                // just moving the values over the correct number
                // of blocks.
                return BigIntFD(b1, b2, shift_block_idx);
            } else {
                return BigIntFD(
                    b1 << shift_mod_idx,
                    (b2 << shift_mod_idx) | (b1 >> (32 - shift_mod_idx)),
                    b2 >> (32 - shift_mod_idx),
                    shift_block_idx
                );
            }
        } else {
            return BigIntFD(b1, b2, 0);
        }
    }

    BigIntFD multiply(const BigIntFD& rhs) const {
        if (this->length == 0) return BigIntFD::zero();
        if (rhs.length == 0)   return BigIntFD::zero();
        if (this->length == 1) return rhs.multiply(blocks[0]);
        if (rhs.length == 1)   return this->multiply(blocks[0]);

        BigIntFD result;
        result.length = this->length + rhs.length;
        result.blocks = new uint32_t[result.length];
        // Need to clear the result data because it is being used in
        // the multiplication loop and it initialially has random data.
        memset(result.blocks, 0, result.length * sizeof(uint32_t));

        // Long form multiplcation but the outer loop goes through
        // each "digit" (block) of the lhs and multiplies through
        // each "digit" of the right hand side.
        uint32_t* lhs_ptr = this->blocks;
        uint32_t* lhs_end = this->blocks + this->length;
        uint32_t* res_ptr = result.blocks;
        while (lhs_ptr != lhs_end) {
            uint64_t multiplier = *lhs_ptr & U64_MASK;
            uint64_t product = 0;
            uint32_t* rhs_ptr = rhs.blocks;
            uint32_t* rhs_end = rhs.blocks + rhs.length;
            uint32_t* ptr = res_ptr;
            while (rhs_ptr != rhs_end) {
                product += (*ptr & U64_MASK) + (multiplier * (*rhs_ptr & U64_MASK));
                *ptr = static_cast<uint32_t>(product);
                product >>= 32; // carry
                ++rhs_ptr;
                ++ptr;
            }
            *ptr = static_cast<uint32_t>(product);

            ++lhs_ptr;
            ++res_ptr;
        }

        if (result.blocks[result.length - 1] == 0) {
            --result.length;
        }

        return result;
    }

    BigIntFD multiply(uint32_t c) const {
        if (this->length == 0) return BigIntFD::zero();

        BigIntFD result;
        result.blocks = new uint32_t[this->length + 1];

        // Basic long multiplication except on blocks instead of digits.
        //
        uint64_t value = c & U64_MASK;
        uint32_t* ptr = blocks;
        uint32_t* end = blocks + length;
        uint32_t* res_ptr = result.blocks;
        uint64_t carry = 0;
        while (ptr != end) {
            uint64_t product = value * (*ptr & U64_MASK) + carry;
            *res_ptr = static_cast<uint32_t>(product);
            carry = product >> 32;
            ++ptr;
            ++res_ptr;
        }

        if (carry != 0) { // Last "digit" carries over.
            *res_ptr = static_cast<uint32_t>(carry);
            result.length = this->length + 1;
        } else {
            result.length = this->length;
        }

        return result;
    }

    BigIntFD multiply(uint32_t c1, uint32_t c2) const {
        BigIntFD result;
        result.blocks = new uint32_t[this->length + 2];

        // Basic long multiplication except on blocks instead of digits.
        //
        uint64_t value = c1 & U64_MASK;
        uint32_t* ptr = this->blocks;
        uint32_t* end = this->blocks + this->length;
        uint32_t* res_ptr = result.blocks;
        uint64_t carry = 0;
        while (ptr != end) {
            uint64_t product = value * (*ptr & U64_MASK) + carry;
            *res_ptr = static_cast<uint32_t>(product);
            carry = product >> 32;
            ++ptr;
            ++res_ptr;
        }
        *res_ptr = static_cast<uint32_t>(carry);
        value = c2 & U64_MASK;
        carry = 0;
        ptr = blocks;
        res_ptr = result.blocks + 1;
        while (ptr != end) {
            uint64_t product = (*res_ptr & U64_MASK) + (value * (*ptr & U64_MASK)) + carry;
            *res_ptr = static_cast<uint32_t>(product);
            carry = product >> 32;
            ++ptr;
            ++res_ptr;
        }

        if (carry != 0) {
            *res_ptr = static_cast<uint32_t>(carry);
            result.length = this->length + 2;
        } else {
            result.length = this->length + 1;
        }

        return result;
    }

    BigIntFD multiply_pow5(int64_t p5) const {
        if (p5 < SMALL_POW5_TABLE_SIZE) {
            return multiply(SMALL_POW5_TABLE[p5]);
        } else {
            // TODO: this is broken?
            BigIntFD big_pow5 = from_pow5(p5);
            return this->multiply(big_pow5);
        }
    }

    BigIntFD left_shift(int64_t shift) const {
        // TODO: if offsets are ever included it may be
        // possible to improve performance here by
        // instead of creating new allocation just reusing
        // the old data and setting the offset to the block
        // shift.

        if (shift == 0 || this->length == 0) {
            return *this;
        }

        int64_t  shift_block_idx = shift >> 5;
        uint32_t shift_mod_idx   = shift & 31;
        BigIntFD result;
    
        if (shift_mod_idx == 0) {
            // Block aligned.
            result.length = this->length + shift_block_idx;
            result.blocks = new uint32_t[result.length];

            // Zero low blocks.
            memset(result.blocks, 0, shift_block_idx * sizeof(uint32_t));
            // Copy blocks into higher indexes.
            memcpy(result.blocks + shift_block_idx,
                   this->blocks,
                   this->length * sizeof(uint32_t));
        } else {
            // Slide the block we are working with by the shift
            // then append the high bits of the previous block.

            result.blocks = new uint32_t[this->length + shift_block_idx + 1];

            uint32_t* from_ptr = this->blocks + this->length - 1;
            uint32_t* to_ptr = result.blocks + this->length + shift_block_idx;

            // How far to shift to extract the previous block's high bits which become the block we
            // are working with low bits.
            uint32_t low_bits_shift = 32 - shift_mod_idx;

            uint32_t high_bits = 0;
            uint32_t low_bits = *from_ptr >> low_bits_shift;
            while (from_ptr > blocks) {
                *to_ptr = high_bits | low_bits;
                high_bits = *from_ptr << shift_mod_idx;

                --from_ptr;
                --to_ptr;

                low_bits = *from_ptr >> low_bits_shift;
            }

            // final block which is also the lowest block only
            // has high bits because its lower bits were shifted.
            *to_ptr = high_bits | low_bits;
            *(to_ptr - 1) = *from_ptr << shift_mod_idx;

            // Zero the low blocks.
            memset(result.blocks, 0, shift_block_idx * sizeof(uint32_t));

            uint32_t result_length = this->length + shift_block_idx+ 1;
            if (result.blocks[result_length - 1] == 0) {
                --result_length;
            }

            result.length = result_length;
        }

        return result;
    }

    BigIntFD subtract(const BigIntFD& sub) const {
        BigIntFD result;
        result.length = this->length;
        result.blocks = new uint32_t[result.length];

        // TODO: could probably make more efficient by using pointer arithmetic.
        int64_t barrow = 0;
        size_t idx = 0;
        for (; idx < sub.length; idx++) {
            int64_t difference = static_cast<int64_t>(this->blocks[idx] & U64_MASK) -
                                 static_cast<int64_t>(sub.blocks[idx]) + barrow;
            // Warning here is not relative since the sub.length is always smaller
            // than the result.length.
            result.blocks[idx] = static_cast<uint32_t>(difference);
            barrow = difference >> 32;
        }
        for (; barrow != 0 && idx < this->length; idx++) {
            uint64_t carry = (this->blocks[idx] & U64_MASK) + barrow;
            result.blocks[idx] = static_cast<uint32_t>(carry);
            barrow = carry >> 32;
        }

        result.trim_leading_zeros();
        return result;
    }

    int compare(const BigIntFD& rhs) {
        if (this->length > rhs.length) {
            return +1;
        } else if (this->length < rhs.length) {
            return -1;
        }

        // Traverse in reverse order because want to compare
        // highest significant blocks first.
        for (int64_t i = this->length - 1; i >= 0; i--) {
            uint32_t lhs_value = this->blocks[i];
            uint32_t rhs_value = rhs.blocks[i];
            if (lhs_value != rhs_value) {
                return lhs_value < rhs_value ? -1 : +1;
            }
        }

        // Exact match.
        return 0;
    }

    void print_blocks() {
        for (int i = 0; i < length; i++) {
            std::cout << blocks[i] << "L, ";
        }
        std::cout << "\n";
    }

private:
    uint32_t* blocks = nullptr;
    size_t    length;

    BigIntFD() {}

    BigIntFD(uint32_t b1, uint32_t b2, size_t offset) {
        if (b1 == 0 && b2 == 0) {
            // Shifting 0 by Offset is still just 0.
            blocks = nullptr;
            length = 0;
            return;
        } 
    
        blocks = new uint32_t[offset + 2];
        memset(blocks, 0, offset * sizeof(uint32_t));
        blocks[offset]   = b1;
        blocks[offset+1] = b2;
        length = offset + 2;
        if (b2 == 0) {
            // Last block is zero, trim.
            --length;
        }
    }

    BigIntFD(uint32_t b1, uint32_t b2, uint32_t b3, size_t offset) {
        blocks = new uint32_t[offset + 3];
        memset(blocks, 0, offset * sizeof(uint32_t));
        blocks[offset]   = b1;
        blocks[offset+1] = b2;
        blocks[offset+2] = b3;
        length = offset + 3;
        trim_leading_zeros();
    }

    void trim_leading_zeros() {
        uint32_t* ptr = blocks + length - 1;
        while (ptr >= blocks) {
            if (*ptr == 0) --length;
            else break;
            --ptr;
        }
    }

    void multiply_and_add_assume_capacity(uint32_t mult, uint32_t add) {
        const uint64_t v = mult & U64_MASK;
        uint32_t* ptr = blocks;
        uint32_t* end = blocks + length;

        // Basic long multiplication except on blocks instead of digits.
        //
        uint64_t product = v * (*ptr & U64_MASK) + (add & U64_MASK);
        *ptr = static_cast<uint32_t>(product);
        product >>= 32; // carry
        ++ptr;

        while (ptr != end) {
            product += v * (*ptr & U64_MASK);
            *ptr = static_cast<uint32_t>(product);
            product >>= 32; // carry
            ++ptr;
        }

        if (product != 0) { // Last "digit" carries over.
            blocks[length++] = static_cast<uint32_t>(product);
        }
    }
};
// Requires symbol to be defined.
BigIntFD BigIntFD::POW5_CACHE[POW5_CACHE_SIZE];

static ParseData parse_float_data(PageAllocator& allocator, llvm::StringRef text, size_t max_small_digits) {

    size_t idx = 0;
    size_t length = text.size();

    bool is_negative = text[0] == '-';
    if (is_negative) {
        ++idx;
    }
   

    // Consuming leading zeros.
    //
    size_t leading_zeros_count     = 0;
    size_t seperators_count        = 0;
    size_t seperator_count_bef_dot = 0;
    size_t decimal_idx             = 0;
    bool   seen_decimal_pt         = false;
    while (idx < length) {
        char c = text[idx];
        if (c == '0') {
            ++leading_zeros_count;
        } else if (c == NUMBER_SEPERATOR) {
            ++seperators_count;
        } else if (c == '.') {
            // Do not break here because there may still be zeros
            // after the decimal point.
            //
            // Ex. 0000.00001  has 8 leading zeros.
            decimal_idx = idx;
            seen_decimal_pt = true;
            seperator_count_bef_dot = seperators_count;;
        } else {
            break;
        }
        ++idx;
    }

    // Continuing to consume digits as long as they can fit into
    // a 64 bit integer.
    //
    uint64_t small_value         = 0;
    size_t   small_digits_count  = 0;
    size_t   trailing_zero_count = 0;
    while (small_digits_count < max_small_digits && idx < length) {
        char c = text[idx];
        if (c >= '1' && c <= '9') { // Digits except zero.
            if (trailing_zero_count != 0) {
                // The trailing zeros are not actually trailing.
                //
                if (small_digits_count + trailing_zero_count >= max_small_digits) {
                    // Not all the zeros can actually fit into the number.
                    size_t taken_zeros = max_small_digits - small_digits_count;
                    trailing_zero_count -= taken_zeros;
                    
                    small_value *= POW10_INT64_TABLE[taken_zeros];
                    small_digits_count = max_small_digits;
                    break;
                }
                small_value *= POW10_INT64_TABLE[trailing_zero_count];
                // Not actually trailing digits so we have to add them.
                small_digits_count += trailing_zero_count;
                trailing_zero_count = 0;
            }
            ++small_digits_count;
            // Scale up by the new digit.
            small_value = small_value * 10 + static_cast<uint64_t>(c - '0');
        } else if (c == '0') {
            // Might be trailing.
            ++trailing_zero_count;
        } else if (c == NUMBER_SEPERATOR) {
            ++seperators_count;
        } else if (c == '.') {
            decimal_idx = idx;
            seen_decimal_pt = true;
            seperator_count_bef_dot = seperators_count;
        } else {
            break;
        }
        ++idx;
    }

    // Stretchy buffer for digits that could not fit into the unsigned 64 bit number.
    //
    size_t buffer_size = length - idx + trailing_zero_count + 1;
    size_t big_digits_count = 0;
    char* big_digits = digits_buffer.expand(allocator, buffer_size);
    if (big_digits) {
        memset(big_digits, '0', buffer_size);
    }

    // Consuming the rest of the digits.
    //
    while (idx < length) {
        char c = text[idx];
        if (c >= '1' && c <= '9') { // Digits except zero.
            if (trailing_zero_count != 0) {
                // The trailing zeros are not actually trailing.
                //
                big_digits_count += trailing_zero_count;
                trailing_zero_count = 0;
            }
            big_digits[big_digits_count++] = c;
        } else if (c == '0') {
            // Might be trailing.
            ++trailing_zero_count;
        } else if (c == NUMBER_SEPERATOR) {
            ++seperators_count;
        } else if (c == '.') {
            decimal_idx = idx;
            seen_decimal_pt = true;
            seperator_count_bef_dot = seperators_count;
        } else {
            break;
        }
        ++idx;
    }

    size_t  digits_count = big_digits_count + small_digits_count;
    int64_t exp = seen_decimal_pt ? decimal_idx - leading_zeros_count - seperator_count_bef_dot - (is_negative ? 1 : 0)
                                  : digits_count + trailing_zero_count;

    // Consuming the optional exponent.
    //
    if (idx < length && (text[idx] == 'E' || text[idx] == 'e')) {
        ++idx;
        int64_t exp_sign = 1;
        if (text[idx] == '+') {
            ++idx;
        } else if (text[idx] == '-') {
            ++idx;
            exp_sign = -1;
        }

        // limit to i32 maximum which is way more than enough.
        const int64_t OverflowAmount = 2147483647 / 10;

        int64_t exp_value = 0;
        while (idx < length) {
            char c = text[idx];
            if (c >= '0' && c <= '9') {
                if (exp_value >= OverflowAmount) {
                    // The next digit will cause overflow or underflow!
                    if (exp_sign == 1) {
                        return ParseData{
                            FloatParseError::Overflow
                        };
                    } else {
                        return ParseData{
                            FloatParseError::Underflow
                        };
                    }
                }
                exp_value = exp_value * 10 + static_cast<int64_t>(c - '0');
            } else {
                break;
            }
            ++idx;
        }

        // Want to do exponent overflow checks here since just adding
        // the value when it overflows could cause the later stages to
        // not know that there was an overflow when the number overflows.
        int64_t exp_limit = 324 + small_digits_count + big_digits_count + trailing_zero_count;
        if (exp_value > exp_limit) {
            if (exp_sign == 1 && exp < 0 && (exp_value + exp < exp_limit)) {
                exp += exp_value;
            } else {
                if (exp_sign == 1) {
                    return ParseData{
                        FloatParseError::Overflow
                    };
                } else {
                    return ParseData{
                        FloatParseError::Underflow
                    };
                }
            }
        } else {
            // Will not overflow. Although could still overflow
            // later when taking into account the digits.
            exp += exp_sign * exp_value;
        }
    }

    return ParseData {
        FloatParseError::None,
        small_value,
        small_digits_count,
        big_digits,
        big_digits_count,
        exp,
        is_negative
    };
}

static size_t get_number_of_leading_zeros(uint32_t value) {
    if (value == 0) return 32;
    size_t n = 31;
    if (value >= 1<<16) { n -= 16; value >>= 16; }
    if (value >= 1<<8 ) { n -=  8; value >>= 8;  }
    if (value >= 1<<4 ) { n -=  4; value >>= 4;  }
    if (value >= 1<<2 ) { n -=  2; value >>= 2;  }
    return n - (value >> 1);
}

static size_t get_number_of_leading_zeros(uint64_t value) {
    uint32_t high_bits = static_cast<uint32_t>(value >> 32ull);
    return high_bits == 0 ? 32 + get_number_of_leading_zeros(static_cast<uint32_t>(value))
                          : get_number_of_leading_zeros(high_bits);
}

static size_t get_number_of_trailing_zeros(uint32_t value) {
    value = ~value & (value - 1);
    if (value == 0) return 0;
    size_t n = 1;
    if (value > 1<<16) { n += 16; value >>= 16; }
    if (value > 1<< 8) { n +=  8; value >>=  8; }
    if (value > 1<< 4) { n +=  4; value >>=  4; }
    if (value > 1<< 2) { n +=  2; value >>=  2; }
    return n + (value >> 1);
}

static size_t GetNumberOfTrailingZeros(uint64_t value) {
    uint32_t low_bits = static_cast<uint32_t>(value);
    return low_bits == 0 ? 32 + GetNumberOfTrailingZeros(static_cast<uint32_t>(value >> 32))
                        : GetNumberOfTrailingZeros(static_cast<uint32_t>(low_bits));
}

template<typename T>
struct CorrectionInfo {
    using IntTy = uint64_t;
    static const IntTy    mantissa_size;
    static const IntTy    mantissa_size;
    static const int64_t  exp_size;
    static const int64_t  exp_bias;
    static const int64_t  exp_mask;
    static const uint64_t sign_mask;
};
template<>
struct CorrectionInfo<double> {
    using IntTy                         = uint64_t;
    static const uint64_t mantissa_size = 52;
    static const uint64_t mantissa_mask = 0xFFFFFFFFFFFFF;
    static const int64_t  exp_size      = 11;
    static const int64_t  exp_bias      = 1023;
    static const uint64_t exp_mask      = 0x7FF0000000000000ull;
    static const uint64_t sign_mask     = 0x8000000000000000ull;
};
template<>
struct CorrectionInfo<float> {
    using IntTy                         = uint32_t;
    static const uint32_t mantissa_size = 23;
    static const uint32_t mantissa_mask = 0x7FFFFF;
    static const int64_t  exp_size      = 9;
    static const int64_t  exp_bias      = 127;
    static const uint32_t exp_mask      = 0x7F800000;
    static const uint64_t sign_mask     = 0x80000000;
};

template<typename T>
static T correct_value(T value, ParseData parse_data) {
    
    // The calculation:
    // 
    // In the paper  "How to Read Floating Point Numbers Accurately" by
    // William D Clinger, section 5 tells us what we are
    // going to solve for. Let x denote the exact value
    // and y our approximation then consider the two formuals:
    //
    // 1.   x/y = f*10^e / (m*b^k).
    // 2.   f*10^e = (m+err)*2^k
    // 
    // Solving for error in equation 2 gives us err = (m(x - y))/y.
    // 
    // The stopping condition for the algorithm is such that err <= 1/2.
    // 
    // So we solve for:
    // 
    // (m(x - y))/y <= 1/2 -> 2m(x - y) <= y
    //                     -> 2mx - 2my <= y
    //                     -> 2m*f*10^e - 2m*m*2^k <= m*2^k
    //                     -> 2*f*10^e - m*2^(k+1) <= 2^k
    //                     -> 2*f*(5^e)*(2^e) - m*2^(k+1) <= 2^k
    //                     -> f*(5^e)*(2^(e+1)) - m*2^(k+1) <= 2^k.
    //

    using IntTy = CorrectionInfo<T>::IntTy;
    IntTy bits = std::bit_cast<IntTy>(value);
    
    
    int64_t e = parse_data.exp - (parse_data.small_digits_count + parse_data.big_digits_count);

    // Powers of 5 portion of the exponent for the exact value. If
    // the exponent of the exact value is negative then it is brought
    // to the denominator of x/y.
    int64_t y5 = std::max(0ll, -e);
    int64_t x5 = std::max(0ll, +e);

    // Construct value from the digits before adjusting by exponent.
    BigIntFD exact_value_p5 = BigIntFD::from_digits(parse_data.small_value,
                                                    parse_data.small_digits_count,
                                                    parse_data.big_digits,
                                                    parse_data.big_digits_count);
    // exact_value_p5.print_blocks();

    if (x5 != 0) {
        exact_value_p5 = exact_value_p5.multiply_pow5(x5);
        // exact_value_p5.print_blocks();
    }

    constexpr IntTy    mantissa_size = CorrectionInfo<T>::mantissa_size;
    constexpr IntTy    mantissa_mask = CorrectionInfo<T>::mantissa_mask;
    constexpr int64_t  exp_size      = CorrectionInfo<T>::exp_size;
    constexpr int64_t  exp_bias      = CorrectionInfo<T>::exp_bias;
    constexpr IntTy    exp_mask      = CorrectionInfo<T>::exp_mask;
    constexpr uint64_t sign_mask     = CorrectionInfo<T>::sign_mask;

    // Now looping until corrected.
    //
    bool finished_constructing_exact = false;
    int64_t prev_x2 = 0;
    BigIntFD exact_value = BigIntFD::zero();

    while (true) {
        int64_t exp_bits      = static_cast<int64_t>(bits >> mantissa_size);
        IntTy   mantissa_bits = bits & mantissa_mask;
        if (exp_bits > 0) { // Normalized.
            mantissa_bits |= (static_cast<IntTy>(1) << mantissa_size); // Add the implicit 1 to the mantissa.
        } else { // Going to normalize the denormalized numbers.
            // To normalize we shift the first 1 in the mantissa
            // until its in the appropriate 53 bit index.
            size_t leading_zeros = get_number_of_leading_zeros(mantissa_bits);
            // -exp_size since get_number_of_leading_zeros also returns the exponent
            // zeros.
            int64_t shift = leading_zeros - static_cast<int64_t>(exp_size);
            mantissa_bits <<= shift;
            exp_bits = 1ll - shift;
        }
        exp_bits -= exp_bias; // Remove the bias.
        size_t trailing_zeros = get_number_of_trailing_zeros(mantissa_bits);
        // Want to remove lower order bits so that 2^trailing_zeros
        // may be factored out in the comparison to exact_value.
        mantissa_bits >>= static_cast<IntTy>(trailing_zeros);
        // Pulling out the trailing zeros forr potential common factor.
        int64_t no_zeros_exp = exp_bits - mantissa_size + trailing_zeros;

        // First letting powers of 2 match the powers of 5
        // to make up the 10^e of the original decimal. That
        // is 10^e = 5^e * 2^e. Then adding the exponent of
        // the approximation remembering that we can move it
        // to the numerator of x/y if negative.
        int64_t y2 = y5;
        int64_t x2 = x5;
        if (no_zeros_exp >= 0) {
            y2 += +no_zeros_exp;
        } else {
            x2 += -no_zeros_exp;
        }
        
        int64_t err_lhs_cmp;
        int64_t cmp_rhs2 = y2;
        if (exp_bits <= -exp_bias) {
            err_lhs_cmp = exp_bits + exp_bias + trailing_zeros;
        } else {
            err_lhs_cmp = 1 + trailing_zeros;
        }
        y2 += err_lhs_cmp;
        x2 += err_lhs_cmp;

        // Factoring out common powers of 2 to reduce calculation time.
        int64_t common_factors_of2 = std::min(y2, std::min(x2, cmp_rhs2));
        y2       -= common_factors_of2;
        x2       -= common_factors_of2;
        cmp_rhs2 -= common_factors_of2;

        BigIntFD est_value = BigIntFD::from_sigf_pow5_pow2(mantissa_bits, y5, y2);
        // est_value.print_blocks();

        if (!finished_constructing_exact || prev_x2 != x2) {
            exact_value = exact_value_p5.left_shift(x2);
            // exact_value.print_blocks();
            finished_constructing_exact = true;
            prev_x2 = x2;
        }

        // Now we compare the results to see if the estimated
        // value matches the exact value.
        BigIntFD difference = BigIntFD::zero();
        int compare_result = est_value.compare(exact_value);
        bool too_big;
        if (compare_result > 0) {
            // The estimated value was too big!
            difference = est_value.subtract(exact_value);
            too_big = true;
            if (trailing_zeros == mantissa_size && exp_bits > -exp_bias + 1) {
                // The estimate is an exact power of 2 and not
                // denormalized.
                cmp_rhs2 -= 1;
                if (cmp_rhs2 < 0) {
                    // Exponent became two small move it the left side.
                    cmp_rhs2 = 0;
                    difference = difference.left_shift(1);
                }
            }
        } else if (compare_result < 0) {
            // The estimated value was too small!
            difference = exact_value.subtract(est_value);
            too_big = false;
        } else {
            // Perfect match! Ending correction!
            break;
        }

        // difference.print_blocks();

        // Not an exact match but may be less than the allowed error.
        BigIntFD rhs_cmp = BigIntFD::from_pow5(y5).left_shift(cmp_rhs2);
        compare_result = difference.compare(rhs_cmp);
        if (compare_result < 0) {
            // The difference is less than 1/2
            // so it's close enough!
            break;
        } else if (compare_result == 0) {
            // The difference is exactly 1/2
            // so going to round to nearest and
            // call that the result!
            if ((bits & 1) != 0) {
                bits += too_big ? -1 : +1;
            }
            break;
        } else {
            // The difference is too much! Must
            // correct the Bits and try again!
            bits += too_big ? -1 : +1;
            if (bits == 0 || bits == exp_mask) {
                // Correction led to possitive infity or 0
                // so ending the correction loop and returning
                // one of those results.
                break;
            }
        }
    }
    
    if (parse_data.is_negative) {
        bits |= sign_mask;
    }
    return std::bit_cast<T>(bits);
}

}

void acorn::initialize_float_parsing(PageAllocator& allocator) {
    static bool initialized = false;
    if (initialized) {
        return;
    }
    initialized = true;

    BigIntFD::initialize_cache(allocator);
}

std::pair<float, acorn::FloatParseError>
acorn::parse_float32_bits(PageAllocator& allocator, llvm::StringRef text) {
    
    auto parse_data = parse_float_data(allocator, text, MAX_SMALL_SINGLE_DIGITS);
    if (parse_data.error != FloatParseError::None) {
        return { 0.0f, parse_data.error };
    }

    uint64_t small_value = parse_data.small_value;
    // Exponent value not including the exponent from the small value digits.
    int64_t  e           = parse_data.exp - static_cast<int64_t>(parse_data.small_digits_count);

    bool is_negative = parse_data.is_negative;

    uint64_t total_digits = parse_data.small_digits_count + parse_data.big_digits_count;
    if (total_digits < MAX_SMALL_SINGLE_DIGITS) {
        float single_value = static_cast<float>(parse_data.small_value);
        if (e == 0 && small_value == 0) { // Easiest case just check for zero!
            return { is_negative ? -0.0f : +0.0f, FloatParseError::None };
        } else if (e >= 0) {
            // Although the number of small digits may already be the maximum
            // digits it is possible to calculate the best approximation by
            // simple scaling of 10. The reason is that IEEE 754 floating point
            // arithmetic specifies that this number will be rounded to the nearest
            // representable number. Rounding to the nearest even if a tie.
            if (e <= SINGLE_POW10_TABLE_LIMIT) {
                single_value *= POW10_SINGLE_TABLE[e];
                return { is_negative ? -single_value : single_value, FloatParseError::None };
            }
            // Same reasoning as above but considering the number of digits
            // in the small value allowing for larger scaling.
            int64_t slop = MAX_SMALL_SINGLE_DIGITS - parse_data.small_digits_count - 1;
            if (e <= SINGLE_POW10_TABLE_LIMIT + slop) {
                single_value *= POW10_SINGLE_TABLE[slop];
                single_value *= POW10_SINGLE_TABLE[e - slop];
                return { is_negative ? -single_value : single_value, FloatParseError::None };
            }
            // Otherwise continuing to harder cases.
        } else {
            // The exponent will actually take into account where the decimal
            // point is. So it is possible the exponent is negative which can
            // result in values like 1.25
            if (e >= -SINGLE_POW10_TABLE_LIMIT) {
                single_value /= POW10_SINGLE_TABLE[-e];
                return { is_negative ? -single_value : single_value, FloatParseError::None };
            }
            // Otherwise continuing to harder cases.
        }
    }
    
    int64_t total_digits_i64 = static_cast<int64_t>(total_digits);
    if (parse_data.exp >= total_digits_i64 &&
        total_digits_i64 + parse_data.exp < MAX_SMALL_DOUBLE_DIGITS) {
        double double_value = static_cast<double>(parse_data.small_value);
        double_value *= POW10_DOUBLE_TABLE[e];
        float single_value = static_cast<float>(double_value);
        return { is_negative ? -single_value : single_value, FloatParseError::None };
    }

    // Creating an approximation which will then be corrected.
    //
    float float_value = static_cast<float>(parse_data.small_value);
    double double_value = static_cast<double>(float_value);
    if (e > 0) {
        if (parse_data.exp > 39) {
            return { 0.0f, FloatParseError::Overflow };
        }

        if ((e & 15) != 0) {
            double_value *= POW10_DOUBLE_TABLE[e & 15];
        }
        e >>= 4;
        if (e != 0) {
            for (size_t i = 0; e > 0; i++, e >>= 1) {
                if ((e & 1) != 0) {
                    double_value *= POW10_DOUBLE_BIG_TABLE[i];
                }
            }
        }
    } else if (e < 0) {
        e = -e;
        if (parse_data.exp < -46) {
            return { 0.0f, FloatParseError::Underflow };
        }
        
        if ((e & 15) != 0) {
            double_value /= POW10_DOUBLE_TABLE[e & 15];
        }
        e >>= 4;
        if (e != 0) {
            for (size_t i = 0; e > 0; i++, e >>= 1) {
                if ((e & 1) != 0) {
                    // We multiply here because the tiny values are
                    // 1/(10^n).
                    double_value *= POW10_DOUBLE_TINY_TABLE[i];
                }
            }
        }
    }

    float single_value = std::max(std::numeric_limits<float>::denorm_min(),
                                  std::min(std::numeric_limits<float>::max(), static_cast<float>(double_value)));

    float result = correct_value(single_value, parse_data);
    if (result == 0.0f) {
        return { 0.0f, FloatParseError::Underflow };
    } else if (result == std::numeric_limits<float>::infinity()) {
        return { 0.0f, FloatParseError::Overflow };
    }

    return { result, FloatParseError::None };
}

std::pair<double, acorn::FloatParseError>
acorn::parse_float64_bits(PageAllocator& allocator, llvm::StringRef text) {

    auto parse_data = parse_float_data(allocator, text, MAX_SMALL_DOUBLE_DIGITS);
    if (parse_data.error != FloatParseError::None) {
        return { 0.0, parse_data.error };
    }

    uint64_t small_value = parse_data.small_value;
    // Exponent value not including the exponent from the small value digits.
    int64_t  e = parse_data.exp - static_cast<int64_t>(parse_data.small_digits_count);

    double double_value = static_cast<double>(parse_data.small_value);

    bool is_negative = parse_data.is_negative;

    uint64_t total_digits = parse_data.small_digits_count + parse_data.big_digits_count;
    if (total_digits < MAX_SMALL_DOUBLE_DIGITS) {
        if (e == 0 && small_value == 0) { // Easiest case just check for zero!
            return { is_negative ? -0.0 : +0.0, FloatParseError::None };
        } else if (e >= 0) {
            // Although the number of small digits may already be the maximum
            // digits it is possible to calculate the best approximation by
            // simple scaling of 10. The reason is that IEEE 754 floating point
            // arithmetic specifies that this number will be rounded to the nearest
            // representable number. Rounding to the nearest even if a tie.
            if (e <= DOUBLE_POW10_TABLE_LIMIT) {
                double_value *= POW10_DOUBLE_TABLE[e];
                return { is_negative ? -double_value : double_value, FloatParseError::None };
            }
            // Same reasoning as above but considering the number of digits
            // in the small value allowing for larger scaling.
            int64_t slop = MAX_SMALL_DOUBLE_DIGITS - parse_data.small_digits_count - 1;
            if (e <= DOUBLE_POW10_TABLE_LIMIT + slop) {
                double_value *= POW10_DOUBLE_TABLE[slop];
                double_value *= POW10_DOUBLE_TABLE[e - slop];
                return { is_negative ? -double_value : double_value, FloatParseError::None };
            }
            // Otherwise continuing to harder cases.
        } else {
            // The exponent will actually take into account where the decimal
            // point is. So it is possible the exponent is negative which can
            // result in values like 1.25
            if (e >= -DOUBLE_POW10_TABLE_LIMIT) {
                double_value /= POW10_DOUBLE_TABLE[-e];
                return { is_negative ? -double_value : double_value, FloatParseError::None };
            }
            // Otherwise continuing to harder cases.
        }
    }

    // Creating an approximation which will then be corrected.
    //
    if (e > 0) {
        if (parse_data.exp > 309) {
            return { 0.0, FloatParseError::Overflow };
        }

        if ((e & 15) != 0) {
            double_value *= POW10_DOUBLE_TABLE[e & 15];
        }
        e >>= 4;
        if (e != 0) {
            size_t i = 0;
            for (; e > 1; i++, e >>= 1) {
                if ((e & 1) != 0) {
                    double_value *= POW10_DOUBLE_BIG_TABLE[i];
                }
            }

            double value_pot_overflow = double_value * POW10_DOUBLE_BIG_TABLE[i];
            // TODO: is relying on std::isinf a good idea?
            if (std::isinf(value_pot_overflow)) {
                value_pot_overflow = value_pot_overflow / 2.0;
                value_pot_overflow *= POW10_DOUBLE_BIG_TABLE[i];
                if (std::isinf(value_pot_overflow)) {
                    return { 0.0, FloatParseError::Overflow };
                }
                value_pot_overflow = std::numeric_limits<double>::max();
            }
            double_value = value_pot_overflow;
        }
    } else if (e < 0) {
        e = -e;
        if (parse_data.exp < -325) {
            return { 0.0, FloatParseError::Underflow };
        }

        if ((e & 15) != 0) {
            double_value /= POW10_DOUBLE_TABLE[e & 15];
        }
        e >>= 4;
        if (e != 0) {
            size_t i = 0;
            for (; e > 1; i++, e >>= 1) {
                if ((e & 1) != 0) {
                    double_value *= POW10_DOUBLE_TINY_TABLE[i];
                }
            }

            double value_pot_underflow = double_value * POW10_DOUBLE_TINY_TABLE[i];
            if (value_pot_underflow == 0.0) {
                value_pot_underflow = double_value * 2.0;
                value_pot_underflow *= POW10_DOUBLE_TINY_TABLE[i];
                if (value_pot_underflow == 0.0) {
                    return { 0.0, FloatParseError::Underflow };
                }
                value_pot_underflow = std::numeric_limits<double>::denorm_min();
            }
            double_value = value_pot_underflow;
        }
    }


    double result = correct_value(double_value, parse_data);
    if (result == 0.0) {
        return { 0.0, FloatParseError::Underflow };
    } else if (result == std::numeric_limits<double>::infinity()) {
        return { 0.0, FloatParseError::Overflow };
    }

    return { result, FloatParseError::None };
}