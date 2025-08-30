#include "ValidLexemesFuzzer.h"

#include <random>

static std::random_device rd;
static std::mt19937 generator(rd());

const char* DIGIT_TO_BASE64_TABLE = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz?=";

static std::vector<char> valid_string_characters;

static std::string create_int_literal(int64_t base, std::string prefix = "") {
    std::string lexeme = "";

    static std::uniform_int_distribution<int32_t> number_dist(INT32_MIN, INT32_MAX);
    static std::uniform_int_distribution<int>     underscore_dist(0, 20);

    int64_t value = static_cast<int64_t>(number_dist(generator));

    if (value == 0) {
        lexeme += prefix + "0";
    } else if (value < 0) {
        lexeme += "-" + prefix;
        value = -value;
    } else {
        lexeme = prefix;
    }

    while (value != 0) {
        int64_t digit = value % base;
        value /= base;

        lexeme += DIGIT_TO_BASE64_TABLE[digit];

        if (value != 0) {
            if (underscore_dist(generator) == 0) {
                lexeme += "_";
            }
        }
    }

    return lexeme;
}

static std::string create_identifier() {
    std::string s = "";

    static std::vector<char> valid_characters = {
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
        'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',
        'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R',
        'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        '_',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'
    };

    static std::uniform_int_distribution<int> identifier_length_dist(1, 50);
    static std::uniform_int_distribution<int> character_dist1(0, valid_characters.size() - 1 - 10);
    static std::uniform_int_distribution<int> character_dist2(0, valid_characters.size() - 1);

    int length = identifier_length_dist(generator);
    for (int i = 0; i < length; i++) {
        int index = i == 0 ? character_dist1(generator) : character_dist2(generator);
        char ch = valid_characters[index];
        s += ch;
    }

    return s;
}

static std::string create_char_literal() {
    static std::uniform_int_distribution<int> dist(0, valid_string_characters.size() - 1);
    char ch = valid_string_characters[dist(generator)];
    std::string s = "'";
    if (ch == '\'' || ch == '\\') {
        s += "\\";
    }
    return s + ch + "'";
}

static std::string create_string_literal() {
    static std::uniform_int_distribution<int> chars_dist(0, valid_string_characters.size() - 1);
    static std::uniform_int_distribution<int> length_dist(0, 30);

    std::string s = "";
    int length = length_dist(generator);

    for (int i = 0; i < length; i++) {
        char ch = valid_string_characters[chars_dist(generator)];
        if (ch == '\"' || ch == '\\') {
            s += "\\";
        }
        s += ch;
    }

    return "\"" + s + "\"";
}

static std::string create_float_literal(int max_whole_digits,
                                        int exp_dist_mean,
                                        int exp_dist_std_dev,
                                        int min_exp_value,
                                        int max_exp_value) {

    std::uniform_int_distribution<int> whole_digit_count_dist(1, max_whole_digits);
    std::uniform_int_distribution<int> fraction_digit_count_dist(1, 40);
    std::uniform_int_distribution<int> digit_dist(0, 9);
    std::uniform_int_distribution<int> has_decimal_dist(0, 100);
    std::uniform_int_distribution<int> has_exponent_dist(0, 2);
    std::normal_distribution<> exponent_value_dist(exp_dist_mean, exp_dist_std_dev);
    std::uniform_int_distribution<int> is_negative_dist(0, 6);

    std::string text = "";
    int negative_chance = is_negative_dist(generator);
    if (negative_chance == 0) {
        text = "-";
    }

    int whole_digits = whole_digit_count_dist(generator);
    for (int j = 0; j < whole_digits; j++) {
        text += digit_dist(generator) + '0';
    }
    int decimal_chance = has_decimal_dist(generator);
    if (decimal_chance != 1) {
        text += ".";
        int fraction_digits = fraction_digit_count_dist(generator);
        for (int j = 0; j < fraction_digits; j++) {
            text += digit_dist(generator) + '0';
        }
    }
    int exponent_chance = has_exponent_dist(generator);
    if (exponent_chance == 1) {
        text += "E";
        int exp_value = static_cast<int>(exponent_value_dist(generator));
        exp_value = std::max(min_exp_value, std::min(max_exp_value, exp_value));
        text += std::to_string(exp_value);
    }

    return text;
}

void fuzzer_valid_lexemes(std::ostream& ostream, acorn::Compiler& compiler) {
    const int OUTPUT_TOKENS = 10000;

    using TokId = acorn::Token;

    std::vector<uint16_t> token_ids = {
        '!', '$', '%', '&', '(', ')', '*', '+', ',', '-',
        '.', '/', ':', ';', '<', '=', '>', '?', '[', ']',
        '^', '{', '}', '|', '~'
    };

    for (char c = 32; c <= 126; c++) {
        valid_string_characters.push_back(c);
    }

    for (uint16_t id = TokId::KEYWORD_START + 1; id < TokId::KeywordEnd; id++) {
        if (id == TokId::COMPTIME_KEYWORD_START || id == TokId::COMPTIME_KEYWORD_END) {
            continue;
        }
        token_ids.push_back(id);
    }

    for (uint16_t id = TokId::KeywordEnd + 1; id < TokId::EOB; id++) {
        switch (id) {
        case TokId::INVALID_STRING_LITERAL:
        case TokId::INVALID_CHAR_LITERAL:
        case TokId::INVALID_NUMBER_LITERAL:
        case TokId::POST_ADD_ADD:
        case TokId::POST_SUB_SUB:
            break;
        default:
            token_ids.push_back(id);
        }
    }

    std::uniform_int_distribution<int> whitespace_dist(1, 4);
    auto output_whitespace = [&whitespace_dist, &ostream]() {
        int count = whitespace_dist(generator);
        for (int i = 0; i < count; i++) {
            ostream << " ";
        }
    };

    std::uniform_int_distribution<uint16_t> dist(0, static_cast<uint16_t>(token_ids.size() - (size_t)1));
    for (int i = 0; i < OUTPUT_TOKENS; i++) {
        if (i % 15 == 0 && i != 0) {
            ostream << "\n";
        }

        uint16_t id = token_ids[dist(generator)];

        if (id < TokId::UNIQUE_TOKENS) {
            ostream << (char) id;
            output_whitespace();
            continue;
        }

        std::string lexeme = "";
        switch (id) {
        case TokId::KW_INT:     case TokId::KW_INT8:     case TokId::KW_INT16:   case TokId::KW_INT32:
        case TokId::KW_INT64:   case TokId::KW_UINT8:    case TokId::KW_UINT16:  case TokId::KW_UINT32:
        //case TokId::KwUInt64:  case TokId::KwISize:    case TokId::KwUSize:   case TokId::KwFloat32:
        //case TokId::KwFloat64: case TokId::KwBool:     case TokId::KwVoid:    case TokId::KwChar:
        //case TokId::KwConst:   case TokId::KwAuto:
        case TokId::KW_AS:      case TokId::KW_IF:       case TokId::KW_ELFIF:    case TokId::KW_ELSE:
        case TokId::KW_IMPORT:  case TokId::KW_CONTINUE: case TokId::KW_BREAK:   case TokId::KW_SWITCH:
        case TokId::KW_CASE:    case TokId::KW_STATIC:   case TokId::KW_STRUCT:  case TokId::KW_ENUM:
        case TokId::KW_THIS:    case TokId::KW_SIZEOF:   case TokId::KW_COPYOBJ: case TokId::KW_MOVEOBJ:
        case TokId::KW_NULL:    case TokId::KW_TRUE:     case TokId::KW_FALSE:
        // These leads to reduce error output since it goes to end of line.
        //case TokId::KwCTIf:    case TokId::KwCTElIf:   case TokId::KwCTElse:  case TokId::KwCTEndIf:
        case TokId::LT_LT:      case TokId::GT_GT:       case TokId::LT_EQ:      case TokId::GT_EQ:
        case TokId::EQ_EQ:      case TokId::EX_EQ:       case TokId::ADD_EQ:     case TokId::SUB_EQ:
        case TokId::MUL_EQ:     case TokId::DIV_EQ:      case TokId::MOD_EQ:     case TokId::AND_EQ:
        case TokId::OR_EQ:      case TokId::CARET_EQ:    case TokId::TILDE_EQ:   case TokId::LT_LT_EQ:
        case TokId::GT_GT_EQ:    case TokId::ADD_ADD:     case TokId::SUB_SUB:    case TokId::SUB_SUB_SUB:
        case TokId::AND_AND:    case TokId::OR_OR:       case TokId::RANGE_EQ:   case TokId::RANGE_LT:
        //case TokId::DotDot:    case TokId::DotDotDot:  case TokId::ColCol:    case TokId::BackslashBackslash: {
            //lexeme = acorn::token_kind_to_string(*compiler.get_context(), id);
            break;
        }
        //case TokId::IntLiteral:
        //    lexeme = create_int_literal(10ull);
        //    break;
        //case TokId::HexLiteral:
        //    lexeme = create_int_literal(16ull, "0x");
        //    break;
        //case TokId::BinLiteral:
        //    lexeme = create_int_literal(2ull, "0b");
        //    break;
        //case TokId::OctLiteral:
        //    lexeme = create_int_literal(8ull, "0");
        //    break;
        //case TokId::Identifier:
        //    lexeme = create_identifier();
        //    break;
        //case TokId::CharLiteral:
        //    lexeme = create_char_literal();
        //    break;
        //case TokId::StringLiteral:
        //    lexeme = create_string_literal();
        //    break;
        //case TokId::FloatLiteral: {
        //    int max_whole_digits = 8;
        //    int min_exp_value = -45;
        //    int max_exp_value = 38;
        //    int exp_dist_mean = 6;
        //    int exp_dist_std_dev = 10;
        //    lexeme = create_float_literal(max_whole_digits, exp_dist_mean, exp_dist_std_dev, min_exp_value, max_exp_value);
        //    lexeme += "f";
        //    break;
        //}
        //case TokId::DoubleLiteral: {
        //    int max_whole_digits = 16;
        //    int min_exp_value = -324;
        //    int max_exp_value = 308;
        //    double exp_dist_mean = 50;
        //    double exp_dist_std_dev = 95;
        //    lexeme = create_float_literal(max_whole_digits, exp_dist_mean, exp_dist_std_dev, min_exp_value, max_exp_value);
        //    break;
        //}
        /*
        case TokId::String16BitLiteral:
        case TokId::String32BitLiteral:*/
        // /default:
        // /    break;
        // /}
// /
        // /if (!lexeme.empty()) {
        // /    ostream << lexeme;
        // /    output_whitespace();
        // /}
    }
}