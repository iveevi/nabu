#pragma once

#include <algorithm>
#include <charconv>
#include <memory>
#include <optional>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include <fast_float/include/fast_float/fast_float.h>

#include <bestd/variant.hpp>
#include <bestd/optional.hpp>
#include <bestd/tuple.hpp>

namespace nabu {

// Space of acceptable functions
template <typename T>
struct signature : std::false_type {};

// Function handles :)
template <typename R, typename ... Args>
struct signature <R (Args...)> : std::true_type {
	using result_type = R;

	static auto accepts(Args ... args) {}

	template <R (*g)(Args...)>
	static auto replica(Args ... args) {
		return g(args...);
	}

	template <auto f, R (*g)(Args...)>
	static auto feed(Args ... args) {
		auto &fr = signature <decltype(f)> ::template replica <f>;
		return fr(g(args...));
	}

	template <auto a, auto b, R (*g)(Args...)>
	static R wrap(Args ... args) {
		a(args...);
		R r = g(args...);
		b(r, args...);
		return r;
	}

	template <typename F>
	static constexpr bool feedable() {
		return std::is_invocable_v <F, R>;
	}
};

// Also function handles... :|
template <typename R, typename ... Args>
struct signature <R (*)(Args...)> : std::true_type {
	using result_type = R;

	static auto accepts(Args ... args) {}

	template <R (*g)(Args...)>
	static R replica(Args ... args) {
		return g(args...);
	}

	template <auto f, R (*g)(Args...)>
	static auto feed(Args ... args) {
		auto &fr = signature <decltype(f)> ::template replica <f>;
		return fr(g(args...));
	}

	template <auto a, auto b, R (*g)(Args...)>
	static R wrap(Args ... args) {
		a(args...);
		R r = g(args...);
		b(r, args...);
		return r;
	}

	template <typename F>
	static constexpr bool feedable() {
		return std::is_invocable_v <F, R>;
	}
};

// Pointers to functions (for deferred definitions) :>
template <typename R, typename ... Args>
struct signature <R (**)(Args...)> : std::true_type {
	using result_type = R;

	static auto accepts(Args ... args) {}

	template <R (**g)(Args...)>
	static auto replica(Args ... args) {
		return (*g)(args...);
	}

	template <auto f, R (**g)(Args...)>
	static auto feed(Args ... args) {
		auto &fr = signature <decltype(f)> ::template replica <f>;
		return fr((*g)(args...));
	}

	template <auto a, auto b, R (**g)(Args...)>
	static R wrap(Args ... args) {
		a(args...);
		R r = (*g)(args...);
		b(r, args...);
		return r;
	}

	template <typename F>
	static constexpr bool feedable() {
		return std::is_invocable_v <F, R>;
	}
};

template <typename T>
concept invocable = signature <T> ::value;

template <typename F, typename G>
concept composable = invocable <F> && invocable <G> && (signature <G> ::template feedable <F> ());

template <auto f, auto g>
requires composable <decltype(f), decltype(g)>
auto &compose = signature <decltype(g)> ::template feed <f, g>;

template <auto a, auto b, auto g>
auto &wrap = signature <decltype(g)> ::template wrap <a, b, g>;

template <typename T, typename... Rest>
constexpr bool all_same = (sizeof...(Rest) == 0) || (std::is_same_v <T, Rest> || ...);

//////////////////////
// Lexing utilities //
//////////////////////

template <size_t N>
struct cstring {
	char value[N];

	constexpr cstring(const char (&str)[N]) {
		std::copy_n(str, N, value);
	}
};

template <size_t N>
bestd::optional <std::string> raw_expanded(const cstring <N> &raw, const std::string &source, size_t &i)
{
	for (size_t n = 0; n < N - 1; n++) {
		if (source[n + i] != raw.value[n])
			return std::nullopt;
	}

	i += N - 1;

	return "?";
}

template <cstring s, typename T>
bestd::optional <T> raw(const std::string &source, size_t &i)
{
	if (auto r = raw_expanded(s, source, i)) {
		if constexpr (std::is_constructible_v <T, std::string>)
			return T(r.value());
		else
			return T();
	}

	return std::nullopt;
}

// Reserved type for no values
template <size_t = 0>
struct null {};

template <typename T>
struct is_null : std::false_type {};

template <size_t I>
struct is_null <null <I>> : std::true_type {};

// Concept for lexer functions
template <typename F>
concept lexer_fn = invocable <F>
		&& bestd::is_optional <typename signature <F> ::result_type>
		&& requires(const F &f,
			    const std::string &source,
			    size_t &i) {
	{ f(source, i) };
};

template <typename T, typename... Rest>
constexpr bool has_duplicates()
{
	if constexpr (sizeof...(Rest) == 0)
		return false;
	else
		return (std::is_same_v <T, Rest> || ...) || has_duplicates <Rest...> ();
}

template <lexer_fn ... Fs>
constexpr bool valid_lexer_group()
{
	return !has_duplicates <typename signature <Fs> ::result_type::value_type...> ();
}

// Token with source range
struct source_tracked_check_t {};

using source_reference = std::shared_ptr <std::string>;

struct source_tracked_t {
	source_reference source;

	size_t line_begin;
	size_t column_begin;

	size_t line_end;
	size_t column_end;

	using check = source_tracked_check_t;
};

// Concept for source tracked objects
template <typename T>
concept is_source_tracked = std::same_as <typename T::check, source_tracked_check_t>;

struct token_check_t {};

template <typename ... Ts>
struct token_t : bestd::variant <Ts...>, source_tracked_t {
	using Super = bestd::variant <Ts...>;
	using Super::Super;

	using check = token_check_t;
};

// Concept for generated token types
template <typename T>
concept is_token = std::same_as <typename T::check, token_check_t>;

template <lexer_fn ... Fs>
struct lexer_group {
	static_assert(valid_lexer_group <Fs...> (),
		"lexers in lexer_group(...) "
		"must produce distinct types");

	std::tuple <const Fs &...> fs;

	using element_t = token_t <typename signature <Fs> ::result_type::value_type...>;
	using result_t = std::vector <element_t>;
	using line_table_t = std::vector <size_t>;

	lexer_group(const Fs &... fs_) : fs(fs_...) {}

	template <size_t I>
	bool eval_i(
		result_t &result,
		const std::string &source,
		const line_table_t &table,
		const source_reference &sref,
		size_t &prev,
		size_t &i
	) const {
		size_t old = i;

		if constexpr (I >= sizeof...(Fs)) {
			return false;
		} else {
			auto fv = std::get <I> (fs)(source, i);
			if (fv) {
				auto fvv = fv.value();

				using T = decltype(fvv);
				if (!is_null <T> ::value) {
					element_t token = fvv;

					token.source = sref;

					auto begin = line_table_get(table, prev + 1);
					token.line_begin = begin.first;
					token.column_begin = begin.second;

					auto end = line_table_get(table, i - 1);
					token.line_end = end.first;
					token.column_end = end.second;

					result.push_back(token);
				}

				// Update previous to current
				prev = old;

				return true;
			}

			// Reset for the next case...
			i = old;

			return eval_i <I + 1> (result, source, table, sref, prev, i);
		}
	}

	bestd::optional <result_t> operator()(const std::string &name, const std::string &s, size_t &i, bool failok = false) const {
		size_t old = i;
		size_t prev = i;
		result_t result;
		line_table_t table = line_table(s);
		source_reference sref = std::make_shared <std::string> (name);
		while (eval_i <0> (result, s, table, sref, prev, i)) {}

		if (failok)
			return result;

		if (i == s.size())
			return result;

		i = old;

		return std::nullopt;
	}

	static line_table_t line_table(const std::string &s) {
		line_table_t table;
		for (size_t i = 0; i < s.size(); i++) {
			if (s[i] == '\n')
				table.push_back(i);
		}

		if (s.back() != '\n')
			table.push_back(s.size() - 1);

		return table;
	}

	static std::pair <size_t, size_t> line_table_get(const line_table_t &table, size_t idx) {
		for (size_t i = 0; i < table.size(); i++) {
			if (table[i] >= idx) {
				size_t begin = (i > 0) ? table[i - 1] : 0;
				return std::make_pair(i + 1, idx - begin);
			}
		}

		return std::make_pair(-1, -1);
	}
};

template <lexer_fn ... Fs>
auto lexer(Fs &... args)
{
	return lexer_group(args...);
}

///////////////////////
// Parsing utilities //
///////////////////////

// Source tracking parsed elements
struct parsed_check_t {};

template <typename T>
struct parsed_t : bestd::optional <T>, source_tracked_t {
	using bestd::optional <T> ::optional;

	static parsed_t <T> from(const T &value,
			const source_tracked_t &a,
			const source_tracked_t &b) {
		parsed_t <T> result = value;
		result.source = a.source;
		result.line_begin = a.line_begin;
		result.column_begin = a.column_begin;
		result.line_end = b.line_end;
		result.column_end = b.column_end;
		return result;
	}

	using check_parsed = parsed_check_t;
};

template <typename T>
concept is_parsed_result = std::same_as <typename T::check_parsed, parsed_check_t>;

// Special optional with semantic difference
struct maybe_check_t {};

template <typename T>
struct maybe_t : bestd::optional <T> {
	using bestd::optional <T> ::optional;

	using check_maybe = maybe_check_t;
};

template <typename T>
concept is_maybe = std::same_as <typename T::check_maybe, maybe_check_t>;

// Parsing context contains tokens and cached information
using FunctionAddress = void *;
using TableReference = std::shared_ptr <void>;

template <is_token Token>
struct parsing_context : std::vector <Token> {
	using base = std::vector <Token>;

	std::unordered_map <FunctionAddress, TableReference> tables;

	parsing_context(const base &tokens) : base(tokens) {}

	template <typename T>
	parsed_t <T> parsed_result(const T &value, size_t begin, size_t end) const {
		size_t current = (end > 0) ? (end - 1) : 0;
		auto &token_begin = (*this)[begin];
		auto &token_end = (*this)[current];
		return parsed_t <T> ::from(value, token_begin, token_end);
	}
};

// Acceptable parser function signatures
template <typename F, typename Token>
concept parser_fn = invocable <F>
		&& is_parsed_result <typename signature <F> ::result_type>
		&& is_token <Token>
		&& requires(F f, parsing_context <Token> &tokens, size_t &i) {
	{ signature <F> ::accepts(tokens, i) };
};

// Memoization table structure
template <is_token Token, parser_fn <Token> F>
struct memoization_table {
	using T = typename signature <F> ::result_type::value_type;

	struct entry {
		bool success;
		size_t end;
		parsed_t <T> value;
	};

	std::unordered_map <size_t, entry> memoized;

	bool contains(size_t begin) const {
		return memoized.contains(begin);
	}

	const parsed_t <T> &get(size_t begin, size_t &current) const {
		auto &entry = memoized.at(begin);
		if (entry.success)
			current = entry.end;
		return entry.value;
	}

	parsed_t <T> success(const parsing_context <Token> &context,
			size_t begin,
			size_t end,
			const T &value) {
		auto result = context.parsed_result(value, begin, end);
		memoized.emplace(begin, entry(true, end, result));
		return result;
	}

	std::nullopt_t fail(size_t begin, size_t &current) {
		memoized.emplace(begin, entry(false, -1, std::nullopt));
		current = begin;
		return std::nullopt;
	}
};

// Table access
template <is_token Token, parser_fn <Token> F>
auto table_for(parsing_context <Token> &tokens, F *ptr)
{
	using table = memoization_table <Token, F>;

	// Automatically construct the table
	auto addr = FunctionAddress(ptr);

	auto it = tokens.tables.find(addr);

	if (it == tokens.tables.end()) {
		auto ref = TableReference(new table());
		tokens.tables.emplace(addr, ref);
		it = tokens.tables.find(addr);
	}

	return reinterpret_cast <table *> (it->second.get());
}

// Terminal states
template <is_token Token, typename T>
requires (Token::template type_index <T> () >= 0)
parsed_t <T> singlet(parsing_context <Token> &context, size_t &i)
{
	size_t old = i;
	if (i >= context.size() || !context[i].template is <T> ())
		return std::nullopt;

	auto value = context[i++].template as <T> ();
	return context.parsed_result(value, old, i);
}

// Possibility of parser
template <typename Token, parser_fn <Token> F>
struct maybe_atom {
	using result_type = typename signature <F> ::result_type::value_type;

	template <auto f>
	static maybe_t <result_type> maybe(parsing_context <Token> &context, size_t &i) {
		size_t old = i;

		auto v = signature <F> ::template replica <f> (context, i);
		if (!v)
			return std::nullopt;

		return context.parsed_result(v.value(), old, i);
	}
};

// Sequences of parsers
template <is_token Token, parser_fn <Token> ... Fs>
struct chain_group {
	using result_type = bestd::tuple <typename signature <Fs> ::result_type::value_type...>;

	template <size_t I, auto f, auto ... fs>
	static bool chain_step(result_type &result, parsing_context <Token> &context, size_t &i) {
		auto fv = signature <decltype(f)> ::template replica <f> (context, i);

		using R = decltype(fv);

		if constexpr (is_maybe <R>) {
			// Failure is OK
			if (fv)
				std::get <I> (result) = fv.value();
		} else {
			// Failure means failure
			if (!fv)
				return false;

			std::get <I> (result) = fv.value();
		}

		if constexpr (sizeof...(fs) > 0)
			return chain_step <I + 1, fs...> (result, context, i);

		return true;
	}

	template <auto ... fs>
	requires ((std::same_as <decltype(fs), Fs>) && ...)
	static parsed_t <result_type> chain(parsing_context <Token> &context, size_t &i) {
		size_t old = i;

		auto table = table_for(context, chain <fs...>);
		if (table->contains(old))
			return table->get(old, i);

		result_type result;
		if (chain_step <0, fs...> (result, context, i))
			return table->success(context, old, i, result);

		return table->fail(old, i);
	}
};

// Options of parsers
template <is_token Token, parser_fn <Token> ... Fs>
requires all_same <typename signature <Fs> ::result_type::value_type...>
struct options_group {
	using head = std::tuple_element_t <0, std::tuple <Fs...>>;
	using result_type = typename signature <head> ::result_type::value_type;

	template <size_t I, auto f, auto ... fs>
	static bool options_step(result_type &result, parsing_context <Token> &tokens, size_t &i) {
		auto fv = signature <decltype(f)> ::template replica <f> (tokens, i);
		if (fv) {
			result = fv.value();
			return true;
		}

		if constexpr (sizeof...(fs) > 0)
			return options_step <I + 1, fs...> (result, tokens, i);

		return false;
	}

	template <auto ... fs>
	requires ((std::same_as <decltype(fs), Fs>) && ...)
	static parsed_t <result_type> options(parsing_context <Token> &context, size_t &i) {
		size_t old = i;

		auto table = table_for(context, options <fs...>);
		if (table->contains(old))
			return table->get(old, i);

		result_type result;
		if (options_step <0, fs...> (result, context, i))
			return table->success(context, old, i, result);

		return table->fail(old, i);
	}
};

// Loops of parsers
template <is_token Token, parser_fn <Token> F, typename D, size_t Min>
struct loop_group {
	using loop_result_t = std::vector <typename signature <F> ::result_type::value_type>;

	template <auto f, auto d>
	requires (std::same_as <decltype(f), F>) && (std::same_as <decltype(d), D>)
	static parsed_t <loop_result_t> loop(parsing_context <Token> &context, size_t &i) {
		size_t old = i;

		loop_result_t result;

		while (true) {
			auto fv = signature <F> ::template replica <f> (context, i);
			if (!fv)
				break;

			result.push_back(fv.value());

			// No delimeter
		}

		if (result.size() < Min) {
			i = old;
			return std::nullopt;
		}

		return context.parsed_result(result, old, i);
	}
};

template <is_token Token, parser_fn <Token> F, parser_fn <Token> D, size_t Min>
struct loop_group <Token, F, D, Min> {
	using result_type = std::vector <typename signature <F> ::result_type::value_type>;

	template <auto f, auto d>
	requires (std::same_as <decltype(f), F>) && (std::same_as <decltype(d), D>)
	static parsed_t <result_type> loop(parsing_context <Token> &context, size_t &i) {
		size_t old = i;
		size_t last = i;

		result_type result;

		while (true) {
			auto fv = signature <F> ::template replica <f> (context, i);
			if (!fv) {
				i = last;
				break;
			}

			result.push_back(fv.value());
			last = i;

			// With delimeter
			auto dv = signature <D> ::template replica <d> (context, i);
			if (!dv)
				break;
		}

		if (result.size() < Min) {
			i = old;
			return std::nullopt;
		}

		return context.parsed_result(result, old, i);
	}
};

template <typename Token, auto f>
auto &maybe = maybe_atom <Token, decltype(f)> ::template maybe <f>;

template <is_token Token, auto ... fs>
auto &chain = chain_group <Token, decltype(fs)...> ::template chain <fs...>;

template <is_token Token, auto ... fs>
auto &options = options_group <Token, decltype(fs)...> ::template options <fs...>;

template <is_token Token, auto f, auto d, size_t min>
auto &loop = loop_group <Token, decltype(f), decltype(d), min> ::template loop <f, d>;

template <typename R, typename T, size_t ... Is>
parsed_t <R> transform(const parsed_t <T> &r)
{
	if (r) {
		const T &v = r.value();

		parsed_t <R> result;
		if constexpr (sizeof...(Is) > 0)
			result = R(std::get <Is> (v)...);
		else
			result = R(v);

		result.source = r.source;
		result.line_begin = r.line_begin;
		result.column_begin = r.column_begin;
		result.line_end = r.line_end;
		result.column_end = r.column_end;
		if constexpr (is_source_tracked <R>) {
			result->source = r.source;
			result->line_begin = r.line_begin;
			result->column_begin = r.column_begin;
			result->line_end = r.line_end;
			result->column_end = r.column_end;
		}

		return result;
	}

	return std::nullopt;
}

template <typename T, size_t I>
auto select(const parsed_t <T> &r)
{
	using U = std::decay_t <decltype(r.value())> ::standard;
	using V = std::tuple_element_t <I, U>;
	using R = parsed_t <V>;

	if (r) {
		auto v = r.value();
		return R(std::get <I> (v));
	}

	return R(std::nullopt);
}

// Debugging the parsing process
inline thread_local bool debugger_enabled = false;
inline thread_local uint32_t debugger_nesting = 0;
inline thread_local std::optional <size_t> debugger_silenced;

inline void debugger(bool b)
{
	debugger_enabled = b;
}

template <typename Token, cstring name, bool silence>
void debugger_head(const parsing_context <Token> &, const size_t &i)
{
	if (!debugger_enabled)
		return;

	debugger_nesting++;
	if (!debugger_silenced) {
		std::string indents(4 * (debugger_nesting - 1), ' ');
		for (uint32_t i = 0; i + 1 < debugger_nesting; i++)
			indents[4 * i] = '|';
		printf("%s[%s @%zu]%c",
			indents.c_str(),
			name.value, i + 1,
			silence ? ' ' : '\n');

		if constexpr (silence)
			debugger_silenced = debugger_nesting;
	}
}

template <typename Token, typename R, cstring name>
void debugger_tail(const parsed_t <R> &r, const parsing_context <Token> &, const size_t &i)
{
	if (!debugger_enabled)
		return;

	bool display = false;
	bool outnow = false;
	if (!debugger_silenced) {
		display = true;
	} else if (debugger_nesting <= debugger_silenced.value()) {
		debugger_silenced.reset();
		display = true;
		outnow = true;
	}

	debugger_nesting--;
	if (display) {
		std::string indents(4 * debugger_nesting, ' ');
		for (uint32_t i = 0; i < debugger_nesting; i++)
			indents[4 * i] = '|';

		std::string actual = outnow ? "" : indents;

		if (r) {
			const char *source = "?";
			if (r.source)
				source = r.source->c_str();

			printf("%s[passed @%s:%d:%d]\n",
				actual.c_str(),
				source,
				r.line_begin,
				r.column_begin);
		} else {
			printf("%s[failed]\n", actual.c_str());
		}

		printf("%s\n", indents.c_str());
	}
}

// Manifesting derivative functions of parsers
template <is_token Token, parser_fn <Token> F>
struct extension {
	using inner_t = typename signature <F> ::result_type::value_type;

	// For non-tuple types
	template <auto parser, typename T, size_t ... Is>
	static constexpr auto convert = compose <transform <T, inner_t, Is...>, parser>;

	template <auto parser, size_t I>
	static constexpr auto pick = compose <select <inner_t, I>, parser>;

	// For debugging
	template <auto parser, cstring name, bool silence>
	static constexpr auto debug = wrap <
		debugger_head <Token, name, silence>,
		debugger_tail <Token, inner_t, name>,
		parser
	>;
};

template <typename Token, auto f, typename T, size_t ... Is>
constexpr auto convert = extension <Token, decltype(f)> ::template convert <f, T, Is...>;

template <typename Token, auto f, size_t I>
constexpr auto pick = extension <Token, decltype(f)> ::template pick <f, I>;

template <typename Token, auto f, cstring name, bool silence>
constexpr auto debug = extension <Token, decltype(f)> ::template debug <f, name, silence>;

template <is_token Token, typename T>
using production = parsed_t <T> (*)(parsing_context <Token> &, size_t &);

} // namespace nabu

// Importing symbols to active namespace
#define nabu_import_definitions(Token)				\
	template <typename T>					\
	constexpr auto singlet = nabu::singlet <Token, T>;	\
								\
	template <auto ... fs>					\
	auto &maybe = nabu::maybe <Token, fs...>;		\
								\
	template <auto ... fs>					\
	auto &chain = nabu::chain <Token, fs...>;		\
								\
	template <auto ... fs>					\
	auto &options = nabu::options <Token, fs...>;		\
								\
	template <auto f, auto d, size_t min>			\
	auto &loop = nabu::loop <Token, f, d, min>;		\
								\
	template <auto f, typename T, size_t ... Is>		\
	auto &convert = nabu::convert <Token, f, T, Is...>;	\
								\
	template <auto f, size_t I>				\
	auto &pick = nabu::pick <Token, f, I>;				\
									\
	template <auto f, nabu::cstring name, bool silence = false>	\
	auto &debug = nabu::debug <Token, f, name, silence>;		\
									\
	template <typename T>						\
	using production = nabu::production <Token, T>;

namespace nabu {

// Standard collection of lexers
template <size_t Slot>
bestd::optional <nabu::null <Slot>> whitespace(const std::string &source, size_t &i)
{
	if (std::isspace(source[i])) {
		i++;
		return nabu::null <Slot> ();
	}

	return std::nullopt;
}

template <size_t Slot>
bestd::optional <nabu::null <Slot>> cpp_inline_comment(const std::string &source, size_t &i)
{
	if (source[i] == '/' && source[i + 1] == '/') {
		i += 2;
		while (source[i++] != '\n') {}
		return nabu::null <Slot> ();
	}

	return std::nullopt;
}

template <size_t Slot>
bestd::optional <nabu::null <Slot>> cpp_block_comment(const std::string &source, size_t &i)
{
	if (source[i] == '/' && source[i + 1] == '*') {
		i += 2;
		while (true) {
			if (source[i] == '*' && source[i + 1] == '/') {
				i += 2;
				break;
			} else {
				i++;
			}
		}

		return nabu::null <Slot> ();
	}

	return std::nullopt;
}

template <std::integral Integer>
bestd::optional <Integer> integer(const std::string &source, size_t &i)
{
	size_t start = i;

	const size_t n = source.size();
	if (start >= n || !std::isdigit(source[start]))
		return std::nullopt;

	size_t j = start;
	while (j < n && std::isdigit(source[j]))
		j++;

	Integer value = 0;
	auto first = source.data() + start;
	auto last = source.data() + j;
	auto result = std::from_chars(first, last, value);

	if (result.ec == std::errc()) {
		i = j;
		return value;
	} else {
		// Overflow or other error: treat as lex failure
		return std::nullopt;
	}
}

template <std::floating_point Floating>
bestd::optional <Floating> floating(const std::string &source, size_t &i)
{
	size_t start = i;
	const size_t n = source.size();
	size_t j = start;

	bool saw_digit_before_dot = false;
	bool saw_dot = false;
	bool saw_digit_after_dot = false;
	bool saw_exponent = false;
	bool saw_digit_in_exponent = false;

	while (j < n && std::isdigit(source[j])) {
		saw_digit_before_dot = true;
		j++;
	}

	if (j < n && source[j] == '.') {
		saw_dot = true;
		j++;

		while (j < n && std::isdigit(source[j])) {
			saw_digit_after_dot = true;
			j++;
		}
	}

	// Do not conflict with integers...
	if (!saw_dot || (!saw_digit_before_dot && !saw_digit_after_dot))
		return std::nullopt;

	if (j < n && (source[j] == 'e' || source[j] == 'E')) {
		saw_exponent = true;
		size_t exp_pos = j++;
		if (j < n && (source[j] == '+' || source[j] == '-'))
			j++;

		size_t exp_digits_start = j;
		while (j < n && std::isdigit(source[j])) {
			saw_digit_in_exponent = true;
			j++;
		}

		if (!saw_digit_in_exponent) {
			j = exp_pos;
			saw_exponent = false;
		}
	}

	const char *first = source.data() + start;
	const char *last = source.data() + j;
	float value = 0.0;

	fast_float::from_chars_result result = fast_float::from_chars(first, last, value);
	if (result.ec == std::errc()) {
		i = j;
		return value;
	} else {
		return std::nullopt;
	}
}

template <typename Container>
bestd::optional <Container> identifier(const std::string &source, size_t &i)
{
	size_t j = i;

	std::string id;

	char c = source[j];
	if (std::isalpha(c) || c == '_')
		id += c;
	else
		return std::nullopt;

	while (true) {
		c = source[++j];
		if (std::isalpha(c) || std::isdigit(c) || c == '_')
			id += c;
		else
			break;
	}

	i = j;

	return Container(id);
}

template <typename Container>
bestd::optional <Container> double_quote_string(const std::string &source, size_t &i)
{
	if (source[i] == '\"') {
		std::string result;
		while (source[++i] != '\"')
			result += source[i];
		i++;

		return Container(result);
	}

	return std::nullopt;
}

} // namespace nabu
