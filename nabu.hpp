#pragma once

#include <algorithm>
#include <optional>
#include <string>
#include <vector>

#include <bestd/variant.hpp>
#include <bestd/optional.hpp>
#include <bestd/tuple.hpp>

namespace nabu {

// Special optional with semantic difference
template <typename T>
struct maybe_ext : bestd::optional <T> {
	using bestd::optional <T> ::optional;
};

template <typename T>
struct is_maybe_ext_base : std::false_type {};

template <typename T>
struct is_maybe_ext_base <maybe_ext <T>> : std::true_type {};


template <typename T>
concept is_maybe_ext = is_maybe_ext_base <T> ::value;

}

namespace bestd {

// Value preserving...
template <typename T>
struct is_optional_base <nabu::maybe_ext <T>> : std::true_type {
	// ... so inner is same type
	using inner_t = nabu::maybe_ext <T>;
};

}

namespace nabu {

// Space of acceptable functions
template <typename T>
struct signature : std::false_type {};

// Function handles :)
template <typename R, typename ... Args>
struct signature <R (Args...)> : std::true_type {
	using result_t = R;
	
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

	template <typename F>
	static constexpr bool feedable() {
		return std::is_invocable_v <F, R>;
	}
};

// Also function handles... :|
template <typename R, typename ... Args>
struct signature <R (*)(Args...)> : std::true_type {
	using result_t = R;
	
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

	template <typename F>
	static constexpr bool feedable() {
		return std::is_invocable_v <F, R>;
	}
};

// Pointers to functions (for deferred definitions) :>
template <typename R, typename ... Args>
struct signature <R (**)(Args...)> : std::true_type {
	using result_t = R;
	
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

	template <typename F>
	static constexpr bool feedable() {
		return std::is_invocable_v <F, R>;
	}
};

// Reference to pointer to function... :(
template <typename R, typename ... Args>
struct signature <R (**&)(Args...)> : std::true_type {
	using result_t = R;
	
	static auto accepts(Args ... args) {}

	template <R (**&g)(Args...)>
	static auto replica(Args ... args) {
		return (*g)(args...);
	}
	
	template <auto f, R (**&g)(Args...)>
	static auto feed(Args ... args) {
		auto &fr = signature <decltype(f)> ::template replica <f>;
		return fr((*g)(args...));
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

template <typename F>
concept optional_returner = invocable <F> && bestd::is_optional <typename signature <F> ::result_t>;

template <typename T, typename... Rest>
constexpr bool all_same = (sizeof...(Rest) == 0) || (std::is_same_v <T, Rest> || ...);

//////////////////////
// Lexing utilities //
//////////////////////

template <size_t N>
struct string_literal {
	char value[N];

	constexpr string_literal(const char (&str)[N]) {
		std::copy_n(str, N, value);
	}
};

template <size_t N>
bestd::optional <std::string> raw_expanded(const string_literal <N> &raw, const std::string &source, size_t &i)
{
	for (size_t n = 0; n < N - 1; n++) {
		if (source[n + i] != raw.value[n])
			return std::nullopt;
	}

	i += N - 1;

	return "?";
}

template <string_literal s, typename T>
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

template <typename F>
concept lexer_fn = optional_returner <F>
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
	return !has_duplicates <typename bestd::is_optional_base <typename signature <Fs> ::result_t> ::inner_t...> ();
}

template <lexer_fn ... Fs>
struct lexer_group {
	static_assert(valid_lexer_group <Fs...> (),
		"lexers in lexer_group(...) "
		"must produce distinct types");

	std::tuple <const Fs &...> fs;

	// TODO: variant...
	using element_t = bestd::variant <typename bestd::is_optional_base <typename signature <Fs> ::result_t> ::inner_t...>;
	using result_t = std::vector <element_t>;

	lexer_group(const Fs &... fs_) : fs(fs_...) {}

	// TODO: compare element index...

	template <size_t I>
	bool eval_i(result_t &result, const std::string &source, size_t &i) const {
		size_t old = i;

		if constexpr (I >= sizeof...(Fs)) {
			return false;
		} else {
			auto fv = std::get <I> (fs)(source, i);
			if (fv) {
				auto fvv = fv.value();

				using T = decltype(fvv);
				if (!is_null <T> ::value)
					result.push_back(fv.value());

				return true;
			}

			// Reset for the next case...
			i = old;

			return eval_i <I + 1> (result, source, i);
		}
	}

	bestd::optional <result_t> operator()(const std::string &s, size_t &i) const {
		size_t old = i;

		result_t result;

		while (eval_i <0> (result, s, i)) {}

		if (i == s.size())
			return result;

		i = old;

		return std::nullopt;
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

template <typename F, typename Token>
concept parser_fn = optional_returner <F>
		&& bestd::is_variant <Token>
		&& requires(F f, const std::vector <Token> &tokens, size_t &i) {
	{ signature <F> ::accepts(tokens, i) };
};

template <bestd::is_variant Token, typename T>
requires (Token::template type_index <T> () >= 0)
bestd::optional <T> singlet(const std::vector <Token> &tokens, size_t &i)
{
	if (!tokens[i].template is <T> ())
		return std::nullopt;

	return tokens[i++].template as <T> ();
}

// Possibility of parser
template <typename Token, parser_fn <Token> F>
struct maybe_atom {
	using direct_t = typename signature <F> ::result_t;
	using result_t = typename bestd::is_optional_base <direct_t> ::inner_t;

	template <auto f>
	static maybe_ext <result_t> maybe(const std::vector <Token> &tokens, size_t &i) {
		auto v = signature <F> ::template replica <f> (tokens, i);
		if (!v)
			return std::nullopt;

		return v.value();
	}
};

// Sequences of parsers
template <bestd::is_variant Token, parser_fn <Token> ... Fs>
struct chain_group {
	using chain_result_t = bestd::tuple <typename bestd::is_optional_base <typename signature <Fs> ::result_t> ::inner_t...>;
	
	template <size_t I, auto f, auto ... fs>
	static bool chain_step(chain_result_t &result, const std::vector <Token> &tokens, size_t &i) {
		auto fv = signature <decltype(f)> ::template replica <f> (tokens, i);

		using R = decltype(fv);

		if constexpr (is_maybe_ext <R>) {
			// Failure is OK
			if (fv)
				std::get <I> (result) = fv.value();
		} else {
			// Failure means failrue
			if (!fv)
				return false;

			std::get <I> (result) = fv.value();
		}
		
		if constexpr (sizeof...(fs) > 0)
			return chain_step <I + 1, fs...> (result, tokens, i);

		return true;
	}

	template <auto ... fs>
	requires ((std::same_as <decltype(fs), Fs>) && ...)
	static bestd::optional <chain_result_t> chain(const std::vector <Token> &tokens, size_t &i) {
		size_t old = i;

		chain_result_t result;
		if (chain_step <0, fs...> (result, tokens, i))
			return result;

		i = old;

		return std::nullopt;
	}
};

// Options of parsers
template <bestd::is_variant Token, parser_fn <Token> ... Fs>
requires all_same <typename bestd::is_optional_base <typename signature <Fs> ::result_t> ::inner_t...>
struct options_group {
	using option_result_t = typename bestd::is_optional_base <typename signature <std::tuple_element_t <0, std::tuple <Fs...>>> ::result_t> ::inner_t;
	
	template <size_t I, auto f, auto ... fs>
	static bool options_step(option_result_t &result, const std::vector <Token> &tokens, size_t &i) {
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
	static bestd::optional <option_result_t> options(const std::vector <Token> &tokens, size_t &i) {
		size_t old = i;

		option_result_t result;
		if (options_step <0, fs...> (result, tokens, i))
			return result;

		i = old;

		return std::nullopt;
	}
};

// Loops of parsers
template <bestd::is_variant Token, parser_fn <Token> F, typename D, bool EmptyOk>
struct loop_group {
	using loop_result_t = std::vector <typename bestd::is_optional_base <typename signature <F> ::result_t> ::inner_t>;

	template <auto f, auto d>
	requires (std::same_as <decltype(f), F>) && (std::same_as <decltype(d), D>)
	static bestd::optional <loop_result_t> loop(const std::vector <Token> &tokens, size_t &i) {
		size_t old = i;

		loop_result_t result;

		while (true) {
			auto fv = signature <F> ::template replica <f> (tokens, i);
			if (!fv) {
				if (result.empty() && !EmptyOk) {
					i = old;
					return std::nullopt;
				}

				break;
			}

			result.push_back(fv.value());
			
			// No delimeter
		}

		return result;
	}
};

template <bestd::is_variant Token, parser_fn <Token> F, parser_fn <Token> D, bool EmptyOk>
struct loop_group <Token, F, D, EmptyOk> {
	using loop_result_t = std::vector <typename bestd::is_optional_base <typename signature <F> ::result_t> ::inner_t>;

	template <auto f, auto d>
	requires (std::same_as <decltype(f), F>) && (std::same_as <decltype(d), D>)
	static bestd::optional <loop_result_t> loop(const std::vector <Token> &tokens, size_t &i) {
		size_t old = i;

		loop_result_t result;

		while (true) {
			auto fv = signature <F> ::template replica <f> (tokens, i);
			if (!fv) {
				if (result.empty() && !EmptyOk) {
					i = old;
					return std::nullopt;
				}

				break;
			}

			result.push_back(fv.value());

			// With delimeter
			auto dv = signature <D> ::template replica <d> (tokens, i);
			if (!dv)
				break;
		}

		return result;
	}
};

template <typename Token, auto f>
auto &maybe = maybe_atom <Token, decltype(f)> ::template maybe <f>;

template <bestd::is_variant Token, auto ... fs>
auto &chain = chain_group <Token, decltype(fs)...> ::template chain <fs...>;

template <bestd::is_variant Token, auto ... fs>
auto &options = options_group <Token, decltype(fs)...> ::template options <fs...>;

template <bestd::is_variant Token, auto f, auto d, bool e>
auto &loop = loop_group <Token, decltype(f), decltype(d), e> ::template loop <f, d>;

template <typename R, typename I, size_t ... Is>
bestd::optional <R> transform(const bestd::optional <I> &r)
{
	if (r) {
		auto v = r.value();
		if constexpr (sizeof...(Is) > 0)
			return R(std::get <Is> (v)...);
		else
			return R(v);
	}

	return std::nullopt;
}

template <invocable F>
struct extension {};

template <optional_returner F>
struct extension <F> {
	using inner_t = typename bestd::is_optional_base <typename signature <F> ::result_t> ::inner_t;

	// For non-tuple types
	template <auto f, typename T, size_t ... Is>
	static constexpr auto convert = compose <transform <T, inner_t, Is...>, f>;
};

template <auto f, typename T, size_t ... Is>
constexpr auto convert = extension <decltype(f)> ::template convert <f, T, Is...>;
	
template <bestd::is_variant Token, typename T>
using production = bestd::optional <T> (*)(const std::vector <Token> &, size_t &);

}

// Importing symbols to active namespace
#define NABU_UTILITIES(Token)					\
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
	template <auto f, auto d, bool b>			\
	auto &loop = nabu::loop <Token, f, d, b>;		\
								\
	template <auto f, typename T, size_t ... Is>		\
	auto &convert = nabu::convert <f, T, Is...>;		\
								\
	template <typename T>					\
	using production = nabu::production <Token, T>;
