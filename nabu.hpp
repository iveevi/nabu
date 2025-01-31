#pragma once

#include <functional>
#include <optional>
#include <algorithm>
#include <string>

#include <bestd/variant.hpp>
#include <bestd/optional.hpp>

// Proxy instantiation for functions
#define hacked(T) *reinterpret_cast <T *> ((void *) nullptr)

template <typename T>
concept invocable = requires(const T &t) {
	{ std::function(t) };
};

template <typename F>
struct optional_returner : std::false_type {};

template <typename R, typename ... Args>
struct optional_returner <std::function <bestd::optional <R> (Args...)>> : std::true_type {
	using result = R;
};

template <invocable F>
struct optional_returner <F> : optional_returner <decltype(std::function(hacked(F)))> {};

template <typename F>
constexpr bool optional_returner_v = optional_returner <F> ::value;

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
struct null {};

template <typename F>
concept lexer_fn = optional_returner_v <F>
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
	return !has_duplicates <typename optional_returner <Fs> ::result...> ();
}

template <lexer_fn ... Fs>
struct lexer_group {
	static_assert(valid_lexer_group <Fs...> (),
		"lexers in lexer_group(...) "
		"must produce distinct types");

	// TODO: variant...
	using element_t = bestd::variant <typename optional_returner <Fs> ::result...>;
	using result_t = std::vector <element_t>;

	std::tuple <Fs...> fs;

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
				if (!std::is_same_v <std::decay_t <T>, null>)
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
auto lexer(const Fs &... args)
{
	return lexer_group(std::function(args)...);
}

template <typename F, typename T>
concept parser_fn = optional_returner_v <F>
		&& requires(const F &f,
			    const std::vector <T> &tokens,
			    size_t &i) {
	{ f(tokens, i) };
};

template <typename Token, parser_fn <Token> ... Fs>
struct parser_chain {
	using result_t = std::tuple <typename optional_returner <Fs> ::result...>;

	std::tuple <Fs...> fs;

	parser_chain(const Fs &... fs_) : fs(fs_...) {}

	template <size_t I>
	bool eval_i(result_t &result, const std::vector <Token> &tokens, size_t &i) const {
		if constexpr (I >= sizeof...(Fs)) {
			return true;
		} else {
			auto fv = std::get <I> (fs)(tokens, i);
			if (!fv)
				return false;

			if (!eval_i <I + 1> (result, tokens, i))
				return false;

			std::get <I> (result) = fv.value();

			return true;
		}
	}

	bestd::optional <result_t> operator()(const std::vector <Token> &tokens, size_t &i) const {
		size_t c = i;
		result_t result;

		if (!eval_i <0> (result, tokens, i)) {
			i = c;
			return std::nullopt;
		}

		return result;
	}
};

template <typename Token, parser_fn <Token> F, bool EmptyOk, typename D = void>
struct parser_loop {

};

// With a delimiter
template <typename Token, parser_fn <Token> F, bool EmptyOk, parser_fn <Token> D>
struct parser_loop <Token, F, EmptyOk, D> {
	using result_t = std::vector <typename optional_returner <F> ::result>;

	F f;
	D d;

	parser_loop(const F &f_, const D &d_) : f(f_), d(d_) {}

	bestd::optional <result_t> operator()(const std::vector <Token> &tokens, size_t &i) const {
		size_t c = i;
		result_t result;

		while (true) {
			auto fv = f(tokens, i);
			if (!fv) {
				if (result.empty() && EmptyOk)
					break;

				i = c;
				return std::nullopt;
			}

			result.push_back(fv.value());

			if (!d(tokens, i))
				break;
		}

		return result;
	}
};

template <typename Token, typename T>
requires (Token::template type_index <T> () >= 0)
bestd::optional <T> parse_token(const std::vector <Token> &tokens, size_t &i)
{
	if (!tokens[i].template is <T> ())
		return std::nullopt;

	return tokens[i++].template as <T> ();
}


// Token 'namespace'-d operations for syntactic sugar
template <typename Token>
struct TokenParser {
	template <typename T>
	static auto singlet(const std::vector <Token> &tokens, size_t &i) {
		return parse_token <Token, T> (tokens, i);
	}

	template <typename T>
	struct singlet_promoter {
		static auto value(const T &) {
			return singlet <T>;
		}
	};
	
	template <parser_fn <Token> F>
	struct singlet_promoter <F> {
		static auto value(const F &f) {
			return f;
		}
	};

	template <parser_fn <Token> ... Fs>
	static auto chain(const Fs &... fs) {
		return parser_chain <Token, decltype(std::function(hacked(Fs)))...> (std::function(fs)...);
	}
	
	template <typename ... Fs>
	static auto chain(const Fs &... fs) {
		return chain(singlet_promoter <Fs> ::value(fs)...);
	}

	template <bool EmptyOk, typename F, typename D>
	static auto loop(const F &f, const D &d) {
		auto ff = std::function(singlet_promoter <F> ::value(f));
		auto fd = std::function(singlet_promoter <D> ::value(d));
		return parser_loop <Token, decltype(ff), EmptyOk, decltype(fd)> (ff, fd);
	}
};
