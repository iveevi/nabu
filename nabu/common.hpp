#ifndef COMMON_H_
#define COMMON_H_

#include <string>
#include <vector>
#include <sstream>
#include <cassert>

///////////////////////
// String formatting //
///////////////////////

std::string format(const std::string &source, const std::vector <std::string> &args)
{
	std::ostringstream oss;
	std::istringstream iss(source);

	char c;
	while (!iss.eof() && (c = iss.get()) != EOF) {
		if (c == '@') {
			size_t i = 0;
			while (!iss.eof() && (c = iss.get()) != EOF && isdigit(c))
				i = 10 * i + (c - '0');
			
			// TODO: throw later
			assert(i > 0);
			oss << args[i - 1] << c;
		} else {
			oss << c;
		}
	}

	return oss.str();
}

template <class ... Args>
void _format_gather(std::vector <std::string> &args, std::string str, Args ... strs)
{
	args.push_back(str);
	_format_gather(args, strs...);
}

template <>
void _format_gather(std::vector <std::string> &args, std::string str)
{
	args.push_back(str);
}

template <class ... Args>
std::string format(const std::string &source, Args ... strs)
{
	std::vector <std::string> args;
	_format_gather(args, strs...);
	return format(source, args);
}

////////////////////////////////
// Source generation sources //
////////////////////////////////

namespace sources {

// Static source templates
constexpr const char *basic_expression = R"(template <> struct nabu::rules::rule <@1> : public @2 {};
)";

constexpr const char *custom_expression = R"(template <>
struct nabu::rules::rule <@1> : public @2 {
	static ret value(Feeder *fd) {
		// Predefined values
		ret _val = @2 ::value(fd);
		if (!_val)
			return nullptr;

		// User source
		@3
	}
};
)";

constexpr const char *main = R"(
#include <iostream>

int main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: %s <file>\n", argv[0]);
		return 1;
	}

	// Open file
	nabu::StringFeeder sf = nabu::StringFeeder::from_file(argv[1]);

	// Parse the file and print JSON
	nabu::ret rptr = nabu::rules::kstar <@1> ::value(&sf);
	std::cout << nabu::getrv(rptr).json() << std::endl;
}
)";


constexpr const char *main_no_json = R"(
#include <iostream>

int main(int argc, char *argv[])
{
	if (argc < 2) {
		fprintf(stderr, "Usage: %s <file>\n", argv[0]);
		return 1;
	}

	// Open file
	nabu::StringFeeder sf = nabu::StringFeeder::from_file(argv[1]);

	// Parse the file
	nabu::rules::rule <@1> ::value(&sf);
}
)";

// Headers and copyright
constexpr const char *hrule_tag = R"(
//////////////////////////
// Rule tag definitions //
//////////////////////////
)";

constexpr const char *hrule_name = R"(
/////////////////
// Rule names //
////////////////
)";

constexpr const char *hrule = R"(
///////////////////////
// Rules definitions //
///////////////////////
)";

constexpr const char *copyright = R"(
/**
 * This file was generated by Nabu (https://github.com/vedavamadathil/nabu).
 * 
 * "MIT License
 * 
 * Copyright (c) 2021 Venkataram Edavamadathil Sivaram
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE."
 */
)";

}

#endif