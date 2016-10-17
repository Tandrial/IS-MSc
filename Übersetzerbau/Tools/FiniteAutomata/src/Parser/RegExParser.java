package Parser;

import RegularExpr.Alternative;
import RegularExpr.Concat;
import RegularExpr.Expression;
import RegularExpr.Iteration;
import RegularExpr.Literal;

/* 
 * A parser to be constructed each time
 * a regular expression needs to be parsed.
 * Source: http://matt.might.net/articles/parsing-regex-with-recursive-descent/
 */
public class RegExParser {

	public static Expression parse(String s) {
		RegExParser parser = new RegExParser();
		parser.input = s;
		return parser.regex();
	}

	/* Recursive descent parsing internals. */

	private String input;

	private char peek() {
		return input.charAt(0);
	}

	private void eat(char c) {
		if (peek() == c)
			this.input = this.input.substring(1);
		else
			throw new RuntimeException("Expected: " + c + "; got: " + peek());
	}

	private char next() {
		char c = peek();
		eat(c);
		return c;
	}

	private boolean more() {
		return input.length() > 0;
	}

	/* Regular expression term types. */

	private Expression regex() {
		Expression term = term();

		if (more() && peek() == '|') {
			eat('|');
			Expression regex = regex();
			return new Alternative(term, regex);
		} else {
			return term;
		}
	}

	private Expression term() {
		Expression factor = null;

		while (more() && peek() != ')' && peek() != '|') {
			Expression nextFactor = factor();
			if (factor == null)
				factor = nextFactor;
			else
				factor = new Concat(factor, nextFactor);
		}
		return factor;
	}

	private Expression factor() {
		Expression base = base();

		while (more() && peek() == '*') {
			eat('*');
			base = new Iteration(base);
		}
		return base;
	}

	private Expression base() {

		switch (peek()) {
		case '(':
			eat('(');
			Expression r = regex();
			eat(')');
			return r;

		case '\\':
			eat('\\');
			char esc = next();
			return new Literal(esc);

		default:
			return new Literal(next());
		}
	}
}
