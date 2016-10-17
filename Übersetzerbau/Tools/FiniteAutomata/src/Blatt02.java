import java.io.IOException;

import FiniteAutomata.DFA;
import FiniteAutomata.FiniteAutomata;
import FiniteAutomata.NFA;
import RegularExpr.Alternative;
import RegularExpr.Concat;
import RegularExpr.Expression;
import RegularExpr.Iteration;
import RegularExpr.Literal;

public class Blatt02 {

	public static void solve(Expression expr, String fileNamePrefix, String label) throws IOException {
		System.out.println("[+] Building NFA");
		NFA nfa = expr.toNFA(true);
		System.out.println("[+] NFA done");
		System.out.println(nfa);
		System.out.println("[+] Converting to DFA");
		DFA dfa = nfa.convertToDFA(true);
		System.out.println(dfa);
		System.out.println("[+] Minimizing DFA");
		DFA dfa_min = dfa.minimize(true);
		System.out.println(dfa_min);
		boolean failed = false;
		for (String s : new String[] { "", "aaa", "aaabbab", "abababbbbaa", "bbaabaababbbabababab", "ab", "bb",
				"ababababababababab" }) {
			boolean result = nfa.simulate(s);
			boolean result_dfa = dfa.simulate(s);
			boolean result_dfa_min = dfa_min.simulate(s);
			if (result != result_dfa) {
				System.out.println("[+] Conversion to DFA failed! L(nfa) != L(dfa)");
				failed = true;
			}

			if (result_dfa != result_dfa_min) {
				System.out.println("[+] Minimizing DFA failed! L(dfa) != L(dfa_min)");
				failed = true;
			}
		}
		if (!failed) {
			System.out.println("[+] All tests passed! Conversion and minimization are correct!");
			System.out.println("[+] Creating .dot and .png files of all FAs generated!");
			String[] graphNames = { "DFA_minimiert", "DFA", "NFA" };
			FiniteAutomata[] fas = new FiniteAutomata[] { dfa_min, dfa, nfa };
			FiniteAutomata.saveToFile(fas, graphNames, fileNamePrefix, label);

			String dotLocation;
			if (System.getProperty("os.name").equals("Windows 7"))
				dotLocation = "C:\\grahpviz\\bin\\dot.exe";
			else
				dotLocation = "dot";

			Runtime.getRuntime().exec(dotLocation + String.format(" %1$s.dot -Tpng -o %1$s.png", fileNamePrefix));
			System.out.println("[+] Done\n");
		}
	}

	public static void main(String[] args) throws IOException {
		String lable = "Aufgabe 2.4.a : ((a|ε)b*)*";
		Expression expr = new Iteration(
				new Concat(new Alternative(new Literal('a'), new Literal(NFA.eps)), new Iteration(new Literal('b'))));
		System.out.println(lable);

		solve(expr, "2_4_a", lable);

		System.out.println("+------------------------------------------------------------------------------+");
		System.out.println("|                                                                              |");
		System.out.println("|                                                                              |");
		System.out.println("+------------------------------------------------------------------------------+\n");

		lable = "Aufgabe 2.4.b : (a|b)*abb(a|b)*";
		Expression repaORb = new Iteration(new Alternative(new Literal('a'), new Literal('b')));
		expr = new Concat(repaORb,
				new Concat(new Literal('a'), new Concat(new Literal('b'), new Concat(new Literal('b'), repaORb))));
		System.out.println(lable);

		solve(expr, "2_4_b", lable);
	}
}
