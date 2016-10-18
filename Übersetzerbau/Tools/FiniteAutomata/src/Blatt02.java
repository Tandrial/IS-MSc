import java.io.IOException;

import FiniteAutomata.DFA;
import FiniteAutomata.FiniteAutomata;
import FiniteAutomata.NFA;
import Parser.ParseException;
import Parser.Parser;
import RegularExpr.Expression;

public class Blatt02 {

  public static void solve(String expr, String label) throws IOException, ParseException {
    System.out.println("+------------------------------------------------------------------------------+");
    System.out.println("|##############################################################################|");
    System.out.println("|##############################################################################|");
    System.out.println("|##############################################################################|");
    System.out.println("+------------------------------------------------------------------------------+\n");
    System.out.println("[+] Parsing expression");
    System.out.println("    in  = " + expr);
    Expression e = Parser.parse(expr);
    if (expr.equals(e.toString())) {
      System.out.println("[+] Parse successful");
      System.out.println("  P(in) = " + e.toString());
    } else {
      System.out.println("[+] Parse failed");
      System.out.println("\t P(in) = " + e.toString());
      return;
    }
    System.out.println("[+] Building NFA");
    NFA nfa = e.toNFA(true);
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
      FiniteAutomata.saveToFile(fas, graphNames, label, label + e.toString());

      String dotLocation;
      if (System.getProperty("os.name").equals("Windows 7"))
        dotLocation = "C:\\grahpviz\\bin\\dot.exe";
      else
        dotLocation = "dot";

      Runtime.getRuntime().exec(dotLocation + String.format(" %1$s.dot -Tpng -o %1$s.png", label));
      System.out.println("[+] Done\n");
    }
  }

  public static void main(String[] args) throws IOException, ParseException {
    solve("(a|b)*", "Aufgabe_2_4_a_");
    solve("((a|ε)b*)*", "Aufgabe_2_4_b_");
    solve("(a|b)*abb(a|b)*", "Aufgabe_2_4_c_");
    solve("(a|b)*a(a|b)(a|b)", "Aufgabe_2_4_d_");
  }
}
