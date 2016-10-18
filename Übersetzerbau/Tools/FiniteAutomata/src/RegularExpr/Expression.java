package RegularExpr;

import FiniteAutomata.NFA;

public interface Expression {

  public NFA toNFA(boolean debugOutput);

  public default void printDebug(NFA nfa) {
    System.out.println("[*] " + this.getClass().getSimpleName() + " with L( " + this + " ):");
    System.out.println(nfa);
  }
}
