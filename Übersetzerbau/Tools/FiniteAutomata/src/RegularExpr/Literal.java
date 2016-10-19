package RegularExpr;

import FiniteAutomata.NFA;
import FiniteAutomata.State;

public class Literal implements Expression {

  String c;

  public Literal(String s) {
    this.c = s;
  }

  @Override
  public String toString() {
    return c;
  }

  @Override
  public NFA toNFA(boolean debugOutput) {

    State neuStart = new State("neuStart");
    neuStart.setInitial(true);
    State neuFinal = new State("neuFinal");
    neuFinal.setFinal(true);

    NFA result = new NFA();

    result.addToAlphabet(c);
    result.addState(neuStart);
    result.addState(neuFinal);

    result.addTransition(neuStart, c, neuFinal.asSet());

    result.renameStates("");

    return result;
  }
}
