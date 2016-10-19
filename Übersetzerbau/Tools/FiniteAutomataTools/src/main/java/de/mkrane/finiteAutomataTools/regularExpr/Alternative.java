package de.mkrane.finiteAutomataTools.regularExpr;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import de.mkrane.finiteAutomataTools.finiteAutomata.NFA;
import de.mkrane.finiteAutomataTools.finiteAutomata.State;

public class Alternative implements Expression {

  Expression t1;
  Expression t2;

  public Alternative(Expression t1, Expression t2) {
    this.t1 = t1;
    this.t2 = t2;
  }

  @Override
  public String toString() {
    return t1.toString() + "|" + t2.toString();
  }

  @Override
  public NFA toNFA(boolean debugOutput) {
    NFA n1 = t1.toNFA(debugOutput);
    NFA n2 = t2.toNFA(debugOutput);

    NFA result = new NFA();
    result.addToAlphabet(NFA.eps);

    State neuStart = new State("neuStart");
    neuStart.setInitial(true);
    result.addState(neuStart);

    State neuFinal = new State("neuFinal");
    neuFinal.setFinal(true);
    result.addState(neuFinal);

    for (NFA n : new NFA[] { n1, n2 }) {
      State oldStart = n.getStartState();
      oldStart.setInitial(false);

      result.addToAlphabet(n.getAlphabet());
      result.addStates(n.getStates());

      result.addTransition(neuStart, NFA.eps, oldStart.asSet());

      for (State state : n.getStates()) {
        Map<String, Set<State>> bla = n.getPossibleTransitions(state);
        for (Entry<String, Set<State>> entry : bla.entrySet()) {
          result.addTransition(state, entry.getKey(), entry.getValue());
        }
      }

      Set<State> oldFinal = n.getFinalStates();
      for (State s : oldFinal) {
        s.setFinal(false);
        result.addTransition(s, NFA.eps, neuFinal.asSet());
      }
    }

    result.renameStates("");

    if (debugOutput)
      printDebug(result);
    return result;
  }
}
