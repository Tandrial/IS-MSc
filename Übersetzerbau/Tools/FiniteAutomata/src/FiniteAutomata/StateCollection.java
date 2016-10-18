package FiniteAutomata;

import java.util.HashSet;
import java.util.Set;

public class StateCollection extends HashSet<State> {
  private static final long serialVersionUID = 1L;

  protected StateCollection() {
  }

  protected StateCollection(Set<State> states) {
    super(states);
  }

  protected void resetMarked() {
    for (State s : this)
      s.setMarked(false);
  }

  protected State getStartState() {
    State result = null;
    for (State s : this) {
      if (s.isInitial()) {
        result = s;
        break;
      }
    }
    return result;
  }

  protected Set<State> getFinalStates() {
    Set<State> finalState = new HashSet<>();
    for (State s : this) {
      if (s.isFinal())
        finalState.add(s);
    }
    return finalState;
  }

  protected State getStateByName(String state) {
    State result = null;
    for (State s : this)
      if (s.getName().equals(state)) {
        result = s;
        break;
      }
    return result;
  }
}
