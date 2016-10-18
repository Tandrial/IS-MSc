package FiniteAutomata;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public class Transition {
  private State                        state;
  protected Map<Character, Set<State>> moves = new HashMap<>();

  public State getState() {
    return state;
  }

  Transition(State state, char c, Set<State> states) {
    this.state = state;
    if (!moves.containsKey(Character.valueOf(c))) {
      moves.put(Character.valueOf(c), states);
    } else {
      moves.get(Character.valueOf(c)).addAll(states);
    }
  }

  public void addMove(char c, Set<State> states) {
    if (!moves.containsKey(c))
      moves.put(c, states);
    else
      moves.get(c).addAll(states);
  }

  public void addTransition(Transition t) {
    for (Entry<Character, Set<State>> entry : t.moves.entrySet()) {
      if (!moves.containsKey(entry.getKey())) {
        moves.put(entry.getKey(), entry.getValue());
      } else {
        moves.get(entry.getKey()).addAll(entry.getValue());
      }
    }
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    List<String> list = new ArrayList<>();
    for (Entry<Character, Set<State>> move : moves.entrySet())
      list.add(state + " x '" + move.getKey() + "' → " + Arrays.toString(move.getValue().toArray()));

    for (int i = 0; i < list.size(); i++) {
      sb.append(list.get(i));
      if (i < list.size() - 1)
        sb.append(", ");

    }
    return sb.toString();
  }
}
