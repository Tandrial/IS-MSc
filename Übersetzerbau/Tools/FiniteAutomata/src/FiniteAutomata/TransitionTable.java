package FiniteAutomata;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class TransitionTable {

	private Map<State, Transition> table = new HashMap<>();

	public void addTransition(Transition t) {
		if (table.containsKey(t.getState())) {
			table.get(t.getState()).addTransition(t);
		} else {
			table.put(t.getState(), t);			
		}
	}

	public Map<Character, Set<State>> getPossibleTransitions(State state) {
		if (table.containsKey(state))
			return table.get(state).moves;
		else
			return new HashMap<>();
	}

	protected State getGoalFromTransition(State start, char c) {
		return getPossibleTransitions(start).get(c).iterator().next();
	}

	@Override
	public String toString() {
		StringBuilder sb = new StringBuilder();
		List<State> states = new ArrayList<>(table.keySet());
		Collections.sort(states, (s1, s2) -> s1.getName().compareTo(s2.getName()));
		for (int i = 0; i < states.size(); i++) {
			sb.append(table.get(states.get(i)));
			if (i < states.size() - 1)
				sb.append(",\n     ");
		}
		return sb.toString();
	}
}
