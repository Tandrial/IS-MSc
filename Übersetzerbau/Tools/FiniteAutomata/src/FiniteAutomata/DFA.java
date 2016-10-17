package FiniteAutomata;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class DFA extends FiniteAutomata {

	@Override
	public String toString() {
		return "DFA = " + super.toString();
	}

	@Override
	public boolean simulate(String word) {
		Set<State> s = new HashSet<>();
		s.add(this.states.getStartState());
		for (char c : word.toCharArray()) {
			s = move(s, c);
			if (s.isEmpty())
				break;
		}
		return !s.isEmpty() && containsEndstate(s);
	}

	public DFA minimize(boolean debugOutput) {
		logger = new StringBuilder();
		logger.append("[*] Building groups\n");

		List<Set<State>> gruppen = buildGroups();

		logger.append("[*] Groups built\n");
		logger.append("[*] Building DFA_min\n");
		DFA dfa_min = new DFA();
		dfa_min.alphabet = new HashSet<>(this.alphabet);

		logger.append("[*] Creating S_min\n");
		dfa_min.states = new StateCollection();
		for (Set<State> gruppe : gruppen)
			dfa_min.states.add(State.mergeStates(gruppe));

		logger.append("[*] Creating δ_min\n");
		dfa_min.lambda = createLambda(dfa_min.states);

		logger.append("[*] Renaming States\n");
		dfa_min.renameStates("");

		logger.append("[*] Done\n");
		if (debugOutput)
			System.out.println(logger.toString());
		return dfa_min;
	}

	private List<Set<State>> buildGroups() {
		List<Set<State>> gruppen = new ArrayList<>();
		Set<State> endStates = this.states.getFinalStates();
		Set<State> nonEndStates = new HashSet<>(this.states);
		nonEndStates.removeAll(endStates);
		gruppen.add(endStates);
		if (nonEndStates.size() > 0)
			gruppen.add(nonEndStates);

		boolean groupWasSplit = false;
		int i = 0;
		do {
			logger.append("\tΠ" + i++ + " = " + gruppen + '\n');
			List<Set<State>> gruppenNeu = new ArrayList<>();
			groupWasSplit = false;
			for (Set<State> gruppe : gruppen) {
				Set<State> alt = new HashSet<>(gruppe);
				Set<State> neu = new HashSet<>();

				boolean groupNeedstoBeSplit = false;
				for (char c : this.alphabet) {
					Set<Set<State>> allGoals = new HashSet<>();

					for (State s : gruppe) {
						Set<State> goal = getGroupWhichIncludes(s, c, gruppen);
						assert (goal != null);
						if (allGoals.size() == 0) {
							allGoals.add(goal);
						} else if (!allGoals.contains(goal)) {
							logger.append("\t\tSplitting " + gruppe);
							groupNeedstoBeSplit = true;

							neu.add(s);
							alt.remove(s);
						}
					}
				}

				if (groupNeedstoBeSplit) {
					logger.append(" into " + alt + " and " + neu + "\n\n");
					groupWasSplit = true;
					gruppenNeu.add(alt);
					gruppenNeu.add(neu);
				} else {
					gruppenNeu.add(alt);
				}
			}
			gruppen = gruppenNeu;

		} while (groupWasSplit);
		return gruppen;
	}

	private TransitionTable createLambda(StateCollection states) {
		TransitionTable result = new TransitionTable();
		for (State s : states) {
			State start = s.getIncludedStates().iterator().next();
			for (char c : this.alphabet) {
				State goal = findStateWithIncludedState(lambda.getGoalFromTransition(start, c), states);
				if (goal != null)
					result.addTransition(new Transition(s, c, goal.asSet()));
			}
		}
		return result;
	}

	private Set<State> getGroupWhichIncludes(State state, char c, List<Set<State>> gruppen) {
		Set<State> result = null;
		state = lambda.getGoalFromTransition(state, c);
		for (Set<State> gruppe : gruppen) {
			if (gruppe.contains(state)) {
				result = gruppe;
				break;
			}
		}
		return result;
	}

	private State findStateWithIncludedState(State s, Set<State> states) {
		State result = null;
		for (State curr : states) {
			if (curr.getIncludedStates().contains(s)) {
				result = curr;
				break;
			}
		}
		return result;
	}
}
