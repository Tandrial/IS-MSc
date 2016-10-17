package RegularExpr;

import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import FiniteAutomata.NFA;
import FiniteAutomata.State;

public class Concat implements Expression {

	Expression t1;
	Expression t2;

	public Concat(Expression t1, Expression t2) {
		this.t1 = t1;
		this.t2 = t2;
	}

	@Override
	public String toString() {
		return t1.toString() + t2.toString();
	}

	@Override
	public NFA toNFA(boolean debugOutput) {
		NFA n1 = t1.toNFA(debugOutput);
		NFA n2 = t2.toNFA(debugOutput);

		NFA result = new NFA();
		result.getAlphabet().add(NFA.eps);

		for (NFA n : new NFA[] { n1, n2 }) {
			result.getAlphabet().addAll(n.getAlphabet());
			result.addStates(n.getStates());

			for (State state : n.getStates()) {
				Map<Character, Set<State>> bla = n.getPossibleTransitions(state);
				for (Entry<Character, Set<State>> entry : bla.entrySet()) {
					result.addTransition(state, entry.getKey(), entry.getValue());
				}
			}
		}

		State n2OldStart = n2.getStartState();
		n2OldStart.setInitial(false);
		Set<State> n1OldFinal = n1.getFinalStates();

		for (State s : n1OldFinal) {
			s.setFinal(false);
			result.addTransition(s, NFA.eps, n2OldStart.asSet());
		}

		result.renameStates("");

		if (debugOutput)
			printDebug(result);
		return result;
	}
}
