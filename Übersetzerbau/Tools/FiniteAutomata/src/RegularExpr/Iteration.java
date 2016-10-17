package RegularExpr;

import java.util.Set;

import FiniteAutomata.NFA;
import FiniteAutomata.State;

public class Iteration implements Expression {

	Expression t1;

	public Iteration(Expression t1) {
		this.t1 = t1;
	}

	@Override
	public String toString() {
		return "(" + t1 + ")*";
	}

	@Override
	public NFA toNFA(boolean debugOutput) {
		NFA result = t1.toNFA(debugOutput);
		State oldStart = result.getStartState();
		oldStart.setInitial(false);
		Set<State> oldFinal = result.getFinalStates();
			
		State neuStart = new State("neuStart");
		neuStart.setInitial(true);
		State neuFinal = new State("neuFinal");
		neuFinal.setFinal(true);

		result.addState(neuStart);
		result.addState(neuFinal);
		result.getAlphabet().add(NFA.eps);

		for (State s : oldFinal) {
			s.setFinal(false);
			result.addTransition(s, NFA.eps, oldStart.asSet());
			result.addTransition(s, NFA.eps, neuFinal.asSet());
		}

		result.addTransition(neuStart, NFA.eps, neuFinal.asSet());
		result.addTransition(neuStart, NFA.eps, oldStart.asSet());

		result.renameStates("");

		if (debugOutput)
			printDebug(result);
		return result;
	}
}
