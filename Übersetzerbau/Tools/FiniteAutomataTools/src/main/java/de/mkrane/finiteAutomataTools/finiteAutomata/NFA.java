package de.mkrane.finiteAutomataTools.finiteAutomata;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.HashSet;
import java.util.Set;

public class NFA extends FiniteAutomata {

  public static final String eps = "\u03b5";

  @Override
  public String toString() {
    return "NFA = " + super.toString();
  }

  @Override
  public boolean simulate(String word) {
    Set<State> s = epsilonClosure(states.getStartState().asSet());

    for (char c : word.toCharArray()) {
      s = epsilonClosure(move(s, String.valueOf(c)));
      if (s.isEmpty())
        break;
    }

    return !s.isEmpty() && containsEndstate(s);
  }

  private Set<State> epsilonClosure(Set<State> startStates) {
    Set<State> closure = new HashSet<>(startStates);
    Deque<State> queue = new ArrayDeque<>(closure);

    while (!queue.isEmpty()) {
      State state = queue.remove();
      for (State move : move(state.asSet(), NFA.eps)) {
        if (!closure.contains(move)) {
          closure.add(move);
          queue.add(move);
        }
      }
    }
    return closure;
  }

  public DFA convertToDFA(boolean debugOutput) {
    logger = new StringBuilder();
    DFA dfa = new DFA();
    dfa.addToAlphabet(this.alphabet);
    dfa.alphabet.remove(NFA.eps);

    // Initialisiere SD mit unmarkiertem Zustand ε-closure({s0});
    StateCollection S_D = new StateCollection();
    State start = State.mergeStates(epsilonClosure(states.getStartState().asSet()));
    S_D.add(start);
    logger.append("[*] Initial S_D = " + S_D + '\n');

    Deque<State> queue = new ArrayDeque<>();
    queue.add(start);
    this.states.resetMarked();

    // while (es gibt einen unmarkierten Zustand T ∈ SD) {
    while (!queue.isEmpty()) {
      // markiere T;
      State currentState = queue.remove();
      logger.append("[*] Checking [" + currentState + "]\n");

      // for (jedes Eingabesymbol a ∈ Σ) {
      for (String c : dfa.alphabet) {
        // T := move(T, a)
        Set<State> T = new HashSet<>();
        for (State s : currentState.getIncludedStates())
          T.addAll(move(s.asSet(), c));
        logger.append(String.format("\tT:= move([%s], '%s') = %s", currentState, c, T));

        // U := ε-closure(T);
        Set<State> closureResult = epsilonClosure(T);
        State U = S_D.getStateByName(State.getMergedName(closureResult));
        if (U == null)
          U = State.mergeStates(closureResult);
        logger.append(String.format(" => epsilon-closure(T) = [%s]%n", State.getMergedName(closureResult)));
        // δD(T, a) := U;
        dfa.lambda.addTransition(new Transition(currentState, c, U.asSet()));

        // if (U ∉ SD) {
        if (!S_D.contains(U)) {
          // füge U als unmarkierten Zustand zu SD hinzu;
          S_D.add(U);
          queue.add(U);
        }
      }
    }
    logger.append("[*] States and Lambda done\n");
    dfa.states = S_D;

    logger.append("[*] Renaming States\n");
    dfa.renameStates("");

    logger.append("[*] Done\n");
    if (debugOutput)
      System.out.println(logger.toString());
    return dfa;
  }
}
