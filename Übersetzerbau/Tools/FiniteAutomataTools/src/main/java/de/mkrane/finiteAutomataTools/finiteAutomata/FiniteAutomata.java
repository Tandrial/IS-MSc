package de.mkrane.finiteAutomataTools.finiteAutomata;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

public abstract class FiniteAutomata {
  protected Set<String>     alphabet = new HashSet<>();
  protected StateCollection states   = new StateCollection();
  protected TransitionTable lambda   = new TransitionTable();
  protected StringBuilder   logger   = new StringBuilder();

  public abstract boolean simulate(String word);

  public void addToAlphabet(String s) {
    alphabet.add(s);
  }

  public void addToAlphabet(Collection<String> alpha) {
    alphabet.addAll(alpha);
  }

  public Set<String> getAlphabet() {
    return alphabet;
  }

  public void addState(State state) {
    this.states.add(state);
  }

  public void addStates(StateCollection states) {
    this.states.addAll(states);
  }

  public StateCollection getStates() {
    return states;
  }

  public State getStartState() {
    return states.getStartState();
  }

  public Set<State> getFinalStates() {
    return states.getFinalStates();
  }

  public void addTransition(State s, String c, Set<State> g) {
    Transition t = new Transition(s, c, g);
    lambda.addTransition(t);
  }

  public Map<String, Set<State>> getPossibleTransitions(State state) {
    return lambda.getPossibleTransitions(state);
  }

  protected Set<State> move(Set<State> currentState, String c) {
    Set<State> result = new HashSet<>();

    for (State state : currentState) {
      Map<String, Set<State>> possibleTransitions = lambda.getPossibleTransitions(state);
      if (possibleTransitions.containsKey(c))
        result.addAll(possibleTransitions.get(c));
    }
    return result;
  }

  protected boolean containsEndstate(Set<State> states) {
    return states.stream().anyMatch(s -> s.isFinal());
  }

  public void renameStates(String prefix) {
    this.states.resetMarked();

    String fmtString = "%sq%d";
    if (this.states.size() >= 10)
      fmtString = "%sq%02d";

    Deque<State> queue = new ArrayDeque<>();
    queue.add(this.getStartState());
    int cnt = 0;
    while (!queue.isEmpty()) {
      State state = queue.remove();
      if (state.isMarked())
        continue;
      state.setMarked(true);
      if (!state.isFinal())
        state.setName(String.format(fmtString, prefix, cnt++));

      for (Set<State> goals : this.getPossibleTransitions(state).values())
        queue.addAll(goals);
    }

    for (State state : states.getFinalStates()) {
      state.setName(String.format(fmtString, prefix, cnt++));
    }
  }

  public static void saveToFile(FiniteAutomata[] fas, String[] clusterNames, String fileName, String graphName)
      throws IOException {
    StringBuilder sb = new StringBuilder();
    int i = 0;
    sb.append("digraph fs {\n");
    sb.append("\trankdir=LR;\n");
    sb.append("\tfontsize=20;\n");
    sb.append("\tnewrank=true");
    sb.append("\tlabel = \"" + graphName + "\";\n");
    sb.append("\t\tnode [shape=circle];\n");
    String ranks = "{ rank=same; ";
    for (FiniteAutomata fa : fas) {
      if (fa == null)
        continue;
      sb.append("\n\tsubgraph cluster_" + clusterNames[i] + " {\n");
      sb.append("\t\tlabel = \"" + clusterNames[i] + "\";\n");
      sb.append("\t\tgraph [ dpi = 1200 ];\n");

      // List with all endStates
      for (State s : fa.states.getFinalStates())
        sb.append(String.format("\t\t{%1$s%2$d [label=\"%1$s\", shape=doublecircle]};\n", s, i));

      // hidden node to get an arrow to point to the Start-Node
      sb.append("\t\tsecret_node" + i + " [style=invis, shape=point, fixedsize=true, width=.6]\n");

      String startName = fa.states.getStartState().toString();
      sb.append(String.format("\t\tsecret_node%1$d -> {%2$s%1$d [label=\"%2$s\"]};%n", i, startName));
      ranks += String.format("%s%d; ", startName, i);
      // Transitions for all the nodes
      for (State s : fa.states) {
        Map<String, Set<State>> moves = fa.lambda.getPossibleTransitions(s);
        for (Entry<String, Set<State>> entry : moves.entrySet()) {
          for (State g : entry.getValue()) {
            sb.append(
                String.format("\t\t{%1$s%2$d [label=\"%1$s\"]} -> {%3$s%2$d [label=\"%3$s\"]} [label = \"%4$s\"];%n", s,
                    i, g, entry.getKey()));
          }
        }
      }
      sb.append("\t}\n");
      i++;
    }
    ranks += "}\n";
    sb.append(ranks);

    sb.append("}\n");
    File file = new File(fileName + ".dot");

    BufferedWriter writer = null;
    try {
      writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"));
      writer.write(sb.toString());
    } finally {
      if (writer != null)
        writer.close();
    }
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("(S, Σ, δ, s0, F)\n");

    sb.append("S = ");
    List<State> states = new ArrayList<>(this.states);
    Collections.sort(states, (s1, s2) -> s1.getName().compareTo(s2.getName()));
    sb.append(Arrays.toString(states.toArray()));
    sb.append("\n");

    sb.append("Σ = ");
    sb.append(Arrays.toString(this.alphabet.toArray()));
    sb.append("\n");

    sb.append("δ = [");
    sb.append(this.lambda);
    sb.delete(sb.length() - 6, sb.length());
    sb.append("]\n");

    sb.append("s0 = ");
    sb.append(this.getStartState());
    sb.append("\n");

    sb.append("F = ");
    List<State> finals = new ArrayList<>(this.getFinalStates());
    Collections.sort(finals, (s1, s2) -> s1.getName().compareTo(s2.getName()));
    sb.append(Arrays.toString(finals.toArray()));
    sb.append("\n");

    return sb.toString();
  }
}
