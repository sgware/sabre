package edu.uky.cs.nil.sabre.comp;

import java.util.HashSet;
import java.util.Set;
import java.util.function.Function;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.graph.StateGraph;
import edu.uky.cs.nil.sabre.hg.ActionNode;
import edu.uky.cs.nil.sabre.hg.CostSet;
import edu.uky.cs.nil.sabre.hg.EffectNode;
import edu.uky.cs.nil.sabre.hg.FluentNode;
import edu.uky.cs.nil.sabre.hg.HeuristicGraph;
import edu.uky.cs.nil.sabre.hg.MaxGraph;
import edu.uky.cs.nil.sabre.hg.PreconditionNode;
import edu.uky.cs.nil.sabre.logic.Effect;
import edu.uky.cs.nil.sabre.logic.False;
import edu.uky.cs.nil.sabre.logic.Precondition;
import edu.uky.cs.nil.sabre.logic.True;
import edu.uky.cs.nil.sabre.logic.Value;
import edu.uky.cs.nil.sabre.util.Worker;

/**
 * A {@link Function function} which maps some {@link
 * edu.uky.cs.nil.sabre.logic.Expression logical expressions} to simpler
 * expressions based on a leveled off {@link HeuristicGraph heuristic graph}.
 * A proposition is said to be reachable if it can ever be true and unreachable
 * if it can never be true.
 * <p>
 * This function initializes a heuristic graph to {@link CompiledProblem#start
 * a compiled problem's initial state} and then extends it until it has leveled
 * off, also accounting for beliefs during that process. A heuristic graph
 * provides a necessary but not sufficient way to detect which values a {@link
 * edu.uky.cs.nil.sabre.Fluent fluent} can have (that is, it cannot reliably
 * detect if a fluent can have a value, but it can reliably detect when a
 * fluent cannot have a value).
 * <p>
 * This function maps expressions in the following ways:
 * <ul>
 * <li>If the expression is a {@link CompiledFluent compiled fluent} and that
 * fluent can only have one possible value, this function returns that value.
 * </li>
 * <li>If the expression is {@link Precondition an atomic precondition}, and
 * that precondition cannot every possible be true, this function returns
 * {@link False#FALSE false}.</li>
 * <li>If the expression is {@link Effect an atomic effect}, and {@link
 * edu.uky.cs.nil.sabre.logic.Assignment#fluent the effect's fluent} is one
 * which can only have one possible value, this function returns {@link
 * True#TRUE true}.</li>
 * </ul>
 * 
 * @author Stephen G. Ware
 */
public class Reachable implements Function<Object, Object> {
	
	private final HeuristicGraph graph;
	
	/**
	 * Constructs a new reachable mapping.
	 * 
	 * @param problem a compiled problem whose initial state will be used to
	 * determine which propositions are reachable and unreachable
	 * @param status a status to update while the compiler runs
	 */
	public Reachable(CompiledProblem problem, Worker.Status status) {
		graph = new MaxGraph(problem, status);
		initialize(graph, new StateGraph(problem).root.afterTriggers());
		for(ActionNode action : graph.actions)
			if(action.label.consenting.size() == 0)
				action.setCost(0);
		do propagateBeliefs(graph);
		while(graph.extend());
	}
	
	private static final void initialize(HeuristicGraph graph, FiniteState state) {
		graph.reset();
		initialize(graph, state, new HashSet<>());
	}
	
	private static final void initialize(HeuristicGraph graph, FiniteState state, Set<FiniteState> visited) {
		if(visited.add(state)) {
			for(FluentNode fluent : graph.fluents)
				fluent.setCost(state.getValue(fluent.label), 0);
			for(Character character : graph.getCharacters())
				initialize(graph, state.getBeliefs(character), visited);
		}
	}
	
	private static final void propagateBeliefs(HeuristicGraph graph) {
		for(FluentNode child : graph.fluents) {
			while(child.label.characters.size() > 0) {
				FluentNode parent = graph.getFluent(child.label.removeFirstCharacter());
				for(CostSet.Entry entry : child)
					parent.setCost(entry.value, entry.cost);
				child = parent;
			}
		}
	}

	@Override
	public Object apply(Object original) {
		if(original instanceof CompiledFluent) {
			FluentNode fluent = graph.getFluent((CompiledFluent) original);
			Value first = null;
			for(CostSet.Entry entry : fluent) {
				if(first == null)
					first = entry.value;
				else
					return original;
			}
			return first;
		}
		else if(original instanceof Precondition) {
			PreconditionNode precondition = graph.getPrecondition((Precondition) original);
			if(precondition.getCost() == Double.POSITIVE_INFINITY)
				return False.FALSE;
		}
		else if(original instanceof Effect) {
			EffectNode effect = graph.getEffect((Effect) original);
			if(apply(effect.fluent.label) instanceof Value)
				return True.TRUE;
		}
		return original;
	}
}