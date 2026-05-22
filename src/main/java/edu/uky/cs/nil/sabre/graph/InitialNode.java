package edu.uky.cs.nil.sabre.graph;

import java.util.Map;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An initial node is a temporary {@link StateNode state node} created while
 * initializing the {@link StateGraph#root root node} of a {@link StateGraph
 * state graph}; an initial node may be a duplicate of other initial nodes but
 * will be replaced by a permanent node once the graph is {@link
 * StateGraph#clean() cleaned}.
 * 
 * @author Stephen G. Ware
 */
class InitialNode extends TemporaryNode {

	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	InitialNode(StateGraph graph, FiniteState state, Map<FiniteState, InitialNode> nodes) {
		super(graph, new Value[graph.fluents.size()]);
		for(Fluent fluent : graph.fluents)
			values[graph.index.apply(fluent)] = state.getValue(fluent);
		nodes.put(state, this);
		for(Character character : graph.characters) {
			FiniteState beliefs = state.getBeliefs(character);
			StateNode child = nodes.get(beliefs);
			if(child == null)
				child = new InitialNode(graph, beliefs, nodes);
			new EpistemicEdge(this, character, child);
		}
	}
}