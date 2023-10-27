package edu.uky.cs.nil.sabre.graph;

import java.util.HashMap;

import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.FiniteState;
import edu.uky.cs.nil.sabre.Fluent;
import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.logic.Value;

/**
 * An initial node is a temporary {@link StateNode state node} created while
 * initializing the {@link StateGraph#root root node} of a {@link StateGraph
 * state graph}; an initial node may be a duplicate of other initial nodes but
 * will be replaced by a permanent node once initializing the graph is
 * complete.
 * 
 * @author Stephen G. Ware
 */
final class InitialNode extends StateNode {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/** The permanent node this temporary node will be replaced with */
	private StateNode replacement = null;
	
	InitialNode(StateGraph graph, FiniteState state, HashMap<FiniteState, InitialNode> map) {
		super(graph, -1, new Value[graph.fluents.size()]);
		map.put(state, this);
		for(Fluent fluent : graph.fluents)
			values[graph.index.get(fluent)] = state.getValue(fluent);
		for(int i=graph.characters.size()-1; i>=0; i--) {
			Character character = graph.characters.get(i);
			FiniteState beliefs = state.getBeliefs(character);
			InitialNode child = map.get(beliefs);
			if(child == null)
				child = new InitialNode(graph, beliefs, map);
			new EpistemicEdge(this, character, child);
		}
	}
	
	@Override
	StateNode resolve() {
		if(replacement == null) {
			values = graph.resolve(values);
			replacement = graph.resolve(this);
			resolve(replacement.epistemicOut);
		}
		return replacement;
	}
	
	private static final void resolve(Edge edge) {
		if(edge != null) {
			resolve(edge.nextOut);
			edge.resolve();
		}
	}
}