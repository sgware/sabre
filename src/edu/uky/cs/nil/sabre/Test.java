package edu.uky.cs.nil.sabre;

import java.io.File;

import edu.uky.cs.nil.sabre.comp.*;
import edu.uky.cs.nil.sabre.io.*;
import edu.uky.cs.nil.sabre.prog.*;
import edu.uky.cs.nil.sabre.search.*;
import edu.uky.cs.nil.sabre.util.*;

public class Test {
	
	public static final String URL = "problems/gramma.txt";
	public static final ProgressionPlanner.Method METHOD = ProgressionPlanner.Method.EXPLANATION_FIRST;
	public static final ProgressionCostFactory COST = ProgressionCostFactory.TEMPORAL;
	public static final ProgressionCostFactory HEURISTIC = new RepeatedRootHeuristic.Factory(GraphHeuristic.SUM);
	public static final Number GOAL = Number.get(1);
	public static final int AUTHOR_TEMPORAL_LIMIT = 3;
	public static final int CHARACTER_TEMPORAL_LIMIT = 5;
	public static final int EPISTEMIC_LIMIT = 1;

	public static void main(String[] args) throws Exception {
		Parser parser = new DefaultParser();
		Problem problem = parser.parse(new File(URL), Problem.class);
		ProgressionPlanner planner = new ProgressionPlanner();
		planner.setMethod(METHOD);
		planner.setCost(COST);
		planner.setHeuristic(HEURISTIC);
		planner.setAuthorTemporalLimit(AUTHOR_TEMPORAL_LIMIT);
		planner.setCharacterTemporalLimit(CHARACTER_TEMPORAL_LIMIT);
		planner.setEpistemicLimit(EPISTEMIC_LIMIT);
		Result<CompiledAction> result = Worker.get(status -> {
			CompiledProblem compiled = planner.compile(problem, status);
			ProgressionSearch search = planner.getSearch(compiled, status);
			search.setGoal(GOAL);			
			System.out.println(Utilities.DEFAULT_PRINTER.toString(search.problem));
			System.out.println(planner);
			System.out.println(search);
			return search.get(status);
		});
		System.out.println("\n" + result);
		if(result.solution != null)
			System.out.println(result.solution);
	}
}