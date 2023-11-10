package edu.uky.cs.nil.sabre.io;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

import edu.uky.cs.nil.sabre.*;
import edu.uky.cs.nil.sabre.Character;
import edu.uky.cs.nil.sabre.Number;
import edu.uky.cs.nil.sabre.etree.EventTree;
import edu.uky.cs.nil.sabre.hg.*;
import edu.uky.cs.nil.sabre.logic.*;
import edu.uky.cs.nil.sabre.prog.*;
import edu.uky.cs.nil.sabre.prog.ProgressionPlanner.Method;
import edu.uky.cs.nil.sabre.search.*;
import edu.uky.cs.nil.sabre.util.*;

/**
 * A {@link Printer printer} which defines {@link ObjectPrinter object
 * printers} for many kinds of common objects, including {@link Problem
 * problems}, {@link Logical logical formulas}, and {@link Plan plans}.
 * 
 * @author Stephen G. Ware
 */
public class DefaultPrinter extends Printer {

	/**
	 * Constructs a new default printer.
	 */
	public DefaultPrinter() {
		setPrinter(java.lang.Boolean.class, (printer, bool, writer) -> {
			writer.append(bool.toString());
		});
		setPrinter(java.lang.Number.class, (printer, number, writer) -> {
			writer.append(number.toString());
		});
		setPrinter(java.lang.String.class, (printer, string, writer) -> {
			writer.append(string);
		});
		setPrinter(ImmutableArray.class, (printer, array, writer) -> {
			writer.append("[");
			for(int i=0; i<array.size(); i++) {
				if(i > 0)
					writer.append(", ");
				printer.print(array.get(i), writer);
			}
			writer.append("]");
		});
		setPrinter(ImmutableList.class, (printer, list, writer) -> {
			writer.append("[");
			boolean first = true;
			while(list.size() > 0) {
				if(first)
					first = false;
				else
					writer.append(", ");
				printer.print(list.first, writer);
				list = list.rest;
			}
			writer.append("]");
		});
		setPrinter(UniqueMap.class, (printer, map, writer) -> {
			writer.append("[");
			int[] index = new int[] { 0 };
			int count = 0;
			boolean first = true;
			while(count < map.size()) {
				@SuppressWarnings("unchecked")
				Object value = map.get(new Unique() {
					@Override
					public int hashCode() {
						return index[0];
					}
				});
				index[0]++;
				if(value != null) {
					if(first)
						first = false;
					else
						writer.append(", ");
					writer.append(Integer.toString(index[0] - 1));
					writer.append(" -> ");
					printer.print(value, writer);
					count++;
				}
			}
			writer.append("]");
		});
		setPrinter(Arithmetic.Operator.class, (printer, operator, writer) -> {
			if(operator.equals(Arithmetic.ADD))
				writer.append(DefaultParser.ADDITION_KEYWORD);
			else if(operator.equals(Arithmetic.SUBTRACT))
				writer.append(DefaultParser.SUBTRACTION_KEYWORD);
			else if(operator.equals(Arithmetic.MULTIPLY))
				writer.append(DefaultParser.MULTIPLICATION_KEYWORD);
			else if(operator.equals(Arithmetic.DIVIDE))
				writer.append(DefaultParser.DIVISION_KEYWORD);
			else
				throw Exceptions.noPrinter("operator \"" + operator.toString() + "\"");
		});
		setPrinter(ArithmeticExpansion.Operator.class, (printer, operator, writer) -> {
			if(operator.equals(ArithmeticExpansion.SUM))
				writer.append(DefaultParser.SUM_KEYWORD);
			else if(operator.equals(ArithmeticExpansion.PRODUCT))
				writer.append(DefaultParser.PRODUCT_KEYWORD);
			else
				throw Exceptions.noPrinter("operator \"" + operator.toString() + "\"");
		});
		setPrinter(Quantified.Quantifier.class, (printer, quantifier, writer) -> {
			if(quantifier.equals(Quantified.UNIVERSAL))
				writer.append(DefaultParser.UNIVERSAL_QUANTIFICATION_KEYWORD);
			else if(quantifier.equals(Quantified.EXISTENTIAL))
				writer.append(DefaultParser.EXISTENTIAL_QUANTIFICATION_KEYWORD);
			else
				throw Exceptions.noPrinter("quantifier \"" + quantifier.toString() + "\"");
		});
		setPrinter(Comparison.Operator.class, (printer, operator, writer) -> {
			if(operator.equals(Comparison.EQUAL_TO))
				writer.append(DefaultParser.EQUAL_TO_KEYWORD);
			else if(operator.equals(Comparison.NOT_EQUAL_TO))
				writer.append(DefaultParser.NOT_EQUAL_TO_KEYWORD);
			else if(operator.equals(Comparison.GREATER_THAN))
				writer.append(DefaultParser.GREATER_THAN_KEYWORD);
			else if(operator.equals(Comparison.GREATER_THAN_OR_EQUAL_TO))
				writer.append(DefaultParser.GREATER_THAN_OR_EQUAL_TO_KEYWORD);
			else if(operator.equals(Comparison.LESS_THAN))
				writer.append(DefaultParser.LESS_THAN_KEYWORD);
			else if(operator.equals(Comparison.LESS_THAN_OR_EQUAL_TO))
				writer.append(DefaultParser.LESS_THAN_OR_EQUAL_TO_KEYWORD);
			else
				throw Exceptions.noPrinter("operator \"" + operator.toString() + "\"");
		});
		setPrinter(Type.class, (printer, type, writer) -> writer.append(type.name));
		setPrinter(Unknown.class, (printer, value, writer) -> writer.append(DefaultParser.UNKNOWN_KEYWORD));
		setPrinter(False.class, (printer, value, writer) -> writer.append(DefaultParser.FALSE_KEYWORD));
		setPrinter(True.class, (printer, value, writer) -> writer.append(DefaultParser.TRUE_KEYWORD));
		setPrinter(Number.class, (printer, number, writer) -> {
			if(number.value % 1 == 0)
				writer.append(Long.toString(number.value.longValue()));
			else
				writer.append(Double.toString(number.value));
		});
		setPrinter(Entity.class, (printer, entity, writer) -> writer.append(entity.name));
		setPrinter(Variable.class, (printer, variable, writer) -> writer.append(variable.name));
		setPrinter(Signature.class, (printer, signature, writer) -> {
			writer.append(signature.name);
			writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
			for(int i=0; i<signature.arguments.size(); i++) {
				if(i > 0) {
					writer.append(DefaultParser.PARAMETER_SEPARATOR);
					writer.append(" ");
				}
				printer.print(signature.arguments.get(i), writer);
			}
			writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
		});
		setPrinter(Fluent.class, (printer, fluent, writer) -> {
			printEpistemic(printer, fluent.characters, fluent.signature, writer);
		});
		setPrinter(Epistemic.class, (printer, epistemic, writer) -> {
			printEpistemic(printer, epistemic.character, epistemic.argument, writer);
		});
		setPrinter(Conditional.class, (printer, conditional, writer) -> {
			for(int i=0; i<conditional.conditions.size(); i++) {
				writer.append(i == 0 ? DefaultParser.CONDITIONAL_FIRST_BRANCH_KEYWORD : DefaultParser.CONDITIONAL_MIDDLE_BRANCH_KEYWORD);
				writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
				printer.print(conditional.conditions.get(i), writer);
				writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
				writer.append(" ");
				wrap(printer, conditional.branches.get(i), writer);
				writer.append(" ");
			}
			if(conditional.conditions.size() > 0) {
				writer.append(DefaultParser.CONDITIONAL_LAST_BRANCH_KEYWORD);
				writer.append(" ");
			}
			printer.print(conditional.branches.get(conditional.branches.size() - 1), writer);
		});
		setPrinter(Arithmetic.class, (printer, arithmetic, writer) -> {
			wrap(printer, arithmetic.left, writer);
			writer.append(" ");
			printer.print(arithmetic.operator, writer);
			writer.append(" ");
			wrap(printer, arithmetic.right, writer);
		});
		setPrinter(ArithmeticExpansion.class, (printer, expansion, writer) -> {
			printer.print(expansion.operator, writer);
			writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
			printVariableDefinition(printer, expansion.variable, writer);
			writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
			writer.append(" ");
			printer.print(expansion.argument, writer);
		});
		setPrinter(Quantified.class, (printer, quantified, writer) -> {
			printer.print(quantified.quantifier, writer);
			writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
			printVariableDefinition(printer, quantified.variable, writer);
			writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
			writer.append(" ");
			printer.print(quantified.argument, writer);
		});
		setPrinter(Conjunction.class, (printer, conjunction, writer) -> {
			if(conjunction.equals(Clause.NULL))
				writer.append(DefaultParser.NULL_CLAUSE_KEYWORD);
			else if(conjunction.arguments.size() == 0) {
				writer.append(DefaultParser.LOGICAL_OPEN_BRACKET);
				writer.append(DefaultParser.LOGICAL_CLOSE_BRACKET);
			}
			else {
				for(int i=0; i<conjunction.arguments.size(); i++) {
					if(i > 0) {
						writer.append(" ");
						writer.append(DefaultParser.CONJUNCTION_KEYWORD);
						writer.append(" ");
					}
					wrap(printer, conjunction.arguments.get(i), writer);
				}
			}
		});
		setPrinter(Disjunction.class, (printer, disjunction, writer) -> {
			if(disjunction.arguments.size() == 0)
				printer.print(disjunction.simplify(), writer);
			else {
				for(int i=0; i<disjunction.arguments.size(); i++) {
					if(i > 0) {
						writer.append(" ");
						writer.append(DefaultParser.DISJUNCTION_KEYWORD);
						writer.append(" ");
					}
					wrap(printer, disjunction.arguments.get(i), writer);
				}
			}
		});
		setPrinter(Negation.class, (printer, negation, writer) -> {
			writer.append(DefaultParser.NEGATION_KEYWORD);
			wrap(printer, negation.argument, writer);
		});		
		setPrinter(Comparison.class, (printer, comparison, writer) -> {
			wrap(printer, comparison.left, writer);
			writer.append(" ");
			printer.print(comparison.operator, writer);
			writer.append(" ");
			wrap(printer, comparison.right, writer);
		});
		setPrinter(Assignment.class, (printer, assignment, writer) -> {
			printer.print(assignment.fluent, writer);
			writer.append(" ");
			writer.append(DefaultParser.ASSIGNMENT_KEYWORD);
			writer.append(" ");
			wrap(printer, assignment.value, writer);
		});
		setPrinter(Effect.class, (printer, effect, writer) -> {
			if(!effect.condition.equals(True.TRUE)) {
				writer.append(DefaultParser.CONDITIONAL_EFFECT_KEYWORD);
				writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
				printer.print(effect.condition, writer);
				writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
				writer.append(" ");
			}
			printer.print(effect.fluent, writer);
			writer.append(" ");
			writer.append(DefaultParser.ASSIGNMENT_KEYWORD);
			writer.append(" ");
			wrap(printer, effect.value, writer);
		});
		setPrinter(TypeConstraint.class, (printer, constraint, writer) -> {
			printer.print(constraint.argument, writer);
			writer.append(" ");
			writer.append(DefaultParser.INHERITANCE_KEYWORD);
			writer.append(" ");
			printer.print(constraint.type, writer);
		});
		setPrinter(Problem.class, (printer, problem, writer) -> printProblem(printer, problem, writer));
		setPrinter(Mapping.Function.class, (printer, mapping, writer) -> {
			writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
			printVariableDefinition(printer, mapping.variable, writer);
			writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
			writer.append(DefaultParser.DEFINITION_INDICATOR);
			writer.append(" ");
			printer.print(mapping.argument, writer);
		});
		setPrinter(InitialState.class, (printer, initial, writer) -> {
			printer.print(initial.clause, writer);
		});
		setPrinter(Beliefs.class, (printer, beliefs, writer) -> {
			printEpistemic(printer, beliefs.character, beliefs.state, writer);
		});
		setPrinter(Event.class, (printer, event, writer) -> {
			printer.print(event.getSignature(), writer);
		});
		setPrinter(Plan.class, (printer, plan, writer) -> {
			boolean first = true;
			for(Object action : plan) {
				if(first)
					first = false;
				else
					writer.append(" ");
				printer.print(action, writer);
			}
		});
		setPrinter(Solution.class, (printer, solution, writer) -> {
			printSolution(printer, solution, writer);
		});
		setPrinter(EventTree.class, (printer, tree, writer) -> {
			printEventTree(printer, tree, writer);
		});
		setPrinter(SumGraph.class, (printer, graph, writer) -> {
			writer.append("Sum Graph:");
			printHeuristicGraph(printer, graph, writer);
		});
		setPrinter(MaxGraph.class, (printer, graph, writer) -> {
			writer.append("Max Graph:");
			printHeuristicGraph(printer, graph, writer);
		});
		setPrinter(Node.class, (printer, node, writer) -> {
			printHeuristicGraphNode(printer, node, Double.POSITIVE_INFINITY, writer);
		});		
		setPrinter(CostSet.class, (printer, set, writer) -> {
			printHeuristicGraphCostSet(printer, set, Double.POSITIVE_INFINITY, writer);
		});
		setPrinter(CostSet.Entry.class, (printer, entry, writer) -> {
			writer.append("(");
			printer.print(entry.value, writer);
			writer.append(" costs ");
			printer.print(entry.cost, writer);
			writer.append(")");
		});
		setPrinter(Method.class, (printer, method, writer) -> {
			writer.append(method.toString());
		});
		setPrinter(ProgressionCost.class, (printer, cost, writer) -> {
			writer.append(cost.toString());
		});
		setPrinter(ProgressionCostFactory.class, (printer, cost, writer) -> {
			writer.append(cost.toString());
		});
		setPrinter(Planner.class, (printer, planner, writer) -> {
			printKeys(printer, planner.name + ":", getPlannerKeys(planner), writer);
		});
		setPrinter(ProgressionPlanner.class, (printer, planner, writer) -> {
			printKeys(printer, planner.name + ":", getProgressionPlannerKeys(planner), writer);
		});
		setPrinter(Search.class, (printer, search, writer) -> {
			printKeys(printer, "Search:", getSearchKeys(search), writer);
		});
		setPrinter(ProgressionSearch.class, (printer, search, writer) -> {
			printKeys(printer, "Heuristic Progression Search:", getProgressionSearchKeys(search), writer);
		});
		setPrinter(Result.class, (printer, result, writer) -> {
			LinkedHashMap<String, Object> keys = getResultKeys(result);
			printKeys(printer, "Result: " + result.message, keys, writer);
			if(result.solution != null) {
				writer.append("\n");
				printer.print(result.solution, writer);
			}
		});
	}
	
	private static final void wrap(Printer printer, Object expression, Writer writer) throws IOException {
		boolean wrap = expression instanceof Conditional ||
			expression instanceof Arithmetic ||
			expression instanceof Conjunction ||
			(expression instanceof Disjunction && !(expression instanceof Value));
		if(wrap)
			writer.append(DefaultParser.LOGICAL_OPEN_BRACKET);
		printer.print(expression, writer);
		if(wrap)
			writer.append(DefaultParser.LOGICAL_CLOSE_BRACKET);
	}
	
	private static final void printEpistemic(Printer printer, Object characters, Object expression, Writer writer) throws IOException {
		int count = 0;
		if(characters instanceof Parameter) {
			writer.append(DefaultParser.EPISTEMIC_KEYWORD);
			writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
			printer.print(characters, writer);
			writer.append(DefaultParser.PARAMETER_SEPARATOR);
			writer.append(" ");
			count = 1;
		}
		else if(characters != null && characters.getClass().isArray()) {
			Object[] array = (Object[]) characters;
			for(int i=0; i<array.length; i++) {
				writer.append(DefaultParser.EPISTEMIC_KEYWORD);
				writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
				printer.print(array[i], writer);
				writer.append(DefaultParser.PARAMETER_SEPARATOR);
				writer.append(" ");
			}
			count = array.length;
		}
		else if(characters != null && characters instanceof Iterable) {
			for(Object character : (Iterable<?>) characters) {
				writer.append(DefaultParser.EPISTEMIC_KEYWORD);
				writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
				printer.print(character, writer);
				writer.append(DefaultParser.PARAMETER_SEPARATOR);
				writer.append(" ");
				count++;
			}
		}
		printer.print(expression, writer);
		for(int i=0; i<count; i++)
			writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
	}
	
	private static final void printProblem(Printer printer, Problem problem, Writer writer) throws IOException {
		for(Type type : problem.universe.types) {
			if(!(type.id == Settings.BOOLEAN_TYPE_ID || type.id == Settings.NUMBER_TYPE_ID || type.id == Settings.ENTITY_TYPE_ID || type.id == Settings.CHARACTER_TYPE_ID)) {
				printTypeDefinition(printer, type, writer);
				writer.append(DefaultParser.DEFINITION_SEPARATOR);
				writer.append("\n");
			}
		}
		for(Entity entity : problem.universe.entities) {
			printEntityDefinition(printer, entity, writer);
			writer.append(DefaultParser.DEFINITION_SEPARATOR);
			writer.append("\n");
		}
		for(Fluent fluent : problem.fluents) {
			printPropertyDefinition(printer, fluent, writer);
			writer.append(DefaultParser.DEFINITION_SEPARATOR);
			writer.append("\n");
		}
		for(Action action : problem.actions) {
			printActionDefinition(printer, problem.universe, action, writer);
			writer.append(DefaultParser.DEFINITION_SEPARATOR);
			writer.append("\n");
		}
		for(Trigger trigger : problem.triggers) {
			printTriggerDefinition(printer, problem.universe, trigger, writer);
			writer.append(DefaultParser.DEFINITION_SEPARATOR);
			writer.append("\n");
		}
		if(problem.initial instanceof Conjunction) {
			Conjunction<?> initial = (Conjunction<?>) problem.initial;
			if(initial.size() < 2)
				printer.print(initial, writer);
			else {
				for(int i=0; i<initial.arguments.size(); i++) {
					if(i > 0) {
						writer.append(" ");
						writer.append(DefaultParser.CONJUNCTION_KEYWORD);
						writer.append("\n");
					}
					printer.print(initial.arguments.get(i), writer);
				}
			}
		}
		else
			printer.print(problem.initial, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n");
		printUtility(printer, null, problem.utility, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		for(Character character : problem.universe.characters) {
			writer.append("\n");
			printUtility(printer, character, problem.utilities.get(character), writer);
			writer.append(DefaultParser.DEFINITION_SEPARATOR);
		}
	}
	
	private static final void printTypeDefinition(Printer printer, Type type, Writer writer) throws IOException {
		writer.append(DefaultParser.TYPE_DEFINITION_KEYWORD);
		writer.append(" ");
		writer.append(type.name);
		writer.append(" ");
		writer.append(DefaultParser.INHERITANCE_KEYWORD);
		writer.append(" ");
		for(int i=0; i<type.parents.size(); i++) {
			if(i > 0) {
				writer.append(DefaultParser.PARAMETER_SEPARATOR);
				writer.append(" ");
			}
			writer.append(type.parents.get(i).name);
		}
	}
	
	private static final void printEntityDefinition(Printer printer, Entity entity, Writer writer) throws IOException {
		writer.append(DefaultParser.ENTITY_DEFINITION_KEYWORD);
		writer.append(" ");
		writer.append(entity.name);
		writer.append(" ");
		writer.append(DefaultParser.INHERITANCE_KEYWORD);
		writer.append(" ");
		for(int i=0; i<entity.types.size(); i++) {
			if(i > 0) {
				writer.append(DefaultParser.PARAMETER_SEPARATOR);
				writer.append(" ");
			}
			writer.append(entity.types.get(i).name);
		}
	}
	
	private static final void printSignatureDefinition(Printer printer, Signature signature, Writer writer) throws IOException {
		writer.append(signature.name);
		writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
		for(int i=0; i<signature.arguments.size(); i++) {
			if(i > 0) {
				writer.append(DefaultParser.PARAMETER_SEPARATOR);
				writer.append(" ");
			}
			printParameterDefinition(printer, signature.arguments.get(i), writer);
		}
		writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
	}
	
	private static final void printParameterDefinition(Printer printer, Parameter parameter, Writer writer) throws IOException {
		if(parameter instanceof Entity)
			writer.append(((Entity) parameter).name);
		else
			printVariableDefinition(printer, (Variable) parameter, writer);
	}
	
	private static final void printVariableDefinition(Printer printer, Variable variable, Writer writer) throws IOException {
		writer.append(variable.name);
		writer.append(" ");
		writer.append(DefaultParser.INHERITANCE_KEYWORD);
		writer.append(" ");
		printer.print(variable.type, writer);
	}
	
	private static final void printPropertyDefinition(Printer printer, Fluent property, Writer writer) throws IOException {
		writer.append(DefaultParser.FLUENT_DEFINITION_KEYWORD);
		writer.append(" ");
		for(Parameter character : property.characters) {
			writer.append(DefaultParser.EPISTEMIC_KEYWORD);
			writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
			printer.print(character, writer);
			writer.append(DefaultParser.PARAMETER_SEPARATOR);
			writer.append(" ");
		}
		printSignatureDefinition(printer, property.signature, writer);
		for(int i=0; i<property.characters.size(); i++)
			writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
		writer.append(" ");
		writer.append(DefaultParser.INHERITANCE_KEYWORD);
		writer.append(" ");
		writer.append(property.type.name);
	}
	
	private static final void printActionDefinition(Printer printer, Universe universe, Action action, Writer writer) throws IOException {
		writer.append(DefaultParser.ACTION_DEFINITION_KEYWORD);
		writer.append(" ");
		printSignatureDefinition(printer, action.signature, writer);
		writer.append(" ");
		writer.append(DefaultParser.EVENT_DEFINITION_OPEN_BRACKET);
		writer.append("\n\t");
		writer.append(DefaultParser.PRECONDITION_KEYWORD);
		writer.append(DefaultParser.DEFINITION_INDICATOR);
		writer.append(" ");
		printer.print(action.precondition, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n\t");
		writer.append(DefaultParser.EFFECT_KEYWORD);
		writer.append(DefaultParser.DEFINITION_INDICATOR);
		writer.append(" ");
		printer.print(action.effect, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n\t");
		writer.append(DefaultParser.CONSENTING_KEYWORD);
		writer.append(DefaultParser.DEFINITION_INDICATOR);
		writer.append(" ");
		writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
		for(int i=0; i<action.consenting.size(); i++) {
			if(i > 0) {
				writer.append(DefaultParser.PARAMETER_SEPARATOR);
				writer.append(" ");
			}
			printer.print(action.consenting.get(i), writer);
		}
		writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n\t");
		writer.append(DefaultParser.OBSERVING_KEYWORD);
		printMapping(printer, universe, action.observing, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n");
		writer.append(DefaultParser.EVENT_DEFINITION_CLOSE_BRACKET);
	}
	
	private static final void printMapping(Printer printer, Universe universe, Mapping<Expression> mapping, Writer writer) throws IOException {
		if(!(mapping instanceof Mapping.Function)) {
			Variable variable = new Variable(Settings.CHARACTER_TYPE_NAME, universe.types.get(Settings.CHARACTER_TYPE_ID));
			if(mapping.occurs(variable))
				variable = Variable.generate(variable.name, variable.type);
			Expression[] conditions = new Expression[universe.characters.size()];
			Expression[] branches = new Expression[conditions.length + 1];
			for(int i=0; i<conditions.length; i++) {
				conditions[i] = new Comparison(Comparison.EQUAL_TO, variable, universe.characters.get(i));
				branches[i] = mapping.get(universe.characters.get(i));
			}
			branches[branches.length - 1] = True.TRUE;
			mapping = new Mapping.Function(variable, new Conditional<>(conditions, branches));
		}
		printer.print(mapping, writer);
	}

	private static final void printTriggerDefinition(Printer printer, Universe universe, Trigger trigger, Writer writer) throws IOException {
		writer.append(DefaultParser.TRIGGER_DEFINITION_KEYWORD);
		writer.append(" ");
		printSignatureDefinition(printer, trigger.signature, writer);
		writer.append(" ");
		writer.append(DefaultParser.EVENT_DEFINITION_OPEN_BRACKET);
		writer.append("\n\t");
		writer.append(DefaultParser.PRECONDITION_KEYWORD);
		writer.append(DefaultParser.DEFINITION_INDICATOR);
		writer.append(" ");
		printer.print(trigger.precondition, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n\t");
		writer.append(DefaultParser.EFFECT_KEYWORD);
		writer.append(DefaultParser.DEFINITION_INDICATOR);
		writer.append(" ");
		printer.print(trigger.effect, writer);
		writer.append(DefaultParser.DEFINITION_SEPARATOR);
		writer.append("\n");
		writer.append(DefaultParser.EVENT_DEFINITION_CLOSE_BRACKET);
	}
	
	private static final void printUtility(Printer printer, Character character, Expression utility, Writer writer) throws IOException {
		writer.append(DefaultParser.UTILITY_DEFINITION_KEYWORD);
		writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
		if(character != null)
			printer.print(character, writer);
		writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
		writer.append(DefaultParser.DEFINITION_INDICATOR);
		writer.append(" ");
		printer.print(utility, writer);
	}
	
	private static final void printSolution(Printer printer, Solution<?> solution, Writer writer) throws IOException {
		printSolution(printer, "", solution, writer);
	}
	
	private static final void printSolution(Printer printer, String indent, Solution<?> solution, Writer writer) throws IOException {
		while(solution.size() > 0) {
			Action first = solution.get(0);
			writer.append(indent);
			printer.print(first, writer);
			for(Parameter other : first.consenting) {
				if(!Utilities.equals(other, solution.getCharacter())) {
					Solution<?> explanation = solution.getExplanation((Character) other);
					if(explanation != null) {
						writer.append("\n");
						printSolution(printer, indent + DefaultParser.BRANCH_INDENT_KEYWORD + " ", explanation.next(), writer);
					}
				}
			}
			writer.append("\n");
			solution = solution.next();
		}
		writer.append(indent);
		writer.append(DefaultParser.GOAL_KEYWORD);
		writer.append(DefaultParser.PARAMETER_LIST_OPEN_BRACKET);
		if(solution.getCharacter() != null) {
			printer.print(solution.getCharacter(), writer);
			writer.append(DefaultParser.PARAMETER_SEPARATOR);
			writer.append(" ");
		}
		printer.print(solution.getGoal(), writer);
		writer.append(DefaultParser.PARAMETER_LIST_CLOSE_BRACKET);
	}
	
	private static final void printEventTree(Printer printer, EventTree<?> tree, Writer writer) throws IOException {
		printEventTree(printer, "", tree, writer);
	}
	
	private static final void printEventTree(Printer printer, String indent, EventTree<?> tree, Writer writer) throws IOException {
		writer.append(indent);
		if(tree.events.size() == 0)
			writer.append("<no events>");
		else {
			for(int i=0; i<tree.events.size(); i++) {
				if(i > 0)
					writer.append(", ");
				printer.print(tree.events.get(i), writer);
			}
		}
		writer.append("\n");
		for(Value value : EventTree.getBranches(tree.expression)) {
			EventTree<?> branch = tree.getBranch(value);
			if(branch != null) {
				writer.append(indent);
				printer.print(tree.expression, writer);
				writer.append(" = ");
				printer.print(value, writer);
				writer.append("\n");
				printEventTree(printer, indent + "| ", branch, writer);
			}
		}
		EventTree<?> irrelevant = tree.getBranch(null);
		if(irrelevant != null) {
			writer.append(indent);
			printer.print(tree.expression, writer);
			writer.append(" irrelevant\n");
			printEventTree(printer, indent + "| ", irrelevant, writer);
		}
	}
	
	private static final void printHeuristicGraph(Printer printer, HeuristicGraph graph, Writer writer) throws IOException {
		for(double level : graph.getLevels()) {
			writer.append("\nLevel ");
			printer.print(level, writer);
			writer.append(":");
			ArrayList<ActionNode> actions = new ArrayList<>();
			for(ActionNode action : graph.actions)
				if(action.getCost() <= level)
					actions.add(action);
			Collections.sort(actions);
			if(actions.size() > 0) {
				writer.append("\n  Actions:");
				for(ActionNode action : actions) {
					writer.append("\n    ");
					printHeuristicGraphNode(printer, action, level, writer);
				}
			}
			writer.append("\n  Fluents:");
			for(FluentNode fluent : graph.fluents) {
				writer.append("\n    ");
				printHeuristicGraphNode(printer, fluent, level, writer);
			}
			writer.append("\n  Utilities:");
			if(graph.getUtility(null) != null) {
				writer.append("\n    utility() = ");
				printHeuristicGraphCostSetCosts(printer, graph.getUtility(null), level, writer);
			}
			for(Character character : graph.getCharacters()) {
				writer.append("\n    utility(");
				printer.print(character, writer);
				writer.append(") = ");
				printHeuristicGraphCostSetCosts(printer, graph.getUtility(character), level, writer);
			}
		}
	}
	
	private static final void printHeuristicGraphNode(Printer printer, Node node, double maxCost, Writer writer) throws IOException {
		printer.print(node.label, writer);
		if(node instanceof CostNode) {
			writer.append(" costs ");
			printer.print(((CostNode) node).getCost(), writer);
		}
		else if(node instanceof CostSet && !(node instanceof ConstantNode)) {
			writer.append(" = ");
			printHeuristicGraphCostSetCosts(printer, (CostSet) node, maxCost, writer);
		}
	}
	
	private static final void printHeuristicGraphCostSet(Printer printer, CostSet set, double maxCost, Writer writer) throws IOException {
		if(set instanceof Node)
			printHeuristicGraphNode(printer, (Node) set, maxCost, writer);
		else
			printHeuristicGraphCostSetCosts(printer, set, maxCost, writer);
	}
	
	private static final void printHeuristicGraphCostSetCosts(Printer printer, CostSet set, double maxCost, Writer writer) throws IOException {
		writer.append("{");
		if(set instanceof Span)
			printHeuristicGraphSpan(printer, (Span) set, maxCost, writer);
		else
			printHeuristicGraphCostSetEntries(printer, set, maxCost, writer);
		writer.append("}");
	}
	
	private static final void printHeuristicGraphCostSetEntries(Printer printer, CostSet set, double maxCost, Writer writer) throws IOException {
		boolean first = true;
		for(CostSet.Entry entry : set) {
			if(entry.cost <= maxCost) {
				if(first)
					first = false;
				else
					writer.append(" ");
				printer.print(entry, writer);
			}
		}
	}
	
	private static final void printHeuristicGraphSpan(Printer printer, Span span, double maxCost, Writer writer) throws IOException {
		double min = Double.NaN;
		double max = Double.NaN;
		boolean first = true;
		for(CostSet.Entry entry : span) {
			if(entry.cost <= maxCost) {
				if(first)
					first = false;
				else
					writer.append(" ");
				writer.append("(");
				if(entry.value instanceof Number) {
					double number = ((Number) entry.value).value;
					if(Double.isNaN(min) || number < min)
						min = number;
					if(Double.isNaN(max) || number > max)
						max = number;
					printer.print(min, writer);
					if(min != max) {
						writer.append(" to ");
						printer.print(max, writer);
					}
				}
				else
					printer.print(entry.value, writer);
				writer.append(" costs ");
				printer.print(entry.cost, writer);
				writer.append(")");
			}
		}
	}
	
	private static final String SEARCH_LIMIT_KEY = "search limit";
	private static final String SPACE_LIMIT_KEY = "space limit";
	private static final String TIME_LIMIT_KEY = "time limit";
	private static final String AUTHOR_TEMPORAL_LIMIT_KEY = "author temporal limit";
	private static final String CHARACTER_TEMPORAL_LIMIT_KEY = "character limit";
	private static final String EPISTEMIC_LIMIT_KEY = "epistemic limit";
	
	private static final LinkedHashMap<String, Object> getPlannerKeys(Planner<?> planner) {
		LinkedHashMap<String, Object> map = new LinkedHashMap<>();
		map.put(SEARCH_LIMIT_KEY, nodeLimit(planner.getSearchLimit()));
		map.put(SPACE_LIMIT_KEY, nodeLimit(planner.getSpaceLimit()));
		map.put(TIME_LIMIT_KEY, timeLimit(planner.getTimeLimit()));
		map.put(AUTHOR_TEMPORAL_LIMIT_KEY, depthLimit(planner.getAuthorTemporalLimit()));
		map.put(CHARACTER_TEMPORAL_LIMIT_KEY, depthLimit(planner.getCharacterTemporalLimit()));
		map.put(EPISTEMIC_LIMIT_KEY, depthLimit(planner.getEpistemicLimit()));
		return map;
	}
	
	private static final String SEARCH_METHOD_KEY = "method";
	private static final String COST_KEY = "cost";
	private static final String HEURISTIC_KEY = "heuristic";
	private static final String EXPLANATION_PRUNING_KEY = "explanation pruning";
	
	private static final LinkedHashMap<String, Object> getProgressionPlannerKeys(ProgressionPlanner planner) {
		LinkedHashMap<String, Object> map = new LinkedHashMap<>();
		map.put(SEARCH_METHOD_KEY, planner.getMethod());
		map.put(COST_KEY, planner.getCost());
		map.put(HEURISTIC_KEY, planner.getHeuristic());
		map.put(EXPLANATION_PRUNING_KEY, planner.getExplanationPruning());
		for(Map.Entry<String, Object> entry : getPlannerKeys(planner).entrySet())
			map.put(entry.getKey(), entry.getValue());
		return map;
	}
	
	private static final String PROBLEM_KEY = "problem";
	private static final String START_UTILITY_KEY = "starting utility";
	private static final String GOAL_KEY = "goal utility";
	
	private static final LinkedHashMap<String, Object> getSearchKeys(Search<?> search) {
		LinkedHashMap<String, Object> map = new LinkedHashMap<>();
		map.put(PROBLEM_KEY, search.problem.name);
		map.put(START_UTILITY_KEY, search.problem.getUtility(search.getStart()));
		map.put(GOAL_KEY, search.getGoal());
		map.put(SEARCH_LIMIT_KEY, nodeLimit(search.searchLimit));
		map.put(SPACE_LIMIT_KEY, nodeLimit(search.spaceLimit));
		map.put(TIME_LIMIT_KEY, timeLimit(search.timeLimit));
		map.put(AUTHOR_TEMPORAL_LIMIT_KEY, depthLimit(search.authorTemporalLimit));
		map.put(CHARACTER_TEMPORAL_LIMIT_KEY, depthLimit(search.characterTemporalLimit));
		map.put(EPISTEMIC_LIMIT_KEY, depthLimit(search.epistemicLimit));
		return map;
	}
	
	private static final LinkedHashMap<String, Object> getProgressionSearchKeys(ProgressionSearch search) {
		LinkedHashMap<String, Object> map = new LinkedHashMap<>();
		Method method = Method.BEST_FIRST;
		if(search instanceof ExplanationFirstSearch)
			method = Method.EXPLANATION_FIRST;
		if(search instanceof GoalFirstSearch)
			method = Method.GOAL_FIRST;
		map.put(SEARCH_METHOD_KEY, method);
		map.put(COST_KEY, search.cost);
		map.put(HEURISTIC_KEY, search.heuristic);
		map.put(EXPLANATION_PRUNING_KEY, search.explanationPruning);
		return insertKeys(map, getSearchKeys(search), 3);
	}
	
	private static final String UNLIMITED = "unlimited";
	
	private static final Object nodeLimit(long limit) {
		return limit == Planner.UNLIMITED_NODES ? UNLIMITED : limit;
	}
	
	private static final Object timeLimit(long limit) {
		return limit == Planner.UNLIMITED_TIME ? UNLIMITED : Utilities.time(limit);
	}
	
	private static final Object depthLimit(int limit) {
		return limit == Planner.UNLIMITED_DEPTH ? UNLIMITED : limit;
	}
	
	private static final String SOLUTION_KEY = "solution";
	private static final String UTILITY_ACHIEVED_KEY = "utility achieved";
	private static final String VISITED_KEY = "nodes visited";
	private static final String GENERATED_KEY = "nodes generated";
	private static final String TIME_KEY = "time";
	
	private static final LinkedHashMap<String, Object> getResultKeys(Result<?> result) {
		LinkedHashMap<String, Object> map = new LinkedHashMap<>();
		boolean solution = result.utility != null && Comparison.GREATER_THAN_OR_EQUAL_TO.test(result.utility, result.goal);
		map.put(SOLUTION_KEY, solution);
		if(result.solution != null)
			map.put(UTILITY_ACHIEVED_KEY, result.utility);
		map.put(VISITED_KEY, result.visited);
		map.put(GENERATED_KEY, result.generated);
		map.put(TIME_KEY, Utilities.time(result.time));
		return map;
	}
	
	private static final LinkedHashMap<String, Object> insertKeys(Map<String, Object> entries, LinkedHashMap<String, Object> map, int index) {
		LinkedHashMap<String, Object> result = new LinkedHashMap<>();
		for(Map.Entry<String, Object> entry : map.entrySet()) {
			if(index == 0)
				for(Map.Entry<String, Object> insert : entries.entrySet())
					result.put(insert.getKey(), insert.getValue());
			result.put(entry.getKey(), entry.getValue());
			index--;
		}
		return result;
	}
	
	private static final void printKeys(Printer printer, String title, Map<String, Object> keys, Writer writer) throws IOException {
		int length = 0;
		for(Map.Entry<String, Object> entry : keys.entrySet())
			length = Math.max(length, entry.getKey().length());
		writer.append(title);
		for(Map.Entry<String, Object> entry : keys.entrySet()) {
			writer.append("\n  ");
			writer.append(entry.getKey());
			writer.append(": ");
			for(int i=entry.getKey().length(); i<length; i++)
				writer.append(" ");
			printer.print(entry.getValue(), writer);
		}
	}
}