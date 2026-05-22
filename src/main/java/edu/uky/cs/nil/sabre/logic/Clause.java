package edu.uky.cs.nil.sabre.logic;

import java.io.ObjectStreamException;

import edu.uky.cs.nil.sabre.Settings;
import edu.uky.cs.nil.sabre.State;
import edu.uky.cs.nil.sabre.io.DefaultParser;
import edu.uky.cs.nil.sabre.util.ImmutableArray;

/**
 * A clause is a {@link Conjunction conjunction} of {@link Atom atoms}. Clauses
 * play a key role in {@link Expression#toPrecondition() disjunctive normal
 * form} expressions and have some additional logic for efficiently {@link
 * #add(Atom) adding} and {@link #remove(Atom) removing} atoms. The atom of a 
 * clause are always in their {@link Comparable#compareTo(Object) natural
 * order}.
 * 
 * @param <A> the type of this clause's arguments
 * @author Stephen G. Ware
 */
public class Clause<A extends Atom> extends Conjunction<A> {
	
	/** Serial version ID */
	private static final long serialVersionUID = Settings.VERSION_UID;
	
	/**
	 * The null clause is a empty clause which is logically equivalent to
	 * {@link False#FALSE false}. Though an empty conjunction would normally be
	 * logically equivalent to {@link True#TRUE true} (see {@link #EMPTY the
	 * empty clause}), this object exists for situations where a clause object
	 * is needed that is trivially false, such as when an {@link
	 * edu.uky.cs.nil.sabre.Event event} has a contradictory effect. Atoms
	 * cannot be added or removed from the null clause (that is, adding atoms
	 * to or removing atoms from the null clause always results in the null
	 * clause itself).
	 */
	public static final Clause<Atom> NULL = new Clause<Atom>(new ImmutableArray<>()) {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public boolean equals(Object other) {
			return this == other;
		}
		
		@Override
		public int hashCode() {
			return super.hashCode() * -1;
		}
		
		@Override
		public String toString() {
			return DefaultParser.NULL_CLAUSE_KEYWORD;
		}
		
		@Override
		public int compareTo(Logical other) {
			if(getClass().equals(other.getClass())) {
				if(other.equals(this))
					return 0;
				else
					return -1;
			}
			else
				return super.compareTo(other);
		}
		
		/**
		 * Returns {@link False#FALSE false}.
		 * 
		 * @returns {@link False#FALSE false}
		 */
		@Override
		public False simplify() {
			return False.FALSE;
		}
		
		/**
		 * Returns {@link False#FALSE false}.
		 * 
		 * @returns {@link False#FALSE false}
		 */
		@Override
		public Value evaluate(State state) {
			return False.FALSE;
		}
		
		/**
		 * Returns {@link True#TRUE true}.
		 * 
		 * @returns {@link True#TRUE true}
		 */
		@Override
		public True negate() {
			return True.TRUE;
		}

		/**
		 * Returns {@link False#FALSE false}.
		 * 
		 * @returns {@link False#FALSE false}
		 */
		@Override
		public Disjunction<Clause<Precondition>> toPrecondition() {
			return False.FALSE.toPrecondition();
		}
		
		/**
		 * Returns {@link #NULL the null clause}.
		 * 
		 * @returns {@link #NULL the null clause}
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Clause<Effect> toEffect() {
			return (Clause<Effect>) (Clause<?>) this;
		}
		
		/**
		 * Returns {@link #NULL the null clause}.
		 * 
		 * @returns {@link #NULL the null clause}
		 */
		@Override
		public Clause<Atom> add(Clause<Atom> clause) {
			return this;
		}
		
		/**
		 * Returns {@link #NULL the null clause}.
		 * 
		 * @returns {@link #NULL the null clause}
		 */
		@Override
		public Clause<Atom> remove(Clause<Atom> clause) {
			return this;
		}
		
		private Object readResolve() throws ObjectStreamException {
			return NULL;
		}
	};
	
	/**
	 * The empty clause is clause with zero atoms which is logically equivalent
	 * to {@link True#TRUE true}.
	 */
	public static final Clause<Atom> EMPTY = new Clause<Atom>(new ImmutableArray<>()) {
		
		/** Serial version ID */
		private static final long serialVersionUID = 1;
		
		@Override
		public boolean equals(Object other) {
			return this == other;
		}
		
		@Override
		public int compareTo(Logical other) {
			if(getClass().equals(other.getClass())) {
				if(other.equals(NULL))
					return 1;
				else if(other.equals(this))
					return 0;
				else
					return -1;
			}
			else
				return super.compareTo(other);
		}
		
		/**
		 * Returns {@link True#TRUE true}.
		 * 
		 * @returns {@link True#TRUE true}
		 */
		@Override
		public True simplify() {
			return True.TRUE;
		}
		
		/**
		 * Returns {@link False#FALSE false}.
		 * 
		 * @returns {@link False#FALSE false}
		 */
		@Override
		public False negate() {
			return False.FALSE;
		}
		
		/**
		 * Returns {@link True#TRUE true}.
		 * 
		 * @returns {@link True#TRUE true}
		 */
		@Override
		public Disjunction<Clause<Precondition>> toPrecondition() {
			return True.TRUE.toPrecondition();
		}
		
		/**
		 * Returns {@link #EMPTY the empty clause}.
		 * 
		 * @returns {@link #EMPTY the empty clause}
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Clause<Effect> toEffect() {
			return (Clause<Effect>) (Clause<?>) this;
		}
		
		private Object readResolve() throws ObjectStreamException {
			return EMPTY;
		}
	};

	private Clause(ImmutableArray<A> arguments) {
		super(arguments);
	}
	
	/**
	 * Constructs a clause from a single atom.
	 * 
	 * @param atom the atom
	 */
	public Clause(A atom) {
		this(new ImmutableArray<>(atom));
	}
	
	/**
	 * Clauses are simplified incrementally as atoms are added or removed, so
	 * this method always returns this object itself with no changes.
	 * 
	 * @return this clause
	 */
	@Override
	public Expression simplify() {
		return this;
	}
	
	@Override
	public Clause<A> prepend(Parameter character) {
		return new Clause<A>(arguments.apply(atom -> ((Atom) atom).prepend(character)));
	}
	
	@Override
	@SuppressWarnings("unchecked")
	public Clause<Effect> toEffect() {
		if(isEffect())
			return (Clause<Effect>) this;
		else
			return super.toEffect();
	}
	
	/**
	 * Tests whether every atom in this clause is a {@link Precondition
	 * precondition}.
	 * 
	 * @return true if every atom is a precondition or if there are no atoms,
	 * false otherwise
	 */
	public boolean isPrecondition() {
		for(int i=0; i<arguments.size(); i++)
			if(!(arguments.get(i) instanceof Precondition))
				return false;
		return true;
	}
	
	/**
	 * Tests whether every atom is an {@link Effect effect}.
	 * 
	 * @return true if every atom is an effect or if there are no atoms, false
	 * otherwise
	 */
	public boolean isEffect() {
		for(int i=0; i<arguments.size(); i++)
			if(!(arguments.get(i) instanceof Effect))
				return false;
		return true;
	}
	
	/**
	 * Adds an atom to this clause and simplifies the clause. See {@link
	 * #add(Clause)} for details on adding to clauses.
	 * 
	 * @param atom the atom to be added
	 * @return the new clause that results from adding the atom
	 */
	public Clause<A> add(A atom) {
		return add(new Clause<A>(new ImmutableArray<>(atom)));
	}
	
	/**
	 * Adds all atoms from this clause and the given clause together to form a
	 * new clause. The resulting clause will not necessarily have as many atoms
	 * as both clauses combined because the clause is simplified as it is
	 * built. For example, if merging the clauses would create a contradiction,
	 * perhaps because an atom from this clause {@link Atom#negates(Atom)
	 * negates} an atom in the given clause, {@link #NULL the null clause} will
	 * be returned. If atoms from the two clauses can be {@link
	 * Atom#combine(Atom) combined}, they will be.
	 * 
	 * @param clause the clause to combine with this clause
	 * @return a simplified clause resulting from combining this clause and the
	 * given clause
	 */
	@SuppressWarnings("unchecked")
	public Clause<A> add(Clause<A> clause) {
		if(clause.equals(NULL))
			return (Clause<A>) NULL;
		A[] atoms = add(null, this, 0, clause, 0, 0);
		if(atoms == null)
			return (Clause<A>) NULL;
		else if(atoms.length == 0)
			return (Clause<A>) EMPTY;
		else
			return new Clause<>(new ImmutableArray<>(atoms));
	}
	
	@SuppressWarnings("unchecked")
	private static final <A extends Atom> A[] add(A atom, Clause<A> c1, int i1, Clause<A> c2, int i2, int size) {
		A a1 = i1 == c1.size() ? null : c1.get(i1);
		A a2 = i2 == c2.size() ? null : c2.get(i2);
		if(atom == null && a1 == null && a2 == null)
			return (A[]) new Atom[size];
		else if(atom == null) {
			if(a1 == null)
				return add(a2, c1, i1, c2, i2 + 1, size);
			else if(a2 == null || a1.compareTo(a2) <= 0)
				return add(a1, c1, i1 + 1, c2, i2, size);
			else
				return add(a2, c1, i1, c2, i2 + 1, size);
		}
		if(a1 != null && atom.negates(a1))
			return null;
		else if(a2 != null && atom.negates(a2))
			return null;
		A a1c = a1 == null ? null : (A) atom.combine(a1);
		if(a1c != null)
			return add(a1c, c1, i1 + 1, c2, i2, size);	
		A a2c = a2 == null ? null : (A) atom.combine(a2);
		if(a2c != null)
			return add(a2c, c1, i1, c2, i2 + 1, size);
		else {
			A[] atoms = add(null, c1, i1, c2, i2, size + 1);
			if(atoms == null)
				return null;
			atoms[size] = atom;
			return atoms;
		}
	}
	
	/**
	 * Removes an atom from this clause. See {@link #remove(Clause)} for
	 * details on removing from clauses.
	 * 
	 * @param atom the atom to be removed
	 * @return a simplified clause resulting from removing the atom
	 */
	public Clause<A> remove(A atom) {
		return remove(new Clause<A>(new ImmutableArray<>(atom)));
	}
	
	/**
	 * Removes atoms from this clause if they appear, or if a more specific
	 * version appears, in the given clause, or returns {@link #NULL the null
	 * clause} if an atom in the given clause contradict this clause.
	 * Let {@code a} be some atom in this clause and {@code b} be an atom in
	 * the given clause. If {@code a} and {@code b} are the same, {@code a}
	 * will be removed. If {@code a} and {@code b} can be {@link
	 * Atom#combine(Atom) combined}, and the result is {@code b}, this means
	 * {@code b} was more specific than {@code a}, and {@code a} will be
	 * removed. If they can be combined and the result is something other than
	 * {@code b}, then {@code a} will not removed. If {@code a} {@link
	 * Atom#negates(Atom) negates} {@code b}, this method returns the {@link
	 * #NULL null clause}.
	 * <p>
	 * This method can be used to regress this clause over the given clause.
	 * This clause can be viewed as a condition (such as the {@link
	 * edu.uky.cs.nil.sabre.Event#getPrecondition() precondition of an event},
	 * though this clause does not need to have {@link Precondition
	 * precondition atoms}) and the given clause can be viewed as changes made
	 * (such as the {@link edu.uky.cs.nil.sabre.Event#getEffect() effect of an
	 * event}, though the given clause does not need to have {@link Effect
	 * effect atoms}). Regressing this clause means removing the conditions
	 * from this clause which the given clause could have caused. So if this
	 * clause has the condition that {@code f > 3} and we regress the effect
	 * {@code f = 4} (which is more specific and satisfied the condition), the
	 * condition would be removed. However, if we regress the effect
	 * {@code f = 2} (which contradicts the condition), than there is no way
	 * that effect could have led to a condition where {@code f > 3}, so this
	 * method returns the null clause.
	 * 
	 * @param clause the clause whose atoms will be removed from this clause
	 * @return this clause, with atoms removed if they (or more specific
	 * versions) appear in the given clause, or {@link #NULL the null clause}
	 * if some atom in the given clause contradicts an atom in this clause
	 */
	@SuppressWarnings("unchecked")
	public Clause<A> remove(Clause<A> clause) {
		if(clause.equals(NULL))
			return (Clause<A>) NULL;
		A[] atoms = remove(this, 0, clause, 0, 0);
		if(atoms == null)
			return (Clause<A>) NULL;
		else if(atoms.length == 0)
			return (Clause<A>) EMPTY;
		else
			return new Clause<>(new ImmutableArray<>(atoms));
	}
	
	@SuppressWarnings("unchecked")
	private static final <A extends Atom> A[] remove(Clause<A> c1, int i1, Clause<A> c2, int i2, int size) {
		if(i1 == c1.size())
			return (A[]) new Atom[size];
		else if(i2 == c2.size()) {
			A[] atoms = remove(c1, i1 + 1, c2, i2, size + 1);
			atoms[size] = c1.get(i1);
			return atoms;
		}
		A a1 = c1.get(i1);
		A a2 = c2.get(i2);
		if(a1.negates(a2))
			return null;
		Atom combined = a1.combine(a2);
		if(combined != null && combined.equals(a2))
			return remove(c1, i1 + 1, c2, i2, size);
		else if(a1.compareTo(a2) < 0) {
			A[] atoms = remove(c1, i1 + 1, c2, i2, size + 1);
			if(atoms == null)
				return null;
			atoms[size] = a1;
			return atoms;
		}
		else
			return remove(c1, i1, c2, i2 + 1, size);
	}
}