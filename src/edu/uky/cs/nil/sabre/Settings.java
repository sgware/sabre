package edu.uky.cs.nil.sabre;

/**
 * Important settings and keywords.
 * 
 * @author Stephen G. Ware
 */
public class Settings {

	/** The full name of this software library */
	public static final String TITLE = "The Sabre Narrative Planner";
	
	/** The list of primary authors */
	public static final String AUTHORS = "Stephen G. Ware";
	
	/** The major version number comes before the decimal points */
	public static final int MAJOR_VERSION_NUMBER = 0;
	
	/** The minor version number comes after the decimal point */
	public static final int MINOR_VERSION_NUMBER = 7;
	
	/** The full version number (major + minor) as a string */
	public static final String VERSION_STRING = MAJOR_VERSION_NUMBER + "." + MINOR_VERSION_NUMBER;
	
	/**
	 * A long encoding the version number which can be used as a serial version UID
	 */
	public static final long VERSION_UID = java.nio.ByteBuffer.allocate(8).putInt(MAJOR_VERSION_NUMBER).putInt(MINOR_VERSION_NUMBER).getLong(0);
	
	/** A header including title, authors, and version number */
	public static final String CREDITS = TITLE + " v" + VERSION_STRING + " by " + AUTHORS;
	
	/** The name of the Boolean type which is pre-defined in all problems */
	public static final String BOOLEAN_TYPE_NAME = "boolean";
	
	/** The ID number and index of the Boolean type */
	public static final int BOOLEAN_TYPE_ID = 0;
	
	/** The comment associated with the Boolean type by default */
	public static final String BOOLEAN_TYPE_COMMENT = "";
	
	/** The name of the number type which is pre-defined in all problems */
	public static final String NUMBER_TYPE_NAME = "number";
	
	/** The ID number and index of the number type */
	public static final int NUMBER_TYPE_ID = 1;
	
	/** The comment associated with the number type by default */
	public static final String NUMBER_TYPE_COMMENT = "";
	
	/**
	 * The name of the entity type which is the parent type of all other types
	 * and is pre-defined in all problems
	 */
	public static final String ENTITY_TYPE_NAME = "entity";
	
	/** The ID number and index of the entity type */
	public static final int ENTITY_TYPE_ID = 2;
	
	/** The comment associated with the entity type by default */
	public static final String ENTITY_TYPE_COMMENT = "";
	
	/**
	 * The name of the character type which represents beings with beliefs and
	 * intentions and is pre-defined in all problems
	 */
	public static final String CHARACTER_TYPE_NAME = "character";
	
	/** The ID number and index of the character type */
	public static final int CHARACTER_TYPE_ID = 3;
	
	/** The comment associated with the character type by default */
	public static final String CHARACTER_TYPE_COMMENT = "";
	
	/** The comment associated with a newly defined type by default */
	public static final String DEFAULT_TYPE_COMMENT = "";
	
	/** The comment associated with a newly defined entity by default */
	public static final String DEFAULT_ENTITY_COMMENT = "";	
	
	/** The comment associated with a newly defined problem by default */
	public static final String DEFAULT_PROBLEM_COMMENT = "";
}