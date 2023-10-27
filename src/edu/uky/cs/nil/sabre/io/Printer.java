package edu.uky.cs.nil.sabre.io;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayDeque;
import java.util.HashMap;

import edu.uky.cs.nil.sabre.Exceptions;

/**
 * A printer accepts any object as input and converts it to a text
 * representation. Its primary method is {@link #print(Object, Writer)}, which
 * converts the given object to text and writes that text to the given {@link
 * Writer writer}. A printer can be configured by associating any {@link Class
 * Java class} with an {@link ObjectPrinter object printer} via the {@link
 * #setPrinter(Class, ObjectPrinter)} method.
 * <p>
 * When printing an object, the printer will first try to find the {@link
 * ObjectPrinter object printer} associated with the object's class. If no
 * object printer is defined for that class, it will search for one defined
 * for the object's superclass or any of its interfaces (and so on,
 * recursively). If it still cannot find an object printer, it throws an
 * {@link IllegalArgumentException}.
 * 
 * @author Stephen G. Ware
 */
public class Printer {

	private final HashMap<Class<?>, ObjectPrinter<?>> printers = new HashMap<>();
	
	/**
	 * Finds the {@link ObjectPrinter object printer} associated with the given
	 * {@link Class Java class}, or if there is no association for that class,
	 * the object printer associated with the class's superclass or one of its
	 * interfaces (and their superclasses and interfaces, and so on).
	 * 
	 * @param <T> the type of the class object
	 * @param type the class object
	 * @return the object printer associated with the class, with its
	 * superclass, with one of its interfaces, or null if no object printer is
	 * associated with this class or any of its parent types
	 */
	@SuppressWarnings("unchecked")
	public <T> ObjectPrinter<? super T> getPrinter(Class<T> type) {
		ArrayDeque<Class<?>> types = new ArrayDeque<>(5);
		types.push(type);
		while(!types.isEmpty()) {
			Class<?> t = types.poll();
			ObjectPrinter<T> printer = (ObjectPrinter<T>) printers.get(t);
			if(printer != null) 
				return printer;
			else if(t.getSuperclass() != null)
				types.add(t.getSuperclass());
			for(Class<?> i : t.getInterfaces())
				types.add(i);
		}
		return null;
	}
	
	/**
	 * Associates an {@link ObjectPrinter object printer} with a {@link
	 * Class Java class}. The object printer will be used to print objects of
	 * this type, or object of types that inherent from this type if no object
	 * printer is defined for those subtypes.
	 * 
	 * @param <T> the type of the class object
	 * @param type the class object
	 * @param printer the object printer to associate with the class
	 */
	public <T> void setPrinter(Class<T> type, ObjectPrinter<? super T> printer) {
		printers.put(type, printer);
	}
	
	/**
	 * Converts an object to text and writes that text to a {@link
	 * Writer writer}. The printer will use the {@link ObjectPrinter object
	 * printer} associated with {@link Object#getClass() the object's class},
	 * or its superclass, or one of its interfaces, and so on according to the
	 * {@link #getPrinter(Class)} method.
	 * 
	 * @param <T> the type of the object being printed
	 * @param object the object to print
	 * @param writer the writer to which text will be written
	 * @throws IOException if an exception occurs while writing text to the
	 * writer
	 */
	@SuppressWarnings("unchecked")
	public <T> void print(T object, Writer writer) throws IOException {
		ObjectPrinter<? super T> printer = getPrinter((Class<T>) object.getClass());
		if(printer == null)
			throw Exceptions.noPrinter(object.getClass().getSimpleName());
		printer.print(this, object, writer);
	}
	
	/**
	 * Converts an object to text and writes that text to a {@link File file}
	 * via {@link #print(Object, Writer)}.
	 * 
	 * @param <T> the type of the object being printed
	 * @param object the object to print
	 * @param file the file to which text will be written
	 * @throws IOException if an exception occurs while writing text to the
	 * file
	 */
	public <T> void print(T object, File file) throws IOException {
		try(BufferedWriter writer = new BufferedWriter(new FileWriter(file))) {
			print(object, writer);
		}
	}
	
	/**
	 * Converts an object to text via {@link #print(Object, Writer)} and
	 * returns that text as a string.
	 * 
	 * @param object the object to print
	 * @return a string of text representing the object
	 */
	public String toString(Object object) {
		StringWriter string = new StringWriter();
		try {
			print(object, string);
		}
		catch(IOException e) {
			// will not happen
		}
		return string.toString();
	}
}