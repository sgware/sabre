package edu.uky.cs.nil.sabre.io;

import java.io.IOException;
import java.io.Writer;

/**
 * An object printer defines how an object should be converted to text. Object
 * printers are associated with {@link Class Java classes} via a {@link Printer
 * printer}.
 * 
 * @param <T> the type of the object this object printer can accept
 * @author Stephen G. Ware
 */
@FunctionalInterface
public interface ObjectPrinter<T> {

	/**
	 * Converts an object into text and writes that text to a {@link
	 * Writer writer}. The {@link Printer printer} that called this method is
	 * also provided as an argument so that it can be used to print any other
	 * objects referenced by the object bring printed.
	 * 
	 * @param printer the printer which called this method
	 * @param object the object to be converted to text
	 * @param output the writer to which the text will be written
	 * @throws IOException if an exception occurs when writing text to the
	 * writer
	 */
	public void print(Printer printer, T object, Writer output) throws IOException;
}