package org.skyve.domain.messages;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Collects errors and warnings encountered during a Skyve data-import or file-upload operation.
 *
 * <p>Unlike most Skyve exceptions, {@code UploadException} is designed to be populated
 * incrementally: the import processor adds {@link Problem} instances as it encounters
 * issues, and the exception is thrown (or returned) only when processing is complete.
 * This allows the UI to present a comprehensive problem report rather than halting on
 * the first error.
 *
 * <p>At most 50 errors and 50 warnings are retained. Calling {@link #addError} after
 * 50 errors have accumulated throws {@code this} immediately to halt further processing.
 *
 * <p>Each {@link Problem} records a "what" (description of the issue) and a "where"
 * (cell reference, line number, or other location identifier).
 */
public class UploadException extends SkyveException {
	private static final long serialVersionUID = 4401759405779814684L;

	// Errors keyed by where; 1 per cell reference is allowed
	private Map<String, Problem> errors = new LinkedHashMap<>();

	// Warnings - any amount of these are allowed
	private List<Problem> warnings = new ArrayList<>();

	/**
	 * Creates a new UploadException instance.
	 */
	public UploadException() {
		super();
	}
	/**
	* Returns true there are any errors or warnings in this Upload.
	* 
	* @return True if any errors or warning problems
	*/
	public boolean hasProblems() {
		return (! (errors.isEmpty() && warnings.isEmpty()));
	}

	/**
	 * Returns true if the list of errors is not empty.
	 * 
	 * @return True if there are errors in this upload
	 */
	public boolean hasErrors() {
		return (! errors.isEmpty());
	}

	/**
	 * Executes addErrors.
	 * @param problems the problems
	 */
	public void addErrors(List<Problem> problems) {
		for (Problem problem : problems) {
			addError(problem);
		}
	}

	/**
	 * Add an error to the list of errors. Only the first 50 errors are stored.
	 * 
	 * Problems with the same where location will overwrite the previous problem.
	 * 
	 * @param problem The problem to add to the list of errors.
	 */
	public void addError(Problem problem) {
		// this will replace any previous problem
		// Hopefully the last problem encountered is the most pertinent
        problem.error = true;
		errors.put(problem.getWhere(), problem);
		if (errors.size() > 50) {
			throw this;
		}
	}

	/**
	 * Add a problem to the list of warnings. Only the first 50 warnings are stored.
	 * 
	 * @param problem The problem to add to the list of warnings.
	 */
	public void addWarning(Problem problem) {
		if (warnings.size() < 50) {
			warnings.add(problem);
		}
	}

	/**
	 * Returns the list of errors that were encountered while processing the file.
	 * 
	 * @return An Iterable of error Problems
	 */
	public Iterable<Problem> getErrors() {
		return errors.values();
	}

	/**
	 * Returns the list of warnings that were encountered while processing the file.
	 * 
	 * @return An Iterable of warning Problems
	 */
	public Iterable<Problem> getWarnings() {
		return warnings;
	}

	/**
	 * A Problem is a warning or error that is associated with a particular location during the upload.
	 */
	public static class Problem implements Comparable<Problem>, Serializable {
		private static final long serialVersionUID = -2520940056151904556L;

		private String what;
		private String where;
		boolean error = false;
		private String string;
		
		/**
		 * Creates a new Problem with the given what and where.
		 * 
		 * @param what Description of the problem
		 * @param where Where the problem occurred, e.g. which line of the file
		 */
		public Problem(String what, String where) {
			this.what = what;
			this.where = (where == null) ? "" : where;
		}

		/**
		 * Returns the description of the problem
		 * 
		 * @return The what describing the problem detected
		 */
		public String getWhat() {
			return what;
		}

		/**
		 * Returns the location of the problem.
		 *
		 * @return Where the problem was detected
		 */
		public String getWhere() {
			return where;
		}
		/**
		 * Returns true if this problem is an error (vs a warning).
		 * 
		 * @return True if this problem is an error, false if it is a warning
		 */
		public boolean isError() {
			return error;
		}
		
		/**
		 * Compares this instance with the given value.
		 * @param o the o
		 * @return the result
		 */
		@Override
		public int compareTo(Problem o) {
			if (o == null) {
				return 1;
			}

			return toString().compareTo(o.toString());
		}

		/**
		 * Returns whether this instance is equal to the given object.
		 * @param o the o
		 * @return the result
		 */
		@Override
		public boolean equals(Object o) {
			if (o instanceof Problem p) {
				return (compareTo(p) == 0);
			}

			return false;
		}
		
		/**
		 * Returns whether hCode.
		 * @return the result
		 */
		@Override
		public int hashCode() {
			return toString().hashCode();
		}

		/**
		 * Returns a string representation of this instance.
		 * @return the result
		 */
		@Override
		public String toString() {
			if (string == null) {
				string = new StringBuilder(128).append(error ? "Error " : "Warning ").append(what).append(" : ").append(where).toString();
			}
			
			return string;
		}
	}
}
