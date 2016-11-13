package org.skyve.bizport;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.skyve.domain.messages.SkyveException;

/**
 * 
 */
public final class BizPortException extends SkyveException {
	private static final long serialVersionUID = 4401759405779814684L;

	/**
	 * 
	 */
	public static final class Problem implements Comparable<Problem> {
		private String what;
		private String where;
		boolean error = false;
		private String string;
		
		/**
		 * 
		 * @param what
		 * @param where
		 */
		public Problem(String what, String where) {
			this.what = what;
			this.where = (where == null) ? "" : where;
		}

		/**
		 * 
		 * @return
		 */
		public String getWhat() {
			return what;
		}

		/**
		 * 
		 * @return
		 */
		public String getWhere() {
			return where;
		}

		public boolean isError() {
			return error;
		}
		
		/**
		 * 
		 */
		@Override
		public int compareTo(Problem o) {
			if (o == null) {
				return 1;
			}

			return toString().compareTo(o.toString());
		}

		/**
		 * 
		 */
		@Override
		public boolean equals(Object o) {
			if (o instanceof Problem) {
				Problem p = (Problem) o;
				return (compareTo(p) == 0);
			}

			return false;
		}
		
		/**
		 * 
		 */
		@Override
		public int hashCode() {
			return toString().hashCode();
		}

		/**
		 * 
		 */
		@Override
		public String toString() {
			if (string == null) {
				string = new StringBuilder(128).append(error ? "Error " : "Warning ").append(what).append(" : ").append(where).toString();
			}
			
			return string;
		}
	}
	
	// Errors keyed by where; 1 per cell reference is allowed
	private Map<String, Problem> errors = new LinkedHashMap<>();
	
	// Warnings - any amount of these are allowed
	private List<Problem> warnings = new ArrayList<>();
	
	/**
	 * 
	 */
	public BizPortException() {
		super();
	}

	/**
	 * 
	 * @return
	 */
	public boolean hasProblems() {
		return (! (errors.isEmpty() && warnings.isEmpty()));
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean hasErrors() {
		return (! errors.isEmpty());
	}

	/**
	 * 
	 * @param problems
	 */
	public void addErrors(List<Problem> problems) {
		for (Problem problem : problems) {
			problem.error = true;
			addError(problem);
		}
	}
	
	/**
	 * 
	 * @param problem
	 */
	public void addError(Problem problem) {
		// this will replace any previous problem
		// Hopefully the last problem encountered is the most pertinent
		errors.put(problem.getWhere(), problem);
		if (errors.size() > 50) {
			throw this;
		}
	}

	/**
	 * 
	 * @param problem
	 */
	public void addWarning(Problem problem) {
		if (warnings.size() < 50) {
			warnings.add(problem);
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public Iterable<Problem> getErrors() {
		return errors.values();
	}
	
	/**
	 * 
	 * @return
	 */
	public Iterable<Problem> getWarnings() {
		return warnings;
	}
}
