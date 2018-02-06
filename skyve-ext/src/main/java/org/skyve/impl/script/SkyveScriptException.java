package org.skyve.impl.script;

public class SkyveScriptException {

	private String message;
	private ExceptionType type;
	private int lineNumber;

	public SkyveScriptException(final ExceptionType type, final String message, final int lineNumber) {
		this.lineNumber = lineNumber;
		this.message = message;
		this.type = type;
	}

	public int getLineNumber() {
		return lineNumber;
	}

	public String getMessage() {
		return message;
	}

	public ExceptionType getType() {
		return type;
	}

	public enum ExceptionType {
		info, warning, error, critical
	}
}
