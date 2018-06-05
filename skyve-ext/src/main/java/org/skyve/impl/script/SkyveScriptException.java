package org.skyve.impl.script;

import org.skyve.domain.messages.SkyveException;

public class SkyveScriptException extends SkyveException {

	private static final long serialVersionUID = -8388742662234355631L;

	private ExceptionType type;
	private int lineNumber;

	public SkyveScriptException(final ExceptionType type, final String message, final int lineNumber) {
		super(message);
		this.lineNumber = lineNumber;
		this.type = type;
	}

	public int getLineNumber() {
		return lineNumber;
	}

	public ExceptionType getType() {
		return type;
	}

	public enum ExceptionType {
		info, warning, error, critical
	}
}
