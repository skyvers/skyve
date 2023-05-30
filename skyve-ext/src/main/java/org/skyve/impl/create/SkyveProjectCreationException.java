package org.skyve.impl.create;

public class SkyveProjectCreationException extends Exception {
    private static final long serialVersionUID = 5254025869573552143L;

	public SkyveProjectCreationException(String message) {
        super(message);
    }

    public SkyveProjectCreationException(String message, Throwable cause) {
        super(message, cause);
    }
}
