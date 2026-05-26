package org.skyve.impl.create;

/**
 * Signals a failure during Skyve project scaffolding by
 * {@link MavenSkyveProject}.
 */
public class SkyveProjectCreationException extends Exception {
    private static final long serialVersionUID = 5254025869573552143L;

	public SkyveProjectCreationException(String message) {
        super(message);
    }

    public SkyveProjectCreationException(String message, Throwable cause) {
        super(message, cause);
    }
}
