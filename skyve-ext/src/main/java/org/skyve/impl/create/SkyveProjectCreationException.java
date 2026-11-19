package org.skyve.impl.create;

import jakarta.annotation.Nullable;

/**
 * Signals a failure during Skyve project scaffolding by
 * {@link MavenSkyveProject}.
 */
public class SkyveProjectCreationException extends Exception {
    private static final long serialVersionUID = 5254025869573552143L;

	public SkyveProjectCreationException(@Nullable String message) {
        super(message);
    }

    public SkyveProjectCreationException(@Nullable String message, @Nullable Throwable cause) {
        super(message, cause);
    }
}
