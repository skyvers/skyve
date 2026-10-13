package org.skyve.impl.generate.client.flutter;

import jakarta.annotation.Nullable;

/**
 * Runtime exception raised by Flutter client generation.
 */
public class FlutterGeneratorException extends RuntimeException {

    private static final long serialVersionUID = 4L;

    public FlutterGeneratorException() {
    }

    public FlutterGeneratorException(@Nullable String message) {
        super(message);
    }

    public FlutterGeneratorException(@Nullable Throwable cause) {
        super(cause);
    }

    public FlutterGeneratorException(@Nullable String message, @Nullable Throwable cause) {
        super(message, cause);
    }

    public FlutterGeneratorException(@Nullable String message, @Nullable Throwable cause, boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
