package org.skyve.impl.generate.client.flutter;

public class FlutterGeneratorException extends RuntimeException {

    private static final long serialVersionUID = 4L;

    public FlutterGeneratorException() {
    }

    public FlutterGeneratorException(String message) {
        super(message);
    }

    public FlutterGeneratorException(Throwable cause) {
        super(cause);
    }

    public FlutterGeneratorException(String message, Throwable cause) {
        super(message, cause);
    }

    public FlutterGeneratorException(String message, Throwable cause, boolean enableSuppression,
            boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

}
