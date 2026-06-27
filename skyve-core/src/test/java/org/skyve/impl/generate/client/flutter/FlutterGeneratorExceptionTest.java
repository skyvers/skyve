package org.skyve.impl.generate.client.flutter;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;

@SuppressWarnings("static-method")
class FlutterGeneratorExceptionTest {

	@Test
	void noArgConstructorCreatesException() {
		FlutterGeneratorException ex = new FlutterGeneratorException();
		assertNotNull(ex);
		assertNull(ex.getMessage());
		assertNull(ex.getCause());
	}

	@Test
	void messageConstructorSetsMessage() {
		FlutterGeneratorException ex = new FlutterGeneratorException("test error");
		assertEquals("test error", ex.getMessage());
		assertNull(ex.getCause());
	}

	@Test
	void causeConstructorSetsCause() {
		RuntimeException cause = new RuntimeException("root");
		FlutterGeneratorException ex = new FlutterGeneratorException(cause);
		assertSame(cause, ex.getCause());
	}

	@Test
	void messageCauseConstructorSetsBoth() {
		RuntimeException cause = new RuntimeException("root");
		FlutterGeneratorException ex = new FlutterGeneratorException("msg", cause);
		assertEquals("msg", ex.getMessage());
		assertSame(cause, ex.getCause());
	}

	@Test
	void fullConstructorWithSuppressedAndWritableStackTrace() {
		RuntimeException cause = new RuntimeException("root");
		FlutterGeneratorException ex = new FlutterGeneratorException("msg", cause, false, false);
		assertEquals("msg", ex.getMessage());
		assertSame(cause, ex.getCause());
	}

	@Test
	void isRuntimeException() {
		FlutterGeneratorException ex = new FlutterGeneratorException("test");
		assertInstanceOf(RuntimeException.class, ex);
	}
}
