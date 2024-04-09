package util;

import java.io.IOException;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;

/**
 * Base Skyve test runner, uses JUnit 5.
 */
public abstract class AbstractH2Test extends InternalBaseH2Test {
	// Add common mocks here
	// @Mock
	// protected WebContext webContext;

	public AbstractH2Test() {
		super();
	}

	@BeforeAll
	public static void setUp() {
		internalSetup();
	}

	@AfterAll
	public static void tearDown() throws IOException {
		internalTearDown();
	}

	@BeforeEach
	@SuppressWarnings("static-method")
	public void beforeBase() throws Exception {
		internalBefore();
	}

	@AfterEach
	@SuppressWarnings("static-method")
	public void afterBase() {
		internalAfter();
	}
}
