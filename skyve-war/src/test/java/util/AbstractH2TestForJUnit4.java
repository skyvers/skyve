package util;

import java.io.IOException;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;

/**
 * Legacy Skyve test runner, uses JUnit 4.
 */
public abstract class AbstractH2TestForJUnit4 extends InternalBaseH2Test {
	// Add common mocks here
	// @Mock
	// protected WebContext webContext;

	public AbstractH2TestForJUnit4() {
		super();
	}

	@BeforeClass
	public static void beforeClass() throws Exception {
		internalSetup();
	}

	@AfterClass
	public static void afterClass() throws IOException {
		internalTearDown();
	}

	@Before
	@SuppressWarnings("static-method")
	public void beforeBase() throws Exception {
		internalBefore();
	}

	@After
	@SuppressWarnings("static-method")
	public void afterBase() {
		internalAfter();
	}
}
