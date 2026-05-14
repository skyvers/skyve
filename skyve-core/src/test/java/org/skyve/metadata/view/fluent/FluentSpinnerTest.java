package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.Spinner;

public class FluentSpinnerTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesSpinner() {
		FluentSpinner s = new FluentSpinner();
		assertNotNull(s.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Spinner spinner = new Spinner();
		FluentSpinner s = new FluentSpinner(spinner);
		assertSame(spinner, s.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void minReturnsSelf() {
		FluentSpinner s = new FluentSpinner();
		FluentSpinner result = s.min(0.0);
		assertSame(s, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void maxReturnsSelf() {
		FluentSpinner s = new FluentSpinner();
		FluentSpinner result = s.max(100.0);
		assertSame(s, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void stepReturnsSelf() {
		FluentSpinner s = new FluentSpinner();
		FluentSpinner result = s.step(1.0);
		assertSame(s, result);
	}
}
