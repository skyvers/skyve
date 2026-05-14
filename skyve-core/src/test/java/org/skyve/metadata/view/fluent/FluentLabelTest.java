package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.Label;

public class FluentLabelTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesLabel() {
		FluentLabel l = new FluentLabel();
		assertNotNull(l.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		Label label = new Label();
		FluentLabel l = new FluentLabel(label);
		assertSame(label, l.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void valueReturnsSelf() {
		FluentLabel l = new FluentLabel();
		FluentLabel result = l.value("My Label");
		assertSame(l, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void forBindingReturnsSelf() {
		FluentLabel l = new FluentLabel();
		FluentLabel result = l.forBinding("someAttribute");
		assertSame(l, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentLabel l = new FluentLabel();
		FluentLabel result = l.invisibleConditionName("hiddenCond");
		assertSame(l, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void formattedReturnsSelf() {
		FluentLabel l = new FluentLabel();
		FluentLabel result = l.formatted(true);
		assertSame(l, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void escapeReturnsSelf() {
		FluentLabel l = new FluentLabel();
		FluentLabel result = l.escape(false);
		assertSame(l, result);
	}
}
