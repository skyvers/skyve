package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;

public class FluentTextFieldTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesTextField() {
		FluentTextField tf = new FluentTextField();
		assertNotNull(tf.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		TextField text = new TextField();
		FluentTextField tf = new FluentTextField(text);
		assertSame(text, tf.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void editableReturnsSelf() {
		FluentTextField tf = new FluentTextField();
		FluentTextField result = tf.editable(false);
		assertSame(tf, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void pixelWidthReturnsSelf() {
		FluentTextField tf = new FluentTextField();
		FluentTextField result = tf.pixelWidth(200);
		assertSame(tf, result);
	}
}
