package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.DialogButton;

class FluentDialogButtonTest {

	@Test
	@SuppressWarnings("static-method")
	void defaultConstructorCreatesDialogButton() {
		FluentDialogButton b = new FluentDialogButton();
		assertNotNull(b.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void wrappingConstructorUsesProvided() {
		DialogButton button = new DialogButton();
		FluentDialogButton b = new FluentDialogButton(button);
		assertSame(button, b.get());
	}

	@Test
	@SuppressWarnings("static-method")
	void displayNameReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.displayName("OK");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void dialogNameReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.dialogName("confirmDialog");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void commandReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.command("save");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void dialogWidthReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.dialogWidth(400);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void dialogHeightReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.dialogHeight(200);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void modalDialogReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.modalDialog(true);
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void invisibleConditionNameReturnsSelf() {
		FluentDialogButton b = new FluentDialogButton();
		FluentDialogButton result = b.invisibleConditionName("hiddenCond");
		assertSame(b, result);
	}

	@Test
	@SuppressWarnings("static-method")
	void fromWithNonEmptyParametersCopiesParametersViaLambda() {
		// Exercises: button.getParameters().forEach(p -> addParameter(new FluentParameter().from(p)))
		DialogButton src = new DialogButton();
		src.getParameters().add(new FluentParameter().get());
		FluentDialogButton b = new FluentDialogButton();
		b.from(src);
		assertEquals(1, b.get().getParameters().size());
	}
}
