package org.skyve.metadata.view.fluent;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.InjectBinding;
import org.skyve.impl.metadata.view.event.RerenderEventAction;
import org.skyve.impl.metadata.view.event.ServerSideActionEventAction;
import org.skyve.impl.metadata.view.event.ToggleDisabledEventAction;
import org.skyve.impl.metadata.view.event.ToggleVisibilityEventAction;
import org.skyve.impl.metadata.view.widget.bound.input.ColourPicker;
import org.skyve.impl.metadata.view.widget.bound.input.Combo;
import org.skyve.impl.metadata.view.widget.bound.input.ContentLink;
import org.skyve.impl.metadata.view.widget.bound.input.DefaultWidget;
import org.skyve.impl.metadata.view.widget.bound.input.Password;
import org.skyve.impl.metadata.view.widget.bound.input.Radio;
import org.skyve.impl.metadata.view.widget.bound.tabular.TreeGrid;

/**
 * Tests for fluent widget/event wrapping constructors not covered elsewhere.
 */
@SuppressWarnings("static-method")
class FluentViewWrappingConstructorTest {

	// ---- FluentColourPicker -------------------------------------------

	@Test
	void colourPickerWrappingConstructorPreservesInstance() {
		ColourPicker existing = new ColourPicker();
		FluentColourPicker fluent = new FluentColourPicker(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentCombo ---------------------------------------------------

	@Test
	void comboWrappingConstructorPreservesInstance() {
		Combo existing = new Combo();
		FluentCombo fluent = new FluentCombo(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentPassword -----------------------------------------------

	@Test
	void passwordWrappingConstructorPreservesInstance() {
		Password existing = new Password();
		FluentPassword fluent = new FluentPassword(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentInjectBinding ------------------------------------------

	@Test
	void injectBindingWrappingConstructorPreservesInstance() {
		InjectBinding existing = new InjectBinding();
		FluentInjectBinding fluent = new FluentInjectBinding(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentTreeGrid -----------------------------------------------

	@Test
	void treeGridWrappingConstructorPreservesInstance() {
		TreeGrid existing = new TreeGrid();
		FluentTreeGrid fluent = new FluentTreeGrid(existing);
		assertSame(existing, fluent.get());
	}

	@Test
	void treeGridRootIdBindingSetsValue() {
		FluentTreeGrid fluent = new FluentTreeGrid().rootIdBinding("parentBizId");
		assertEquals("parentBizId", fluent.get().getRootIdBinding());
	}

	// ---- FluentRadio --------------------------------------------------

	@Test
	void radioWrappingConstructorPreservesInstance() {
		Radio existing = new Radio();
		FluentRadio fluent = new FluentRadio(existing);
		assertSame(existing, fluent.get());
	}

	@Test
	void radioVerticalSetsValue() {
		FluentRadio fluent = new FluentRadio().vertical(true);
		assertNotNull(fluent.get().getVertical());
		assertEquals(Boolean.TRUE, fluent.get().getVertical());
	}

	// ---- FluentDefaultWidget ------------------------------------------

	@Test
	void defaultWidgetWrappingConstructorPreservesInstance() {
		DefaultWidget existing = new DefaultWidget();
		FluentDefaultWidget fluent = new FluentDefaultWidget(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentContentLink --------------------------------------------

	@Test
	void contentLinkWrappingConstructorPreservesInstance() {
		ContentLink existing = new ContentLink();
		FluentContentLink fluent = new FluentContentLink(existing);
		assertSame(existing, fluent.get());
	}

	@Test
	void contentLinkValueSetsValue() {
		FluentContentLink fluent = new FluentContentLink().value("myContent");
		assertEquals("myContent", fluent.get().getValue());
	}

	@Test
	void contentLinkEditableSetsValue() {
		FluentContentLink fluent = new FluentContentLink().editable(true);
		assertEquals(Boolean.TRUE, fluent.get().getEditable());
	}

	// ---- FluentRerenderEventAction ------------------------------------

	@Test
	void rerenderWrappingConstructorPreservesInstance() {
		RerenderEventAction existing = new RerenderEventAction();
		FluentRerenderEventAction fluent = new FluentRerenderEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentToggleDisabledEventAction ------------------------------

	@Test
	void toggleDisabledWrappingConstructorPreservesInstance() {
		ToggleDisabledEventAction existing = new ToggleDisabledEventAction();
		FluentToggleDisabledEventAction fluent = new FluentToggleDisabledEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentToggleVisibilityEventAction ----------------------------

	@Test
	void toggleVisibilityWrappingConstructorPreservesInstance() {
		ToggleVisibilityEventAction existing = new ToggleVisibilityEventAction();
		FluentToggleVisibilityEventAction fluent = new FluentToggleVisibilityEventAction(existing);
		assertSame(existing, fluent.get());
	}

	// ---- FluentServerSideActionEventAction ----------------------------

	@Test
	void serverSideActionWrappingConstructorPreservesInstance() {
		ServerSideActionEventAction existing = new ServerSideActionEventAction();
		FluentServerSideActionEventAction fluent = new FluentServerSideActionEventAction(existing);
		assertSame(existing, fluent.get());
	}
}
