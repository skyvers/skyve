package org.skyve.impl.metadata.view.container.form;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.view.widget.bound.input.TextField;
import org.skyve.impl.metadata.view.HorizontalAlignment;

public class FormItemTest {

	@Test
	@SuppressWarnings("static-method")
	public void defaultConstructorHasNullFields() {
		FormItem item = new FormItem();
		assertNull(item.getWidget());
		assertNull(item.getColspan());
		assertNull(item.getRowspan());
		assertNull(item.getLabel());
		assertNull(item.getShowLabel());
		assertNull(item.getShowHelp());
		assertNull(item.getHelp());
		assertNull(item.getRequired());
		assertNull(item.getRequiredMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetWidget() {
		FormItem item = new FormItem();
		TextField widget = new TextField();
		item.setWidget(widget);
		assertEquals(widget, item.getWidget());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetColspanRowspan() {
		FormItem item = new FormItem();
		item.setColspan(Integer.valueOf(2));
		item.setRowspan(Integer.valueOf(3));
		assertEquals(Integer.valueOf(2), item.getColspan());
		assertEquals(Integer.valueOf(3), item.getRowspan());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetHorizontalAlignment() {
		FormItem item = new FormItem();
		item.setHorizontalAlignment(HorizontalAlignment.left);
		assertEquals(HorizontalAlignment.left, item.getHorizontalAlignment());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetLabelHorizontalAlignment() {
		FormItem item = new FormItem();
		item.setLabelHorizontalAlignment(HorizontalAlignment.right);
		assertEquals(HorizontalAlignment.right, item.getLabelHorizontalAlignment());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getLocalisedLabelFallsBackToLabel() {
		FormItem item = new FormItem();
		item.setLabel("My Label");
		// Util.i18n() falls back to the key when no resource bundle is available
		assertEquals("My Label", item.getLocalisedLabel());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getLocalisedLabelNullWhenLabelNull() {
		FormItem item = new FormItem();
		assertNull(item.getLocalisedLabel());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getLocalisedHelpFallsBackToHelp() {
		FormItem item = new FormItem();
		item.setHelp("Help text");
		assertEquals("Help text", item.getLocalisedHelp());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getLocalisedRequiredMessageFallsBackToMessage() {
		FormItem item = new FormItem();
		item.setRequiredMessage("Required field");
		assertEquals("Required field", item.getLocalisedRequiredMessage());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetShowLabelAndShowHelp() {
		FormItem item = new FormItem();
		item.setShowLabel(Boolean.TRUE);
		item.setShowHelp(Boolean.FALSE);
		assertEquals(Boolean.TRUE, item.getShowLabel());
		assertEquals(Boolean.FALSE, item.getShowHelp());
	}

	@Test
	@SuppressWarnings("static-method")
	public void setAndGetRequired() {
		FormItem item = new FormItem();
		item.setRequired(Boolean.TRUE);
		assertEquals(Boolean.TRUE, item.getRequired());
	}

	@Test
	@SuppressWarnings("static-method")
	public void getPropertiesIsNotNull() {
		FormItem item = new FormItem();
		assertNull(item.getProperties().get("nonexistent"));
	}
}
