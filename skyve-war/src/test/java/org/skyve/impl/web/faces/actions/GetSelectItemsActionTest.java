package org.skyve.impl.web.faces.actions;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;

import org.junit.jupiter.api.Test;

import jakarta.faces.model.SelectItem;
import modules.test.AbstractSkyveTest;
import modules.test.domain.AllAttributesPersistent;

/**
 * Tests for GetSelectItemsAction using the filter constructor
 * (no bean, uses module/document/binding parameters).
 */
public class GetSelectItemsActionTest extends AbstractSkyveTest {

	@Test
	void callbackWithEnumBindingReturnsItems() throws Exception {
		List<SelectItem> items = new GetSelectItemsAction(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.enum3PropertyName,
				false).callback();

		assertNotNull(items);
		assertFalse(items.isEmpty());
	}

	@Test
	void callbackWithEnumBindingAndIncludeEmptyItemIncludesEmptyEntry() throws Exception {
		List<SelectItem> items = new GetSelectItemsAction(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.enum3PropertyName,
				true).callback();

		assertNotNull(items);
		assertFalse(items.isEmpty());
		// first item should be the empty item
		SelectItem firstItem = items.get(0);
		assertNotNull(firstItem);
		assertTrue(firstItem.getLabel().isEmpty());
	}

	@Test
	void callbackWithEnumBindingIncludesAllEnumValues() throws Exception {
		List<SelectItem> items = new GetSelectItemsAction(
				AllAttributesPersistent.MODULE_NAME,
				AllAttributesPersistent.DOCUMENT_NAME,
				AllAttributesPersistent.enum3PropertyName,
				false).callback();

		// AllAttributesPersistent.Enum3 has: one, two, three
		assertTrue(items.size() >= 3);
	}
}
