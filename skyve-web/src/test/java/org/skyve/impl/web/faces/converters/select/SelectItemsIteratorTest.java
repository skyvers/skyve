package org.skyve.impl.web.faces.converters.select;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UISelectItem;
import jakarta.faces.component.UISelectItems;
import jakarta.faces.context.ExternalContext;
import jakarta.faces.context.FacesContext;
import jakarta.faces.model.SelectItem;

@SuppressWarnings({"static-method", "boxing"})
class SelectItemsIteratorTest {
	private FacesContext facesContext;
	private ExternalContext externalContext;

	@BeforeEach
	void setUp() {
		facesContext = mock(FacesContext.class);
		externalContext = mock(ExternalContext.class);
		when(facesContext.getExternalContext()).thenReturn(externalContext);
		when(externalContext.getRequestMap()).thenReturn(new HashMap<>());
	}

	private UIComponent mockParent(UIComponent... children) {
		UIComponent parent = mock(UIComponent.class);
		List<UIComponent> childList = new ArrayList<>(Arrays.asList(children));
		when(parent.getChildren()).thenReturn(childList);
		return parent;
	}

	private UISelectItem mockSelectItemWith(SelectItem selectItem) {
		UISelectItem uiItem = mock(UISelectItem.class);
		when(uiItem.getValue()).thenReturn(selectItem);
		return uiItem;
	}

	private UISelectItem mockSelectItemProperties(String value, String label, String desc, boolean disabled) {
		UISelectItem uiItem = mock(UISelectItem.class);
		when(uiItem.getValue()).thenReturn(null);
		when(uiItem.getItemValue()).thenReturn(value);
		when(uiItem.getItemLabel()).thenReturn(label);
		when(uiItem.getItemDescription()).thenReturn(desc);
		when(uiItem.isItemDisabled()).thenReturn(disabled);
		when(uiItem.isItemEscaped()).thenReturn(false);
		when(uiItem.isNoSelectionOption()).thenReturn(false);
		return uiItem;
	}

	private UISelectItems mockSelectItemsWithValue(Object value) {
		UISelectItems uiItems = mock(UISelectItems.class);
		when(uiItems.getValue()).thenReturn(value);
		Map<String, Object> attrs = new HashMap<>();
		when(uiItems.getAttributes()).thenReturn(attrs);
		return uiItems;
	}

	private UISelectItems mockSelectItemsWithValueAndAttrs(Object value, Map<String, Object> attrs) {
		UISelectItems uiItems = mock(UISelectItems.class);
		when(uiItems.getValue()).thenReturn(value);
		when(uiItems.getAttributes()).thenReturn(attrs);
		return uiItems;
	}

	@Test
	void emptyParentHasNoNext() {
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent());
		assertFalse(it.hasNext());
	}

	@Test
	void nextOnEmptyThrowsNoSuchElement() {
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent());
		assertThrows(NoSuchElementException.class, it::next);
	}

	@Test
	void removeThrowsUnsupportedOperation() {
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent());
		assertThrows(UnsupportedOperationException.class, it::remove);
	}

	@Test
	void uiSelectItemWithSelectItemValue() {
		SelectItem selectItem = new SelectItem("value1", "Label1");
		UISelectItem uiItem = mockSelectItemWith(selectItem);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItem));
		assertTrue(it.hasNext());
		SelectItem result = it.next();
		assertEquals("value1", result.getValue());
		assertEquals("Label1", result.getLabel());
		assertFalse(it.hasNext());
	}

	@Test
	void uiSelectItemWithNullValueUsesProperties() {
		UISelectItem uiItem = mockSelectItemProperties("myVal", "myLabel", "myDesc", true);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItem));
		SelectItem result = it.next();
		assertEquals("myVal", result.getValue());
		assertEquals("myLabel", result.getLabel());
		assertEquals("myDesc", result.getDescription());
		assertTrue(result.isDisabled());
	}

	@Test
	void multipleUiSelectItemChildren() {
		UISelectItem i0 = mockSelectItemProperties("v0", "l0", null, false);
		UISelectItem i1 = mockSelectItemProperties("v1", "l1", null, false);
		UISelectItem i2 = mockSelectItemProperties("v2", "l2", null, false);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(i0, i1, i2));
		int count = 0;
		while (it.hasNext()) { it.next(); count++; }
		assertEquals(3, count);
	}

	@Test
	void uiSelectItemsWithSingleSelectItem() {
		UISelectItems uiItems = mockSelectItemsWithValue(new SelectItem("single", "Single"));
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertTrue(it.hasNext());
		assertEquals("single", it.next().getValue());
		assertFalse(it.hasNext());
	}

	@Test
	void uiSelectItemsWithListOfSelectItems() {
		UISelectItems uiItems = mockSelectItemsWithValue(Arrays.asList(
				new SelectItem("a", "A"), new SelectItem("b", "B"), new SelectItem("c", "C")));
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(3, count);
	}

	@Test
	void uiSelectItemsWithListOfGenericObjects() {
		UISelectItems uiItems = mockSelectItemsWithValue(Arrays.asList("alpha", "beta", "gamma"));
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(3, count);
	}

	@Test
	void uiSelectItemsWithNullValueGivesNoItems() {
		UISelectItems uiItems = mockSelectItemsWithValue(null);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertFalse(it.hasNext());
	}

	@Test
	void uiSelectItemsWithMapValue() {
		Map<String, String> map = new LinkedHashMap<>();
		map.put("key1", "val1");
		map.put("key2", "val2");
		UISelectItems uiItems = mockSelectItemsWithValue(map);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(2, count);
	}

	@Test
	void uiSelectItemsWithArrayOfSelectItems() {
		UISelectItems uiItems = mockSelectItemsWithValue(
				new SelectItem[] { new SelectItem("x", "X"), new SelectItem("y", "Y") });
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(2, count);
	}

	@Test
	void uiSelectItemsWithArrayOfGenericObjects() {
		UISelectItems uiItems = mockSelectItemsWithValue(new String[] {"one", "two", "three"});
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(3, count);
	}

	@Test
	void uiSelectItemsWithIllegalValueThrowsIllegalArgument() {
		UISelectItems uiItems = mockSelectItemsWithValue(Integer.valueOf(42));
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertThrows(IllegalArgumentException.class, it::hasNext);
	}

	@Test
	void mixedChildren() {
		UISelectItem single = mockSelectItemProperties("s", "S", null, false);
		UISelectItems multi = mockSelectItemsWithValue(
				Arrays.asList(new SelectItem("a", "A"), new SelectItem("b", "B")));
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(single, multi));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(3, count);
	}

	@Test
	void hasNextIsIdempotent() {
		UISelectItem uiItem = mockSelectItemProperties("v", "l", null, false);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItem));
		assertTrue(it.hasNext());
		assertTrue(it.hasNext());
		it.next();
		assertFalse(it.hasNext());
		assertFalse(it.hasNext());
	}

	@Test
	void emptyListGivesNoItems() {
		UISelectItems uiItems = mockSelectItemsWithValue(new ArrayList<>());
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertFalse(it.hasNext());
	}

	@Test
	void mapWithNullValueEntry() {
		Map<String, String> map = new LinkedHashMap<>();
		map.put("k", null);
		UISelectItems uiItems = mockSelectItemsWithValue(map);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		SelectItem item = it.next();
		assertEquals("", item.getValue());
	}

	@Test
	void genericObjectNoSelectionOptionAttribute() {
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("noSelectionOption", "true");
		UISelectItems uiItems = mockSelectItemsWithValueAndAttrs(Arrays.asList("a"), attrs);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertTrue(it.next().isNoSelectionOption());
	}

	@Test
	void genericObjectNoSelectionValueMatchingItem() {
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("noSelectionValue", "a");
		UISelectItems uiItems = mockSelectItemsWithValueAndAttrs(Arrays.asList("a", "b"), attrs);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertTrue(it.next().isNoSelectionOption());
		assertFalse(it.next().isNoSelectionOption());
	}

	@Test
	void genericObjectDisabledAttribute() {
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("itemDisabled", "true");
		UISelectItems uiItems = mockSelectItemsWithValueAndAttrs(Arrays.asList("x"), attrs);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertTrue(it.next().isDisabled());
	}

	@Test
	void genericObjectEscapeAttribute() {
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("itemLabelEscaped", "true");
		UISelectItems uiItems = mockSelectItemsWithValueAndAttrs(Arrays.asList("x"), attrs);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertTrue(it.next().isEscape());
	}

	@Test
	void genericObjectDescriptionAttribute() {
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("itemDescription", "desc");
		UISelectItems uiItems = mockSelectItemsWithValueAndAttrs(Arrays.asList("x"), attrs);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		assertEquals("desc", it.next().getDescription());
	}

	@Test
	void iteratorWithVarAttributeAndFixedLabel() {
		Map<String, Object> attrs = new HashMap<>();
		attrs.put("var", "item");
		attrs.put("itemLabel", "fixedLabel");
		UISelectItems uiItems = mockSelectItemsWithValueAndAttrs(Arrays.asList("a", "b"), attrs);
		SelectItemsIterator it = new SelectItemsIterator(facesContext, mockParent(uiItems));
		int count = 0;
		while (it.hasNext()) { assertNotNull(it.next()); count++; }
		assertEquals(2, count);
	}
}
