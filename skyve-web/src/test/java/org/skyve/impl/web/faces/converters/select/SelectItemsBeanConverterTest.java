package org.skyve.impl.web.faces.converters.select;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.types.Enumeration;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.web.WebContext;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

import jakarta.faces.component.UIComponent;
import jakarta.faces.component.UISelectItem;
import jakarta.faces.model.SelectItem;
import jakarta.faces.model.SelectItemGroup;

@SuppressWarnings("static-method")
class SelectItemsBeanConverterTest {

	private SelectItemsBeanConverter converter;

	@BeforeEach
	void before() {
		converter = new SelectItemsBeanConverter();
	}

	// ---- getAsObject ----

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getAsObjectReturnsNullForNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@SuppressWarnings("null") // passes null on purpose
	@Test
	void getAsObjectReturnsNullForEmptyString() {
		assertNull(converter.getAsObject(null, null, ""));
	}

	// ---- getAsString ----

	@Test
	void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@Test
	void getAsStringDelegatesToToStringForOtherValues() {
		assertEquals("42", converter.getAsString(null, null, Integer.valueOf(42)));
	}

	@Test
	void getAsStringUsesToCodeForEnumeration() {
		Enumeration e = new Enumeration() {
			private static final long serialVersionUID = 1L;

			@Override
			public String toCode() {
				return "TEST_CODE";
			}

			@Override
			public String toLocalisedDescription() {
				return "Test Description";
			}

			@Override
			public DomainValue toDomainValue() {
				return new DomainValue(toCode(), toLocalisedDescription());
			}
		};
		assertEquals("TEST_CODE", converter.getAsString(null, null, e));
	}

	// ---- isEmpty ----

	@Test
	void isEmptyReturnsTrueForNull() {
		assertTrue(SelectItemsBeanConverter.isEmpty(null));
	}

	@Test
	void isEmptyReturnsTrueForEmptyArray() {
		assertTrue(SelectItemsBeanConverter.isEmpty(new SelectItem[0]));
	}

	@Test
	void isEmptyReturnsFalseForNonEmptyArray() {
		assertFalse(SelectItemsBeanConverter.isEmpty(new SelectItem[]{new SelectItem("v", "l")}));
	}

	// ---- getAsString with Bean and BeanMapAdapter ----

	@Test
	void getAsStringReturnsBizIdForBean() {
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("testBizId");
		assertEquals("testBizId", converter.getAsString(null, null, bean));
	}

	@Test
	void getAsStringReturnsBizIdForBeanMapAdapter() {
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("adapterBizId");
		WebContext webContext = mock(WebContext.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, webContext);
		assertEquals("adapterBizId", converter.getAsString(null, null, adapter));
	}

	@Test
	void getAsStringReturnsEmptyStringForBeanMapAdapterOfNoBean() {
		BeanMapAdapter adapter = new BeanMapAdapter(null, mock(WebContext.class));
		assertEquals("", converter.getAsString(null, null, adapter));
	}

	// ---- getAsObject with component items ----

	private static UIComponent mockMenuWithItems(UISelectItem... items) {
		List<UIComponent> children = new ArrayList<>();
		for (UISelectItem item : items) {
			children.add(item);
		}
		UIComponent menu = mock(UIComponent.class);
		when(menu.getChildren()).thenReturn(children);
		return menu;
	}

	@SuppressWarnings("boxing")
	private static UISelectItem mockSelectItem(Object itemValue, String itemLabel, boolean noSelectionOption) {
		UISelectItem uiItem = mock(UISelectItem.class);
		when(uiItem.getValue()).thenReturn(null);
		when(uiItem.getItemValue()).thenReturn(itemValue);
		when(uiItem.getItemLabel()).thenReturn(itemLabel);
		when(uiItem.getItemDescription()).thenReturn("");
		when(uiItem.isItemDisabled()).thenReturn(false);
		when(uiItem.isItemEscaped()).thenReturn(true);
		when(uiItem.isNoSelectionOption()).thenReturn(noSelectionOption);
		return uiItem;
	}

	@Test
	void getAsObjectFindsMatchingSelectItem() {
		UISelectItem uiItem = mockSelectItem("myValue", "My Label", false);
		UIComponent menu = mockMenuWithItems(uiItem);
		@SuppressWarnings("null") // passes null on purpose
		Object result = converter.getAsObject(null, menu, "myValue");
		assertEquals("myValue", result);
	}

	@Test
	void getAsObjectReturnsNullWhenNoMatchingSelectItem() {
		UISelectItem uiItem = mockSelectItem("other", "Other", false);
		UIComponent menu = mockMenuWithItems(uiItem);
		@SuppressWarnings("null") // passes null on purpose
		Object result = converter.getAsObject(null, menu, "notFound");
		assertNull(result);
	}

	@Test
	void getAsObjectSkipsNoSelectionOption() {
		UISelectItem uiItem = mockSelectItem("target", "Target", true);
		UIComponent menu = mockMenuWithItems(uiItem);
		@SuppressWarnings("null") // passes null on purpose
		Object result = converter.getAsObject(null, menu, "target");
		assertNull(result);
	}

	@Test
	void getAsObjectSearchesWithinSelectItemGroup() {
		SelectItem[] subItems = {new SelectItem("grouped", "Grouped Label")};
		SelectItemGroup group = new SelectItemGroup("Group", "group-desc", false, subItems);
		UISelectItem groupItem = mock(UISelectItem.class);
		when(groupItem.getValue()).thenReturn(group);
		UIComponent menu = mockMenuWithItems(groupItem);
		@SuppressWarnings("null") // passes null on purpose
		Object result = converter.getAsObject(null, menu, "grouped");
		assertEquals("grouped", result);
	}

	@Test
	void getAsObjectReturnsNullWhenGroupHasNoMatch() {
		SelectItem[] subItems = {new SelectItem("sub", "Sub Label")};
		SelectItemGroup group = new SelectItemGroup("Group", "", false, subItems);
		UISelectItem groupItem = mock(UISelectItem.class);
		when(groupItem.getValue()).thenReturn(group);
		UIComponent menu = mockMenuWithItems(groupItem);
		@SuppressWarnings("null") // passes null on purpose
		Object result = converter.getAsObject(null, menu, "notFound");
		assertNull(result);
	}
}
