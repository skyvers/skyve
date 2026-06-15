package org.skyve.impl.web.faces.converters.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
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
public class SelectItemsBeanConverterTest {

	private SelectItemsBeanConverter converter;

	@Before
	public void before() {
		converter = new SelectItemsBeanConverter();
	}

	// ---- getAsObject ----

	@Test
	public void getAsObjectReturnsNullForNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	public void getAsObjectReturnsNullForEmptyString() {
		assertNull(converter.getAsObject(null, null, ""));
	}

	// ---- getAsString ----

	@Test
	public void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@Test
	public void getAsStringDelegatesToToStringForOtherValues() {
		assertEquals("42", converter.getAsString(null, null, Integer.valueOf(42)));
	}

	@Test
	public void getAsStringUsesToCodeForEnumeration() {
		Enumeration e = new Enumeration() {
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
	public void isEmptyReturnsTrueForNull() {
		assertTrue(SelectItemsBeanConverter.isEmpty(null));
	}

	@Test
	public void isEmptyReturnsTrueForEmptyArray() {
		assertTrue(SelectItemsBeanConverter.isEmpty(new SelectItem[0]));
	}

	@Test
	public void isEmptyReturnsFalseForNonEmptyArray() {
		assertFalse(SelectItemsBeanConverter.isEmpty(new SelectItem[]{new SelectItem("v", "l")}));
	}

	// ---- getAsString with Bean and BeanMapAdapter ----

	@Test
	public void getAsStringReturnsBizIdForBean() {
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("testBizId");
		assertEquals("testBizId", converter.getAsString(null, null, bean));
	}

	@Test
	public void getAsStringReturnsBizIdForBeanMapAdapter() {
		Bean bean = mock(Bean.class);
		when(bean.getBizId()).thenReturn("adapterBizId");
		WebContext webContext = mock(WebContext.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, webContext);
		assertEquals("adapterBizId", converter.getAsString(null, null, adapter));
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
	public void getAsObjectFindsMatchingSelectItem() {
		UISelectItem uiItem = mockSelectItem("myValue", "My Label", false);
		UIComponent menu = mockMenuWithItems(uiItem);
		Object result = converter.getAsObject(null, menu, "myValue");
		assertEquals("myValue", result);
	}

	@Test
	public void getAsObjectReturnsNullWhenNoMatchingSelectItem() {
		UISelectItem uiItem = mockSelectItem("other", "Other", false);
		UIComponent menu = mockMenuWithItems(uiItem);
		Object result = converter.getAsObject(null, menu, "notFound");
		assertNull(result);
	}

	@Test
	public void getAsObjectSkipsNoSelectionOption() {
		UISelectItem uiItem = mockSelectItem("target", "Target", true);
		UIComponent menu = mockMenuWithItems(uiItem);
		Object result = converter.getAsObject(null, menu, "target");
		assertNull(result);
	}

	@Test
	public void getAsObjectSearchesWithinSelectItemGroup() {
		SelectItem[] subItems = {new SelectItem("grouped", "Grouped Label")};
		SelectItemGroup group = new SelectItemGroup("Group", "group-desc", false, subItems);
		UISelectItem groupItem = mock(UISelectItem.class);
		when(groupItem.getValue()).thenReturn(group);
		UIComponent menu = mockMenuWithItems(groupItem);
		Object result = converter.getAsObject(null, menu, "grouped");
		assertEquals("grouped", result);
	}

	@Test
	public void getAsObjectReturnsNullWhenGroupHasNoMatch() {
		SelectItem[] subItems = {new SelectItem("sub", "Sub Label")};
		SelectItemGroup group = new SelectItemGroup("Group", "", false, subItems);
		UISelectItem groupItem = mock(UISelectItem.class);
		when(groupItem.getValue()).thenReturn(group);
		UIComponent menu = mockMenuWithItems(groupItem);
		Object result = converter.getAsObject(null, menu, "notFound");
		assertNull(result);
	}
}
