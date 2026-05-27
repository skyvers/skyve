package org.skyve.impl.web.faces.converters.select;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;

import org.junit.Before;
import org.junit.Test;
import org.primefaces.component.picklist.PickList;
import org.primefaces.model.DualListModel;
import org.skyve.metadata.model.document.Bizlet.DomainValue;

public class AssociationPickListConverterTest {

	private AssociationPickListConverter converter;

	@Before
	public void before() {
		converter = new AssociationPickListConverter();
	}

	// ---- getAsString ----

	@Test
	public void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@Test
	public void getAsStringReturnsCode() {
		DomainValue dv = new DomainValue("CODE", "Label");
		assertEquals("CODE", converter.getAsString(null, null, dv));
	}

	// ---- getAsObject ----

	@Test
	public void getAsObjectReturnsNullForNullValue() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	public void getAsObjectReturnsNullForEmptyValue() {
		assertNull(converter.getAsObject(null, null, ""));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void getAsObjectFindsInSource() {
		DomainValue dv1 = new DomainValue("A", "Alpha");
		DomainValue dv2 = new DomainValue("B", "Beta");
		DualListModel<DomainValue> model = mock(DualListModel.class);
		when(model.getSource()).thenReturn(Arrays.asList(dv1, dv2));
		when(model.getTarget()).thenReturn(Collections.emptyList());

		PickList pickList = mock(PickList.class);
		when(pickList.getValue()).thenReturn(model);

		assertEquals(dv1, converter.getAsObject(null, pickList, "A"));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void getAsObjectFindsInTarget() {
		DomainValue dv1 = new DomainValue("A", "Alpha");
		DomainValue dv2 = new DomainValue("B", "Beta");
		DualListModel<DomainValue> model = mock(DualListModel.class);
		when(model.getSource()).thenReturn(Collections.emptyList());
		when(model.getTarget()).thenReturn(Arrays.asList(dv1, dv2));

		PickList pickList = mock(PickList.class);
		when(pickList.getValue()).thenReturn(model);

		assertEquals(dv2, converter.getAsObject(null, pickList, "B"));
	}

	@SuppressWarnings("unchecked")
	@Test
	public void getAsObjectReturnsNullWhenNotFound() {
		DualListModel<DomainValue> model = mock(DualListModel.class);
		when(model.getSource()).thenReturn(Collections.emptyList());
		when(model.getTarget()).thenReturn(Collections.emptyList());

		PickList pickList = mock(PickList.class);
		when(pickList.getValue()).thenReturn(model);

		assertNull(converter.getAsObject(null, pickList, "NOTFOUND"));
	}
}
