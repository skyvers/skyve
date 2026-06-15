package org.skyve.impl.web.faces.converters.select;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.web.WebContext;
class AssociationAutoCompleteConverterTest {

	private AssociationAutoCompleteConverter converter;

	@BeforeEach
	void before() {
		converter = new AssociationAutoCompleteConverter();
	}

	@Test
	void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@Test
	void getAsObjectReturnsNullForNull() {
		assertNull(converter.getAsObject(null, null, null));
	}

	@Test
	void getAsObjectReturnsNullForBlank() {
		assertNull(converter.getAsObject(null, null, "  "));
	}

	@Test
	void getAsStringReturnsEmptyStringForOtherObject() {
		assertEquals("", converter.getAsString(null, null, "notABean"));
	}

	@Test
	void getAsStringFormatsBean() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("modName");
		when(bean.getBizDocument()).thenReturn("docName");
		when(bean.getBizId()).thenReturn("bizId");
		assertEquals("modName.docName.bizId", converter.getAsString(null, null, bean));
	}

	@Test
	void getAsStringFormatsBeanMapAdapter() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("mod");
		when(bean.getBizDocument()).thenReturn("doc");
		when(bean.getBizId()).thenReturn("id");
		WebContext webContext = mock(WebContext.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, webContext);
		assertEquals("mod.doc.id", converter.getAsString(null, null, adapter));
	}
}
