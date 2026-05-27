package org.skyve.impl.web.faces.converters.select;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Test;
import org.skyve.domain.Bean;
import org.skyve.impl.web.faces.models.BeanMapAdapter;
import org.skyve.web.WebContext;

public class AssociationAutoCompleteConverterTest {

	private AssociationAutoCompleteConverter converter;

	@Before
	public void before() {
		converter = new AssociationAutoCompleteConverter();
	}

	@Test
	public void getAsStringReturnsEmptyStringForNull() {
		assertEquals("", converter.getAsString(null, null, null));
	}

	@Test
	public void getAsStringReturnsEmptyStringForOtherObject() {
		assertEquals("", converter.getAsString(null, null, "notABean"));
	}

	@Test
	public void getAsStringFormatsBean() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("modName");
		when(bean.getBizDocument()).thenReturn("docName");
		when(bean.getBizId()).thenReturn("bizId");
		assertEquals("modName.docName.bizId", converter.getAsString(null, null, bean));
	}

	@Test
	public void getAsStringFormatsBeanMapAdapter() {
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("mod");
		when(bean.getBizDocument()).thenReturn("doc");
		when(bean.getBizId()).thenReturn("id");
		WebContext webContext = mock(WebContext.class);
		BeanMapAdapter adapter = new BeanMapAdapter(bean, webContext);
		assertEquals("mod.doc.id", converter.getAsString(null, null, adapter));
	}
}
