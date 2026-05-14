package org.skyve.util;

import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.mockito.Mockito;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;

public class BinderTargetMetaDataTest {

	@Test
	@SuppressWarnings("static-method")
	public void constructorStoresAllFields() {
		Document doc = Mockito.mock(Document.class);
		Attribute attr = Mockito.mock(Attribute.class);
		Binder.TargetMetaData tmd = new Binder.TargetMetaData(doc, attr, String.class);
		assertSame(doc, tmd.getDocument());
		assertSame(attr, tmd.getAttribute());
		assertSame(String.class, tmd.getType());
	}

	@Test
	@SuppressWarnings("static-method")
	public void constructorWithNullAttributeIsAllowed() {
		Document doc = Mockito.mock(Document.class);
		Binder.TargetMetaData tmd = new Binder.TargetMetaData(doc, null, Integer.class);
		assertSame(doc, tmd.getDocument());
		assertNull(tmd.getAttribute());
		assertSame(Integer.class, tmd.getType());
	}
}
