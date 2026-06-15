package org.skyve.impl.web.service;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;

import java.util.Map;

import org.junit.Test;
import org.skyve.metadata.view.View.ViewType;

@SuppressWarnings("static-method")
public class ClientDocumentTest {

	@Test
	public void getDocumentReturnsNullInitially() {
		ClientDocument cd = new ClientDocument();
		assertNull(cd.getDocument());
	}

	@Test
	public void setAndGetDocumentRoundtrip() {
		ClientDocument cd = new ClientDocument();
		org.skyve.metadata.model.document.Document doc = org.mockito.Mockito.mock(org.skyve.metadata.model.document.Document.class);
		cd.setDocument(doc);
		assertSame(doc, cd.getDocument());
	}

	@Test
	public void getViewsReturnsNonNullEmptyMapInitially() {
		ClientDocument cd = new ClientDocument();
		Map<ViewType, ?> views = cd.getViews();
		assertNotNull(views);
	}
}
