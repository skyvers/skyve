package org.skyve.impl.web.service;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.mockito.Mockito.mock;

import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.view.View.ViewType;

@SuppressWarnings("static-method")
class ClientDocumentTest {

	@Test
	void getDocumentReturnsNullInitially() {
		ClientDocument cd = new ClientDocument();
		assertNull(cd.getDocument());
	}

	@Test
	void setAndGetDocumentRoundtrip() {
		ClientDocument cd = new ClientDocument();
		org.skyve.metadata.model.document.Document doc = mock(org.skyve.metadata.model.document.Document.class);
		cd.setDocument(doc);
		assertSame(doc, cd.getDocument());
	}

	@Test
	void getViewsReturnsNonNullEmptyMapInitially() {
		ClientDocument cd = new ClientDocument();
		Map<ViewType, ?> views = cd.getViews();
		assertNotNull(views);
	}
}
