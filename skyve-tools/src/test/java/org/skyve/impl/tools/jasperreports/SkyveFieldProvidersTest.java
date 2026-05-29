package org.skyve.impl.tools.jasperreports;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.JRQuery;

class SkyveFieldProvidersTest {
	private static JRDataset dataset(String queryText) {
		JRQuery query = mock(JRQuery.class);
		when(query.getText()).thenReturn(queryText);

		JRDataset dataset = mock(JRDataset.class);
		when(dataset.getQuery()).thenReturn(query);
		return dataset;
	}

	@AfterEach
	void clearAppsJarDirectory() {
		UtilImpl.APPS_JAR_DIRECTORY = null;
	}

	@Test
	void skyveQueryFieldsProviderReportsNoDesignerSupport() throws Exception {
		SkyveQueryFieldsProvider provider = new SkyveQueryFieldsProvider();
		assertNull(provider.designQuery(null, null, null));
		assertNull(provider.getEditorComponent(null));
		assertFalse(provider.hasEditorComponent());
		assertFalse(provider.hasQueryDesigner());
		assertFalse(provider.supportsAutomaticQueryExecution());
		assertTrue(provider.supportsGetFieldsOperation());
	}

	@Test
	void skyveDocumentFieldsProviderReportsNoDesignerSupport() throws Exception {
		SkyveDocumentFieldsProvider provider = new SkyveDocumentFieldsProvider();
		assertNull(provider.designQuery(null, null, null));
		assertNull(provider.getEditorComponent(null));
		assertFalse(provider.hasEditorComponent());
		assertFalse(provider.hasQueryDesigner());
		assertFalse(provider.supportsAutomaticQueryExecution());
		assertTrue(provider.supportsGetFieldsOperation());
	}

	@Test
	void skyveDocumentFieldsProviderResolvesDocumentMetadata() throws Exception {
		UtilImpl.APPS_JAR_DIRECTORY = "/Users/mike/_/skyve/skyve-war/src/main/java/";
		JRField[] fields = new SkyveDocumentFieldsProvider().getFields(null, dataset("admin.DocumentNumber"), null);

		assertNotNull(SkyveDocumentFieldsProvider.getDocument("admin.DocumentNumber"));
		assertTrue("DocumentNumber".equals(SkyveDocumentFieldsProvider.getDocument("admin.DocumentNumber").getName()));
		assertTrue(fields.length >= 4);
		assertNotNull(fields[0].getValueClassName());
	}
}