package org.skyve.impl.tools.jasperreports;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.nio.file.Files;
import java.nio.file.Path;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.util.UtilImpl;

import net.sf.jasperreports.engine.JRDataset;
import net.sf.jasperreports.engine.JRField;
import net.sf.jasperreports.engine.JRQuery;

@SuppressWarnings("static-method")
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
		UtilImpl.APPS_JAR_DIRECTORY = skyveWarMainJava().toString().replace('\\', '/') + '/';
		JRField[] fields = new SkyveDocumentFieldsProvider().getFields(null, dataset("admin.DocumentNumber"), null);

		assertNotNull(SkyveDocumentFieldsProvider.getDocument("admin.DocumentNumber"));
		assertEquals("DocumentNumber", SkyveDocumentFieldsProvider.getDocument("admin.DocumentNumber").getName());
		assertTrue(fields.length >= 4);
		assertNotNull(fields[0].getValueClassName());
	}

	private static Path skyveWarMainJava() {
		return repositoryRoot().resolve("skyve-war").resolve("src").resolve("main").resolve("java");
	}

	private static Path repositoryRoot() {
		Path root = Path.of(System.getProperty("maven.multiModuleProjectDirectory", System.getProperty("user.dir")));
		if (Files.exists(root.resolve("skyve-war"))) {
			return root;
		}
		Path parent = root.getParent();
		if ((parent != null) && Files.exists(parent.resolve("skyve-war"))) {
			return parent;
		}
		return root;
	}
}
