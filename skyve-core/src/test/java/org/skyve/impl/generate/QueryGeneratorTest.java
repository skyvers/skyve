package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.repository.module.MetaDataQueryContentColumnMetaData.DisplayType;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.module.query.MetaDataQueryContentColumn;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.SortDirection;

/**
 * Tests for {@link QueryGenerator}.
 */
class QueryGeneratorTest {

	// ----- generate() -------------------------------------------------------

	@Test
	@SuppressWarnings("static-method")
	void generateWithNoDocumentsReturnsEmptyList() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		when(module.getDocumentRefs()).thenReturn(new LinkedHashMap<>());

		List<QueryDefinition> result = QueryGenerator.generate(customer, module, false);

		assertNotNull(result);
		assertTrue(result.isEmpty());
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateWithNonPersistableDocumentExcludesIt() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		Document nonPersistable = mock(Document.class);
		when(nonPersistable.isPersistable()).thenReturn(Boolean.FALSE);
		when(module.getDocument(customer, "TransientDoc")).thenReturn(nonPersistable);

		Map<String, Module.DocumentRef> refs = new LinkedHashMap<>();
		refs.put("TransientDoc", mock(Module.DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);

		List<QueryDefinition> result = QueryGenerator.generate(customer, module, false);

		assertTrue(result.isEmpty(), "Non-persistable document should not appear in result");
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateWithPersistableDocumentIncludesIt() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		Document persistable = mock(Document.class);
		when(persistable.isPersistable()).thenReturn(Boolean.TRUE);
		when(module.getDocument(customer, "PersDoc")).thenReturn(persistable);

		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(module.getDocumentDefaultQuery(customer, "PersDoc", false)).thenReturn(query);

		Map<String, Module.DocumentRef> refs = new LinkedHashMap<>();
		refs.put("PersDoc", mock(Module.DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);

		List<QueryDefinition> result = QueryGenerator.generate(customer, module, false);

		assertEquals(1, result.size());
		assertEquals(query, result.get(0));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generatePassesIncludeAssociationBizKeysFlagToDefaultQuery() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		Document persistable = mock(Document.class);
		when(persistable.isPersistable()).thenReturn(Boolean.TRUE);
		when(module.getDocument(customer, "Doc")).thenReturn(persistable);

		MetaDataQueryDefinition queryWithBizKeys = mock(MetaDataQueryDefinition.class);
		when(module.getDocumentDefaultQuery(customer, "Doc", true)).thenReturn(queryWithBizKeys);

		Map<String, Module.DocumentRef> refs = new LinkedHashMap<>();
		refs.put("Doc", mock(Module.DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);

		List<QueryDefinition> result = QueryGenerator.generate(customer, module, true);

		assertEquals(1, result.size());
		assertEquals(queryWithBizKeys, result.get(0));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateWithMixedDocumentsOnlyReturnsPersistable() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		Document persistable = mock(Document.class);
		when(persistable.isPersistable()).thenReturn(Boolean.TRUE);
		when(module.getDocument(customer, "PersDoc")).thenReturn(persistable);

		Document transientDoc = mock(Document.class);
		when(transientDoc.isPersistable()).thenReturn(Boolean.FALSE);
		when(module.getDocument(customer, "TransientDoc")).thenReturn(transientDoc);

		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(module.getDocumentDefaultQuery(customer, "PersDoc", false)).thenReturn(query);

		Map<String, Module.DocumentRef> refs = new LinkedHashMap<>();
		refs.put("PersDoc", mock(Module.DocumentRef.class));
		refs.put("TransientDoc", mock(Module.DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);

		List<QueryDefinition> result = QueryGenerator.generate(customer, module, false);

		assertEquals(1, result.size());
		assertEquals(query, result.get(0));
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateQueryXmlIncludesMetadataQueryColumnsAndTimeout() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		Document persistable = mock(Document.class);
		when(persistable.isPersistable()).thenReturn(Boolean.TRUE);
		when(module.getDocument(customer, "PersDoc")).thenReturn(persistable);

		MetaDataQueryContentColumn contentColumn = mock(MetaDataQueryContentColumn.class);
		when(contentColumn.getBinding()).thenReturn("attachment");
		when(contentColumn.getDisplayName()).thenReturn("Attachment");
		when(contentColumn.getName()).thenReturn("attachmentCol");
		when(contentColumn.getSortOrder()).thenReturn(SortDirection.ascending);
		when(contentColumn.isHidden()).thenReturn(Boolean.TRUE);
		when(contentColumn.getDisplay()).thenReturn(DisplayType.thumbnail);
		when(contentColumn.getPixelWidth()).thenReturn(Integer.valueOf(120));
		when(contentColumn.getPixelHeight()).thenReturn(Integer.valueOf(80));

		MetaDataQueryColumn projectedColumn = mock(MetaDataQueryColumn.class);
		when(projectedColumn.getBinding()).thenReturn("name");
		when(projectedColumn.getDisplayName()).thenReturn("Name");
		when(projectedColumn.getName()).thenReturn("nameCol");
		when(projectedColumn.getSortOrder()).thenReturn(SortDirection.descending);
		when(projectedColumn.isHidden()).thenReturn(Boolean.FALSE);

		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getName()).thenReturn("defaultQuery");
		when(query.getDescription()).thenReturn("Default metadata query");
		when(query.getDocumentName()).thenReturn("PersDoc");
		when(query.getDocumentation()).thenReturn("Generated query docs");
		when(query.getColumns()).thenReturn(List.of(contentColumn, projectedColumn));
		when(query.getTimeoutInSeconds()).thenReturn(Integer.valueOf(30));

		when(module.getDocumentDefaultQuery(customer, "PersDoc", true)).thenReturn(query);

		Map<String, Module.DocumentRef> refs = new LinkedHashMap<>();
		refs.put("PersDoc", mock(Module.DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);

		String xml = QueryGenerator.generateQueryXML(customer, module, true);

		assertTrue(xml.contains("defaultQuery"));
		assertTrue(xml.contains("Generated query docs"));
		assertTrue(xml.contains("attachmentCol"));
		assertTrue(xml.contains("thumbnail"));
		assertTrue(xml.contains("timeoutInSeconds=\"30\""));
		verify(module).getDocumentDefaultQuery(customer, "PersDoc", true);
	}

	@Test
	@SuppressWarnings({"static-method", "boxing"})
	void generateQueryXmlOmitsTimeoutWhenNonPositive() {
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);

		Document persistable = mock(Document.class);
		when(persistable.isPersistable()).thenReturn(Boolean.TRUE);
		when(module.getDocument(customer, "PersDoc")).thenReturn(persistable);

		MetaDataQueryDefinition query = mock(MetaDataQueryDefinition.class);
		when(query.getName()).thenReturn("defaultQuery");
		when(query.getDescription()).thenReturn("Default metadata query");
		when(query.getDocumentName()).thenReturn("PersDoc");
		when(query.getColumns()).thenReturn(List.of());
		when(query.getTimeoutInSeconds()).thenReturn(Integer.valueOf(0));

		when(module.getDocumentDefaultQuery(customer, "PersDoc", false)).thenReturn(query);

		Map<String, Module.DocumentRef> refs = new LinkedHashMap<>();
		refs.put("PersDoc", mock(Module.DocumentRef.class));
		when(module.getDocumentRefs()).thenReturn(refs);

		String xml = QueryGenerator.generateQueryXML(customer, module, false);

		assertTrue(xml.contains("defaultQuery"));
		assertTrue(! xml.contains("timeoutInSeconds"));
	}
}
