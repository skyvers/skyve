package org.skyve.impl.generate;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;

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
}
