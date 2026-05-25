package org.skyve.impl.metadata.module.query;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;

@SuppressWarnings("static-method")
class MetaDataQueryDefinitionImplTest {

	@Test
	void setAndGetDocumentName() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setDocumentName("Contact");
		assertEquals("Contact", query.getDocumentName());
	}

	@Test
	void setAndGetPolymorphic() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getPolymorphic());
		query.setPolymorphic(Boolean.TRUE);
		assertEquals(Boolean.TRUE, query.getPolymorphic());
	}

	@Test
	void setAndGetAggregate() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertFalse(query.isAggregate());
		query.setAggregate(true);
		assertTrue(query.isAggregate());
	}

	@Test
	void setAndGetFromClause() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getFromClause());
		query.setFromClause("admin_Contact as bean");
		assertEquals("admin_Contact as bean", query.getFromClause());
	}

	@Test
	void setFromClauseToNullClearsIt() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setFromClause("admin_Contact as bean");
		query.setFromClause(null);
		assertNull(query.getFromClause());
	}

	@Test
	void setAndGetFilterClause() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getFilterClause());
		query.setFilterClause("bean.name = 'Test'");
		assertEquals("bean.name = 'Test'", query.getFilterClause());
	}

	@Test
	void setFilterClauseToNullClearsIt() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setFilterClause("bean.name = 'Test'");
		query.setFilterClause(null);
		assertNull(query.getFilterClause());
	}

	@Test
	void setAndGetGroupClause() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getGroupClause());
		query.setGroupClause("bean.name");
		assertEquals("bean.name", query.getGroupClause());
	}

	@Test
	void setGroupClauseToNullClearsIt() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setGroupClause("bean.name");
		query.setGroupClause(null);
		assertNull(query.getGroupClause());
	}

	@Test
	void setAndGetOrderClause() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getOrderClause());
		query.setOrderClause("bean.name asc");
		assertEquals("bean.name asc", query.getOrderClause());
	}

	@Test
	void setOrderClauseToNullClearsIt() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setOrderClause("bean.name asc");
		query.setOrderClause(null);
		assertNull(query.getOrderClause());
	}

	@Test
	void getColumnsReturnsEmptyListByDefault() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNotNull(query.getColumns());
		assertTrue(query.getColumns().isEmpty());
	}

	// ---- QueryDefinitionImpl (parent) getter/setter coverage ----

	@Test
	void setAndGetName() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getName());
		query.setName("myQuery");
		assertEquals("myQuery", query.getName());
	}

	@Test
	void setAndGetDescription() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setDescription("Lists contacts");
		assertEquals("Lists contacts", query.getDescription());
	}

	@Test
	void setAndGetDocumentation() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNull(query.getDocumentation());
		query.setDocumentation("Detailed docs");
		assertEquals("Detailed docs", query.getDocumentation());
	}

	@Test
	void setAndGetTimeoutInSeconds() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertEquals(0, query.getTimeoutInSeconds());
		query.setTimeoutInSeconds(30);
		assertEquals(30, query.getTimeoutInSeconds());
	}

	@Test
	void getPropertiesNotNull() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		assertNotNull(query.getProperties());
		assertTrue(query.getProperties().isEmpty());
	}

	@Test
	void toStringNotNull() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setName("testQuery");
		assertNotNull(query.toString());
	}

	// ---- owningModule and getDocumentModule ----

	@Test
	void setAndGetOwningModule() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		Module mockModule = mock(Module.class);
		query.setOwningModule(mockModule);
		assertSame(mockModule, query.getOwningModule());
	}

	@Test
	void getDocumentModuleReturnsSameModuleWhenNoReferencedModuleName() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		Module mockModule = mock(Module.class);
		Customer mockCustomer = mock(Customer.class);

		DocumentRef mockRef = mock(DocumentRef.class);
		when(mockRef.getReferencedModuleName()).thenReturn(null);

		Map<String, DocumentRef> docRefs = new TreeMap<>();
		docRefs.put("Contact", mockRef);
		when(mockModule.getDocumentRefs()).thenReturn(Collections.unmodifiableMap(docRefs));

		query.setOwningModule(mockModule);
		query.setDocumentName("Contact");

		Module result = query.getDocumentModule(mockCustomer);
		assertSame(mockModule, result);
	}

	@Test
	void getDocumentModuleReturnsReferencedModuleWhenSet() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		Module owningModule = mock(Module.class);
		Module referencedModule = mock(Module.class);
		Customer mockCustomer = mock(Customer.class);

		DocumentRef mockRef = mock(DocumentRef.class);
		when(mockRef.getReferencedModuleName()).thenReturn("admin");

		Map<String, DocumentRef> docRefs = new TreeMap<>();
		docRefs.put("User", mockRef);
		when(owningModule.getDocumentRefs()).thenReturn(Collections.unmodifiableMap(docRefs));
		when(mockCustomer.getModule("admin")).thenReturn(referencedModule);

		query.setOwningModule(owningModule);
		query.setDocumentName("User");

		Module result = query.getDocumentModule(mockCustomer);
		assertSame(referencedModule, result);
	}

	@Test
	void polymorphicSetToNull() {
		MetaDataQueryDefinitionImpl query = new MetaDataQueryDefinitionImpl();
		query.setPolymorphic(Boolean.TRUE);
		query.setPolymorphic(null);
		assertNull(query.getPolymorphic());
	}
}
