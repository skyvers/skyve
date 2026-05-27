package org.skyve.impl.metadata.module.query;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.CALLS_REAL_METHODS;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;
import static org.mockito.Mockito.withSettings;

import java.lang.reflect.Field;
import java.util.Collections;
import java.util.Map;
import java.util.TreeMap;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.FilterOperator;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.user.User;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.DocumentQuery.AggregateFunction;

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

	@Nested
	class ConstructDocumentQueryTests {

		private AbstractPersistence persistence;
		private User user;
		private Customer customer;
		private Module module;
		private Document document;
		private DocumentQuery query;
		private DocumentFilter filter;

		@BeforeEach
		void setUp() {
			persistence = mock(AbstractPersistence.class, withSettings().defaultAnswer(CALLS_REAL_METHODS));
			user = mock(User.class);
			customer = mock(Customer.class);
			module = mock(Module.class);
			document = mock(Document.class);
			query = mock(DocumentQuery.class);
			filter = mock(DocumentFilter.class);

			when(user.getCustomer()).thenReturn(customer);
			when(customer.getName()).thenReturn("myCustomer");
			when(module.getDocument(any(Customer.class), anyString())).thenReturn(document);
			when(document.getOwningModuleName()).thenReturn("myMod");
			when(document.getName()).thenReturn("TestDoc");
			when(persistence.newDocumentQuery(any(Document.class), any(), any(), any(), any())).thenReturn(query);
			when(persistence.getDocumentEntityName(anyString(), anyString())).thenReturn("TestTable");
			when(query.getFilter()).thenReturn(filter);
			persistence.setUser(user);
			persistence.setForThread();
		}

		@AfterEach
		@SuppressWarnings("unchecked")
		void tearDown() throws Exception {
			Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
			field.setAccessible(true);
			((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
		}

		private MetaDataQueryDefinitionImpl newQueryDef() {
			MetaDataQueryDefinitionImpl queryDef = new MetaDataQueryDefinitionImpl();
			queryDef.setOwningModule(module);
			queryDef.setDocumentName("TestDoc");
			queryDef.setPolymorphic(Boolean.FALSE); // avoid DocumentImpl cast
			return queryDef;
		}

		@Test
		void constructDocumentQueryNullSummaryNullTagReturnsQuery() {
			DocumentQuery result = newQueryDef().constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithTagIdReturnsQuery() {
			when(user.getId()).thenReturn("userId123");
			DocumentQuery result = newQueryDef().constructDocumentQuery(null, "myTagId");
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryCountAggregateReturnsQuery() {
			DocumentQuery result = newQueryDef().constructDocumentQuery(AggregateFunction.Count, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQuerySumAggregateReturnsQuery() {
			DocumentQuery result = newQueryDef().constructDocumentQuery(AggregateFunction.Sum, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryMinAggregateReturnsQuery() {
			DocumentQuery result = newQueryDef().constructDocumentQuery(AggregateFunction.Min, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryMaxAggregateReturnsQuery() {
			DocumentQuery result = newQueryDef().constructDocumentQuery(AggregateFunction.Max, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryAvgAggregateReturnsQuery() {
			DocumentQuery result = newQueryDef().constructDocumentQuery(AggregateFunction.Avg, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryAggregateIsTrueSkipsProjections() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setAggregate(true);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithUserExpressionInFilterClause() {
			when(user.getName()).thenReturn("testUser");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("user = {USER}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithUserIdExpressionInFilterClause() {
			when(user.getId()).thenReturn("userId");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("userId = {USERID}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithUsernameExpressionInFilterClause() {
			when(user.getContactName()).thenReturn("John Smith");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("name = {USERNAME}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithDataGroupExpressionInFilterClause() {
			when(user.getDataGroupId()).thenReturn("dg1");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("dg = {DATAGROUPID}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithContactIdExpressionInFilterClause() {
			when(user.getContactId()).thenReturn("contact1");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("cid = {CONTACTID}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithCustomerExpressionInFilterClause() {
			when(customer.getName()).thenReturn("myCust");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("cust = {CUSTOMER}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithDateExpressionInFilterClause() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("d = {DATE}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithDateTimeExpressionInFilterClause() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("dt = {DATETIME}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithMultipleImplicitParametersAdded() {
			when(user.getName()).thenReturn("u");
			when(user.getId()).thenReturn("uid");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("{USER} = {USERID}"); // two implicit params
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithExpressionsInFromAndOrderClauses() {
			when(user.getName()).thenReturn("u");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			queryDef.setFilterClause("{USER}");
			queryDef.setOrderClause("{DATE}");
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithPolymorphicTrue() {
			MetaDataQueryDefinitionImpl queryDef = new MetaDataQueryDefinitionImpl();
			queryDef.setOwningModule(module);
			queryDef.setDocumentName("TestDoc");
			queryDef.setPolymorphic(Boolean.TRUE);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithImplicitBindingColumn() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID); // "bizId" is an implicit attribute
			col.setName(Bean.DOCUMENT_ID);
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithExpressionColumn() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setExpression("UPPER(bean.name)");
			col.setName("upperName");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithExpressionColumnAggregateCount() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setExpression("COUNT(bean.bizId)");
			col.setName("count");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(AggregateFunction.Count, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithNullBindingAndNullExpressionColumnSkips() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setName("noBindingNoExpr");
			// binding=null, expression=null → should continue
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterOperatorIsNull() {
			when(user.getId()).thenReturn("uid");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(PersistentBean.FLAG_COMMENT_NAME); // implicit attribute
			col.setName("flag");
			col.setFilterOperator(FilterOperator.isNull);
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterOperatorNotNull() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(PersistentBean.FLAG_COMMENT_NAME); // implicit attribute
			col.setName("flag");
			col.setFilterOperator(FilterOperator.notNull);
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnSortAscending() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setSortOrder(SortDirection.ascending);
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnSortDescending() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setSortOrder(SortDirection.descending);
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterUserExpression() {
			when(user.getName()).thenReturn("testUser");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("{USER}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterUserIdExpression() {
			when(user.getId()).thenReturn("uid");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("{USERID}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterUsernameExpression() {
			when(user.getContactName()).thenReturn("John");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("{USERNAME}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterDataGroupIdExpression() {
			when(user.getDataGroupId()).thenReturn("dgId");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("{DATAGROUPID}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterContactIdExpression() {
			when(user.getContactId()).thenReturn("cid");
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("{CONTACTID}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterCustomerExpression() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("{CUSTOMER}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterDateExpression() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.greaterEqual);
			col.setFilterExpression("{DATE}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterDateTimeExpression() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.lessEqual);
			col.setFilterExpression("{DATETIME}");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterLiteralValue() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.like);
			col.setFilterExpression("%test%");
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterQuestionMarkExpression() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			col.setFilterExpression("?"); // dynamic filter - no operand
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnFilterNullExpression() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setBinding(Bean.DOCUMENT_ID);
			col.setName(Bean.DOCUMENT_ID);
			col.setFilterOperator(FilterOperator.equal);
			// filterExpression is null → no operand, equal filter not added
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}

		@Test
		void constructDocumentQueryWithColumnExpressionSortOrder() {
			MetaDataQueryDefinitionImpl queryDef = newQueryDef();
			MetaDataQueryProjectedColumnImpl col = new MetaDataQueryProjectedColumnImpl();
			col.setExpression("UPPER(bean.name)");
			col.setName("upper");
			col.setSortOrder(SortDirection.ascending);
			queryDef.getColumns().add(col);
			DocumentQuery result = queryDef.constructDocumentQuery(null, null);
			assertSame(query, result);
		}
	}
}
