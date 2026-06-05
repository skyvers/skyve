package org.skyve.impl.tag;

import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.DynamicPersistentBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.app.admin.Tag;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.BizQL;
import org.skyve.persistence.DocumentFilter;
import org.skyve.persistence.DocumentQuery;

@SuppressWarnings({"static-method", "unchecked"})
class DefaultTagManagerTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void getReturnsSingletonInstance() {
		assertSame(DefaultTagManager.get(), DefaultTagManager.get());
	}

	@Test
	void untagBeanDelegatesToDeleteStatementWithBeanIdentity() throws Exception {
		BizQL query = bindPersistenceWithBizQL();
		Bean bean = mock(Bean.class);
		when(bean.getBizModule()).thenReturn("sales");
		when(bean.getBizDocument()).thenReturn("Opportunity");
		when(bean.getBizId()).thenReturn("BIZ-1");

		DefaultTagManager.get().untag("TAG-1", bean);

		verify(query).putParameter(Bean.DOCUMENT_ID, "TAG-1");
		verify(query).putParameter(Bean.USER_ID, "USER-1");
		verify(query).putParameter("taggedModule", "sales");
		verify(query).putParameter("taggedDocument", "Opportunity");
		verify(query).putParameter("taggedBizId", "BIZ-1");
		verify(query).execute();
	}

	@Test
	void clearDeletesCurrentUserRowsForTag() throws Exception {
		BizQL query = bindPersistenceWithBizQL();

		DefaultTagManager.get().clear("TAG-2");

		verify(query).putParameter(Bean.DOCUMENT_ID, "TAG-2");
		verify(query).putParameter(Bean.USER_ID, "USER-1");
		verify(query).execute();
	}

	@Test
	@SuppressWarnings("resource")
	void iterateBuildsTaggedIterableAndClosesUnderlyingIterable() throws Exception {
		AutoClosingIterable<Bean> taggedRows = mock(AutoClosingIterable.class);
		BizQL query = bindPersistenceWithBizQL();
		when(query.projectedIterable()).thenReturn(taggedRows);

		try (AutoClosingIterable<Bean> result = DefaultTagManager.get().iterate("TAG-3")) {
			assertTrue(result instanceof TaggedIterable);
		}
		verify(query).putParameter(Bean.DOCUMENT_ID, "TAG-3");
		verify(query).putParameter(Bean.USER_ID, "USER-1");
		verify(taggedRows).close();
	}

	@Test
	void untagUsesTaggedDocumentDeleteBizQL() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		BizQL query = mock(BizQL.class);
		when(user.getId()).thenReturn("USER-1");
		when(persistence.getUser()).thenReturn(user);
		ArgumentCaptor<String> bizql = ArgumentCaptor.forClass(String.class);
		when(persistence.newBizQL(bizql.capture())).thenReturn(query);
		when(query.putParameter(anyString(), any())).thenReturn(query);
		bindPersistenceToThread(persistence);

		DefaultTagManager.get().untag("TAG-4", "crm", "Contact", "BIZ-4");

		assertTrue(bizql.getValue().contains("delete from {admin.Tagged}"));
		assertTrue(bizql.getValue().contains("bean.tag.bizId = :bizId"));
		assertTrue(bizql.getValue().contains("bean.bizUserId = :bizUserId"));
	}

	@Test
	void getTagIdReturnsNullWhenNoTagMatches() throws Exception {
		DocumentQuery query = bindPersistenceWithDocumentQuery(List.of());
		DocumentFilter filter = query.getFilter();

		String result = DefaultTagManager.get().getTagId("Important");

		assertNull(result);
		verify(query).addBoundProjection(Bean.DOCUMENT_ID);
		verify(filter).addEquals(AppConstants.NAME_ATTRIBUTE_NAME, "Important");
		verify(filter).addEquals(Bean.USER_ID, "USER-1");
	}

	@Test
	void getTagIdReturnsProjectedBizIdWhenTagMatches() throws Exception {
		DynamicBean tag = dynamicBean("TAG-7", "Important");
		bindPersistenceWithDocumentQuery(List.of(tag));

		assertEquals("TAG-7", DefaultTagManager.get().getTagId("Important"));
	}

	@Test
	void getTagsReturnsDomainValuesForCurrentUserOrderedByName() throws Exception {
		DynamicBean first = dynamicBean("TAG-1", "Alpha");
		DynamicBean second = dynamicBean("TAG-2", "Beta");
		DocumentQuery query = bindPersistenceWithDocumentQuery(List.of(first, second));

		List<DomainValue> result = DefaultTagManager.get().getTags();

		assertEquals(2, result.size());
		assertEquals("TAG-1", result.get(0).getCode());
		assertEquals("Alpha", result.get(0).getLocalisedDescription());
		assertEquals("TAG-2", result.get(1).getCode());
		assertEquals("Beta", result.get(1).getLocalisedDescription());
		verify(query).addBoundProjection(Bean.DOCUMENT_ID);
		verify(query).addBoundProjection(AppConstants.NAME_ATTRIBUTE_NAME);
		verify(query).addBoundOrdering(AppConstants.NAME_ATTRIBUTE_NAME);
	}

	@Test
	void createBuildsAndSavesUserOwnedTag() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = adminUser();
		Document tagDocument = adminDocument(persistence, user, AppConstants.TAG_DOCUMENT_NAME);
		Tag tag = mock(Tag.class);
		when(tagDocument.newInstance(user)).thenReturn(tag);
		when(persistence.save(tagDocument, tag)).thenReturn(tag);
		when(tag.getBizId()).thenReturn("TAG-9");
		bindPersistenceToThread(persistence);

		String result = DefaultTagManager.get().create("Visible Tag", true);

		assertEquals("TAG-9", result);
		verify(tag).setName("Visible Tag");
		verify(tag).setVisible(Boolean.TRUE);
	}

	@Test
	void tagCreatesTaggedBeanAndRunsMergeLifecycle() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = adminUser();
		Document tagDocument = adminDocument(persistence, user, AppConstants.TAG_DOCUMENT_NAME);
		Document taggedDocument = user.getCustomer()
									  .getModule(AppConstants.ADMIN_MODULE_NAME)
									  .getDocument(user.getCustomer(), AppConstants.TAGGED_DOCUMENT_NAME);
		PersistentBean tag = mock(PersistentBean.class);
		HashMap<String, Object> taggedProperties = new HashMap<>();
		taggedProperties.put(AppConstants.TAG_ATTRIBUTE_NAME, null);
		taggedProperties.put(AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME, null);
		taggedProperties.put(AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME, null);
		taggedProperties.put(AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME, null);
		DynamicPersistentBean tagged = new DynamicPersistentBean(AppConstants.ADMIN_MODULE_NAME,
																	AppConstants.TAGGED_DOCUMENT_NAME,
																	taggedProperties);
		when(persistence.retrieve(tagDocument, "TAG-10")).thenReturn(tag);
		when(taggedDocument.newInstance(user)).thenReturn(tagged);
		bindPersistenceToThread(persistence);

		DefaultTagManager.get().tag("TAG-10", "crm", "Contact", "BIZ-10");

		assertSame(tag, tagged.get(AppConstants.TAG_ATTRIBUTE_NAME));
		assertEquals("crm", tagged.get(AppConstants.TAGGED_MODULE_ATTRIBUTE_NAME));
		assertEquals("Contact", tagged.get(AppConstants.TAGGED_DOCUMENT_ATTRIBUTE_NAME));
		assertEquals("BIZ-10", tagged.get(AppConstants.TAGGED_BIZID_ATTRIBUTE_NAME));
		verify(persistence).preMerge(taggedDocument, tagged);
		verify(persistence).upsertBeanTuple(tagged);
		verify(persistence).postMerge(taggedDocument, tagged);
	}

	@Test
	void deleteRemovesCurrentUsersTag() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = adminUser();
		Document tagDocument = adminDocument(persistence, user, AppConstants.TAG_DOCUMENT_NAME);
		BizQL clearQuery = mock(BizQL.class);
		PersistentBean tag = mock(PersistentBean.class);
		when(persistence.newBizQL(anyString())).thenReturn(clearQuery);
		when(clearQuery.putParameter(anyString(), any())).thenReturn(clearQuery);
		when(persistence.retrieveAndLock(tagDocument, "TAG-11")).thenReturn(tag);
		when(tag.getBizUserId()).thenReturn("USER-1");
		bindPersistenceToThread(persistence);

		DefaultTagManager.get().delete("TAG-11");

		verify(clearQuery).execute();
		verify(persistence).delete(tagDocument, tag);
	}

	@Test
	void deleteRejectsTagsOwnedByAnotherUser() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = adminUser();
		Document tagDocument = adminDocument(persistence, user, AppConstants.TAG_DOCUMENT_NAME);
		BizQL clearQuery = mock(BizQL.class);
		PersistentBean tag = mock(PersistentBean.class);
		when(persistence.newBizQL(anyString())).thenReturn(clearQuery);
		when(clearQuery.putParameter(anyString(), any())).thenReturn(clearQuery);
		when(persistence.retrieveAndLock(tagDocument, "TAG-12")).thenReturn(tag);
		when(tag.getBizUserId()).thenReturn("OTHER");
		when(user.getName()).thenReturn("Alex");
		bindPersistenceToThread(persistence);
		DefaultTagManager manager = DefaultTagManager.get();

		assertThrows(IllegalArgumentException.class, () -> manager.delete("TAG-12"));
		verify(persistence, never()).delete(any(), any());
	}

	private static BizQL bindPersistenceWithBizQL() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		BizQL query = mock(BizQL.class);
		when(user.getId()).thenReturn("USER-1");
		when(persistence.getUser()).thenReturn(user);
		when(persistence.newBizQL(anyString())).thenReturn(query);
		when(query.putParameter(anyString(), any())).thenReturn(query);
		bindPersistenceToThread(persistence);
		return query;
	}

	private static DocumentQuery bindPersistenceWithDocumentQuery(List<Bean> results) throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		DocumentQuery query = mock(DocumentQuery.class);
		DocumentFilter filter = mock(DocumentFilter.class);
		when(user.getId()).thenReturn("USER-1");
		when(persistence.getUser()).thenReturn(user);
		when(persistence.newDocumentQuery(AppConstants.ADMIN_MODULE_NAME, AppConstants.TAG_DOCUMENT_NAME)).thenReturn(query);
		when(query.getFilter()).thenReturn(filter);
		when(query.projectedResults()).thenReturn(results);
		bindPersistenceToThread(persistence);
		return query;
	}

	private static DynamicBean dynamicBean(String id, String name) {
		HashMap<String, Object> properties = new HashMap<>();
		properties.put(Bean.DOCUMENT_ID, id);
		properties.put(AppConstants.NAME_ATTRIBUTE_NAME, name);
		return new DynamicBean(AppConstants.ADMIN_MODULE_NAME, AppConstants.TAG_DOCUMENT_NAME, properties);
	}

	private static User adminUser() {
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module adminModule = mock(Module.class);
		Document tagDocument = mock(Document.class);
		Document taggedDocument = mock(Document.class);
		when(user.getId()).thenReturn("USER-1");
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule(AppConstants.ADMIN_MODULE_NAME)).thenReturn(adminModule);
		when(adminModule.getDocument(customer, AppConstants.TAG_DOCUMENT_NAME)).thenReturn(tagDocument);
		when(adminModule.getDocument(customer, AppConstants.TAGGED_DOCUMENT_NAME)).thenReturn(taggedDocument);
		return user;
	}

	private static Document adminDocument(AbstractPersistence persistence, User user, String documentName) {
		when(persistence.getUser()).thenReturn(user);
		return user.getCustomer()
					.getModule(AppConstants.ADMIN_MODULE_NAME)
					.getDocument(user.getCustomer(), documentName);
	}

	private static void bindPersistenceToThread(AbstractPersistence persistence) throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).set(persistence);
	}

	private static void unbindPersistenceFromThread() throws Exception {
		Field field = AbstractPersistence.class.getDeclaredField("threadLocalPersistence");
		field.setAccessible(true);
		((ThreadLocal<AbstractPersistence>) field.get(null)).remove();
	}
}
