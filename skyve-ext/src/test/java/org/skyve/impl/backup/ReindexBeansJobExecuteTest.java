package org.skyve.impl.backup;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.junit.Test;
import org.skyve.content.ContentManager;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;
import org.skyve.persistence.SQL;

@SuppressWarnings({"static-method", "resource"})
public class ReindexBeansJobExecuteTest {
	@Test
	public void executeSkipsDocumentWithoutIndexableFields() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Attribute attribute = mock(Attribute.class);
		ContentManager truncateCm = mock(ContentManager.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("demo");
		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(module.getDocumentRefs()).thenReturn(Map.of("ArchivedDoc", ref));
		when(ref.getOwningModuleName()).thenReturn("admin");
		when(module.getDocument(customer, "ArchivedDoc")).thenReturn(document);
		doReturn(Boolean.TRUE).when(document).isPersistable();
		doReturn(List.of(attribute)).when(document).getAllAttributes(customer);
		when(document.getOwningModuleName()).thenReturn("admin");
		when(document.getName()).thenReturn("ArchivedDoc");

		TestableReindexBeansJob job = new TestableReindexBeansJob(persistence, "dyn_table", truncateCm);
		job.execute();

		verify(truncateCm).truncateBeanIndexing("demo");
		verify(persistence, never()).begin();
		verify(persistence, never()).newSQL(anyString());
		assertEquals(100, job.getPercentComplete());
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Skipping document admin.ArchivedDoc")));
		assertTrue(job.getLog().stream().anyMatch(entry -> entry.contains("Reindex beans complete")));
	}

	@Test
	public void executeReindexesDynamicDocumentAndEvictsPerBizId() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Field field = mock(Field.class);
		SQL sql = mock(SQL.class);
		PersistentBean bean = mock(PersistentBean.class);
		ContentManager truncateCm = mock(ContentManager.class);
		ContentManager loopCm = mock(ContentManager.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("demo");
		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(module.getDocumentRefs()).thenReturn(Map.of("ArchivedDoc", ref));
		when(ref.getOwningModuleName()).thenReturn("admin");
		when(module.getDocument(customer, "ArchivedDoc")).thenReturn(document);
		doReturn(Boolean.TRUE).when(document).isPersistable();
		doReturn(Boolean.TRUE).when(document).isDynamic();
		doReturn(List.of(field)).when(document).getAllAttributes(customer);
		when(field.getIndex()).thenReturn(IndexType.textual);
		when(persistence.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(anyString(), anyString(), org.mockito.ArgumentMatchers.anyBoolean())).thenReturn(sql);
		when(sql.scalarIterable(String.class)).thenReturn(new ListIterable<>(List.of("A", "B")));
		when(persistence.retrieve(document, "A")).thenReturn(bean);
		when(persistence.retrieve(document, "B")).thenReturn(null);

		TestableReindexBeansJob job = new TestableReindexBeansJob(persistence, "dyn_table", truncateCm, loopCm);
		job.execute();

		verify(truncateCm).truncateBeanIndexing("demo");
		verify(persistence).begin();
		verify(persistence).commit(false);
		verify(persistence).reindex(bean);
		verify(persistence, times(2)).evictAllCached();
		assertEquals(100, job.getPercentComplete());
	}

	@Test
	public void executeReindexesStaticDocumentViaDocumentQuery() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		Document document = mock(Document.class);
		Field field = mock(Field.class);
		DocumentQuery documentQuery = mock(DocumentQuery.class);
		PersistentBean firstBean = mock(PersistentBean.class);
		PersistentBean secondBean = mock(PersistentBean.class);
		ContentManager truncateCm = mock(ContentManager.class);
		ContentManager loopCm = mock(ContentManager.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("demo");
		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(module.getDocumentRefs()).thenReturn(Map.of("ArchivedDoc", ref));
		when(ref.getOwningModuleName()).thenReturn("admin");
		when(module.getDocument(customer, "ArchivedDoc")).thenReturn(document);
		doReturn(Boolean.TRUE).when(document).isPersistable();
		doReturn(Boolean.FALSE).when(document).isDynamic();
		doReturn(List.of(field)).when(document).getAllAttributes(customer);
		when(field.getIndex()).thenReturn(IndexType.both);
		when(persistence.newDocumentQuery(document)).thenReturn(documentQuery);
		when(documentQuery.beanIterable()).thenReturn(new ListIterable<>(List.of(firstBean, secondBean)));

		TestableReindexBeansJob job = new TestableReindexBeansJob(persistence, "dyn_table", truncateCm, loopCm);
		job.execute();

		verify(documentQuery).noTimeout();
		verify(persistence).reindex(firstBean);
		verify(persistence).reindex(secondBean);
		assertEquals(100, job.getPercentComplete());
	}

	@Test
	public void executeIgnoresDocumentRefOwnedByDifferentModule() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		DocumentRef ref = mock(DocumentRef.class);
		ContentManager truncateCm = mock(ContentManager.class);

		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getName()).thenReturn("demo");
		when(customer.getModules()).thenReturn(List.of(module));
		when(module.getName()).thenReturn("admin");
		when(module.getDocumentRefs()).thenReturn(Map.of("ArchivedDoc", ref));
		when(ref.getOwningModuleName()).thenReturn("other");

		TestableReindexBeansJob job = new TestableReindexBeansJob(persistence, "dyn_table", truncateCm);
		job.execute();

		verify(module, never()).getDocument(customer, "ArchivedDoc");
		verify(persistence, never()).begin();
		assertEquals(100, job.getPercentComplete());
	}

	private static final class TestableReindexBeansJob extends ReindexBeansJob {
		private final AbstractPersistence persistence;
		private final String dynamicIdentifier;
		private final ArrayDeque<ContentManager> contentManagers = new ArrayDeque<>();

		private TestableReindexBeansJob(AbstractPersistence persistence, String dynamicIdentifier, ContentManager... cms) {
			this.persistence = persistence;
			this.dynamicIdentifier = dynamicIdentifier;
			for (ContentManager cm : cms) {
				contentManagers.add(cm);
			}
		}

		@Override
		protected AbstractPersistence getPersistence() {
			return persistence;
		}

		@Override
		protected String getDynamicEntityPersistenceIdentifier(Customer customer) {
			return dynamicIdentifier;
		}

		@Override
		protected ContentManager newContentManager() {
			return contentManagers.removeFirst();
		}
	}

	private static final class ListIterable<T> implements AutoClosingIterable<T> {
		private final List<T> values;

		private ListIterable(List<T> values) {
			this.values = values;
		}

		@Override
		public Iterator<T> iterator() {
			return values.iterator();
		}

		@Override
		public void close() {
			// Nothing to close for test list-backed iterable.
		}
	}
}
