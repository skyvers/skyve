package org.skyve.impl.tag;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.persistence.AutoClosingIterable;

@SuppressWarnings({"static-method", "unchecked", "resource"})
class TaggedIterableTest {
	@AfterEach
	void tearDown() throws Exception {
		unbindPersistenceFromThread();
	}

	@Test
	void iteratorResolvesTaggedRowsToTargetBeans() throws Exception {
		Document document = bindPersistence();
		Bean target = mock(Bean.class);
		AbstractPersistence persistence = AbstractPersistence.get();
		when(persistence.retrieve(document, "BIZ-1")).thenReturn(target);

		Iterator<Bean> iterator = taggedIterable(taggedRow("sales", "Opportunity", "BIZ-1")).iterator();

		assertTrue(iterator.hasNext());
		assertSame(target, iterator.next());
		assertFalse(iterator.hasNext());
	}

	@Test
	void iteratorSkipsRowsThatCannotBeResolved() throws Exception {
		Document document = bindPersistence();
		Bean target = mock(Bean.class);
		AbstractPersistence persistence = AbstractPersistence.get();
		when(persistence.retrieve(document, "BIZ-2")).thenReturn(target);

		Iterator<Bean> iterator = taggedIterable(taggedRow(null, "Opportunity", "missing"),
													taggedRow("sales", "Opportunity", "BIZ-2")).iterator();

		assertTrue(iterator.hasNext());
		assertSame(target, iterator.next());
		assertFalse(iterator.hasNext());
		verify(persistence).retrieve(document, "BIZ-2");
	}

	private static AutoClosingIterable<Bean> taggedIterable(Bean... rows) {
		return new TaggedIterable(new AutoClosingIterable<>() {
			@Override
			public Iterator<Bean> iterator() {
				return List.of(rows).iterator();
			}

			@Override
			public void close() {
				// no-op
			}
		});
	}

	private static Bean taggedRow(String module, String document, String bizId) {
		HashMap<String, Object> values = new HashMap<>();
		values.put("taggedModule", module);
		values.put("taggedDocument", document);
		values.put("taggedBizId", bizId);
		return new DynamicBean("admin", "Tagged", values);
	}

	private static Document bindPersistence() throws Exception {
		AbstractPersistence persistence = mock(AbstractPersistence.class);
		User user = mock(User.class);
		Customer customer = mock(Customer.class);
		Module module = mock(Module.class);
		Document document = mock(Document.class);
		when(persistence.getUser()).thenReturn(user);
		when(user.getCustomer()).thenReturn(customer);
		when(customer.getModule("sales")).thenReturn(module);
		when(module.getDocument(customer, "Opportunity")).thenReturn(document);
		bindPersistenceToThread(persistence);
		return document;
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
