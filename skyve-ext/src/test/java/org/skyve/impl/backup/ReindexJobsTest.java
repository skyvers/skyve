package org.skyve.impl.backup;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;

class ReindexJobsTest {

	@Test
	@SuppressWarnings("static-method")
	void reindexAttachmentsHasContentReturnsTrueForContentAndImageFields() throws Exception {
		Method method = ReindexAttachmentsJob.class.getDeclaredMethod("hasContent", Table.class);
		method.setAccessible(true);

		Table contentTable = new Table("doc", "doc");
		contentTable.fields.put("file", new BackupField(AttributeType.content, Attribute.Sensitivity.none));

		Table imageTable = new Table("img", "img");
		imageTable.fields.put("photo", new BackupField(AttributeType.image, Attribute.Sensitivity.none));

		assertTrue(((Boolean) method.invoke(null, contentTable)).booleanValue());
		assertTrue(((Boolean) method.invoke(null, imageTable)).booleanValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void reindexAttachmentsHasContentReturnsFalseWhenNoContentFields() throws Exception {
		Method method = ReindexAttachmentsJob.class.getDeclaredMethod("hasContent", Table.class);
		method.setAccessible(true);

		Table table = new Table("doc", "doc");
		table.fields.put("name", new BackupField(AttributeType.text, Attribute.Sensitivity.none));

		assertFalse(((Boolean) method.invoke(null, table)).booleanValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void reindexBeansNeedsIndexingReturnsFalseForNonPersistableDocument() throws Exception {
		Method method = ReindexBeansJob.class.getDeclaredMethod("needsIndexing", Customer.class, Document.class);
		method.setAccessible(true);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		doReturn(Boolean.FALSE).when(document).isPersistable();

		assertFalse(((Boolean) method.invoke(null, customer, document)).booleanValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void reindexBeansNeedsIndexingReturnsTrueForTextualOrBothIndex() throws Exception {
		Method method = ReindexBeansJob.class.getDeclaredMethod("needsIndexing", Customer.class, Document.class);
		method.setAccessible(true);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Field textual = mock(Field.class);
		Field both = mock(Field.class);

		doReturn(Boolean.TRUE).when(document).isPersistable();
		when(textual.getIndex()).thenReturn(IndexType.textual);
		when(both.getIndex()).thenReturn(IndexType.both);
		doReturn(Arrays.asList(textual, both)).when(document).getAllAttributes(customer);

		assertTrue(((Boolean) method.invoke(null, customer, document)).booleanValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void reindexBeansNeedsIndexingReturnsTrueForMemoWithNullIndex() throws Exception {
		Method method = ReindexBeansJob.class.getDeclaredMethod("needsIndexing", Customer.class, Document.class);
		method.setAccessible(true);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Memo memo = mock(Memo.class);

		doReturn(Boolean.TRUE).when(document).isPersistable();
		when(memo.getIndex()).thenReturn(null);
		doReturn(Collections.singletonList(memo)).when(document).getAllAttributes(customer);

		assertTrue(((Boolean) method.invoke(null, customer, document)).booleanValue());
	}

	@Test
	@SuppressWarnings("static-method")
	void reindexBeansNeedsIndexingReturnsFalseForNonMemoFieldWithNullIndex() throws Exception {
		Method method = ReindexBeansJob.class.getDeclaredMethod("needsIndexing", Customer.class, Document.class);
		method.setAccessible(true);

		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		Field field = mock(Field.class);

		doReturn(Boolean.TRUE).when(document).isPersistable();
		when(field.getIndex()).thenReturn(null);
		doReturn(Collections.singletonList(field)).when(document).getAllAttributes(customer);

		assertFalse(((Boolean) method.invoke(null, customer, document)).booleanValue());
	}
}
