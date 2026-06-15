package org.skyve.impl.backup;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.doReturn;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.lang.reflect.Method;
import java.util.List;

import org.junit.Test;
import org.skyve.impl.metadata.model.document.field.Field;
import org.skyve.impl.metadata.model.document.field.Field.IndexType;
import org.skyve.impl.metadata.model.document.field.Memo;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.document.Document;

@SuppressWarnings({"static-method", "boxing"})
public class ReindexJobDecisionLogicTest {
	@Test
	public void needsIndexingReturnsFalseForNonPersistableDocument() throws Exception {
		Customer customer = mock(Customer.class);
		Document document = mock(Document.class);
		when(document.isPersistable()).thenReturn(Boolean.FALSE);

		assertFalse(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void needsIndexingReturnsTrueForBothIndexField() throws Exception {
		Customer customer = mock(Customer.class);
		Field field = mock(Field.class);
		when(field.getIndex()).thenReturn(IndexType.both);

		Document document = persistableDocumentWith(customer, field);
		assertTrue(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void needsIndexingReturnsTrueForTextualIndexField() throws Exception {
		Customer customer = mock(Customer.class);
		Field field = mock(Field.class);
		when(field.getIndex()).thenReturn(IndexType.textual);

		Document document = persistableDocumentWith(customer, field);
		assertTrue(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void needsIndexingReturnsTrueForMemoWithNullIndex() throws Exception {
		Customer customer = mock(Customer.class);
		Memo memo = mock(Memo.class);
		when(memo.getIndex()).thenReturn(null);

		Document document = persistableDocumentWith(customer, memo);
		assertTrue(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void needsIndexingReturnsFalseForMemoWithNoneIndex() throws Exception {
		Customer customer = mock(Customer.class);
		Memo memo = mock(Memo.class);
		when(memo.getIndex()).thenReturn(IndexType.none);

		Document document = persistableDocumentWith(customer, memo);
		assertFalse(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void needsIndexingReturnsFalseForPersistableDocumentWithNoFields() throws Exception {
		Customer customer = mock(Customer.class);
		Attribute attribute = mock(Attribute.class);

		Document document = persistableDocumentWith(customer, attribute);
		assertFalse(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void needsIndexingReturnsFalseForFieldWithNullIndexThatIsNotMemo() throws Exception {
		Customer customer = mock(Customer.class);
		Field field = mock(Field.class);
		when(field.getIndex()).thenReturn(null);

		Document document = persistableDocumentWith(customer, field);
		assertFalse(invokeNeedsIndexing(customer, document));
	}

	@Test
	public void hasContentReturnsTrueForContentField() throws Exception {
		Table table = new Table("agnostic", "persistent");
		table.fields.put("upload", new BackupField(AttributeType.content, Sensitivity.none));

		assertTrue(invokeHasContent(table));
	}

	@Test
	public void hasContentReturnsTrueForImageField() throws Exception {
		Table table = new Table("agnostic", "persistent");
		table.fields.put("image", new BackupField(AttributeType.image, Sensitivity.none));

		assertTrue(invokeHasContent(table));
	}

	@Test
	public void hasContentReturnsFalseWhenNoContentFieldsExist() throws Exception {
		Table table = new Table("agnostic", "persistent");
		table.fields.put("name", new BackupField(AttributeType.text, Sensitivity.none));

		assertFalse(invokeHasContent(table));
	}

	private static Document persistableDocumentWith(Customer customer, Attribute attribute) {
		Document document = mock(Document.class);
		when(document.isPersistable()).thenReturn(Boolean.TRUE);
		doReturn(List.of(attribute)).when(document).getAllAttributes(customer);
		return document;
	}

	private static boolean invokeNeedsIndexing(Customer customer, Document document) throws Exception {
		Method method = ReindexBeansJob.class.getDeclaredMethod("needsIndexing", Customer.class, Document.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, customer, document)).booleanValue();
	}

	private static boolean invokeHasContent(Table table) throws Exception {
		Method method = ReindexAttachmentsJob.class.getDeclaredMethod("hasContent", Table.class);
		method.setAccessible(true);
		return ((Boolean) method.invoke(null, table)).booleanValue();
	}
}
