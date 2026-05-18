package modules.admin.MailLog.models;

import static org.hamcrest.CoreMatchers.hasItems;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertEquals;

import java.util.List;
import java.util.stream.Collectors;

import org.apache.lucene.document.Document;
import org.apache.lucene.document.Field.Store;
import org.apache.lucene.document.TextField;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.junit.jupiter.api.Test;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.util.Binder;

import modules.admin.domain.MailLog;

class ArchivedMailLogListModelTest {

	@Test
	void testColumnsAndProjectionsContainExpectedBindings() {
		ArchivedMailLogListModelForTest<Bean> model = new ArchivedMailLogListModelForTest<>();

		List<String> bindings = model.getColumns().stream()
									.map(c -> c.getBinding())
									.collect(Collectors.toList());
		assertThat(bindings, hasItems(MailLog.timestampPropertyName,
									 MailLog.dispatchStatusPropertyName,
									 MailLog.providerPropertyName,
									 MailLog.toRecipientsPropertyName,
									 MailLog.subjectPropertyName,
									 MailLog.isBulkPropertyName,
									 MailLog.mailCountPropertyName,
									 MailLog.recipientCountPropertyName));

		assertTrue(model.getProjections().contains(Bean.DOCUMENT_ID));
		assertTrue(model.getProjections().contains(PersistentBean.LOCK_NAME));
		assertTrue(model.getProjections().contains(Bean.BIZ_KEY));
		assertTrue(model.getProjections().contains(MailLog.timestampPropertyName));
	}

	@Test
	void testConvertToBeanAndDefaultSort() {
		ArchivedMailLogListModelForTest<Bean> model = new ArchivedMailLogListModelForTest<>();
		String timestamp = DocumentConverter.dateToString(new java.util.Date());

		Document luceneDoc = new Document();
		luceneDoc.add(new TextField(Bean.DOCUMENT_ID, "mail-1", Store.YES));
		luceneDoc.add(new TextField(Bean.USER_ID, "user-1", Store.YES));
		luceneDoc.add(new TextField(MailLog.timestampPropertyName, timestamp, Store.YES));
		luceneDoc.add(new TextField(MailLog.dispatchStatusPropertyName, "SENT", Store.YES));
		luceneDoc.add(new TextField(MailLog.providerPropertyName, "smtp", Store.YES));
		luceneDoc.add(new TextField(MailLog.toRecipientsPropertyName, "to@skyve.org", Store.YES));
		luceneDoc.add(new TextField(MailLog.subjectPropertyName, "Subject", Store.YES));
		luceneDoc.add(new TextField(MailLog.isBulkPropertyName, "true", Store.YES));
		luceneDoc.add(new TextField(MailLog.mailCountPropertyName, "2", Store.YES));
		luceneDoc.add(new TextField(MailLog.recipientCountPropertyName, "3", Store.YES));

		DynamicBean bean = (DynamicBean) model.convert(luceneDoc);
		assertThat(bean.getBizModule(), is(MailLog.MODULE_NAME));
		assertThat(bean.getBizDocument(), is(MailLog.DOCUMENT_NAME));
		assertThat(Binder.get(bean, MailLog.dispatchStatusPropertyName), is("SENT"));
		assertThat(Binder.get(bean, MailLog.providerPropertyName), is("smtp"));
		assertThat(Binder.get(bean, MailLog.toRecipientsPropertyName), is("to@skyve.org"));
		assertThat(Binder.get(bean, MailLog.subjectPropertyName), is("Subject"));
		assertThat(Binder.get(bean, MailLog.isBulkPropertyName), is(Boolean.TRUE));
		assertThat(Binder.get(bean, MailLog.mailCountPropertyName), is(Long.valueOf(2)));
		assertThat(Binder.get(bean, MailLog.recipientCountPropertyName), is(Long.valueOf(3)));

		Sort sort = model.defaultSort();
		assertEquals(1, sort.getSort().length);
		SortField sortField = sort.getSort()[0];
		assertThat(sortField.getField(), is(DocumentConverter.toSortBinding(MailLog.timestampPropertyName)));
		assertThat(sortField.getType(), is(Type.STRING));
		assertTrue(sortField.getReverse());
	}

	private static class ArchivedMailLogListModelForTest<U extends Bean> extends ArchivedMailLogListModel<U> {
		Bean convert(Document luceneDoc) {
			return convertToBean(luceneDoc);
		}

		Sort defaultSort() {
			return getDefaultSort();
		}
	}
}
