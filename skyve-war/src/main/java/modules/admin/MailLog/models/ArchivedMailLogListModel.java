package modules.admin.MailLog.models;

import static java.util.stream.Collectors.toCollection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.stream.Stream;

import org.apache.lucene.document.Document;
import org.apache.lucene.search.Sort;
import org.apache.lucene.search.SortField;
import org.apache.lucene.search.SortField.Type;
import org.skyve.archive.support.DocumentConverter;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.archive.list.ArchivedDocumentListModel;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

import modules.admin.domain.MailLog;

/**
 * Archived list model for {@link MailLog} documents.
 * <p>
 * Reads archived Lucene documents, projects key Mail Log summary fields, and maps
 * results into dynamic beans for list-grid rendering.
 * </p>
 *
 * @param <U> The bean type.
 */
public class ArchivedMailLogListModel<U extends Bean> extends ArchivedDocumentListModel<U> {

	private final List<String> mailLogBindings = List.of(MailLog.timestampPropertyName,
														MailLog.dispatchStatusPropertyName,
														MailLog.providerPropertyName,
														MailLog.toRecipientsPropertyName,
														MailLog.subjectPropertyName,
														MailLog.isBulkPropertyName,
														MailLog.mailCountPropertyName,
														MailLog.recipientCountPropertyName);

	@Override
	public String getDescription() {
		return "The list of all archived Mail Logs.";
	}

	@Override
	protected String toSortBinding(String binding) {
		return DocumentConverter.toSortBinding(binding);
	}

	@Override
	protected Bean convertToBean(Document luceneDoc) {
		Map<String, Object> props = new HashMap<>();

		Consumer<String> putStringField = binding -> props.put(binding, luceneDoc.get(binding));

		Stream.of(Bean.DOCUMENT_ID,
					Bean.USER_ID,
					MailLog.dispatchStatusPropertyName,
					MailLog.providerPropertyName,
					MailLog.toRecipientsPropertyName,
					MailLog.subjectPropertyName)
				.forEach(putStringField);

		String timestampStr = luceneDoc.get(MailLog.timestampPropertyName);
		props.put(MailLog.timestampPropertyName, new Timestamp(DocumentConverter.stringToDate(timestampStr)));

		String isBulk = luceneDoc.get(MailLog.isBulkPropertyName);
		if (isBulk != null) {
			props.put(MailLog.isBulkPropertyName, Boolean.valueOf(isBulk));
		}

		String mailCount = luceneDoc.get(MailLog.mailCountPropertyName);
		if (mailCount != null) {
			props.put(MailLog.mailCountPropertyName, Long.valueOf(mailCount));
		}

		String recipientCount = luceneDoc.get(MailLog.recipientCountPropertyName);
		if (recipientCount != null) {
			props.put(MailLog.recipientCountPropertyName, Long.valueOf(recipientCount));
		}

		props.put(PersistentBean.FLAG_COMMENT_NAME, null);
		return new DynamicBean(getModule(), getDocument(), props);
	}

	@Override
	protected String getModule() {
		return MailLog.MODULE_NAME;
	}

	@Override
	protected String getDocument() {
		return MailLog.DOCUMENT_NAME;
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return mailLogBindings.stream()
							.map(this::createColumn)
							.collect(toCollection(ArrayList::new));
	}

	@SuppressWarnings("static-method")
	private MetaDataQueryColumn createColumn(String binding) {
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(binding);
		column.setSortable(true);
		return column;
	}

	@Override
	public Set<String> getProjections() {
		Set<String> projections = new LinkedHashSet<>();

		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);

		projections.addAll(mailLogBindings);
		return projections;
	}

	@Override
	protected Sort getDefaultSort() {
		return new Sort(new SortField(toSortBinding(MailLog.timestampPropertyName), Type.STRING, true));
	}
}
