package modules.admin.Audit.models;

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

import modules.admin.domain.Audit;

/**
 * Archived list model for {@link Audit} documents.
 * <p>
 * Reads archived Lucene documents, projects core Audit fields, and maps results
 * into dynamic beans for archived list-grid rendering.
 * </p>
 *
 * @param <U> The bean type.
 */
public class ArchivedAuditListModel<U extends Bean> extends ArchivedDocumentListModel<U> {

	private List<String> auditBindings = List.of(Audit.timestampPropertyName,
			Audit.userNamePropertyName,
			Audit.operationPropertyName,
			Audit.auditModuleNamePropertyName,
			Audit.auditDocumentNamePropertyName,
			Audit.auditBizKeyPropertyName,
			Audit.auditBizIdPropertyName);

	/**
	 * Performs the getDescription operation.
	 * @return the operation result
	 */
	@Override
	public String getDescription() {
		return "The list of all Audits.";
	}

	/**
	 * Performs the toSortBinding operation.
	 * @param binding the binding value
	 * @return the operation result
	 */
	@Override
	protected String toSortBinding(String binding) {
		return DocumentConverter.toSortBinding(binding);
	}

	/**
	 * Performs the convertToBean operation.
	 * @param luceneDoc the luceneDoc value
	 * @return the operation result
	 */
	@Override
	protected Bean convertToBean(Document luceneDoc) {
		Map<String, Object> props = new HashMap<>();

		Consumer<String> putStringField = binding -> {
			props.put(binding, luceneDoc.get(binding));
		};

		Stream.of(
				Bean.DOCUMENT_ID,
				Audit.userNamePropertyName,
				Bean.USER_ID,
				Audit.operationPropertyName,
				Audit.auditModuleNamePropertyName,
				Audit.auditDocumentNamePropertyName,
				Audit.auditBizIdPropertyName,
				Audit.auditBizKeyPropertyName)
				.forEach(putStringField);

		String dateStr = luceneDoc.get(Audit.timestampPropertyName);
		props.put(Audit.timestampPropertyName, new Timestamp(DocumentConverter.stringToDate(dateStr)));
		props.put(PersistentBean.FLAG_COMMENT_NAME, null);

		return new DynamicBean(getModule(), getDocument(), props);
	}

	/**
	 * Performs the getModule operation.
	 * @return the operation result
	 */
	@Override
	protected String getModule() {
		return Audit.MODULE_NAME;
	}

	/**
	 * Performs the getDocument operation.
	 * @return the operation result
	 */
	@Override
	protected String getDocument() {
		return Audit.DOCUMENT_NAME;
	}

	/**
	 * Performs the getColumns operation.
	 * @return the operation result
	 */
	@Override
	public List<MetaDataQueryColumn> getColumns() {

		return auditBindings.stream()
				.map(this::createColumn)
				.collect(toCollection(ArrayList::new));
	}

	/**
	 * Creates a projected query column definition for an archived-audit binding.
	 *
	 * @param binding The binding to expose in the archived projection.
	 * @return A sortable projected column for the supplied binding.
	 */
	@SuppressWarnings("static-method")
	private MetaDataQueryColumn createColumn(String binding) {
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(binding);
		column.setSortable(true);

		if (Audit.auditBizIdPropertyName.equals(binding)) {
			column.setHidden(true);
		}

		return column;
	}

	/**
	 * Performs the getProjections operation.
	 * @return the operation result
	 */
	@Override
	public Set<String> getProjections() {

		Set<String> projections = new LinkedHashSet<>();

		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);

		projections.addAll(auditBindings);

		return projections;
	}

	/**
	 * Performs the getDefaultSort operation.
	 * @return the operation result
	 */
	@Override
	protected Sort getDefaultSort() {
		// Default to sorting by timestamp, descending
		return new Sort(new SortField(toSortBinding(Audit.timestampPropertyName), Type.STRING, true));
	}
}
