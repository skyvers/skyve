package modules.admin.SystemDashboard.models;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.InMemoryListModel;
import org.skyve.persistence.DocumentQuery;

import modules.admin.domain.SecurityLog;
import modules.admin.domain.SystemDashboard;

/**
 * Displays recent {@link SecurityLog} entries.
 * 
 * @author Simeon Solomou
 */
public class RecentSecurityLogModel extends InMemoryListModel<SystemDashboard> {
	private List<MetaDataQueryColumn> columns = new ArrayList<>();

	/**
	 * Initializes list metadata including driving document and visible columns.
	 *
	 * @param customer the current customer metadata context
	 * @param runtime whether the model is being initialized for runtime usage
	 */
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		// Set driving document
		Module module = customer.getModule(SecurityLog.MODULE_NAME);
		Document document = module.getDocument(customer, SecurityLog.DOCUMENT_NAME);
		setDrivingDocument(module, document);

		// Add columns
		defineColumn(SecurityLog.timestampPropertyName, "Security Log");
		defineColumn(SecurityLog.usernamePropertyName, "Username");
		defineColumn(SecurityLog.eventTypePropertyName, "Event Type");
		defineColumn(SecurityLog.eventMessagePropertyName, "Event Message");

		super.postConstruct(customer, runtime);
	}

	/**
	 * Defines a projected read-only list column.
	 *
	 * @param binding the document binding
	 * @param displayName the user-facing column title
	 */
	private void defineColumn(final String binding, final String displayName) {
		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setDisplayName(displayName);
		column.setBinding(binding);
		column.setEditable(false);
		column.setFilterable(false);

		columns.add(column);
	}

	/**
	 * Returns the list description shown by the view model framework.
	 *
	 * @return a short model description
	 */
	@Override
	public String getDescription() {
		return "Recent Security Logs";
	}

	/**
	 * Returns configured projected columns for the in-memory list model.
	 *
	 * @return projected security-log columns
	 */
	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	/**
	 * Retrieves the most recent security log rows.
	 *
	 * @return up to five most recent security log beans
	 * @throws Exception if query execution fails
	 */
	@Override
	public List<Bean> getRows() throws Exception {
		DocumentQuery q = CORE.getPersistence()
				.newDocumentQuery(SecurityLog.MODULE_NAME, SecurityLog.DOCUMENT_NAME);
		q.addBoundOrdering(SecurityLog.timestampPropertyName, SortDirection.descending);
		q.setMaxResults(5);
		return q.beanResults();
	}

	/**
	 * Rejects row updates because this model is read-only.
	 *
	 * @param bizId the row identifier
	 * @param properties requested updates
	 * @return never returns normally
	 * @throws Exception always throws {@link IllegalStateException}
	 */
	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("Not implemented");
	}

	/**
	 * Rejects row removal because this model is read-only.
	 *
	 * @param bizId the row identifier
	 * @throws Exception always throws {@link IllegalStateException}
	 */
	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("Not implemented");
	}
}
