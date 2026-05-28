package modules.admin.Communication.models;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeSet;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;

import modules.admin.DataMaintenance.models.BackupsModel;
import modules.admin.domain.Communication;
import modules.admin.domain.DownloadFolder;

/**
 * Lists generated communication batches available for download and cleanup.
 */
public class BatchesModel extends ListModel<Communication> {
	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<MetaDataQueryColumn> columns = new ArrayList<>(1);

	/**
	 * Performs the postConstruct operation.
	 * @param customer the customer value
	 * @param runtime the runtime value
	 */
	@Override
	public void postConstruct(org.skyve.metadata.customer.Customer customer, boolean runtime) {
		drivingDocument = customer.getModule(DownloadFolder.MODULE_NAME).getDocument(customer, DownloadFolder.DOCUMENT_NAME);

		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);
		projections.add(DownloadFolder.namePropertyName);

		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(DownloadFolder.namePropertyName);
		column.setSortable(false);
		columns.add(column);
	}

	/**
	 * Performs the getDescription operation.
	 * @return the operation result
	 */
	@Override
	public String getDescription() {
		return "All DownloadFolders";
	}

	/**
	 * Performs the getDrivingDocument operation.
	 * @return the operation result
	 */
	@Override
	public Document getDrivingDocument() {
		return drivingDocument;
	}

	/**
	 * Performs the getColumns operation.
	 * @return the operation result
	 */
	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	/**
	 * Performs the getProjections operation.
	 * @return the operation result
	 */
	@Override
	public Set<String> getProjections() {
		return projections;
	}

	/**
	 * Performs the getFilter operation.
	 * @return the operation result
	 */
	@Override
	public Filter getFilter() {
		// not required
		return null;
	}

	/**
	 * Performs the newFilter operation.
	 * @return the operation result
	 */
	@Override
	public Filter newFilter() {
		// not required
		return null;
	}
	
	/**
	 * Performs the putParameter operation.
	 * @param name the name value
	 * @param value the value value
	 */
	@Override
	public void putParameter(String name, Object value) {
		// not required
	}

	/**
	 * Performs the fetch operation.
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Page fetch() throws Exception {
		Communication communication = getBean();
		return BackupsModel.fetchFolders(communication.getBasePath(), getStartRow(), getEndRow());
	}

	/**
	 * Performs the iterate operation.
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	/**
	 * Performs the update operation.
	 * @param bizId the bizId value
	 * @param properties the properties value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	/**
	 * Performs the remove operation.
	 * @param bizId the bizId value
	 * @throws Exception if the operation fails
	 */
	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}
}
