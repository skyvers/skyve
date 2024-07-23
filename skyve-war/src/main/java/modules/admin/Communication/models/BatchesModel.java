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

public class BatchesModel extends ListModel<Communication> {
	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<MetaDataQueryColumn> columns = new ArrayList<>(1);

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

	@Override
	public String getDescription() {
		return "All DownloadFolders";
	}

	@Override
	public Document getDrivingDocument() {
		return drivingDocument;
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Set<String> getProjections() {
		return projections;
	}

	@Override
	public Filter getFilter() {
		// not required
		return null;
	}

	@Override
	public Filter newFilter() {
		// not required
		return null;
	}
	
	@Override
	public void putParameter(String name, Object value) {
		// not required
	}

	@Override
	public Page fetch() throws Exception {
		Communication communication = getBean();
		return BackupsModel.fetchFolders(communication.getBasePath(), getStartRow(), getEndRow());
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}
}
