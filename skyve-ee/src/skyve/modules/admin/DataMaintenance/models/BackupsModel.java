package modules.admin.DataMaintenance.models;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeSet;

import modules.admin.DownloadFolder.DownloadFolderBizlet;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DownloadFolder;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.metadata.module.query.QueryColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.util.Util;

public class BackupsModel extends ListModel<DataMaintenance> {
	private static final long serialVersionUID = -7192916420761744760L;

	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<QueryColumn> columns = new ArrayList<>(1);
	
	public BackupsModel() throws Exception {
		Customer c = CORE.getUser().getCustomer();
		drivingDocument = c.getModule(DownloadFolder.MODULE_NAME).getDocument(c, DownloadFolder.DOCUMENT_NAME);

		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);
		projections.add(DownloadFolder.namePropertyName);

		QueryColumnImpl column = new QueryColumnImpl();
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
	public List<QueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Set<String> getProjections() {
		return projections;
	}

	@Override
	public Filter getFilter() throws Exception {
		// not required
		return null;
	}

	@Override
	public Filter newFilter() throws Exception {
		// not required
		return null;
	}

	@Override
	public Page fetch() throws Exception {
		String customerName = CORE.getUser().getCustomerName();
		String backupDirPrefix = Util.getContentDirectory() + "backup_" + customerName;
		
		return DownloadFolderBizlet.fetchBackups(backupDirPrefix, getStartRow(), getEndRow());
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties)
	throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}
}
