package modules.admin.DataMaintenance.models;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import modules.admin.domain.Backup;
import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.metadata.module.query.QueryColumnImpl;
import org.skyve.wildcat.util.UtilImpl;

public class BackupsModel extends ListModel<DataMaintenance> {
	private static final long serialVersionUID = -7192916420761744760L;

	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<QueryColumn> columns = new ArrayList<>(1);
	
	public BackupsModel() throws Exception {
		Customer c = CORE.getUser().getCustomer();
		drivingDocument = c.getModule(Backup.MODULE_NAME).getDocument(c, Backup.DOCUMENT_NAME);
		
		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);
		projections.add(Backup.namePropertyName);
		
		QueryColumnImpl column = new QueryColumnImpl();
		column.setBinding(Backup.namePropertyName);
		column.setSortable(false);
		columns.add(column);
	}
	
	@Override
	public String getDescription() {
		return "All Backups";
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
		String backupDirPrefix = UtilImpl.CONTENT_DIRECTORY + "backup_" + customerName;
		
		File[] folders = new File(backupDirPrefix).listFiles(new FileFilter() {
			@Override
			public boolean accept(File pathname) {
				return pathname.isDirectory() && (pathname.getName().length() == 14);
			}
		});
		
		// Sort the folder names
		Set<String> folderNames = new TreeSet<>();
		if (folders != null) {
			for (File folder : folders) {
				folderNames.add(folder.getName());
			}
		}
		
		int start = getStartRow();
		int end = getEndRow();
		OptimisticLock lock = new OptimisticLock(CORE.getUser().getName(), new Date());
		
		List<Bean> rows = new ArrayList<>(end - start);
		int i = 0;
		for (String folderName : folderNames) {
			if (i >= start) {
				Map<String, Object> properties = new TreeMap<>();
				properties.put(Bean.DOCUMENT_ID, folderName);
				properties.put(PersistentBean.LOCK_NAME, lock);
				properties.put(PersistentBean.TAGGED_NAME, null);
				properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
				properties.put(Bean.BIZ_KEY, folderName);
				properties.put(Backup.namePropertyName, folderName);
				rows.add(new MapBean(Backup.MODULE_NAME, Backup.DOCUMENT_NAME, properties));

				if (i >= end) {
					break;
				}
			}
			i++;
		}

		Page page = new Page();
		page.setTotalRows(folderNames.size());
		page.setRows(rows);
System.out.println(start + " : " + end + " : " + page.getRows().size() + " : " + page.getTotalRows());

		Map<String, Object> properties = new TreeMap<>();
		properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
		page.setSummary(new MapBean(Backup.MODULE_NAME, Backup.DOCUMENT_NAME, properties));
		return page;
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
		String customerName = CORE.getUser().getCustomerName();
		File backupDir = new File(UtilImpl.CONTENT_DIRECTORY + "backup_" + customerName + File.separator + bizId);
		if (backupDir.exists() && backupDir.isDirectory()) {
			backupDir.delete();
		}
	}
}
