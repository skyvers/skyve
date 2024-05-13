package modules.admin.DataMaintenance.models;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.util.FileUtil;

import modules.admin.DataMaintenance.DataMaintenanceExtension;
import modules.admin.domain.DataMaintenance;
import modules.admin.domain.DownloadFolder;

public class BackupsModel extends ListModel<DataMaintenance> {
	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<MetaDataQueryColumn> columns = new ArrayList<>(1);
	
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		drivingDocument = customer.getModule(DownloadFolder.MODULE_NAME).getDocument(customer, DownloadFolder.DOCUMENT_NAME);

		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);
		projections.add(DownloadFolder.namePropertyName);
		projections.add(DownloadFolder.sizePropertyName);

		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(DownloadFolder.namePropertyName);
		column.setSortable(false);
		columns.add(column);

		MetaDataQueryProjectedColumnImpl sizeColumn = new MetaDataQueryProjectedColumnImpl();
		sizeColumn.setBinding(DownloadFolder.sizePropertyName);
		sizeColumn.setSortable(false);
		columns.add(sizeColumn);
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
		return fetchBackups(DataMaintenanceExtension.backupDirectoryPrefix(), getStartRow(), getEndRow());
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

	/**
	 * Generalised fetch for DownloadFolder
	 * 
	 * @param dirPath
	 * @param startRow
	 * @param endRow
	 * @return
	 */
	public static Page fetchFolders(String dirPath, int startRow, int endRow) {
		File[] folders = new File(dirPath).listFiles(new FileFilter() {
			@Override
			public boolean accept(File pathname) {
				return pathname.isDirectory() && (pathname.getName().length() == 14);
			}
		});

		// Sort the folder names
		Set<String> folderNames = new TreeSet<>(Collections.reverseOrder());
		if (folders != null) {
			for (File folder : folders) {
				folderNames.add(folder.getName());
			}
		}

		return fetch(folderNames, startRow, endRow);
	}

	/**
	 * Fetch backup zip files (name and size).
	 *
	 * @param dirPath
	 * @param startRow
	 * @param endRow
	 * @return
	 * @throws IOException
	 */
	public static Page fetchBackups(String dirPath, int startRow, int endRow) throws IOException {
		Map<String, Long> backups = new TreeMap<>();
		if (ExternalBackup.areExternalBackupsEnabled()) {
			ExternalBackup backupInstance = ExternalBackup.getInstance();
			for (String s : backupInstance.listBackups()) {
				Long fileSize = Long.valueOf(backupInstance.getFileSize(s));
				if (fileSize != null) {
					// Convert to MB for readability
					fileSize = Long.valueOf(fileSize.longValue() / (1024 * 1024));
					backups.put(s, fileSize);
				} else {
					backups.put(s, Long.valueOf(0));
				}
			}
		} else {
			File[] files = FileUtil.listFiles(new File(dirPath), ".*.zip", SortDirection.descending);
			if (files != null) {
				for (File file : files) {
					Long fileSize = Long.valueOf(Files.size(file.toPath()));
					if (fileSize != null) {
						// Convert to MB for readability
						fileSize = Long.valueOf(fileSize.longValue() / (1024 * 1024));
						backups.put(file.getName(), fileSize);
					} else {
						backups.put(file.getName(), Long.valueOf(0));
					}
				}
			}
		}
		return fetchBackups(backups, startRow, endRow);
	}

	/**
	 * Fetch backups - name and size.
	 * Return a page of backups.
	 *
	 * @param things
	 * @param startRow
	 * @param endRow
	 * @return
	 */
	private static Page fetchBackups(Map<String, Long> backups, int startRow, int endRow) {
		int start = startRow;
		int end = endRow;
		OptimisticLock lock = new OptimisticLock(CORE.getUser().getName(), new Date());

		List<Bean> rows = new ArrayList<>(end - start);
		int i = 0;
		for (String backupName : backups.keySet()) {
			if (i >= start) {
				Map<String, Object> properties = new TreeMap<>();
				properties.put(Bean.DOCUMENT_ID, backupName);
				properties.put(PersistentBean.LOCK_NAME, lock);
				properties.put(PersistentBean.TAGGED_NAME, null);
				properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
				properties.put(Bean.BIZ_KEY, backupName);
				properties.put(DownloadFolder.namePropertyName, backupName);
				properties.put(DownloadFolder.sizePropertyName, backups.get(backupName));
				rows.add(new DynamicBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));

				if (i >= end) {
					break;
				}
			}
			i++;
		}

		Page page = new Page();
		page.setTotalRows(backups.size());
		page.setRows(rows);

		Map<String, Object> properties = new TreeMap<>();
		properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
		page.setSummary(new DynamicBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));
		return page;
	}

	/**
	 * Return a page of things.
	 *
	 * @param things
	 * @param startRow
	 * @param endRow
	 * @return
	 */
	private static Page fetch(Set<String> things, int startRow, int endRow) {
		int start = startRow;
		int end = endRow;
		OptimisticLock lock = new OptimisticLock(CORE.getUser().getName(), new Date());

		List<Bean> rows = new ArrayList<>(end - start);
		int i = 0;
		for (String thing : things) {
			if (i >= start) {
				Map<String, Object> properties = new TreeMap<>();
				properties.put(Bean.DOCUMENT_ID, thing);
				properties.put(PersistentBean.LOCK_NAME, lock);
				properties.put(PersistentBean.TAGGED_NAME, null);
				properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
				properties.put(Bean.BIZ_KEY, thing);
				properties.put(DownloadFolder.namePropertyName, thing);
				rows.add(new DynamicBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));

				if (i >= end) {
					break;
				}
			}
			i++;
		}

		Page page = new Page();
		page.setTotalRows(things.size());
		page.setRows(rows);

		Map<String, Object> properties = new TreeMap<>();
		properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
		page.setSummary(new DynamicBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));
		return page;
	}
}
