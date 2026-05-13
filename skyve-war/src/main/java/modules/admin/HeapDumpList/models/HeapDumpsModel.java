package modules.admin.HeapDumpList.models;

import java.io.File;
import java.util.ArrayList;
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

import modules.admin.HeapDumpList.util.HeapDumpUtil;
import modules.admin.domain.DownloadFolder;
import modules.admin.domain.HeapDumpList;

/**
 * List model for displaying heap dump files for download in the content directory.
 */
public class HeapDumpsModel extends ListModel<HeapDumpList> {

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
	}

	@Override
	public String getDescription() {
		return "All Heap Dumps";
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
		// Not required
		return null;
	}

	@Override
	public Filter newFilter() {
		// Not required
		return null;
	}

	@Override
	public void putParameter(String name, Object value) {
		// Not required
	}

	@Override
	public Page fetch() throws Exception {
		return fetchHeapDumps(
				HeapDumpUtil.getDirectory(),
				getStartRow(),
				getEndRow());
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		throw new IllegalStateException();
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties)
			throws Exception {
		throw new IllegalStateException();
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException();
	}

	/**
	 * Lists heap dump files in the given directory.
	 *
	 * @param dirPath the directory path
	 * @param startRow the start row for paging
	 * @param endRow the end row for paging
	 * @return a page containing heap dump beans
	 */
	private static Page fetchHeapDumps(String dirPath, int startRow, int endRow) {
		List<String> solvExports = new ArrayList<>();
		File[] files = FileUtil.listFiles(new File(dirPath), ".*.hprof", SortDirection.descending);
		if (files != null) {
			for (File file : files) {
				solvExports.add(file.getName());
			}
		}

		return fetchHeapDumps(solvExports, startRow, endRow);
	}

	/**
	 * Converts a list of heap dump filenames to a page of dynamic beans.
	 *
	 * @param heapDumps the list of heap dump filenames
	 * @param startRow the start row for paging
	 * @param endRow the end row for paging
	 * @return a page containing heap dump beans
	 */
	private static Page fetchHeapDumps(List<String> heapDumps, int startRow, int endRow) {
		int start = startRow;
		int end = endRow;
		OptimisticLock lock = new OptimisticLock(CORE.getUser().getName(), new Date());

		List<Bean> rows = new ArrayList<>(end - start);
		int i = 0;
		for (String backupName : heapDumps) {
			if (i >= start) {
				Map<String, Object> properties = new TreeMap<>();
				properties.put(Bean.DOCUMENT_ID, backupName);
				properties.put(PersistentBean.LOCK_NAME, lock);
				properties.put(PersistentBean.TAGGED_NAME, null);
				properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
				properties.put(Bean.BIZ_KEY, backupName);
				properties.put(DownloadFolder.namePropertyName, backupName);
				rows.add(new DynamicBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));

				if (i >= end) {
					break;
				}
			}
			
			i++;
		}

		Page page = new Page();
		page.setTotalRows(heapDumps.size());
		page.setRows(rows);

		Map<String, Object> properties = new TreeMap<>();
		properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
		page.setSummary(new DynamicBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));
		
		return page;
	}
}