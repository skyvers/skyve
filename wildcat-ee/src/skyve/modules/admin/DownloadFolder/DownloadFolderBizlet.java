package modules.admin.DownloadFolder;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;

import modules.admin.domain.DownloadFolder;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.metadata.module.query.QueryColumnImpl;

public class DownloadFolderBizlet extends Bizlet<DownloadFolder> {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6554942396689007986L;



	/**
	 * fetch - generalised fetch for DownloadFolder
	 * @param backupDirPrefix
	 * @param startRow
	 * @param endRow
	 * @return
	 */
	public static Page fetch(String backupDirPrefix, int startRow, int endRow) {
		
		File[] folders = new File(backupDirPrefix).listFiles(new FileFilter() {
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

		int start = startRow;
		int end = endRow;
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
				properties.put(DownloadFolder.namePropertyName, folderName);
				rows.add(new MapBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));

				if (i >= end) {
					break;
				}
			}
			i++;
		}

		Page page = new Page();
		page.setTotalRows(folderNames.size());
		page.setRows(rows);

		Map<String, Object> properties = new TreeMap<>();
		properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
		page.setSummary(new MapBean(DownloadFolder.MODULE_NAME, DownloadFolder.DOCUMENT_NAME, properties));
		return page;

	}


}
