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

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.backup.ExternalBackup;
import org.skyve.metadata.SortDirection;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.util.FileUtil;

import modules.admin.domain.DownloadFolder;

public class DownloadFolderBizlet extends Bizlet<DownloadFolder> {
	/**
	 * fetch - generalised fetch for DownloadFolder
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
	 * Fetch backup zip files.
	 *
	 * @param dirPath
	 * @param startRow
	 * @param endRow
	 * @return
	 */
	public static Page fetchBackups(String dirPath, int startRow, int endRow) {
		Set<String> backups = new TreeSet<>();
		if (ExternalBackup.areExternalBackupsEnabled()) {
			backups.addAll(ExternalBackup.getInstance().listBackups());
		} else {
			File[] files = FileUtil.listFiles(new File(dirPath), ".*.zip", SortDirection.descending);
			if (files != null) {
				for (File file : files) {
					backups.add(file.getName());
				}
			}
		}

		return fetch(backups, startRow, endRow);
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
