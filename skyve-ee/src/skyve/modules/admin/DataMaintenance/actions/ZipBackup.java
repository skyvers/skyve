package modules.admin.DataMaintenance.actions;

import java.io.File;

import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.FileUtil;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

public class ZipBackup extends DownloadAction<DataMaintenance> {
	private static final long serialVersionUID = 4544317770456317465L;

	@Override
	public Download download(DataMaintenance bean, WebContext webContext)
	throws Exception {
		
		
		String customerName = CORE.getUser().getCustomerName();
		String batchPath = Util.getContentDirectory() + "backup_" + customerName + File.separator + bean.getSelectedBackupTimestampFolderName();
		String zipName = String.format("backup_%s_%s.zip", customerName, bean.getSelectedBackupTimestampFolderName());
		
		return FileUtil.prepareZipDownload(batchPath, zipName, webContext);
	}
}
