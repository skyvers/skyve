package modules.admin.Communication.actions;

import java.io.File;

import modules.admin.domain.Communication;

import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.FileUtil;
import org.skyve.web.WebContext;

public class ZipBatch extends DownloadAction<Communication> {
	private static final long serialVersionUID = 4544317770456317465L;

	@Override
	public Download download(Communication bean, WebContext webContext) throws Exception {
		
		bean.setRefreshBatches(Boolean.FALSE);
		
		String customerName = CORE.getUser().getCustomerName();
		String batchPath = UtilImpl.CONTENT_DIRECTORY + "batch_" + customerName + File.separator + bean.getSelectedBatchTimestampFolderName();
		String zipName = String.format("batch_%s_%s.zip", customerName, bean.getSelectedBatchTimestampFolderName());
		
		return FileUtil.prepareZipDownload(batchPath, zipName, webContext);
	}

}
