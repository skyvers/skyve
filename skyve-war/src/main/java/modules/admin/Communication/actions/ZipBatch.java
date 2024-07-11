package modules.admin.Communication.actions;

import java.io.File;

import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.util.FileUtil;
import org.skyve.web.WebContext;

import modules.admin.domain.Communication;

public class ZipBatch extends DownloadAction<Communication> {
	@Override
	public void prepare(Communication bean, WebContext webContext) throws Exception {
		bean.setRefreshBatches(Boolean.FALSE);
	}
	
	@Override
	public Download download(Communication bean, WebContext webContext) throws Exception {
		
		String batchPath = bean.getBasePath() + File.separator + bean.getSelectedBatchTimestampFolderName();
		String zipName = FileUtil.constructSafeFilePath("", bean.getSelectedBatchTimestampFolderName() , ".zip", false, bean.getDescription()+ "_");
		
		return FileUtil.prepareZipDownload(batchPath, zipName);
	}
}
