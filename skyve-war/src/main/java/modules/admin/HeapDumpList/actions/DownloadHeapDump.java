package modules.admin.HeapDumpList.actions;

import java.io.File;

import org.skyve.content.MimeType;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.Download;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.web.WebContext;

import modules.admin.HeapDumpList.util.HeapDumpUtil;
import modules.admin.domain.HeapDumpList;

/**
 * Action to download a selected heap dump file from the content directory.
 */
public class DownloadHeapDump extends DownloadAction<HeapDumpList> {

	@Override
	public void prepare(HeapDumpList bean, WebContext webContext) throws Exception {
		String selectedName = bean.getSelectedName();
		File heapDump = new File(HeapDumpUtil.getDirectory() + File.separator + selectedName);

		if (!heapDump.exists()) {
			throw new ValidationException(new Message("heap dump " + selectedName + " no longer exists"));
		}
	}

	@Override
	public Download download(
			HeapDumpList bean,
			WebContext webContext)
			throws Exception {
		String selectedName = bean.getSelectedName();
		final File heapDump = new File(HeapDumpUtil.getDirectory() + File.separator + selectedName);

		return new Download(selectedName, heapDump, MimeType.octetStream);
	}
}