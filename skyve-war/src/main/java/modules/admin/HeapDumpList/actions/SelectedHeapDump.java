package modules.admin.HeapDumpList.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.HeapDumpList;

/**
 * Action to select a heap dump file from the the list of heap dump files in the content directory.
 */
public class SelectedHeapDump implements ServerSideAction<HeapDumpList> {

	@Override
	public ServerSideActionResult<HeapDumpList> execute(HeapDumpList bean, WebContext webContext) {
		bean.setRefresh(Boolean.FALSE);
		
		return new ServerSideActionResult<>(bean);
	}
}