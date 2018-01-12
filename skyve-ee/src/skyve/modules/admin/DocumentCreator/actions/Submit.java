package modules.admin.DocumentCreator.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DocumentCreator;

public class Submit implements ServerSideAction<DocumentCreator> {

	private static final long serialVersionUID = -9077781738031503002L;

	@Override
	public ServerSideActionResult<DocumentCreator> execute(DocumentCreator bean, WebContext webContext) throws Exception {

		// TODO write out all the module and document.xml files

		return new ServerSideActionResult<>(bean);
	}
}
