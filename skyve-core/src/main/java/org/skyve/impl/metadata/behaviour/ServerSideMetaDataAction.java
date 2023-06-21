package org.skyve.impl.metadata.behaviour;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

/**
 * A ServerSideAction that executes it's metadaDataAction delegate.
 */
public class ServerSideMetaDataAction implements ServerSideAction<Bean> {
	private ActionMetaData metaDataAction = null;
	
	public ServerSideMetaDataAction(ActionMetaData metaDataAction) {
		this.metaDataAction = metaDataAction;
	}
	
	@Override
	public ServerSideActionResult<Bean> execute(Bean bean, WebContext webContext) throws Exception {
		metaDataAction.execute(bean);
		return new ServerSideActionResult<>(bean);
	}
}
