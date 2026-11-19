package org.skyve.impl.metadata.behaviour;

import org.skyve.domain.Bean;
import org.skyve.impl.metadata.repository.behaviour.ActionMetaData;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A ServerSideAction that executes it's metadaDataAction delegate.
 */
public class ServerSideMetaDataAction implements ServerSideAction<Bean> {
	private ActionMetaData metaDataAction = null;
	
	public ServerSideMetaDataAction(@Nonnull ActionMetaData metaDataAction) {
		this.metaDataAction = metaDataAction;
	}
	
	@Override
	public @Nonnull ServerSideActionResult<Bean> execute(@Nonnull Bean bean, @Nullable WebContext webContext) throws Exception {
		metaDataAction.execute(bean);
		return new ServerSideActionResult<>(bean);
	}
}
