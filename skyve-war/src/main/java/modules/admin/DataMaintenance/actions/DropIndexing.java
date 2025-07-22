package modules.admin.DataMaintenance.actions;

import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.DataMaintenance;

/**
 * Drop/Delete the content indexing (not just a truncate).
 * This is good for upgrading the content management.
 */
public class DropIndexing implements ServerSideAction<DataMaintenance> {
	@Override
	@SuppressWarnings("resource") // See below
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		// This content manager is not closed as the content management is stopped and started
		ContentManager cm = EXT.newContentManager();
		cm.dropIndexing();
		
		// Obtain a new content manager to prepare new indexing
		try (ContentManager newCM = EXT.newContentManager()) {
			// do nothing
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
