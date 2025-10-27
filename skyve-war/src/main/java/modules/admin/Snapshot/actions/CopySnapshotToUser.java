package modules.admin.Snapshot.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Snapshot;

/**
 * Copies a snapshot to another user, effectively donating the snapshot.
 * Creates a new snapshot instance with the same name assigned to the target user.
 */
public class CopySnapshotToUser implements ServerSideAction<Snapshot> {
	/**
	 * Copy a snapshot to a user - ie donate a snapshot.
	 */
	@Override
	public ServerSideActionResult<Snapshot> execute(Snapshot bean, WebContext webContext) throws Exception {

		if (bean.getCopyToUser() != null) {

			// copy tag and tagged items
			Snapshot newSnapshot= Snapshot.newInstance();
			newSnapshot.setName(bean.getName());
			newSnapshot.setBizUserId(bean.getCopyToUser().getBizId());
			Persistence pers = CORE.getPersistence();
			pers.upsertBeanTuple(newSnapshot);			
		}
		return new ServerSideActionResult<>(bean);
	}
}
