package modules.admin.Snapshot.actions;

import modules.admin.domain.Snapshot;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class CopySnapshotToUser implements ServerSideAction<Snapshot> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = 2886341074753936987L;

	/**
	 * Update the payment batch details.
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
