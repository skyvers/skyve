package modules.admin.Snapshots.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.admin.domain.Snapshot;
import modules.admin.domain.Snapshots;

/**
 * Persists user-defined ordering for snapshots in a selected module/query
 * context.
 */
public class Reorder implements ServerSideAction<Snapshots> {
	private static String update = "update ADM_Snapshot set ordinal = :ordinal where bizId = :bizId";

	/**
	 * Reorder the snapshots based on the snapshotsToReorder collection sequence.
	 *
	 * @param bean the snapshots aggregate containing the reordered collection
	 * @param webContext the current web context
	 * @return the action result wrapping the updated bean
	 * @throws Exception if SQL updates fail
	 */
	@Override
	public ServerSideActionResult<Snapshots> execute(Snapshots bean, WebContext webContext) throws Exception {
		Persistence p = CORE.getPersistence();
		List<Snapshot> snapshotsToReorder = bean.getSnapshotsToReorder(); 
		for (int i = 0, l = snapshotsToReorder.size(); i < l; i++) {
			Snapshot snapshot = snapshotsToReorder.get(i);
			p.newSQL(update).putParameter(Snapshot.ordinalPropertyName, Integer.valueOf(i)).putParameter(Bean.DOCUMENT_ID, snapshot.getBizId(), false).execute();
		}
		
		return new ServerSideActionResult<>(bean);
	}
}
