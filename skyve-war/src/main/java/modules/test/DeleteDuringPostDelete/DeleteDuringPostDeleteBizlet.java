package modules.test.DeleteDuringPostDelete;

import org.skyve.CORE;
import org.skyve.metadata.model.document.Bizlet;

import modules.test.domain.DeleteDuringPostDelete;

import jakarta.annotation.Nonnull;

/**
 * Demonstrates post-delete lifecycle behaviour that removes an aggregated association.
 */
public class DeleteDuringPostDeleteBizlet extends Bizlet<DeleteDuringPostDelete> {
	/**
	 * Deletes the associated aggregated document after the parent has been removed.
	 *
	 * @param bean the deleted parent bean
	 * @throws Exception if cleanup fails
	 */
	@Override
	public void postDelete(@Nonnull DeleteDuringPostDelete bean) throws Exception {
		CORE.getPersistence().delete(bean.getAggregatedAssociation());
	}
}
