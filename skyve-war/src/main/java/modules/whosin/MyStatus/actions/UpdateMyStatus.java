package modules.whosin.MyStatus.actions;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

import modules.whosin.domain.MyStatus;
import modules.whosin.domain.Staff;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Saves the current user's staff status details from the MyStatus form.
 */
public class UpdateMyStatus implements ServerSideAction<MyStatus> {
	/**
	 * Persists the associated staff record and returns to the same form.
	 *
	 * @param bean the status bean containing staff updates
	 * @param webContext the active web context
	 * @return a result containing the updated bean
	 * @throws Exception if persistence fails
	 */
	@Override
	public @Nonnull ServerSideActionResult<MyStatus> execute(@Nonnull MyStatus bean, @Nullable WebContext webContext) throws Exception {

		Persistence pers= CORE.getPersistence();
		
		Staff myStaff= pers.save(bean.getMyStaff());
		bean.setMyStaff(myStaff);
		
		return new ServerSideActionResult<>(bean); // stay on the same form
	}
}
