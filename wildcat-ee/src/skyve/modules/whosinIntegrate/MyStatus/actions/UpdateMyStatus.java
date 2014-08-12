package modules.whosinIntegrate.MyStatus.actions;

import modules.whosinIntegrate.domain.Staff;
import modules.whosinIntegrate.domain.MyStatus;

import org.skyve.CORE;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.persistence.Persistence;
import org.skyve.web.WebContext;

public class UpdateMyStatus implements ServerSideAction<MyStatus> {
	/**
	 * For Serialization
	 */
	private static final long serialVersionUID = -4317908281075686229L;

	@Override
	public ServerSideActionResult execute(MyStatus bean, WebContext webContext) throws Exception {

		Persistence pers= CORE.getPersistence();
		
		Staff myStaff= pers.save(bean.getMyStaff());
		bean.setMyStaff(myStaff);
		
		return new ServerSideActionResult(bean); // stay on the same form
	}
}
