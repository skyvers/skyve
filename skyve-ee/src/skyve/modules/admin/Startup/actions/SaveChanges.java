package modules.admin.Startup.actions;

import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.domain.Startup;

public class SaveChanges implements ServerSideAction<Startup> {

	private static final long serialVersionUID = -2131639715230917502L;

	@Override
	public ServerSideActionResult<Startup> execute(Startup bean, WebContext webContext) throws Exception {

		// TODO Auto-generated method stub
		return new ServerSideActionResult<>(bean);
	}

}
