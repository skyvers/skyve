package modules.admin.DataMaintenance.actions;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.web.WebContext;

import modules.admin.ModulesUtil;
import modules.admin.domain.DataMaintenance;

public class Truncate implements ServerSideAction<DataMaintenance> {
	
	@Override
	public ServerSideActionResult<DataMaintenance> execute(DataMaintenance bean, WebContext webContext)
	throws Exception {
		bean.setRefreshContent(Boolean.TRUE);

		//check password confirmation checks out
		if(bean.getConfirmPassword()==null) {
			throw new ValidationException(new Message(DataMaintenance.confirmPasswordPropertyName, "Enter your password to enable truncation"));
		}
		
		if(ModulesUtil.currentAdminUser()==null) {
			throw new ValidationException(new Message("No valid user exist"));
		}
		
		if(EXT.checkPassword(bean.getConfirmPassword(), ModulesUtil.currentAdminUser().getPassword())) {
			
			org.skyve.impl.backup.Truncate.truncate(bean.getSchemaName(), true, true);
		
			//re-inject bootstrap user if option is selected
			if(Boolean.TRUE.equals(bean.getInjectBootstrapUser())) {
				EXT.bootstrap(CORE.getPersistence());
			}
			
		} else {
			throw new ValidationException(new Message(DataMaintenance.confirmPasswordPropertyName, "Password does not match"));
		}
				
		return new ServerSideActionResult<>(bean);
	}
}
