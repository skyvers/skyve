package modules.admin.ControlPanel.actions;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.ServerSideActionResult;
import org.skyve.util.Binder;
import org.skyve.util.Util;
import org.skyve.web.WebContext;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.Generic;

public class AddAPIKey implements ServerSideAction<ControlPanelExtension> {
	@Override
	public ServerSideActionResult<ControlPanelExtension> execute(ControlPanelExtension bean, WebContext webContext) 
	throws Exception {
		
		ValidationException v = new ValidationException();
		if(bean.getNewProperty().getText5001()==null) {
			v.getMessages().add(new Message(Binder.createCompoundBinding(ControlPanel.newPropertyPropertyName, Generic.text5001PropertyName), "The new API Key must have a name"));
		}
		if(bean.getNewProperty().getText5002()==null) {
			v.getMessages().add(new Message(Binder.createCompoundBinding(ControlPanel.newPropertyPropertyName, Generic.text5002PropertyName), "The new API Key must have a value"));
		}
		if(!v.getMessages().isEmpty()) {
			throw v;
		}
		
		//find index of last first key
		int index = 0;
		for(Generic g: bean.getStartupProperties()) {
			if(g.getText5001().length()>3 && g.getText5001().startsWith(ControlPanelExtension.API_STANZA_KEY)) {
				index = bean.getStartupProperties().indexOf(g);
			}
		}
		
		Generic generic = Util.cloneBySerialization(bean.getNewProperty());
		generic.setText5001(ControlPanelExtension.API_STANZA_KEY + bean.getNewProperty().getText5001());
		bean.getStartupProperties().add(index+1, generic);
		
		//and clear for another API
		bean.setNewProperty(Generic.newInstance());
		
		return new ServerSideActionResult<>(bean);
	}
}
