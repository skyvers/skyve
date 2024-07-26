package modules.admin.Generic;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.domain.Generic;

public class GenericBizlet extends Bizlet<Generic> {

	@Override
	public Generic resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof ConfigurationExtension) {
			Generic generic = Generic.newInstance();
			generic.setText5001(bizId);
			return generic;
		}
		return super.resolve(bizId, conversationBean, webContext);
	}
	
}
