package modules.admin.Generic;

import java.util.Locale;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.Startup.StartupExtension;
import modules.admin.domain.Generic;

public class GenericBizlet extends Bizlet<Generic> {

	@Override
	public Generic resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		if (conversationBean instanceof ConfigurationExtension || conversationBean instanceof StartupExtension) {
			Generic generic = Generic.newInstance();
			// Get country name for code
			Locale locale = new Locale("", bizId);
			String countryName = locale.getDisplayCountry();
			generic.setText5001(bizId);
			generic.setText5002(countryName);
			return generic;
		}
		return super.resolve(bizId, conversationBean, webContext);
	}
	
}
