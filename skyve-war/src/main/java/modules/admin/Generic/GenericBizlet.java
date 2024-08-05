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
		// used by Startup to populate the collection of selected countries as domain values
		if (conversationBean instanceof ConfigurationExtension || conversationBean instanceof StartupExtension) {
			return createCountryFromCode(bizId);
		}
		return super.resolve(bizId, conversationBean, webContext);
	}

	/**
	 * Creates a {@link Generic} from this 2 letter country code (sent in as the bizId).
	 * 
	 * @param countryCode The 2-letter country code
	 * @return A Generic representing this country to be held in a collection
	 */
	private static Generic createCountryFromCode(String countryCode) {
		Generic generic = Generic.newInstance();
		Locale locale = new Locale("", countryCode);
		String countryName = locale.getDisplayCountry();
		generic.setBizId(countryCode);
		generic.setText5001(countryCode);
		generic.setText5002(countryName);
		return generic;
	}
}
