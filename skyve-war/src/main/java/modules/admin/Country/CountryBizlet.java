package modules.admin.Country;

import org.skyve.domain.Bean;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import jakarta.inject.Inject;

public class CountryBizlet extends Bizlet<CountryExtension> {
	@Inject
	private transient CountryService countryService;

	/**
	 * Resolve a country based on its bizId - the ISO 2 letter country code.
	 */
	@Override
	public CountryExtension resolve(String bizId, Bean conversationBean, WebContext webContext) throws Exception {
		return countryService.fromCode(bizId);
	}
}
