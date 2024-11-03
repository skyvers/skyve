package modules.admin.Startup;

import java.util.List;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.Country.CountryExtension;
import modules.admin.domain.Startup;

public class StartupBizlet extends Bizlet<StartupExtension> {
	public static final String MAP_LAYER_GMAP = "google.maps.MapTypeId.ROADMAP";
	public static final String MAP_LAYER_OPEN_STREET_MAP = "[L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {maxZoom: 19, attribution: '&copy; <a href=\\\\\\\"https://www.openstreetmap.org/copyright\\\\\\\">OpenStreetMap</a> contributors'})]";

	@Override
	public List<DomainValue> getVariantDomainValues(String attributeName) throws Exception {
		if (Startup.geoIPCountriesPropertyName.equals(attributeName)) {
			return CountryExtension.getCountries();
		}

		return super.getVariantDomainValues(attributeName);
	}

	@Override
	public StartupExtension newInstance(StartupExtension bean) throws Exception {
		// set all the current property values from the current configuration
		bean.loadProperties();

		return bean;
	}

	@Override
	public void preRerender(String source, StartupExtension bean, WebContext webContext) throws Exception {

		if (Startup.mapTypePropertyName.equals(source)) {
			// set the default layers for the selected map type
			switch (bean.getMapType()) {
				case gmap:
					bean.setMapLayer(MAP_LAYER_GMAP);
					break;
				case leaflet:
					bean.setMapLayer(MAP_LAYER_OPEN_STREET_MAP);
					break;
				default:
					break;
			}
		} else if (Startup.backupTypePropertyName.equals(source)) {
			// clear the azure backup config if switching to none/internal
			switch (bean.getBackupType()) {
				case azure:
					if (bean.getBackupDirectoryName() == null) {
						bean.setBackupDirectoryName(UtilImpl.ARCHIVE_NAME);
					}
					break;
				case none:
					bean.setBackupConnectionString(null);
					bean.setBackupDirectoryName(null);
					break;
				default:
					break;
			}
		}

		super.preRerender(source, bean, webContext);
	}

	@Override
	public void validate(StartupExtension bean, ValidationException e) throws Exception {
		if (bean.getBackupDirectoryName() != null) {
			if (bean.getBackupDirectoryName().length() < 3) {
				e.getMessages().add(new Message(Startup.backupDirectoryNamePropertyName,
						"Backup directory name must be at least 3 characters"));
			}

			if (bean.getBackupDirectoryName().length() > 63) {
				e.getMessages().add(new Message(Startup.backupDirectoryNamePropertyName,
						"Backup directory name cannot be more than 63 characters"));
			}
		}
		if(bean.getCaptchaType() != null) {
			switch(bean.getCaptchaType()) {
				case googleRecaptcha:
					if(bean.getApiGoogleRecaptchaSiteKey() == null) {
						e.getMessages().add(new Message(Startup.apiGoogleRecaptchaSiteKeyPropertyName,
								"Site Key cannot be null if using Google Recaptcha"));
					}
					break;
				case cloudflareTurnstile:
					if(bean.getApiCloudflareTurnstileSiteKey() == null) {
						e.getMessages().add(new Message(Startup.apiCloudflareTurnstileSiteKeyPropertyName,
								"Site Key cannot be null if using Cloudflare Turnstile"));
					}
					if(bean.getApiCloudflareTurnstileSecretKey() == null) {
						e.getMessages().add(new Message(Startup.apiCloudflareTurnstileSecretKeyPropertyName,
								"Secret Key cannot be null if using Cloudflare Turnstile"));
					}
					break;
				default:
					break;
			}
		}

		super.validate(bean, e);
	}
}
