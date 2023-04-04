package modules.admin.Startup;

import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.domain.Startup;

public class StartupBizlet extends Bizlet<StartupExtension> {

	public static final String MAP_LAYER_GMAP = "google.maps.MapTypeId.ROADMAP";
	public static final String MAP_LAYER_OPEN_STREET_MAP = "[L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {maxZoom: 19, attribution: '&copy; <a href=\\\\\\\"https://www.openstreetmap.org/copyright\\\\\\\">OpenStreetMap</a> contributors'})]";

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

}
