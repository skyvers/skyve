package modules.admin.Startup;

import org.apache.commons.lang3.StringUtils;
import org.locationtech.jts.geom.Coordinate;
import org.locationtech.jts.geom.GeometryFactory;
import org.locationtech.jts.io.ParseException;
import org.locationtech.jts.io.WKTReader;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.web.WebContext;

import modules.admin.domain.Startup;
import modules.admin.domain.Startup.MapType;

public class StartupBizlet extends Bizlet<Startup> {

	private static final long serialVersionUID = 8910966230892423369L;

	private static final String MAP_LAYER_GMAP = "google.maps.MapTypeId.ROADMAP";
	private static final String MAP_LAYER_OPEN_STREET_MAP = "[L.tileLayer('https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {maxZoom: 19, attribution: '&copy; <a href=\\\\\\\"https://www.openstreetmap.org/copyright\\\\\\\">OpenStreetMap</a> contributors'})]";

	@Override
	public Startup newInstance(Startup bean) throws Exception {
		// set all the current property values from the current configuration
		bean.setApiGoogleMapsKey(UtilImpl.GOOGLE_MAPS_V3_API_KEY);
		bean.setApiGoogleRecaptchaKey(UtilImpl.GOOGLE_RECAPTCHA_SITE_KEY);

		bean.setEnvironmentIdentifier(UtilImpl.ENVIRONMENT_IDENTIFIER);
		bean.setEnvironmentSupportEmail(UtilImpl.SUPPORT_EMAIL_ADDRESS);

		bean.setMailBogusSend(Boolean.valueOf(UtilImpl.SMTP_TEST_BOGUS_SEND));
		bean.setMailPassword(UtilImpl.SMTP_PWD);
		bean.setMailPort(Integer.valueOf(UtilImpl.SMTP_PORT));
		bean.setMailSender(UtilImpl.SMTP_SENDER);
		bean.setMailServerUrl(UtilImpl.SMTP);
		bean.setMailTestRecipient(UtilImpl.SMTP_TEST_RECIPIENT);
		bean.setMailUsername(UtilImpl.SMTP_UID);

		String mapCentre = UtilImpl.MAP_CENTRE;
		if(StringUtils.isBlank(mapCentre)) {
			bean.setMapCentre(new GeometryFactory().createPoint(new Coordinate(0, 0)));
		} else {
			try {
				bean.setMapCentre(new WKTReader().read(mapCentre));
			} catch (ParseException e) {
				bean.setMapCentre(new GeometryFactory().createPoint(new Coordinate(0, 0)));
			}	
		}
		bean.setMapLayer(UtilImpl.MAP_LAYERS);
		bean.setMapType(MapType.fromCode(UtilImpl.MAP_TYPE.name()));
		bean.setMapZoom(Integer.valueOf(UtilImpl.MAP_ZOOM));

		return bean;
	}

	@Override
	public void preRerender(String source, Startup bean, WebContext webContext) throws Exception {

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
		}

		super.preRerender(source, bean, webContext);
	}

}
