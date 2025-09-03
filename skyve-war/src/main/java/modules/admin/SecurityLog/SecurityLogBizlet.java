package modules.admin.SecurityLog;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.util.IPGeolocation;
import org.skyve.web.WebContext;

public class SecurityLogBizlet extends Bizlet<SecurityLogExtension> {
	/**
	 * Populate the "where" fields when edited.
	 */
	@Override
	public SecurityLogExtension preExecute(ImplicitActionName actionName,
											SecurityLogExtension bean,
											Bean parentBean,
											WebContext webContext)
	throws Exception {
		if (ImplicitActionName.Edit.equals(actionName)) {
			IPGeolocation geoIP = bean.getGeoIP();
			bean.setCity(geoIP.city());
			bean.setRegion(geoIP.region());
			bean.setCountry(geoIP.getCountry());
		}
		return super.preExecute(actionName, bean, parentBean, webContext);
	}
}
