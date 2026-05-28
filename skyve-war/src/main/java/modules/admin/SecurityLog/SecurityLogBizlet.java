package modules.admin.SecurityLog;

import org.skyve.domain.Bean;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet;
import org.skyve.util.IPGeolocation;
import org.skyve.web.WebContext;

/**
 * Enriches security log records with GeoIP-derived location data when an
 * existing log entry is opened.
 */
public class SecurityLogBizlet extends Bizlet<SecurityLogExtension> {
	/**
	 * Populates city, region, and country fields from the resolved GeoIP record
	 * when the implicit action is edit.
	 *
	 * @param actionName the implicit action being executed
	 * @param bean the security log bean being prepared
	 * @param parentBean the parent bean for nested edit contexts, if any
	 * @param webContext the current web request context
	 * @return the prepared bean returned by the superclass pipeline
	 * @throws Exception if pre-execution processing fails
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
