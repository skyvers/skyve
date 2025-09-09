package modules.admin.Dashboard;

import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.metadata.model.document.Document;

import modules.admin.domain.User;

public class DashboardUtil {

	public static final String DEFAULT_DASHBOARD_ICON = "fa-solid fa-house";
	public static final String HOME_DASHBOARD_PLURAL_ALIAS = "Home DashBoards";
	public static final String HOME_DASHBOARD_SINGULAR_ALIAS = "Home DashBoard";
	public static final String HOME_DASHBOARD = "HomeDashboard";

	// used for 14 day dashboard calculations
	public static final Long TWO_WEEKS_AGO = Long.valueOf(System.currentTimeMillis() - 1209600000L);

	/**
	 * Convenience method to get an attribute display name for use in validation exception messages etc.
	 * 
	 * @param moduleName
	 * @param documentName
	 * @param attributeName
	 * @return
	 */
	public static String attributeDisplayName(String moduleName, String documentName, String attributeName) {
		Customer customer = CORE.getCustomer();
		org.skyve.metadata.module.Module module = customer.getModule(moduleName);
		Document document = module.getDocument(customer, documentName);
		Attribute a = document.getAttribute(attributeName);
		if (a != null) {
			return a.getDisplayName();
		}
		return attributeName; // fail silently and return something that may be helpful to facilitate debugging
	}

	/**
	 * Returns a domain value of a document name and plural alias, for selection by the user
	 * to nominate what data set is being exported/imported
	 * 
	 * @return
	 */
	public static DomainValue documentDomainValue(String documentName) {
		Customer customer = CORE.getCustomer();
		org.skyve.metadata.module.Module module = customer.getModule(User.MODULE_NAME);
		Document doc = module.getDocument(customer, documentName);
		return new DomainValue(documentName, doc.getPluralAlias());
	}

	/**
	 * Returns a the singular alias for a document
	 * 
	 * @return
	 */
	public static String documentSingularAlias(String documentName) {
		Customer customer = CORE.getCustomer();
		org.skyve.metadata.module.Module module = customer.getModule(User.MODULE_NAME);
		Document doc = module.getDocument(customer, documentName);
		return doc.getSingularAlias();
	}

}
