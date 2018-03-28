package modules.admin.util;

import modules.admin.domain.UserLoginRecord;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/skyve/modules/admin/UserLoginRecord/UserLoginRecordFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class UserLoginRecordFactory extends AbstractDomainFactory<UserLoginRecord > {

	@Override
	public UserLoginRecord getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(UserLoginRecord.MODULE_NAME);
		Document document = module.getDocument(customer, UserLoginRecord.DOCUMENT_NAME);

		UserLoginRecord userLoginRecord = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return userLoginRecord;
	}
}