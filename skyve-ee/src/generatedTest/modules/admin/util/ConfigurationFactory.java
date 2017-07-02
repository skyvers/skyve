package modules.admin.util;

import modules.admin.domain.Configuration;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/ConfigurationFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class ConfigurationFactory extends AbstractDomainFactory<Configuration> {

	@Override
	public Configuration getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Configuration.MODULE_NAME);
		Document document = module.getDocument(customer, Configuration.DOCUMENT_NAME);

		Configuration configuration = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return configuration;
	}
}