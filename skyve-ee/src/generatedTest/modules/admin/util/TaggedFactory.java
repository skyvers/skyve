package modules.admin.util;

import modules.admin.domain.Tagged;
import modules.admin.util.TagFactory;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/TaggedFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class TaggedFactory extends AbstractDomainFactory<Tagged> {

	@Override
	public Tagged getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Tagged.MODULE_NAME);
		Document document = module.getDocument(customer, Tagged.DOCUMENT_NAME);

		Tagged tagged = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);
		tagged.setTag(new TagFactory().getInstance());

		return tagged;
	}
}