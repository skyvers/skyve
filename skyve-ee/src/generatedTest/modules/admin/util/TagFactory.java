package modules.admin.util;

import modules.admin.domain.Tag;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/TagFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
public class TagFactory extends AbstractDomainFactory<Tag> {

	@Override
	public Tag getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(Tag.MODULE_NAME);
		Document document = module.getDocument(customer, Tag.DOCUMENT_NAME);

		Tag tag = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return tag;
	}
}