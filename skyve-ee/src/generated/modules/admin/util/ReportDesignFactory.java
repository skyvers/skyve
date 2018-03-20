package modules.admin.util;

import modules.admin.domain.ReportDesign;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/ReportDesignFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class ReportDesignFactory extends AbstractDomainFactory<ReportDesign > {

	@Override
	public ReportDesign getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(ReportDesign.MODULE_NAME);
		Document document = module.getDocument(customer, ReportDesign.DOCUMENT_NAME);

		ReportDesign reportDesign = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return reportDesign;
	}
}