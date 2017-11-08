package modules.admin.util;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.domain.ControlPanel;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFactory;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/ControlPanelFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
@SkyveFactory
public class ControlPanelFactory extends AbstractDomainFactory<ControlPanelExtension > {

	@Override
	public ControlPanelExtension getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(ControlPanel.MODULE_NAME);
		Document document = module.getDocument(customer, ControlPanel.DOCUMENT_NAME);

		ControlPanelExtension controlPanel = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return controlPanel;
	}
}