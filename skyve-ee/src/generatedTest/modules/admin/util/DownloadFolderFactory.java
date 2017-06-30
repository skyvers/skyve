package modules.admin.util;

import modules.admin.domain.DownloadFolder;
import org.skyve.CORE;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.util.Util;
import util.AbstractDomainFactory;

/**
 * Generated - local changes will be overwritten.
 * Create class src/test/modules/admin/util/DownloadFolderFactoryExtension.java
 * to extend this class and customise specific values for this document.
 */
public class DownloadFolderFactory extends AbstractDomainFactory<DownloadFolder> {

	@Override
	public DownloadFolder getInstance() throws Exception {
		Customer customer = CORE.getUser().getCustomer();
		Module module = customer.getModule(DownloadFolder.MODULE_NAME);
		Document document = module.getDocument(customer, DownloadFolder.DOCUMENT_NAME);

		DownloadFolder downloadFolder = Util.constructRandomInstance(CORE.getPersistence().getUser(), module, document, 1);

		return downloadFolder;
	}
}