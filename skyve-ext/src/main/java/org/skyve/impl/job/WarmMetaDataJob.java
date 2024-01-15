package org.skyve.impl.job;

import java.util.List;
import java.util.Map.Entry;

import org.quartz.Job;
import org.quartz.JobExecutionContext;
import org.quartz.JobExecutionException;
import org.skyve.impl.metadata.customer.CustomerImpl;
import org.skyve.impl.metadata.repository.ProvidedRepositoryFactory;
import org.skyve.impl.metadata.repository.router.Router;
import org.skyve.impl.metadata.repository.router.UxUiMetadata;
import org.skyve.impl.metadata.view.ViewImpl;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.metadata.repository.ProvidedRepository;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;

/**
 * This job visits all the metadata for each customer after app deployment.
 * This job is executed only if access control is on so that the metadata is loaded and ready.
 * 
 * @author mike
 */
public class WarmMetaDataJob implements Job {
	@Override
	public void execute(JobExecutionContext context)
	throws JobExecutionException {
		try {
			Util.LOGGER.info("Warm metadata");
			ProvidedRepository repository = ProvidedRepositoryFactory.get();
			Router router = repository.getRouter();
			List<UxUiMetadata> uxuis = router.getUxUis();
			for (String customerName : repository.getAllCustomerNames()) {
				Util.LOGGER.info("Warm metadata for customer " + customerName);
				CustomerImpl customer = (CustomerImpl) repository.getCustomer(customerName);
				for (Module module : customer.getModules()) {
					for (Entry<String, DocumentRef> documentRef : module.getDocumentRefs().entrySet()) {
						if (documentRef.getValue().getReferencedModuleName() == null) { // belongs to this model
							Document document = module.getDocument(customer, documentRef.getKey());
							for (UxUiMetadata uxui : uxuis) {
								String uxuiName = uxui.getName();
								ViewImpl v = (ViewImpl) document.getView(uxuiName, customer, ViewType.create.toString());
								v.getAccesses(customer, document, uxuiName);
								v = (ViewImpl) document.getView(uxuiName, customer, ViewType.edit.toString());
								v.getAccesses(customer, document, uxuiName);
							}
						}
					}
				}
			}
			Util.LOGGER.info("Successfully warmed metadata");
		}
		catch (Exception e) {
			throw new JobExecutionException("Error encountered whilst warming metadata", e);
		}
	}
}
