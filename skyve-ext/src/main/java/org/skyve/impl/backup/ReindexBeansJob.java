package org.skyve.impl.backup;

import java.util.List;
import java.util.Map;

import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.job.CancellableJob;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.persistence.DocumentQuery;

public class ReindexBeansJob extends CancellableJob {
	private static final long serialVersionUID = 3902304459915888093L;

	@Override
	public void execute() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		List<String> log = getLog();
		String trace;
		

		// truncate the bean content ready to reindex
		try (ContentManager cm = EXT.newContentManager()) {
			trace = "Truncate Beans";
			log.add(trace);
			UtilImpl.LOGGER.info(trace);
			cm.truncateBeans(customer.getName());
		}
		
		// reindex
		List<Module> modules = customer.getModules();
		float i = 0, l = modules.size();
		for (Module module : modules) {
			i++;
			String moduleName = module.getName();

			Map<String, DocumentRef> refs = module.getDocumentRefs();
			for (String documentName : refs.keySet()) {
				DocumentRef ref = refs.get(documentName);
				// is the document defined in this module?
				if (moduleName.equals(ref.getOwningModuleName())) {
					Document document = module.getDocument(customer, documentName);
					Persistent persistent = document.getPersistent(); 
					if ((persistent != null) && (persistent.getName() != null)) { // is persistent
						try {
							persistence.begin();
							try (ContentManager cm = EXT.newContentManager()) {
								// Don't check if a document has indexable fields as we
								// may need to have nodes deleted
								// (i.e. a document field used to be indexed but now is not)
								trace = String.format("Reindex document %s.%s", module.getName(), documentName);
								log.add(trace);
								UtilImpl.LOGGER.info(trace);
								DocumentQuery query = persistence.newDocumentQuery(document);
								try (AutoClosingIterable<PersistentBean> it = query.beanIterable()) {
									for (PersistentBean bean : it) {
										persistence.reindex(bean);
										persistence.evictCached(bean);
									}
								}
							}
						}
						finally {
							persistence.commit(false);
						}
					}
				}
			}
			setPercentComplete((int) (i / l * 100f));
		}
		setPercentComplete(100);
	}
}
