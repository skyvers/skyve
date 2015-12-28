package org.skyve.wildcat.backup;

import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.persistence.DocumentQuery;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;

public class Reindex {
	private Reindex() {
		// nothing to see here
	}
	
	public static void reindex() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		for (Module module : customer.getModules()) {
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				Persistent persistent = document.getPersistent(); 
				if ((persistent != null) && (persistent.getName() != null)) { // is persistent
					try {
						persistence.begin();
						try (ContentManager cm = EXT.newContentManager()) {
							// Dont check if a document has indexable fields as we
							// may need to have nodes deleted
							// (ie, a document field used to be indexed but now is not)
							UtilImpl.LOGGER.info("Reindex document " + module.getName() + '.' + documentName);
							DocumentQuery query = persistence.newDocumentQuery(document);
							for (Bean bean : query.beanIterable()) {
								persistence.reindex((PersistentBean) bean);
								persistence.evictCached(bean);
							}
						}
					}
					finally {
						persistence.commit(false);
					}
				}
			}
		}
	}
	
	public static void main(String[] args) throws Exception {
		if (args.length != 8) {
			System.err.println("args are <customerName> <content directory> <content file storage?> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		BackupUtil.initialise(args[0], args[1], args[2], args[3], args[4], args[5], args[6], args[7]);
		try {
			reindex();
		}
		finally {
			BackupUtil.finalise();
			
			// This is required to stop the process hanging at the end
			System.exit(0);
		}
	}
}
