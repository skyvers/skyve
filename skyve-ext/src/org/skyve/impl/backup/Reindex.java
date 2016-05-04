package org.skyve.impl.backup;

import java.util.Map;

import org.skyve.EXT;
import org.skyve.content.ContentManager;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.Module.DocumentRef;
import org.skyve.persistence.DocumentQuery;
import org.skyve.impl.backup.BackupUtil;

public class Reindex {
	private Reindex() {
		// nothing to see here
	}
	
	public static void reindex() throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		for (Module module : customer.getModules()) {
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
