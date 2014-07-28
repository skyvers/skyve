package org.skyve.wildcat.tools.backup;

import javax.jcr.Session;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Persistent;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.persistence.DocumentQueryImpl;
import org.skyve.wildcat.util.UtilImpl;

public class Reindex {
	private static void reindex(String customerName) throws Exception {
		AbstractPersistence persistence = AbstractPersistence.get();
		Customer customer = persistence.getUser().getCustomer();
		for (Module module : customer.getModules()) {
			for (String documentName : module.getDocumentRefs().keySet()) {
				Document document = module.getDocument(customer, documentName);
				Persistent persistent = document.getPersistent(); 
				if ((persistent != null) && (persistent.getName() != null)) { // is persistent
					try {
						persistence.begin();
						Session jcrSession = ContentUtil.getFullSession(customerName);
						try {
							// Dont check if a document has indexable fields as we
							// may need to have nodes deleted
							// (ie, a document field used to be indexed but now is not)
							UtilImpl.LOGGER.info("Reindex document " + documentName);
							DocumentQueryImpl query = new DocumentQueryImpl(document);
							for (Bean bean : persistence.iterate(query)) {
								persistence.reindex((PersistentBean) bean);
								persistence.evictCached(bean);
							}
						}
						finally {
							if (jcrSession != null) {
								jcrSession.logout();
							}
						}
					}
					finally {
						persistence.commit(false);
					}
				}
			}
		}
		persistence.commit(true);
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 7) {
			System.err.println("args are <customerName> <content directory> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		BackupUtil.initialize(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
		reindex(args[0]);
	}
}
