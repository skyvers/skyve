package org.skyve.wildcat.tools.backup;

import java.io.FileOutputStream;

import javax.jcr.Session;

import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.metadata.repository.AbstractRepository;
import org.skyve.wildcat.persistence.AbstractPersistence;

public class ExportCMS {
	private static void exportCMS(String contentDirectory,
									String databaseDialect,
									String databaseJdbcDriver,
									String databaseConnectionUrl,
									String databaseUsername,
									String databasePassword) 
	throws Exception {
		try {
			for (String customerName : AbstractRepository.get().getAllCustomerNames()) {
				BackupUtil.initialize(customerName,
										databaseDialect,
										contentDirectory,
										databaseJdbcDriver,
										databaseConnectionUrl,
										databaseUsername, 
										databasePassword);

				Session session = null;
				try {
					session = ContentUtil.getFullSession(customerName);
					session.exportSystemView("/", 
												new FileOutputStream("./export_" + customerName + ".xml"), 
												false, 
												false);
				}
				finally {
					if (session != null) {
						session.logout();
					}
				}
			}
		}
		finally {
			AbstractPersistence.get().disposeAllPersistenceInstances();
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 6) {
			System.err.println("args are <content directory> <dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		exportCMS(args[0], args[1], args[2], args[3], args[4], args[5]);
	}
}
