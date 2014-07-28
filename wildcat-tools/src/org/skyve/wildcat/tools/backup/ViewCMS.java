package org.skyve.wildcat.tools.backup;

import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.RepositoryException;
import javax.jcr.Session;

import org.skyve.wildcat.content.ContentUtil;
import org.skyve.wildcat.util.UtilImpl;

public class ViewCMS {
	private static void viewCMS(String customerName) throws Exception {
		Session session = null;
		try {
			session = ContentUtil.getFullSession(customerName);
			viewNode(session.getRootNode());
		}
		finally {
			if (session != null) {
				session.logout();
			}
		}
	}

	private static void viewNode(Node node) throws RepositoryException {
		UtilImpl.LOGGER.info(node.getPath() + " : " + node.getPrimaryNodeType().getName());
		NodeIterator i = node.getNodes();
		while (i.hasNext()) {
			viewNode(i.nextNode());
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 7) {
			System.err.println("args are <customerName> <content directory> <DB dialect> <DB driver> <DB URL> <DB username> <DB password>");
			System.exit(1);
		}
		BackupUtil.initialize(args[0], args[1], args[2], args[3], args[4], args[5], args[6]);
		viewCMS(args[1]);
	}
}
