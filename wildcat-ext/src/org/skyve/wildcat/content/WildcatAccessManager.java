package org.skyve.wildcat.content;

import javax.jcr.AccessDeniedException;
import javax.jcr.ItemNotFoundException;
import javax.jcr.NoSuchWorkspaceException;
import javax.jcr.RepositoryException;
import javax.security.auth.Subject;

import org.apache.jackrabbit.core.HierarchyManager;
import org.apache.jackrabbit.core.ItemId;
import org.apache.jackrabbit.core.security.AMContext;
import org.apache.jackrabbit.core.security.AccessManager;
import org.apache.jackrabbit.spi.Path;
import org.apache.jackrabbit.spi.Path.Element;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.user.User;
import org.skyve.wildcat.metadata.user.SuperUser;
import org.skyve.wildcat.persistence.AbstractPersistence;
import org.skyve.wildcat.util.UtilImpl;

public class WildcatAccessManager implements AccessManager {
	/**
	 * Subject whose access rights this AccessManager should reflect
	 */
	protected Subject subject;

	/**
	 * Subject's first Principal name
	 */
	private String contentUserName;

	/**
	 * hierarchy manager used for ACL-based access control model
	 */
	protected HierarchyManager hierMgr;

	private boolean initialized = false;

	@Override
	public void init(AMContext context) 
	throws AccessDeniedException, Exception {
		if (initialized) {
			throw new IllegalStateException("already initialized");
		}
		subject = context.getSubject();
		contentUserName = subject.getPrincipals().iterator().next().getName();
		hierMgr = context.getHierarchyManager();
		initialized = true;
	}

	@Override
	public void close() 
	throws Exception {
		if (! initialized) {
			throw new IllegalStateException("not initialized");
		}

		subject = null;
		contentUserName = null;
		hierMgr = null;
		initialized = false;
	}

	@Override
	public boolean canAccess(String workspaceName) 
	throws NoSuchWorkspaceException, RepositoryException {
		if (! initialized) {
			throw new IllegalStateException("not initialized");
		}

		User user = AbstractPersistence.get().getUser();
		Customer customer = null;
		try {
			customer = user.getCustomer();
		}
		catch (MetaDataException e) {
			return false;
		}

		return (workspaceName.equals(customer.getName()));
	}

	@Override
	public void checkPermission(ItemId id, int permissions) 
	throws AccessDeniedException, ItemNotFoundException, RepositoryException {
		if (! isGranted(id, permissions)) {
			throw new AccessDeniedException();
		}
	}

	@Override
	public boolean isGranted(ItemId itemId, int permissions) 
	throws ItemNotFoundException, RepositoryException {
		if (! initialized) {
			throw new IllegalStateException("not initialized");
		}

		// if full user then user can do anything
		if (ContentUtil.FULL_USER.equals(contentUserName)) {
			return true;
		}

		// not full user, but trying to write or remove - no go
		if (((permissions & WRITE) == WRITE) || ((permissions & REMOVE) == REMOVE)) {
			if (UtilImpl.SECURITY_TRACE) {
				UtilImpl.LOGGER.info("WildcatAccessManager - Tried to write to or remove from repository as the query user");
			}
			return false;
		}

		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		if (user instanceof SuperUser) {
			return true;
		}

		// still trying to get full path - this is an interim node, so allow
		Path path = hierMgr.getPath(itemId);
		Element[] elements = path.getElements();
		int elementsLength = elements.length;
		if (elementsLength < 7) // elements[0] is the root element
		{
			return true;
		}

		String bizCustomer = elements[1].getName().getLocalName();
		String bizModule = elements[2].getName().getLocalName();
		String bizDocument = elements[3].getName().getLocalName();
		String bizDataGroupId = elements[4].getName().getLocalName();
		if ("null".equals(bizDataGroupId)) {
			bizDataGroupId = null;
		}
		String bizUserId = elements[5].getName().getLocalName();
		String bizId = elements[6].getName().getLocalName();

		try {
			Customer customer = user.getCustomer();
			// customer is hitting wrong workspace - WTF!!!
			if (! customer.getName().equals(bizCustomer)) {
				if (UtilImpl.SECURITY_TRACE) UtilImpl.LOGGER.info("WildcatAccessManager - denied - user belongs to customer" + 
																customer.getName() + ", not customer " + bizCustomer);
				return false;
			}

			if (! user.canReadBean(bizId, bizModule, bizDocument, bizCustomer, bizDataGroupId, bizUserId)) {
				return false;
			}
		}
		catch (MetaDataException e) {
			// This can happen when a document was indexed but then the customer access was taken away
			if (UtilImpl.SECURITY_TRACE) System.err.println("Could not get the document for " + bizModule + '.' + bizDocument);
			return false;
		}
		catch (DomainException e) {
			// This happens when the data was deleted but the CMS was not kept in sync
			if (UtilImpl.SECURITY_TRACE) System.err.println("Could not retrieve bean " + bizModule + '.' + bizDocument + " with ID " + bizId);
			return false;
		}
		/*
		 * catch (ItemNotFoundException e) { // This happens when the data was deleted int the CMS but was not kept in sync with
		 * bizhub DB if (Util.SECURITY_TRACE) System.err.println("Could not retrieve content for bean " + bizModule + '.' +
		 * bizDocument + " with ID " + bizId); return false; } catch (RepositoryException e) { if (Util.SECURITY_TRACE)
		 * System.err.println("Could not retrieve from CMS for bean " + bizModule + '.' + bizDocument + " with ID " + bizId + " : "
		 * + e.getMessage()); return false; } catch (IOException e) { if (Util.SECURITY_TRACE)
		 * System.err.println("Could not retrieve from CMS for bean " + bizModule + '.' + bizDocument + " with ID " + bizId + " : "
		 * + e.getMessage()); return false; }
		 */
		return true;
	}
}
