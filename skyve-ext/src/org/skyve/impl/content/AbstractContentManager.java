package org.skyve.impl.content;

import org.skyve.content.ContentManager;
import org.skyve.domain.messages.DomainException;
import org.skyve.impl.metadata.user.SuperUser;
import org.skyve.impl.persistence.AbstractPersistence;
import org.skyve.impl.util.UtilImpl;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.user.User;
import org.skyve.impl.content.AbstractContentManager;

public abstract class AbstractContentManager implements ContentManager {
	public static Class<? extends AbstractContentManager> IMPLEMENTATION_CLASS;
	
	public static AbstractContentManager get() {
		try {
			AbstractContentManager result = IMPLEMENTATION_CLASS.newInstance();
			return result;
		}
		catch (Exception e) {
			throw new IllegalArgumentException(IMPLEMENTATION_CLASS + " was not a good choice.", e);
		}
	}
	
	public abstract void init() throws Exception;
	public abstract void dispose() throws Exception;
	
	/**
	 * Append a balanced folder structure for storing a content file based on it's content ID.
	 * 
	 * @param id The content ID
	 * @param pathToAppendTo	The path to append to.
	 */
	public static void appendBalancedFolderPathFromContentId(String id, StringBuilder pathToAppendTo) {
		pathToAppendTo.append(id.substring(5, 7)).append('/');
		pathToAppendTo.append(id.substring(10, 12)).append('/');
		pathToAppendTo.append(id.substring(15, 17)).append('/');
	}
	
	/**
	 * Indicates if the current user can read the given content or not
	 * 
	 * @param bizCustomer
	 * @param bizModule
	 * @param bizDocument
	 * @param bizDataGroupId
	 * @param bizUserId
	 * @param bizId
	 * @return true if the content can be read, otherwsie false.
	 */
	public static boolean canReadContent(String bizCustomer,
											String bizModule,
											String bizDocument,
											String bizDataGroupId,
											String bizUserId,
											String bizId) {
		AbstractPersistence persistence = AbstractPersistence.get();
		User user = persistence.getUser();
		if (user instanceof SuperUser) {
			return true;
		}

		try {
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

		return true;
	}
}
