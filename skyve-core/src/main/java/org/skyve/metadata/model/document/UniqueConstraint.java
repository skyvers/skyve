package org.skyve.metadata.model.document;

import java.util.List;

import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.user.DocumentPermissionScope;
import org.skyve.util.Util;

import jakarta.xml.bind.annotation.XmlType;

/**
 * This class is used for collection unique constraints and for document unique constraints.
 * Document Unique Constraints are converted from repository.documentUniqueConstraint
 */
public interface UniqueConstraint extends NamedMetaData {
	/**
	 * 
	 */
	@XmlType(namespace = XMLMetaData.DOCUMENT_NAMESPACE)
	public static enum DocumentScope {
		/**
		 * 
		 */
		global(DocumentPermissionScope.global),
		
		/**
		 * 
		 */
		customer(DocumentPermissionScope.customer),
		
		/**
		 * 
		 */
		dataGroup(DocumentPermissionScope.dataGroup),
		
		/**
		 * 
		 */
		user(DocumentPermissionScope.user);

		private DocumentPermissionScope scope;
		
		/**
		 * 
		 * @param scope
		 */
		private DocumentScope(DocumentPermissionScope scope) {
			this.scope = scope;
		}

		/**
		 * 
		 * @return
		 */
		public DocumentPermissionScope toDocumentPermissionScope() {
			return scope;
		}
	}

	/**
	 * 
	 * @return
	 */
	public DocumentScope getScope();
	
	/**
	 * 
	 * @return
	 */
	public String getDescription();
	
	public default String getLocalisedDescription() {
		return Util.i18n(getDescription());
	}
	
	/**
	 * 
	 * @return Returns the message.
	 */
	public String getMessage();
	
	/**
	 * 
	 * @return
	 */
	public List<String> getFieldNames();
}
