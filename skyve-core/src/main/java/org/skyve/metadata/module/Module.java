package org.skyve.metadata.module;

import java.util.List;
import java.util.Map;

import org.skyve.metadata.MetaData;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.menu.Menu;
import org.skyve.metadata.module.query.BizQLDefinition;
import org.skyve.metadata.module.query.MetaDataQueryDefinition;
import org.skyve.metadata.module.query.QueryDefinition;
import org.skyve.metadata.module.query.SQLDefinition;
import org.skyve.metadata.user.Role;
import org.skyve.metadata.view.View.ViewType;
import org.skyve.util.Util;

/**
 * 
 */
public interface Module extends NamedMetaData {
	/**
	 * 
	 */
	public static class DocumentRef implements MetaData {
		private static final long serialVersionUID = -7522103396710688388L;

		// The module name where the document is defined
		// (depends on moduleRef and relatedTo attributes).
		private String owningModuleName;

		// The value of moduleRef attribute in the XML.
		private String referencedModuleName;

		private String defaultQueryName;

		/**
		 * 
		 * @return
		 */
		public String getDefaultQueryName() {
			return defaultQueryName;
		}

		/**
		 * 
		 * @param defaultQueryName
		 */
		public void setDefaultQueryName(String defaultQueryName) {
			this.defaultQueryName = defaultQueryName;
		}

		/**
		 * 
		 * @return
		 */
		public String getOwningModuleName() {
			return owningModuleName;
		}

		/**
		 * 
		 * @param owningModuleName
		 */
		public void setOwningModuleName(String owningModuleName) {
			this.owningModuleName = owningModuleName;
		}

		/**
		 * 
		 * @param documentName
		 * @return
		 */
		public String getModuleNameDotDocumentName(String documentName) {
			String result = null;

			if (referencedModuleName != null) {
				result = new StringBuilder(32).append(referencedModuleName).append('.').append(documentName).toString();
			}
			else {
				result = new StringBuilder(32).append(owningModuleName).append('.').append(documentName).toString();
			}

			return result;
		}

		/**
		 * 
		 * @return
		 */
		public String getReferencedModuleName() {
			return referencedModuleName;
		}

		/**
		 * 
		 * @param referencedModuleName
		 */
		public void setReferencedModuleName(String referencedModuleName) {
			this.referencedModuleName = referencedModuleName;
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public String getTitle();
	
	public default String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isPrototype();

	/**
	 * 
	 * @return
	 */
	public Map<String, DocumentRef> getDocumentRefs();

	/**
	 * 
	 * @param customer
	 * @param documentName
	 * @return
	 */
	public MetaDataQueryDefinition getDocumentDefaultQuery(Customer customer, String documentName);

	/**
	 *
	 * @param customer
	 * @param documentName
	 * @param includeAssociationBizKeys
	 * @return
	 */
	public MetaDataQueryDefinition getDocumentDefaultQuery(Customer customer, String documentName, boolean includeAssociationBizKeys);

	/**
	 * 
	 * @param customer Can be null which means that this method returns the un-overridden document.
	 * @param documentName
	 * @return
	 */
	public Document getDocument(Customer customer, String documentName);
	
	/**
	 * 
	 * @param jobName
	 * @return
	 */
	public JobMetaData getJob(String jobName);
	
	/**
	 * Unmodifiable list
	 * @return
	 */
	public List<JobMetaData> getJobs();
	
	/**
	 * 
	 * @param queryName
	 * @return
	 */
	public MetaDataQueryDefinition getMetaDataQuery(String queryName);
	
	/**
	 * 
	 * @param queryName
	 * @return
	 */
	public SQLDefinition getSQL(String queryName);

	/**
	 * 
	 * @param queryName
	 * @return
	 */
	public BizQLDefinition getBizQL(String queryName);

	/**
	 * 
	 * @return
	 */
	public List<QueryDefinition> getMetadataQueries();
	
	/**
	 * 
	 * @param roleName
	 * @return
	 */
	public Role getRole(String roleName);
	
	/**
	 * 
	 * @return
	 */
	public List<Role> getRoles();
	
	/**
	 * 
	 * @return
	 */
	public ViewType getHomeRef();
	
	/**
	 * 
	 * @return
	 */
	public String getHomeDocumentName();
	
	/**
	 * 
	 * @return
	 */
	public Menu getMenu();
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
}
