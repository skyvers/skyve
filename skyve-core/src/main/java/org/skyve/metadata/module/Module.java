package org.skyve.metadata.module;

import java.util.List;
import java.util.Map;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.SerializableMetaData;
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
public interface Module extends NamedMetaData, PersistentMetaData {
	/**
	 * 
	 */
	public static class DocumentRef implements SerializableMetaData {
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
		public @Nullable String getDefaultQueryName() {
			return defaultQueryName;
		}

		/**
		 * 
		 * @param defaultQueryName
		 */
		public void setDefaultQueryName(@Nullable String defaultQueryName) {
			this.defaultQueryName = defaultQueryName;
		}

		/**
		 * 
		 * @return
		 */
		public @Nonnull String getOwningModuleName() {
			return owningModuleName;
		}

		/**
		 * 
		 * @param owningModuleName
		 */
		public void setOwningModuleName(@Nonnull String owningModuleName) {
			this.owningModuleName = owningModuleName;
		}

		/**
		 * 
		 * @param documentName
		 * @return
		 */
		public @Nonnull String getModuleNameDotDocumentName(@Nonnull String documentName) {
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
		public @Nullable String getReferencedModuleName() {
			return referencedModuleName;
		}

		/**
		 * 
		 * @param referencedModuleName
		 */
		public void setReferencedModuleName(@Nullable String referencedModuleName) {
			this.referencedModuleName = referencedModuleName;
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public @Nonnull String getTitle();
	
	public default @Nonnull String getLocalisedTitle() {
		return Util.i18n(getTitle());
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean isPrototype();

	public FormLabelLayout getFormLabelLayout();
	
	/**
	 * 
	 * @return
	 */
	public @Nonnull Map<String, DocumentRef> getDocumentRefs();

	/**
	 * 
	 * @param customer
	 * @param documentName
	 * @return
	 */
	public @Nonnull MetaDataQueryDefinition getDocumentDefaultQuery(@Nullable Customer customer, @Nonnull String documentName);

	/**
	 *
	 * @param customer
	 * @param documentName
	 * @param includeAssociationBizKeys
	 * @return
	 */
	public @Nonnull MetaDataQueryDefinition getDocumentDefaultQuery(@Nullable Customer customer, @Nonnull String documentName, boolean includeAssociationBizKeys);

	/**
	 * 
	 * @param customer Can be null which means that this method returns the un-overridden document.
	 * @param documentName
	 * @return
	 */
	public @Nonnull Document getDocument(@Nullable Customer customer, @Nonnull String documentName);
	
	/**
	 * 
	 * @param jobName
	 * @return
	 */
	public @Nonnull JobMetaData getJob(@Nonnull String jobName);
	
	/**
	 * Unmodifiable list
	 * @return
	 */
	public @Nonnull List<JobMetaData> getJobs();
	
	/**
	 * 
	 * @param queryName
	 * @return
	 */
	public @Nonnull MetaDataQueryDefinition getMetaDataQuery(@Nonnull String queryName);
	
	/**
	 * 
	 * @param queryName
	 * @return
	 */
	public @Nonnull SQLDefinition getSQL(@Nonnull String queryName);

	/**
	 * 
	 * @param queryName
	 * @return
	 */
	public @Nonnull BizQLDefinition getBizQL(@Nonnull String queryName);

	/**
	 * 
	 * @return
	 */
	public @Nonnull List<QueryDefinition> getMetadataQueries();
	
	/**
	 * 
	 * @param roleName
	 * @return
	 */
	public @Nonnull Role getRole(@Nonnull String roleName);
	
	/**
	 * 
	 * @return
	 */
	public @Nonnull List<Role> getRoles();
	
	/**
	 * 
	 * @return
	 */
	public @Nullable ViewType getHomeRef();
	
	/**
	 * 
	 * @return
	 */
	public @Nullable String getHomeDocumentName();
	
	/**
	 * 
	 * @return
	 */
	public @Nonnull Menu getMenu();
	
	/**
	 * 
	 * @return
	 */
	public @Nullable String getDocumentation();
}
