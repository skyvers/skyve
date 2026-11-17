package org.skyve.metadata.module;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.skyve.impl.metadata.view.container.form.FormLabelLayout;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.MetaDataException;
import org.skyve.metadata.NamedMetaData;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.ReloadableMetaData;
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

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * The metadata contract for a Skyve module.
 *
 * <p>A module is the primary organisational unit in a Skyve application — a logical
 * grouping of {@link Document documents}, named queries, security roles, background jobs,
 * and a navigation menu. Modules are declared in {@code {moduleName}/{moduleName}.xml}
 * and loaded by the repository at startup.
 *
 * <p>Module documents may be defined locally or imported from another module via a
 * {@code <document>} ref with a {@code moduleRef} attribute. The
 * {@link DocumentRef} inner class records this cross-module relationship.
 *
 * <p>Threading: {@code Module} implementations are long-lived singletons read concurrently.
 * Implementations must be thread-safe after initialisation.
 *
 * @see org.skyve.metadata.customer.Customer#getModule(String)
 * @see org.skyve.metadata.customer.Customer#getModules()
 */
public interface Module extends NamedMetaData, PersistentMetaData, ReloadableMetaData, DecoratedMetaData {

	/**
	 * Reference to a document within a module. 
	 * This class allows referencing documents defined in other modules.
	 */
	public static class DocumentRef implements DecoratedMetaData {
		private static final long serialVersionUID = -7522103396710688388L;

		// The module name where the document is defined
		// (depends on moduleRef and relatedTo attributes).
		private String owningModuleName;

		// The value of moduleRef attribute in the XML.
		private String referencedModuleName;

		private String defaultQueryName;
		
		private Map<String, String> properties = new LinkedHashMap<>();

		/**
		 * Returns the default query name for this document reference.
		 * 
		 * @return The default query name or null if not specified
		 */
		public @Nullable String getDefaultQueryName() {
			return defaultQueryName;
		}

		/**
		 * Sets the default query name for this document reference.
		 * 
		 * @param defaultQueryName The default query name to set
		 */
		public void setDefaultQueryName(@Nullable String defaultQueryName) {
			this.defaultQueryName = defaultQueryName;
		}

		/**
		 * Returns the name of the module that owns this document.
		 * 
		 * @return The owning module name
		 */
		public @Nonnull String getOwningModuleName() {
			return owningModuleName;
		}

		/**
		 * Sets the name of the module that owns this document.
		 * 
		 * @param owningModuleName The module name to set as owner
		 */
		public void setOwningModuleName(@Nonnull String owningModuleName) {
			this.owningModuleName = owningModuleName;
		}

		/**
		 * Returns the fully qualified document name (module.document).
		 * Uses the referenced module name if specified, otherwise uses the owning module name.
		 * 
		 * @param documentName The name of the document
		 * @return The fully qualified module.document name
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
		 * Returns the referenced module name if this document references a document in another module.
		 * 
		 * @return The referenced module name or null if the document is defined in the owning module
		 */
		public @Nullable String getReferencedModuleName() {
			return referencedModuleName;
		}

		/**
		 * Sets the referenced module name.
		 * 
		 * @param referencedModuleName The module name containing the referenced document
		 */
		public void setReferencedModuleName(@Nullable String referencedModuleName) {
			this.referencedModuleName = referencedModuleName;
		}
		
		/**
		 * Returns the properties.
		 * @return the result
		 */
		@Override
		public @Nonnull Map<String, String> getProperties() {
			return properties;
		}
	}
	
	/**
	 * Returns the name of this module, which module conversion requires.
	 *
	 * @return The module name
	 */
	@Override
	@Nonnull String getName();

	/**
	 * Returns the title of this module.
	 * 
	 * @return The module title
	 */
	@Nonnull String getTitle();
	
	/**
	 * Returns the localised title of this module.
	 * Uses the resource bundle to get the localised version if available.
	 * 
	 * @return The localised module title
	 */
	default @Nonnull String getLocalisedTitle() {
		return Util.nullSafeI18n(getTitle());
	}
	
	/**
	 * <p>
	 * Indicates whether this module is a prototype.
	 * </p>
	 * 
	 * <p>
	 * To assist in rapid development and prototyping, Skyve provides a prototype option for modules
	 * which changes a number of default Skyve behaviours. Developers should note that the prototype
	 * option is designed to provide indicative results that may not necessarily provide optimal
	 * performance.
	 * </p>
	 * 
	 * <p>
	 * The two key differences prototype mode introduces are:
	 * </p>
	 * 
	 * <ul>
	 * <li>inclusion of association/reference columns in default queries (i.e. where no defaultQueryName
	 * is specified, Skyve will include columns for associations, displaying the bizKey value for the
	 * associated bean)</li>
	 * <li>inclusion of content items in default queries as thumbnails</li>
	 * </ul>
	 * 
	 * @return true if this module is a prototype, false otherwise
	 */
	boolean isPrototype();

	/**
	 * Returns the form label layout configuration for this module.
	 * 
	 * @return The form label layout for this module
	 */
	@Nullable FormLabelLayout getFormLabelLayout();
	
	/**
	 * Returns a map of document references belonging to this module.
	 * The map is keyed by document name.
	 * 
	 * @return A map of document references
	 */
	@Nonnull Map<String, DocumentRef> getDocumentRefs();

	/**
	 * Returns the default query for a specified document.
	 * 
	 * @param customer The customer context, may be null for system context
	 * @param documentName The name of the document
	 * @return The default metadata query definition for the document
	 */
	@Nonnull MetaDataQueryDefinition getDocumentDefaultQuery(@Nullable Customer customer, @Nonnull String documentName);

	/**
	 * Returns the default query for a specified document with option to include association bizKeys.
	 *
	 * @param customer The customer context, may be null for system context
	 * @param documentName The name of the document
	 * @param includeAssociationBizKeys Whether to include business keys for associations
	 * @return The default metadata query definition for the document
	 */
	@Nonnull MetaDataQueryDefinition getDocumentDefaultQuery(@Nullable Customer customer, @Nonnull String documentName, boolean includeAssociationBizKeys);

	/**
	 * Returns the document with the specified name.
	 * 
	 * @param customer The customer context, may be null to get the un-overridden document
	 * @param documentName The name of the document to retrieve
	 * @return The document; never {@code null}
	 * @throws MetaDataException if the module does not declare or reference a document of that name
	 */
	@Nonnull Document getDocument(@Nullable Customer customer, @Nonnull String documentName);
	
	/**
	 * Returns the job metadata with the specified name.
	 * 
	 * @param jobName The name of the job to retrieve
	 * @return The job metadata; never {@code null}
	 * @throws MetaDataException if the module has no metadata of that name, or the metadata of that
	 * 			name is not a job
	 */
	@Nonnull JobMetaData getJob(@Nonnull String jobName);
	
	/**
	 * Returns an unmodifiable list of all jobs defined in this module.
	 * 
	 * @return An unmodifiable list of job metadata
	 */
	@Nonnull List<JobMetaData> getJobs();
	
	/**
	 * Returns the metadata query with the specified name.
	 * 
	 * @param queryName The name of the query to retrieve
	 * @return The metadata query definition, or {@code null} if the module has no metadata of that
	 * 			name, or the metadata of that name is not a metadata query
	 */
	@Nullable MetaDataQueryDefinition getMetaDataQuery(@Nonnull String queryName);
	
	default @Nonnull MetaDataQueryDefinition getNullSafeMetaDataQuery(@Nonnull String queryName) {
		MetaDataQueryDefinition result = getMetaDataQuery(queryName);
		if (result == null) {
			throw new MetaDataException("Query " + queryName + " does not exist in module " + getName());
		}
		return result;
	}

	/**
	 * Returns the SQL query with the specified name.
	 * 
	 * @param queryName The name of the SQL query to retrieve
	 * @return The SQL query definition, or {@code null} if the module has no metadata of that name,
	 * 			or the metadata of that name is not a SQL query
	 */
	@Nullable SQLDefinition getSQL(@Nonnull String queryName);

	/**
	 * Returns the SQL query with the specified name, throwing if it does not exist.
	 *
	 * @param queryName the SQL query name; must not be {@code null}
	 * @return the SQL query definition; never {@code null}
	 * @throws MetaDataException if this module has no SQL query with that name
	 */
	default @Nonnull SQLDefinition getNullSafeSQL(@Nonnull String queryName) {
		SQLDefinition result = getSQL(queryName);
		if (result == null) {
			throw new MetaDataException("SQL query " + queryName + " does not exist in module " + getName());
		}
		return result;
	}

	/**
	 * Returns the BizQL query with the specified name.
	 * 
	 * @param queryName The name of the BizQL query to retrieve
	 * @return The BizQL query definition, or {@code null} if the module has no metadata of that name,
	 * 			or the metadata of that name is not a BizQL query
	 */
	@Nullable BizQLDefinition getBizQL(@Nonnull String queryName);

	/**
	 * Returns the BizQL query with the specified name, throwing if it does not exist.
	 *
	 * @param queryName the BizQL query name; must not be {@code null}
	 * @return the BizQL query definition; never {@code null}
	 * @throws MetaDataException if this module has no BizQL query with that name
	 */
	default @Nonnull BizQLDefinition getNullSafeBizQL(@Nonnull String queryName) {
		BizQLDefinition result = getBizQL(queryName);
		if (result == null) {
			throw new MetaDataException("BizQL query " + queryName + " does not exist in module " + getName());
		}
		return result;
	}

	/**
	 * Returns a list of all metadata queries defined in this module.
	 * 
	 * @return A list of query definitions
	 */
	@Nonnull List<QueryDefinition> getMetadataQueries();
	
	/**
	 * Returns the role with the specified name.
	 * 
	 * @param roleName The name of the role to retrieve
	 * @return The role, or {@code null} if the module has no metadata of that name, or the metadata
	 * 			of that name is not a role
	 */
	@Nullable Role getRole(@Nonnull String roleName);

	/**
	 * Returns the named role, throwing if it does not exist.
	 *
	 * @param roleName  the role name; must not be {@code null}
	 * @return the role; never {@code null}
	 * @throws org.skyve.metadata.MetaDataException if no role with that name exists in this module
	 */
	default @Nonnull Role getNullSafeRole(@Nonnull String roleName) {
		Role result = getRole(roleName);
		if (result == null) {
			throw new MetaDataException("Role " + roleName + " does not exist in module " + getName());
		}
		return result;
	}
	
	/**
	 * Returns a list of all roles defined in this module.
	 * 
	 * @return A list of roles
	 */
	@Nonnull List<Role> getRoles();
	
	/**
	 * Returns the home view type reference for this module.
	 * This indicates what view should be displayed when the module is accessed.
	 * 
	 * @return The home view type or null if not defined
	 */
	@Nullable ViewType getHomeRef();
	
	/**
	 * Returns the home document name for this module.
	 * 
	 * @return The home document name
	 */
	@Nonnull String getHomeDocumentName();
	
	/**
	 * Returns the menu definition for this module.
	 * 
	 * @return The module's menu definition
	 */
	@Nonnull Menu getMenu();
	
	/**
	 * Returns the documentation text for this module.
	 * 
	 * @return The module documentation or null if not defined
	 */
	@Nullable String getDocumentation();
}
