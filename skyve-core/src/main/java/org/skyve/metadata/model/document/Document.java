package org.skyve.metadata.model.document;

import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.DecoratedMetaData;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.ReloadableMetaData;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute.Sensitivity;
import org.skyve.metadata.model.Model;
import org.skyve.metadata.user.User;
import org.skyve.metadata.view.View;
import org.skyve.metadata.view.model.chart.ChartModel;
import org.skyve.metadata.view.model.comparison.ComparisonModel;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.map.MapModel;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * A Document represents a business domain concept in the Skyve framework.
 * It defines the structure, behavior, and persistence aspects of a business entity.
 * Documents can be extended through inheritance and may have associated views,
 * actions, bizlets, and other metadata components.
 */
public interface Document extends Model, PersistentMetaData, ReloadableMetaData, DecoratedMetaData {
	/**
	 * Creates a new instance of the document bean.
	 * 
	 * @param <T> The type of Bean to create
	 * @param user The current user context for which the instance is created
	 * @return A new instance of the document bean
	 * @throws Exception If the bean cannot be instantiated
	 */
	public @Nonnull <T extends Bean> T newInstance(@Nonnull User user) throws Exception;
	
	/**
	 * Returns the business key expression for this document.
	 * The business key is used for identifying the document instance in a human-readable form.
	 * 
	 * @return The business key expression or null if not defined
	 */
	public @Nullable String getBizKeyExpression();
	
	/**
	 * Returns the sensitivity level of the business key.
	 * This controls how the business key is displayed or exported in different contexts.
	 * 
	 * @return The sensitivity level of the business key or null if not specified
	 */
	public @Nullable Sensitivity getBizKeySensitity();

	/**
	 * Sets the business key value for the given bean based on the defined business key expression.
	 * 
	 * @param bean The bean for which to set the business key
	 */
	public void setBizKey(@Nonnull PersistentBean bean);
	
	/**
	 * Retrieves a unique constraint definition by name.
	 * 
	 * @param name The name of the unique constraint to retrieve
	 * @return The unique constraint or null if not found
	 */
	public @Nullable UniqueConstraint getUniqueConstraint(@Nonnull String name);
	
	/**
	 * Retrieves a dynamic image defined for this document.
	 * 
	 * @param <T> The bean type
	 * @param customer The customer context
	 * @param name The name of the dynamic image
	 * @return The dynamic image definition or null if not found
	 */
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name);

	/**
	 * Gets all unique constraints defined directly on this document (not including those from parent documents).
	 * 
	 * @return A list of unique constraints for this document
	 */
	public @Nonnull List<UniqueConstraint> getUniqueConstraints();

	/**
	 * Retrieves a reference by its name.
	 * 
	 * @param referenceName The name of the reference to retrieve
	 * @return The reference or null if not found
	 */
	public Reference getReferenceByName(String referenceName);

	/**
	 * Retrieves the document that is related through the specified relation.
	 * 
	 * @param customer The customer context
	 * @param relationName The name of the relation
	 * @return The related document or null if not found
	 */
	public Document getRelatedDocument(Customer customer, String relationName);
	
	/**
	 * Gets all reference names defined in this document.
	 * 
	 * @return A set of reference names
	 */
	public Set<String> getReferenceNames();
	
	/**
	 * Gets all condition names defined in this document.
	 * 
	 * @return A set of condition names
	 */
	public Set<String> getConditionNames();
	
	/**
	 * Retrieves a condition by its name.
	 * 
	 * @param conditionName The name of the condition to retrieve
	 * @return The condition or null if not found
	 */
	public Condition getCondition(String conditionName);

	/**
	 * Gets all defined action names for this document.
	 * 
	 * @return A set of action names
	 */
	public Set<String> getDefinedActionNames();

	/**
	 * Gets the name of the parent document if this document extends another.
	 * 
	 * @return The parent document name or null if this is a base document
	 */
	public String getParentDocumentName();
	
	/**
	 * Indicates whether this document has ordered persistence.
	 * 
	 * @return true if this document is ordered, false otherwise
	 */
	public boolean isOrdered();
	
	/**
	 * Retrieves the parent document for this document.
	 * 
	 * @param customer The customer context
	 * @return The parent document or null if this is a base document
	 */
	public @Nullable Document getParentDocument(@Nullable Customer customer);
	
	/**
	 * Retrieves a specific view of this document.
	 * 
	 * @param uxui The UX/UI identifier to get the appropriate view variant
	 * @param customer The customer context
	 * @param name The name of the view to retrieve
	 * @return The requested view
	 */
	public @Nonnull View getView(@Nullable String uxui, @Nullable Customer customer, @Nonnull String name);
	
	/**
	 * Gets the documentation text for this document.
	 * 
	 * @return The documentation text or null if none is defined
	 */
	public @Nullable String getDocumentation();
	
	/**
	 * Retrieves the bizlet associated with this document.
	 * 
	 * @param <T> The bean type
	 * @param customer The customer context
	 * @return The bizlet or null if none is defined
	 */
	public @Nullable <T extends Bean> Bizlet<T> getBizlet(@Nullable Customer customer);
	
	/**
	 * Retrieves a comparison model for this document.
	 * 
	 * @param <T> The bean type
	 * @param <C> The comparison bean type
	 * @param customer The customer context
	 * @param modelName The name of the model to retrieve
	 * @param runtime Whether to get the runtime version
	 * @return The comparison model
	 */
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, String modelName, boolean runtime);
	
	/**
	 * Retrieves a map model for this document.
	 * 
	 * @param <T> The bean type
	 * @param customer The customer context
	 * @param modelName The name of the model to retrieve
	 * @param runtime Whether to get the runtime version
	 * @return The map model
	 */
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, String modelName, boolean runtime);
	
	/**
	 * Retrieves a chart model for this document.
	 * 
	 * @param <T> The bean type
	 * @param customer The customer context
	 * @param modelName The name of the model to retrieve
	 * @param runtime Whether to get the runtime version
	 * @return The chart model
	 */
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, String modelName, boolean runtime);
	
	/**
	 * Retrieves a list model for this document.
	 * 
	 * @param <T> The bean type
	 * @param customer The customer context
	 * @param modelName The name of the model to retrieve
	 * @param runtime Whether to get the runtime version
	 * @return The list model
	 */
	public <T extends Bean> ListModel<T> getListModel(Customer customer, String modelName, boolean runtime);

	/**
	 * Retrieves a server-side action for this document.
	 * 
	 * @param customer The customer context
	 * @param className The class name of the action
	 * @param runtime Whether to get the runtime version
	 * @return The server-side action
	 */
	public ServerSideAction<Bean> getServerSideAction(Customer customer, String className, boolean runtime);
	
	/**
	 * Retrieves a bizExport action for this document.
	 * 
	 * @param customer The customer context
	 * @param className The class name of the action
	 * @param runtime Whether to get the runtime version
	 * @return The business export action
	 */
	public BizExportAction getBizExportAction(Customer customer, String className, boolean runtime);
	
	/**
	 * Retrieves a bizImport action for this document.
	 * 
	 * @param customer The customer context
	 * @param className The class name of the action
	 * @param runtime Whether to get the runtime version
	 * @return The business import action
	 */
	public BizImportAction getBizImportAction(Customer customer, String className, boolean runtime);
	
	/**
	 * Retrieves a download action for this document.
	 * 
	 * @param <Bean> The bean type
	 * @param customer The customer context
	 * @param className The class name of the action
	 * @param runtime Whether to get the runtime version
	 * @return The download action
	 */
	public DownloadAction<Bean> getDownloadAction(Customer customer, String className, boolean runtime);
	
	/**
	 * Retrieves an upload action for this document.
	 * 
	 * @param <Bean> The bean type
	 * @param customer The customer context
	 * @param className The class name of the action
	 * @param runtime Whether to get the runtime version
	 * @return The upload action
	 */
	public UploadAction<Bean> getUploadAction(Customer customer, String className, boolean runtime);
}
