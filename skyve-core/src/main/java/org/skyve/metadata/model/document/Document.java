package org.skyve.metadata.model.document;

import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.metadata.PersistentMetaData;
import org.skyve.metadata.controller.BizExportAction;
import org.skyve.metadata.controller.BizImportAction;
import org.skyve.metadata.controller.DownloadAction;
import org.skyve.metadata.controller.ServerSideAction;
import org.skyve.metadata.controller.UploadAction;
import org.skyve.metadata.customer.Customer;
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
 * 
 */
public interface Document extends Model, PersistentMetaData {
	/**
	 * 
	 * @param user
	 * @return
	 * @throws Exception
	 */
	public @Nonnull <T extends Bean> T newInstance(@Nonnull User user) throws Exception;
	
	public @Nullable String getBizKeyExpression();
	
	/**
	 * Set the bizKey for static/dynamic beans.
	 * 
	 * @param bean
	 */
	public void setBizKey(@Nonnull PersistentBean bean);
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	public @Nullable UniqueConstraint getUniqueConstraint(@Nonnull String name);
	
	/**
	 * 
	 * @param customer
	 * @param name
	 * @return
	 */
	public <T extends Bean> DynamicImage<T> getDynamicImage(Customer customer, String name);

	/**
	 * Get the unique constraints for this document only - no super-documents.
	 * @return
	 */
	public @Nonnull List<UniqueConstraint> getUniqueConstraints();

	/**
	 * Get the unique constraints for this document and any super-documents for a given customer.
	 * @param customer	The given customer.
	 * @return	All the unique constraints.
	 */
	public @Nonnull List<UniqueConstraint> getAllUniqueConstraints(Customer customer);

	/**
	 * 
	 * @param referenceName
	 * @return
	 */
	public Reference getReferenceByName(String referenceName);

	/**
	 * 
	 * @param customer
	 * @param relationName
	 * @return
	 */
	public Document getRelatedDocument(Customer customer, String relationName);
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getReferencedDocumentNames();
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getReferenceNames();
	
	/**
	 * 
	 * @return
	 */
	public Set<String> getConditionNames();
	
	/**
	 * 
	 * @return
	 */
	public Condition getCondition(String conditionName);

	/**
	 * 
	 * @return
	 */
	public Set<String> getDefinedActionNames();

	/**
	 * 
	 * @param customer
	 * @return
	 */
	public Set<Document> getReferencedDocuments(Customer customer);
	
	/**
	 * 
	 * @return
	 */
	public String getParentDocumentName();
	
	/**
	 * 
	 * @return
	 */
	public boolean isOrdered();
	
	/**
	 * 
	 * @param customer
	 * @return
	 */
	public @Nullable Document getParentDocument(@Nullable Customer customer);
	
	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param name
	 * @return
	 */
	public @Nonnull View getView(@Nullable String uxui, @Nullable Customer customer, @Nonnull String name);
	
	/**
	 * 
	 * @return
	 */
	public @Nullable String getDocumentation();
	
	public @Nullable <T extends Bean> Bizlet<T> getBizlet(@Nullable Customer customer);
	public <T extends Bean, C extends Bean> ComparisonModel<T, C> getComparisonModel(Customer customer, String modelName, boolean runtime);
	public <T extends Bean> MapModel<T> getMapModel(Customer customer, String modelName, boolean runtime);
	public <T extends Bean> ChartModel<T> getChartModel(Customer customer, String modelName, boolean runtime);
	public <T extends Bean> ListModel<T> getListModel(Customer customer, String modelName, boolean runtime);

	public ServerSideAction<Bean> getServerSideAction(Customer customer, String className, boolean runtime);
	public BizExportAction getBizExportAction(Customer customer, String className, boolean runtime);
	public BizImportAction getBizImportAction(Customer customer, String className, boolean runtime);
	public DownloadAction<Bean> getDownloadAction(Customer customer, String className, boolean runtime);
	public UploadAction<Bean> getUploadAction(Customer customer, String className, boolean runtime);
}
