package org.skyve.metadata.model.document;

import java.util.List;
import java.util.Set;

import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
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

/**
 * 
 */
public interface Document extends Model {
	/**
	 * 
	 * @param user
	 * @return
	 * @throws Exception
	 */
	public <T extends Bean> T newInstance(User user) throws Exception;
	
	public String getBizKeyExpression();
	
	/**
	 * Set the bizKey for static/dynamic beans.
	 * 
	 * @param bean
	 */
	public void setBizKey(PersistentBean bean);
	
	/**
	 * 
	 * @param name
	 * @return
	 */
	public UniqueConstraint getUniqueConstraint(String name);
	
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
	public List<UniqueConstraint> getUniqueConstraints();

	/**
	 * Get the unique constraints for this document and any super-documents for a given customer.
	 * @param customer	The given customer.
	 * @return	All the unique constraints.
	 */
	public List<UniqueConstraint> getAllUniqueConstraints(Customer customer);

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
	public Document getParentDocument(Customer customer);
	
	/**
	 * 
	 * @param uxui
	 * @param customer
	 * @param name
	 * @return
	 */
	public View getView(String uxui, Customer customer, String name);
	
	/**
	 * 
	 * @return
	 */
	public String getDocumentation();
	
	public <T extends Bean> Bizlet<T> getBizlet(Customer customer);
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
