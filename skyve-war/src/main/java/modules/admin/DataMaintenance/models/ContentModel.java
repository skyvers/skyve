package modules.admin.DataMaintenance.models;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.content.ContentIterable.ContentIterator;
import org.skyve.content.ContentManager;
import org.skyve.content.SearchResult;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.messages.DomainException;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.content.AbstractContentManager;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.MetaDataQueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.slf4j.Logger;

import modules.admin.domain.Content;
import modules.admin.domain.DataMaintenance;

/**
 * Lists content repository records and projections for the Data Maintenance content grid.
 */
public class ContentModel extends ListModel<DataMaintenance> {
	private static final Logger LOGGER = SkyveLoggerFactory.getLogger(ContentModel.class);
	private static final String NOT_IMPLEMENTED = "NOT IMPLEMENTED";

	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<MetaDataQueryColumn> columns = new ArrayList<>(1);

	/**
	 * Performs the postConstruct operation.
	 * @param customer the customer value
	 * @param runtime the runtime value
	 */
	@Override
	public void postConstruct(Customer customer, boolean runtime) {
		drivingDocument = customer.getModule(Content.MODULE_NAME).getDocument(customer, Content.DOCUMENT_NAME);

		projections.add(Bean.DOCUMENT_ID);
		projections.add(PersistentBean.LOCK_NAME);
		projections.add(PersistentBean.TAGGED_NAME);
		projections.add(PersistentBean.FLAG_COMMENT_NAME);
		projections.add(Bean.BIZ_KEY);
		projections.add(Content.attributeNamePropertyName);
		projections.add(Content.contentBizIdPropertyName);
		projections.add(Content.contentIdPropertyName);
		projections.add(Content.customerNamePropertyName);
		projections.add(Content.documentNamePropertyName);
		projections.add(Content.lastModifiedPropertyName);
		projections.add(Content.moduleNamePropertyName);
		projections.add(Content.contentPropertyName);

		MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.customerNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.moduleNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.documentNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.attributeNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.contentBizIdPropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.contentIdPropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.lastModifiedPropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new MetaDataQueryProjectedColumnImpl();
		column.setBinding(Content.contentPropertyName);
		column.setSortable(false);
		columns.add(column);
	}

	/**
	 * Performs the getDescription operation.
	 * @return the operation result
	 */
	@Override
	public String getDescription() {
		return "All Content";
	}

	/**
	 * Performs the getDrivingDocument operation.
	 * @return the operation result
	 */
	@Override
	public Document getDrivingDocument() {
		return drivingDocument;
	}

	/**
	 * Performs the getColumns operation.
	 * @return the operation result
	 */
	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	/**
	 * Performs the getProjections operation.
	 * @return the operation result
	 */
	@Override
	public Set<String> getProjections() {
		return projections;
	}

	/**
	 * Performs the getFilter operation.
	 * @return the operation result
	 */
	@Override
	public Filter getFilter() {
		// not required
		return null;
	}

	/**
	 * Performs the newFilter operation.
	 * @return the operation result
	 */
	@Override
	public Filter newFilter() {
		// not required
		return null;
	}

	/**
	 * Performs the putParameter operation.
	 * @param name the name value
	 * @param value the value value
	 */
	@Override
	public void putParameter(String name, Object value) {
		// not required
	}

	/**
	 * Performs the fetch operation.
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	@SuppressWarnings({"boxing", "java:S3776"}) // complexity OK
	public Page fetch() throws Exception {
		try (ContentManager cm = newContentManager()) {
			int start = getStartRow();
			int end = getEndRow();

			String userName = CORE.getUser().getName();

			List<Bean> rows = new ArrayList<>(end - start);
			ContentIterator it = cm.all().iterator();
			int i = 0;
			while (it.hasNext()) {
				SearchResult hit = it.next();
				String bizCustomer = hit.getCustomerName();
				String bizModule = hit.getModuleName();
				String bizDocument = hit.getDocumentName();
				String bizDataGroupId = hit.getBizDataGroupId();
				String bizUserId = hit.getBizUserId();
				String bizId = hit.getBizId();
				String attributeName = hit.getAttributeName();
				if (canAccessContent(bizCustomer,
						bizModule,
						bizDocument,
						bizDataGroupId,
						bizUserId,
						bizId,
						attributeName)) {
					if (i >= start) {
						String contentId = hit.getContentId();
						Map<String, Object> properties = new TreeMap<>();
						properties.put(Bean.DOCUMENT_ID, (contentId != null) ? contentId : bizId);
						Date lastModified = hit.getLastModified();
						if (lastModified != null) {
							properties.put(PersistentBean.LOCK_NAME, new OptimisticLock(userName, lastModified));
							properties.put(Content.lastModifiedPropertyName, new Timestamp(lastModified));
						} else {
							properties.put(PersistentBean.LOCK_NAME, new OptimisticLock(userName, new Date()));
							properties.put(Content.lastModifiedPropertyName, null);
						}
						properties.put(PersistentBean.TAGGED_NAME, null);
						properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
						properties.put(Bean.BIZ_KEY, "Content");
						properties.put(Content.attributeNamePropertyName, hit.getAttributeName());
						properties.put(Content.contentBizIdPropertyName, bizId);
						properties.put(Content.contentIdPropertyName, contentId);
						properties.put(Content.customerNamePropertyName, bizCustomer);
						properties.put(Content.documentNamePropertyName, bizDocument);
						properties.put(Content.moduleNamePropertyName, bizModule);
						properties.put(Content.contentPropertyName, hit.getExcerpt());
						rows.add(new DynamicBean(Content.MODULE_NAME, Content.DOCUMENT_NAME, properties));

						if (i >= end) {
							break;
						}
					}
					i++;
				}
			}
			Page page = new Page();
			page.setTotalRows(it.getTotalHits());
			page.setRows(rows);
			LOGGER.info("Content Model start = {} : end = {} : size = {} : total rows = {} ",
					start, end, page.getRows().size(), page.getTotalRows());

			Map<String, Object> properties = new TreeMap<>();
			page.setSummary(new DynamicBean(Content.MODULE_NAME, Content.DOCUMENT_NAME, properties));
			return page;
		}
	}

	@SuppressWarnings({ "static-method", "resource" }) // test seam
	protected ContentManager newContentManager() throws DomainException {
		return EXT.newContentManager();
	}

	@SuppressWarnings("static-method") // test seam
	protected boolean canAccessContent(String bizCustomer,
										String bizModule,
										String bizDocument,
										String bizDataGroupId,
										String bizUserId,
										String bizId,
										String attributeName) {
		return AbstractContentManager.canAccessContent(bizCustomer,
				bizModule,
				bizDocument,
				bizDataGroupId,
				bizUserId,
				bizId,
				attributeName);
	}

	/**
	 * Performs the iterate operation.
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		throw new IllegalStateException(NOT_IMPLEMENTED);
	}

	/**
	 * Performs the update operation.
	 * @param bizId the bizId value
	 * @param properties the properties value
	 * @return the operation result
	 * @throws Exception if the operation fails
	 */
	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties)
			throws Exception {
		throw new IllegalStateException(NOT_IMPLEMENTED);
	}

	/**
	 * Performs the remove operation.
	 * @param bizId the bizId value
	 * @throws Exception if the operation fails
	 */
	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException(NOT_IMPLEMENTED);
	}
}
