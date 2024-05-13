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
import org.skyve.util.Util;

import modules.admin.domain.Content;
import modules.admin.domain.DataMaintenance;

public class ContentModel extends ListModel<DataMaintenance> {
	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<MetaDataQueryColumn> columns = new ArrayList<>(1);
	
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
	
	@Override
	public String getDescription() {
		return "All Content";
	}

	@Override
	public Document getDrivingDocument() {
		return drivingDocument;
	}

	@Override
	public List<MetaDataQueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Set<String> getProjections() {
		return projections;
	}

	@Override
	public Filter getFilter() {
		// not required
		return null;
	}

	@Override
	public Filter newFilter() {
		// not required
		return null;
	}
	
	@Override
	public void putParameter(String name, Object value) {
		// not required
	}

	@Override
	public Page fetch() throws Exception {
		try (ContentManager cm = EXT.newContentManager()) {
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
				if (AbstractContentManager.canAccessContent(bizCustomer, 
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
						}
						else {
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
			Util.LOGGER.info(String.format("Content Model start = %d : end = %d : size = %d : total rows = %d ",
											Integer.valueOf(start),
											Integer.valueOf(end),
											Integer.valueOf(page.getRows().size()),
											Long.valueOf(page.getTotalRows())));

			Map<String, Object> properties = new TreeMap<>();
			properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
			page.setSummary(new DynamicBean(Content.MODULE_NAME, Content.DOCUMENT_NAME, properties));
			return page;
		}
	}

	@Override
	public AutoClosingIterable<Bean> iterate() throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties)
	throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}
}
