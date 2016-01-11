package modules.admin.DataMaintenance.models;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.TreeSet;

import modules.admin.domain.Content;
import modules.admin.domain.DataMaintenance;

import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.Bean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.query.QueryColumn;
import org.skyve.metadata.view.model.list.Filter;
import org.skyve.metadata.view.model.list.ListModel;
import org.skyve.metadata.view.model.list.Page;
import org.skyve.persistence.AutoClosingIterable;
import org.skyve.wildcat.content.ContentIterable.ContentIterator;
import org.skyve.wildcat.content.ContentManager;
import org.skyve.wildcat.content.SearchResult;
import org.skyve.wildcat.domain.MapBean;
import org.skyve.wildcat.metadata.module.query.QueryColumnImpl;

public class ContentModel extends ListModel<DataMaintenance> {
	private static final long serialVersionUID = -5285830669475992183L;

	private Document drivingDocument = null;
	private Set<String> projections = new TreeSet<>();
	private List<QueryColumn> columns = new ArrayList<>(1);
	
	public ContentModel() throws Exception {
		Customer c = CORE.getUser().getCustomer();
		drivingDocument = c.getModule(Content.MODULE_NAME).getDocument(c, Content.DOCUMENT_NAME);
		
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
		
		QueryColumnImpl column = new QueryColumnImpl();
		column.setBinding(Content.customerNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
		column.setBinding(Content.moduleNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
		column.setBinding(Content.documentNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
		column.setBinding(Content.attributeNamePropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
		column.setBinding(Content.contentBizIdPropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
		column.setBinding(Content.contentIdPropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
		column.setBinding(Content.lastModifiedPropertyName);
		column.setSortable(false);
		columns.add(column);
		column = new QueryColumnImpl();
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
	public List<QueryColumn> getColumns() {
		return columns;
	}

	@Override
	public Set<String> getProjections() {
		return projections;
	}

	@Override
	public Filter getFilter() throws Exception {
		return null;
	}

	@Override
	public Filter newFilter() throws Exception {
		return null;
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
				if (i >= start) {
					Map<String, Object> properties = new TreeMap<>();
					properties.put(Bean.DOCUMENT_ID, hit.getContentId());
					properties.put(PersistentBean.LOCK_NAME, new OptimisticLock(userName, hit.getLastModified()));
					properties.put(PersistentBean.TAGGED_NAME, null);
					properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
					properties.put(Bean.BIZ_KEY, hit.getContentId());
					properties.put(Content.attributeNamePropertyName, hit.getAttributeName());
					properties.put(Content.contentBizIdPropertyName, hit.getBizId());
					properties.put(Content.contentIdPropertyName, hit.getContentId());
					properties.put(Content.customerNamePropertyName, hit.getCustomerName());
					properties.put(Content.documentNamePropertyName, hit.getDocumentName());
					properties.put(Content.lastModifiedPropertyName, new Timestamp(hit.getLastModified()));
					properties.put(Content.moduleNamePropertyName, hit.getModuleName());
					properties.put(Content.contentPropertyName, hit.getExcerpt());
					rows.add(new MapBean(Content.MODULE_NAME, Content.DOCUMENT_NAME, properties));

					if (i >= end) {
						break;
					}
				}
				i++;
			}
			Page page = new Page();
			page.setTotalRows(it.getTotalHits());
			page.setRows(rows);
System.out.println(start + " : " + end + " : " + page.getRows().size() + " : " + page.getTotalRows());

			Map<String, Object> properties = new TreeMap<>();
			properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
			page.setSummary(new MapBean(Content.MODULE_NAME, Content.DOCUMENT_NAME, properties));
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
