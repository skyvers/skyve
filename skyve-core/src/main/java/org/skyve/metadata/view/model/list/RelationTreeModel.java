package org.skyve.metadata.view.model.list;

import java.util.ArrayList;
import java.util.List;
import java.util.SortedMap;

import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.impl.metadata.module.query.MetaDataQueryProjectedColumnImpl;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.module.query.MetaDataQueryColumn;

/**
 * A Tree Model that displays any document relations present in the edited document.
 * That is, associations, collections, inverseOnes and inverseManys.
 * The tree grid has one column, the document's bizKey.
 * Update() and remove() are not implemented so make sure your treeGrid is read-only or implement them yourself.
 * 
 * @author mike
 *
 * @param <T>	The edited bean type.
 */
public abstract class RelationTreeModel<T extends Bean> extends InMemoryListModel<T> {
	private static final long serialVersionUID = 7803713350746172725L;

	private List<MetaDataQueryColumn> columns = null;
	private String[] stopDocuments;
	
	/**
	 * Construct a new tree model
	 * @param stopDocuments	Document names to exclude from the tree.
	 */
	public RelationTreeModel(String... stopDocuments) throws Exception {
		this.stopDocuments = stopDocuments;
		Customer c = CORE.getUser().getCustomer();
		Module m = c.getModule(ListModel.ADMIN_MODULE_NAME);
		Document d = m.getDocument(c, ListModel.GENERIC_DOCUMENT_NAME);
		setDrivingDocument(m, d);
	}
	
	@Override
	public List<Bean> getRows() throws Exception {
		return new ArrayList<>(); // empty list ready for the filter to add rows to
	}

	@Override
	public Filter newFilter() throws Exception {
		// This filter adds the rows for the bizParentId given
		return new RelationTreeModelFilter<>(getBean(), stopDocuments);
	}
	
	/**
	 * One column, the "Related" column.
	 */
	@Override
	public List<MetaDataQueryColumn> getColumns() {
		if (columns == null) {
			columns = new ArrayList<>(1);

			MetaDataQueryProjectedColumnImpl column = new MetaDataQueryProjectedColumnImpl();
	        column.setBinding(ListModel.MEMO_1_PROPERTY_NAME);
	        column.setDisplayName("Related");
	        column.setSortable(false);
	        columns.add(column);
		}
		
		return columns;
	}

	/**
	 * Not implemented
	 */
	@Override
	public Bean update(String bizId, SortedMap<String, Object> properties) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}

	/**
	 * Not implemented
	 */
	@Override
	public void remove(String bizId) throws Exception {
		throw new IllegalStateException("NOT IMPLEMENTED");
	}
}
