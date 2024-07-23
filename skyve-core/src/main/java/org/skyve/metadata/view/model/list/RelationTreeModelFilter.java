package org.skyve.metadata.view.model.list;

import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.UUID;

import org.apache.commons.lang3.mutable.MutableObject;
import org.skyve.CORE;
import org.skyve.domain.Bean;
import org.skyve.domain.DynamicBean;
import org.skyve.domain.HierarchicalBean;
import org.skyve.domain.PersistentBean;
import org.skyve.domain.app.AppConstants;
import org.skyve.domain.types.OptimisticLock;
import org.skyve.metadata.customer.Customer;
import org.skyve.metadata.model.Attribute;
import org.skyve.metadata.model.Attribute.AttributeType;
import org.skyve.metadata.model.document.Document;
import org.skyve.metadata.model.document.Relation;
import org.skyve.metadata.module.Module;
import org.skyve.metadata.user.User;
import org.skyve.util.BeanVisitor;
import org.skyve.util.Binder;
import org.skyve.util.Icons;

/**
 * This filter produces the child rows for the RelationTreeModel.
 * 
 * @author mike
 *
 * @param <T>	The edited bean type.
 */
public class RelationTreeModelFilter<T extends Bean> extends InMemoryFilter {
	private T bean = null;
	private String parentId = null;
	private Set<String> stopDocuments = new TreeSet<>();
	
	RelationTreeModelFilter(T bean, String... stopDocuments) {
		this.bean = bean;
		if (stopDocuments != null) {
			for (String stop : stopDocuments) {
				this.stopDocuments.add(stop);
			}
		}
	}
	
	@Override
	public void addNull(String binding) {
		if (! HierarchicalBean.PARENT_ID.equals(binding)) {
			super.addNull(binding);
		}
	}

	@Override
	public void addEquals(String binding, String value) {
		if (HierarchicalBean.PARENT_ID.equals(binding)) {
			parentId = value;
		}
		else {
			super.addEquals(binding, value);
		}
	}

	@Override
	public void filter(List<Bean> rows) {
		if (bean != null) {
			if (parentId == null) { // root node
				// Add our bean as the root node
				rows.add(newOutlineFromBean(bean, null));
			}
			else {
				Bean parent = findParent();
				addChildren(rows, parent);
			}
		}
	}

	/**
	 *  Find the bean for the tree node that was just expanded
	 */
	private Bean findParent() {
		MutableObject<Bean> result = new MutableObject<>();
		
		Customer c = CORE.getCustomer();
		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());

		// Find the bean in the object graph
		String searchId = parentId.substring(0, 36); // remove the random ID from the end
		new BeanVisitor(false, true, false) {
			@Override
			protected boolean accept(String binding,
										Document document,
										Document owningDocument,
										Relation owningRelation,
										Bean visitedBean)
			throws Exception {
				if (searchId.equals(visitedBean.getBizId())) {
					result.setValue(visitedBean);
					return false;
				}
				return true;
			}
		}.visit(d, bean, c);

		return result.getValue();
	}
	
	/**
	 * Add the children to the rows to return for the parent node just expanded.
	 */
	private void addChildren(List<Bean> rows, Bean parent) {
		Customer c = CORE.getCustomer();
		Module m = c.getModule(parent.getBizModule());
		Document d = m.getDocument(c, parent.getBizDocument());

		for (Attribute a : d.getAllAttributes(c)) {
			AttributeType at = a.getAttributeType();
			if (AttributeType.association.equals(at) || AttributeType.inverseOne.equals(at)) {
				// Check it is not a document to stop on
				if (! stopDocuments.contains(((Relation) a).getDocumentName())) {
					Bean value = (Bean) Binder.get(parent, a.getName());
					if (value != null) {
						rows.add(newOutlineFromBean(value, parentId));
					}
				}
			}
			else if (AttributeType.collection.equals(at) || AttributeType.inverseMany.equals(at)) {
				// check it is not a document to stop on
				if (! stopDocuments.contains(((Relation) a).getDocumentName())) {
					@SuppressWarnings("unchecked")
					List<Bean> values = (List<Bean>) Binder.get(parent, a.getName());
					if (values != null) {
						for (Bean value : values) {
							rows.add(newOutlineFromBean(value, parentId));
						}
					}
				}
			}
		}
	}

	/**
	 * Generate a DynamicBean for the bean to add as a row.
	 */
	private static DynamicBean newOutlineFromBean(Bean bean, String bizParentId) {
		final User u = CORE.getUser();
		final Customer c = u.getCustomer();
		
		Map<String, Object> properties = new TreeMap<>();
		// concat a random ID to handle cyclic object graphs
		properties.put(Bean.DOCUMENT_ID, bean.getBizId() + UUID.randomUUID().toString());
		properties.put(PersistentBean.LOCK_NAME, new OptimisticLock(u.getName(), new Date()));
		properties.put(PersistentBean.TAGGED_NAME, null);
		properties.put(PersistentBean.FLAG_COMMENT_NAME, null);
		properties.put(HierarchicalBean.PARENT_ID, bizParentId);

		Module m = c.getModule(bean.getBizModule());
		Document d = m.getDocument(c, bean.getBizDocument());
		String icon = d.getIconStyleClass();
		
		properties.put(AppConstants.MEMO_1_ATTRIBUTE_NAME,
						String.format("<i class=\"%s\"></i>&nbsp;%s",
										(icon == null) ? Icons.FONT_DOCUMENT : icon,
										bean.getBizKey()));

		return new DynamicBean(bean.getBizModule(), bean.getBizDocument(), properties);
	}
}
